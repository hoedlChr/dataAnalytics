# Patel et al. (2026), Smartphones, Online Music Streaming, and Traffic Fatalities
# Reproducible analysis of the supplied SQLite database.

library(DBI)
library(tidyr)
library(dplyr)

.db_path = 'unfaelle-spotify.db'
.study_start = as.Date('2017-01-01')
.study_end = as.Date('2022-12-31')

# 1. Paper Table: fix the event cohort before examining the outcomes.
# These are the PAPER'S album-level stream counts, not the US top-200 song sums.
# There is no album ID/membership in the supplied Spotify tables, so the cohort
# cannot be independently ranked from those tables. Track release_date is not
# sufficient: singles, reissues and deluxe editions can have different dates.

paper_albums = tibble(
  event=1:10,
  album=c('Midnights','Certified Lover Boy','Un Verano Sin Ti','Scorpion',
          'Mr. Morale & the Big Steppers',"Harry's House",'Her Loss','Donda',
          "Red (Taylor's Version)",'Folklore'),
  artist=c('Taylor Swift','Drake','Bad Bunny','Drake','Kendrick Lamar',
           'Harry Styles','Drake and 21 Savage','Kanye West','Taylor Swift','Taylor Swift'),
  release_date=as.Date(c('2022-10-21','2021-09-03','2022-05-06','2018-06-29',
                         '2022-05-13','2022-05-20','2022-11-04','2021-08-29',
                         '2021-11-12','2020-07-24')),
  paper_first_day_streams=c(184695609,153441565,145811373,132384203,99582729,
                            97621794,97390844,94455883,90556180,79443136))

# 2. SQL sums FATALS (people killed). Aggregate streams before joining any artist table:
# a song can have multiple artists and a naive join would multiply its streams.

.get_raw_daily_data = function(db_file=.db_path) {
  con = DBI::dbConnect(RSQLite::SQLite(),
                       db_file,
                       flags=RSQLite::SQLITE_RO)
  
  daily_sql =
    "SELECT date, COUNT(*) AS crashes, SUM(FATALS) AS fatalities
   FROM nhtsa_accidents 
   GROUP BY date ORDER BY date"
  
  stream_sql =
    "SELECT r.date,
            SUM(r.streams)/1e6 AS streams_m,
            SUM(CASE WHEN r.date = t.release_date
                    THEN r.streams ELSE 0 END)/1e6 AS new_streams_m,
            SUM(CASE WHEN r.date <> t.release_date
                    THEN r.streams ELSE 0 END)/1e6 AS old_streams_m
    FROM spotify_ranking AS r
    JOIN spotify_track AS t ON t.id = r.track_id
    GROUP BY r.date
    ORDER BY r.date"

  daily_fars = DBI::dbGetQuery(con,daily_sql) |> mutate(date=as.Date(date))
  daily_spotify = DBI::dbGetQuery(con,stream_sql) |> mutate(date=as.Date(date))
  
  DBI::dbDisconnect(con)
  list(fars=daily_fars, spotify=daily_spotify)
}

# 3. Calendar controls. Stata-style weeks are Jan 1-7 = week 1, ...,
# capped at 52 (last week has 8/9 days), not ISO calendar weeks.
# Holiday convention is explicit: actual national holidays AND observed weekdays.
# Include Juneteenth only from 2021. No state/local/inauguration holidays.

.dow_num <- function(x) as.integer(format(x,'%w')) # Sunday 0 ... Saturday 6

.holiday_calendar <- function(years) {
  one_year <- function(y) {
    nth <- function(m,w,n) {
      d <- as.Date(sprintf('%04d-%02d-01',y,m)); d+(w-.dow_num(d))%%7+7*(n-1)
    }
    memorial <- as.Date(sprintf('%04d-05-31',y))
    memorial <- memorial-(.dow_num(memorial)-1)%%7
    fixed <- as.Date(sprintf('%04d-%s',y,c('01-01','07-04','11-11','12-25')))
    names(fixed) <- c('New Year','Independence','Veterans','Christmas')
    if(y>=2021) fixed <- c(fixed,Juneteenth=as.Date(sprintf('%04d-06-19',y)))
    observed <- fixed+ifelse(.dow_num(fixed)==6,-1,ifelse(.dow_num(fixed)==0,1,0))
    tibble(date=c(unname(fixed),unname(observed),nth(1,1,3),nth(2,1,3),
                  memorial,nth(9,1,1),nth(10,1,2),nth(11,4,4)),
           holiday=c(names(fixed),names(fixed),'MLK','Washington','Memorial',
                     'Labor','Columbus','Thanksgiving'))
  }
  bind_rows(lapply(years,one_year)) |> distinct(date,holiday)
}

holidays <- .holiday_calendar(2016:2023)

get_daily_data = function(db_file=.db_path) {
  raw_data = .get_raw_daily_data(db_file)
  left_join(raw_data$fars, raw_data$spotify,by='date') |>
    mutate(dow=factor(.dow_num(date)),
           week=factor(pmin(52L,(as.integer(format(date,'%j'))-1L)%/%7L+1L)),
           year=factor(format(date,'%Y')),
           holiday=as.integer(date %in% holidays$date))
}


# 4. Stack 21 dates per event, including duplicate calendar dates in overlapping
# event windows. That is the literal album-day description in the article.

make_stack <- function(cohort, daily_data, offsets=-10:10) {
  tidyr::crossing(event=cohort$event, rel=offsets) |>
    left_join(cohort, by='event') |>
    mutate(date=release_date+rel,
           release=as.integer(rel == 0),
           rel_f=factor(rel,levels=offsets)) |>
    left_join(daily_data, by='date')
}

make_stack_no_overlap = function(cohort, daily_data, nextday=FALSE, offsets=-10:10) {
  stack = make_stack(cohort, daily_data, offsets)
  stack |>
    group_by(date) |>
    filter(
      if (any(rel == 0))
        rel == 0
      else if (nextday && any(rel == 1))
        rel == 1
      else TRUE) |>
    ungroup()
}




# 5. Linear models and cluster-robust CR1 standard errors, implemented directly
# so sandwich/fixest are not required. CR1 matches the usual Stata OLS correction:
# G/(G-1)*(N-1)/(N-K). Inference uses t(G-1)

.robust_linear_result <- function(fit, L, cluster) {
  X <- model.matrix(fit)
  beta <- coef(fit)
  
  # Keep only estimable coefficients
  keep <- !is.na(beta)
  X <- X[, keep, drop = FALSE]
  beta <- beta[keep]
  L <- L[keep]
  
  n <- nrow(X)
  k <- ncol(X)
  g <- length(unique(cluster))
  
  # Cluster-robust covariance matrix
  bread <- solve(crossprod(X))
  scores <- rowsum(
    X * residuals(fit),
    cluster,
    reorder = FALSE
  )
  
  V <- (g / (g - 1)) * ((n - 1) / (n - k)) *
    bread %*% crossprod(scores) %*% bread
  
  est <- drop(L %*% beta)
  se  <- sqrt(max(0, drop(L %*% V %*% L)))
  
  crit <- qt(.975, df = g - 1)
  
  tibble(
    estimate = est,
    se = se,
    lower = est - crit * se,
    upper = est + crit * se,
    p = if (se > 0)
      2 * pt(-abs(est / se), df = g - 1)
    else
      NA_real_
  )
}

.robust_coef <- function(fit, term, cluster) {
  beta <- coef(fit)
  L <- setNames(numeric(length(beta)), names(beta))
  
  # Reference factor level => difference to itself is zero
  if (term %in% names(beta))
    L[term] <- 1
  
  .robust_linear_result(fit, L, cluster)
}

.robust_pred <- function(fit, data, param, val, cluster) {
  nd <- data
  
  if (is.factor(nd[[param]])) {
    nd[[param]] <- factor(val, levels = levels(nd[[param]]))
  } else {
    nd[[param]] <- val
  }
  
  # Construct the design matrix for the counterfactual data
  Xnew <- model.matrix(
    delete.response(terms(fit)),
    data = nd,
    contrasts.arg = fit$contrasts,
    xlev = fit$xlevels
  )
  
  # Average design vector = average predicted outcome
  L <- colMeans(Xnew)
  
  # Make sure it exactly matches coef(fit)
  Lfull <- setNames(
    numeric(length(coef(fit))),
    names(coef(fit))
  )
  Lfull[names(L)] <- L
  
  .robust_linear_result(fit, Lfull, cluster) |>
    mutate(param = param, value = val, .before = 1)
}

.get_coef_names = function(model, data, variable) {
  mm = model.matrix(model)
  assign = attr(mm, "assign")
  terms = attr(terms(model), "term.labels")
  idx = which(terms == variable)
  names = colnames(mm)[assign == idx]
  if (is.factor(data[[variable]]))
    c(paste0(variable, levels(data[[variable]])[1]), names)
  else
    names
}

robust_coefs = function(formula, data, variable, cluster="event") {
  model = lm(formula, data=data)
  names = .get_coef_names(model, data, variable)
  purrr::map_dfr(names, function(name) {
    res = .robust_coef(model, name, data[[cluster]])
    tibble::add_column(coef=name, res, .before = T)
  })
}

robust_preds = function(formula, data, variable, values=NULL, cluster="event") {
  model = lm(formula, data=data)
  if (is.null(values)) {
    if (is.factor(data[[variable]]))
      values = levels(data[[variable]])
    else
      values = 1
  } 
  purrr::map_dfr(values, function(value) {
    .robust_pred(model, data, variable, value, data[[cluster]])
  })
}


# 6. Album-specific comparison, eFigure 6: use same-weekday +/-1,2,3 weeks,
# excluding holidays and dates within 3 days of another included album release.
# CI uses the control-day mean's SE only, as in the supplement; it does not
# incorporate uncertainty in the single observed release-day count.

.get_album_controls = function(albums, reference_days) {
  bind_rows(lapply(1:nrow(albums), function(i) {
    ds = albums$release_date[i] + reference_days
    other = albums$release_date[-i]
    tibble(event=albums$event[i],
           date=ds,
           holiday=ds %in% holidays$date,
           near_other=sapply(ds,
                             function(z) any(abs(as.numeric(z-other))<=3)),
           included=!holiday & !near_other)
  }))
}

get_album_results = function(albums, reference_days=c(-21,-14,-7,7,14,21), fars=daily) {
  .get_album_controls(albums, reference_days) |>
    left_join(fars |> select(date, fatalities), by='date') |>
    filter(included) |>
    group_by(event) |>
    summarise(n_controls=n(),
              control_mean=mean(fatalities),
              control_se=sd(fatalities)/sqrt(n()),
              .groups='drop') |>
    left_join(albums |> select(event, album, artist, release_date),
              by='event') |>
    left_join(fars |> select(release_date=date,
                             release_deaths=fatalities,
                             release_streams_m=streams_m),
              by='release_date') |>
    mutate(difference=release_deaths - control_mean,
           lower=difference-qt(.975, n_controls-1)*control_se,
           upper=difference+qt(.975, n_controls-1)*control_se)
}


#####################

# Identify releases based on certain criteria

identify_releases = function(
    increase_threshold=0.20,
    top_n=15L,
    min_artist_tracks=5L,
    min_recent_tracks=5L,
    release_lookback=2L,
    fars=daily) {
  
  # Seven calendar lags; NA propagates if even one previous date is unavailable.
  previous = do.call(cbind,
                     lapply(1:7,function(k) dplyr::lag(fars$streams_m, k)))
  fars$previous_7_day_mean = rowMeans(previous, na.rm=FALSE)
  surge = !is.na(fars$previous_7_day_mean) &
    fars$streams_m >= (1+increase_threshold) * fars$previous_7_day_mean
  candidate_dates = fars$date[surge]
  
  con = DBI::dbConnect(RSQLite::SQLite(),.db_path,flags=RSQLite::SQLITE_RO)
  on.exit(DBI::dbDisconnect(con), add=TRUE)
  
  ranking <- DBI::dbGetQuery(con,
                             'SELECT date, track_id, rank, streams/1e6 as streams_m FROM spotify_ranking
     WHERE date BETWEEN ? AND ?
     ORDER BY date, rank',
                             params=list(as.character(.study_start-7L), as.character(.study_end)))
  ranking$date <- as.Date(ranking$date)
  tracks = DBI::dbGetQuery(con,
                           'SELECT id AS track_id, release_date FROM spotify_track')
  tracks$release_date = as.Date(tracks$release_date)
  artists = DBI::dbGetQuery(con,
                            'SELECT id AS artist_id, name AS artist_name FROM spotify_artist')
  links = DBI::dbGetQuery(con,
                          'SELECT DISTINCT track_id, artist_id FROM spotify_track_artist')
  
  credited = ranking |>
    filter(date %in% candidate_dates) |>
    left_join(links, by='track_id', relationship='many-to-many') |>
    left_join(tracks,by='track_id', relationship='many-to-one') |>
    mutate(recent = !is.na(release_date) &
             release_date <= date &
             release_date >= date - release_lookback)
  
  # Distinct links above ensure a repeated link cannot make an artist qualify.
  credited |>
    filter(rank <= top_n) |>
    group_by(`date`, artist_id) |>
    summarise(top_tracks=n_distinct(track_id),
              recent_top_tracks=n_distinct(track_id[recent]),
              .groups='drop') |>
    filter(top_tracks >= min_artist_tracks,
           recent_top_tracks >= min_recent_tracks) |>
    select(`date`, artist_id) |>
    inner_join(
      credited |> group_by(`date`, artist_id) |>
        summarise(total_streams_m=sum(streams_m), .groups='drop'),
      by=c('date','artist_id'),
      relationship='one-to-one') |>
    left_join(artists,
              by='artist_id',
              relationship='many-to-one') |>
    arrange(date,
            dplyr::desc(total_streams_m),
            artist_name) |>
    select(date, artist_name, total_streams_m)
}

make_albums_from_releases = function(releases) {
  releases |>
    group_by(date) |>
    summarize(artist=paste(artist_name, collapse=", "),
              streams=sum(total_streams_m)) |>
    mutate(event=row_number(), album=artist) |>
    rename(release_date=date)
}
