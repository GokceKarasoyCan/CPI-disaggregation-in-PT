import pandas as pd
import forecast_evaluation as fe

forecast_data = pd.read_parquet("data/forecast_datacg.parquet")
outturn_data = pd.read_parquet("data/outturn_datacg.parquet")

forecast_data['date'] = (
    pd.PeriodIndex(
        forecast_data['date'].str.replace(r'\s+', '', regex=True),
        freq='Q'
    )
    .to_timestamp(how='end')
    .normalize()
)
forecast_data['vintage_date'] = (
    pd.PeriodIndex(
        forecast_data['vintage_date'].str.replace(r'\s+', '', regex=True),
        freq='Q'
    )
    .to_timestamp(how='end')
    .normalize()
)
forecast_data['metric'] = 'pop'

outturn_data['date'] = (
    pd.PeriodIndex(
        outturn_data['date'].str.replace(r'\s+', '', regex=True),
        freq='Q'
    )
    .to_timestamp(how='end')
    .normalize()
)
outturn_data['vintage_date'] = (
    pd.PeriodIndex(
        outturn_data['vintage_date'].str.replace(r'\s+', '', regex=True),
        freq='Q'
    )
    .to_timestamp(how='end')
    .normalize()
)

outturn_data["metric"] = "pop"

forecast_data = fe.ForecastData(forecasts_data = forecast_data, outturns_data = outturn_data)

forecast_data.run_dashboard(host="127.0.0.1")