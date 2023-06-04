
def obt_stock_hist(stock,country, from_d, to_d, as_json,order,interval):    
    import investpy
    data = investpy.stocks.get_stock_historical_data(stock=stock, country=country, from_date=from_d, to_date=to_d, as_json=as_json, order=order, interval=interval)
    return(data)
  
