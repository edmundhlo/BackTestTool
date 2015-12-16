library( R6)
Moo changes
# Class to characterize database table that holds a given Data Item
Strategy <- R6Class(
  
  public = list(
    
    initialize = function( )
    {
      private$selections = list();
      private$allocations = list();
      private$rebalance_dates = list();
    },
    
    set_security_selection_alogorithm = function( function_pointer_to_selection_alogorithm )
    {sss
      private$function_pointer_to_selection_alogorithm = function_pointer_to_selection_alogorithm;
    },
    
    set_allocation_scheme = function( function_pointer_to_allocation_scheme )
    {
      private$function_pointer_to_allocation_scheme = function_pointer_to_allocation_scheme;
    },
    
    set_investment_universe = function()
    {
      
    },
    
    back_test = function( start_date,  end_date, rebalance_frequency )
    {
      valid_trading_days = RebalanceFrequency$new();
      
      
      rebalance_dates = valid_trading_days$list_first_trading_days( start_date, end_date, rebalance_frequency  );
      
      last_performance_report_date = tail( valid_trading_days$list_last_trading_days( start_date, end_date, rebalance_frequency  )$caldt, 1);
      

      portfolio_history = BackTest$new( rebalance_dates$caldt[1] );
      
      private$selections = list();
      private$allocations = list();
      private$rebalance_dates = list();
      

      
      for( d in 1: nrow(rebalance_dates)   )
      {

        private$rebalance_dates = append( private$rebalance_dates, (rebalance_dates$caldt[d]), after = length(private$rebalance_dates ) );
        
         security_selection = private$function_pointer_to_selection_alogorithm( rebalance_dates$caldt[d], rebalance_frequency);
         private$selections = append( private$selections, (security_selection), after = length(private$selections ) );
         
         
         allocation = private$function_pointer_to_allocation_scheme(  security_selection, rebalance_dates$caldt[d], rebalance_dates$caldt[d] );
         private$allocations = append( private$allocations, (allocation), after = length(private$allocations ) );
         


         portfolio_history$createRebalancePoint( as.Date( rebalance_dates$caldt[d]) , allocation);
        
         
         message( paste0( "Done ", as.Date( rebalance_dates$caldt[d]) ) );
         
      }
      
      if( nrow(rebalance_dates) > 0 )
      {
        portfolio_history$createRebalancePoint( as.Date(last_performance_report_date), allocation);
      }
      
   
      
   #   port_perf = portfolio_history$calculateHistoricalPortfolioReturns( );
      
      return( portfolio_history);
      
      
    },
    
    
    
    select_securities = function( start_date,  end_date, rebalance_frequency )
    {
      valid_trading_days = RebalanceFrequency$new();
      
      
      rebalance_dates = valid_trading_days$list_first_trading_days( start_date, end_date, rebalance_frequency  );
      
      last_performance_report_date = tail( valid_trading_days$list_last_trading_days( start_date, end_date, rebalance_frequency  )$caldt, 1);
      
      
      portfolio_history = BackTest$new( rebalance_dates$caldt[1] );
      
      private$selections = list();
      private$allocations = list();
      private$rebalance_dates = list();
      
      
      
      for( d in 1: nrow(rebalance_dates)   )
      {
        
        private$rebalance_dates = append( private$rebalance_dates, (rebalance_dates$caldt[d]), after = length(private$rebalance_dates ) );
        
        security_selection = private$function_pointer_to_selection_alogorithm( rebalance_dates$caldt[d], rebalance_frequency);
        private$selections = append( private$selections, (security_selection), after = length(private$selections ) );
        
        
        message( paste0( "Done ", as.Date( rebalance_dates$caldt[d]) ) );
        
      }
      

      return( security_selection)
      
      
    },
    compute_allocations= function(  )
    {

      
      
      
      for( d in 1: length( private$rebalance_dates )   )
      {
        

       
        
        allocation = private$function_pointer_to_allocation_scheme(  private$selections[[d]], private$rebalance_dates[[d]], private$rebalance_dates[[d]] );
        private$allocations = append( private$allocations, (allocation), after = length(private$allocations ) );
        
        
        message( paste0( "Done ", as.Date( private$rebalance_dates[[d]]) ) );
        
      }
      
      
      return( allocation)
      
      
    },

    
    
    getSelections = function( )
    {
      return( private$selections);
    },
    getAllocations = function( )
    {
      return( private$allocations);
    },
    getRebalanceDates = function( )
    {
      return( private$rebalance_dates);
    }
  ),
  
  private = list(
    
    rebalance_dates = "list",
    selections = "list",
    allocations = "list",
    function_pointer_to_selection_alogorithm = "closure",
    function_pointer_to_allocation_scheme = "closure"

    
    
  )
)
    
  