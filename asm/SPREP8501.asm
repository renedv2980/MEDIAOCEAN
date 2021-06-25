*          DATA SET SPREP8501  AT LEVEL 007 AS OF 08/29/00                      
*PHASE SP8501A                                                                  
         TITLE 'SP085-01  SPOT ADJ FACTOR PRINT'                                
SP8501   CSECT                                                                  
         PRINT NOGEN                                                            
         SPACE                                                                  
         FSPEC USE,SPRQ03                                                       
         FSPEC OPEN,DEMFILES                                                    
         RSPEC REQUEST,NOREP                                                    
         SPACE                                                                  
         SSPEC H1,99,AGYNAME                                                    
         SSPEC H2,99,AGYADD                                                     
         SSPEC H3,99,REPORT                                                     
         SSPEC H4,99,PAGE                                                       
         SPACE                                                                  
         SSPEC H1,1,C'MEDIA'                                                    
         SSPEC H2,1,C'RTG SRCE'                                                 
         SSPEC H3,1,C'CLIENT'                                                   
         SSPEC H4,1,C'MARKET'                                                   
         SPACE                                                                  
         SSPEC H1,50,C'SEASONAL  VARIANCE  INDICES'                             
         SSPEC H2,50,C'---------------------------'                             
         SSPEC H6,1,C'TO DERIVE FACTORS, DIVIDE THE "TO" MONTH BY THE "X        
               FROM" MONTH.  FOR EXAMPLE, IF YOU WISHED TO ADJUST'              
         SSPEC H7,5,C'THE FEBRUARY BOOK TO APRIL, DIVIDE THE APRIL INDEX        
               X BY THE FEBRUARY INDEX.'                                        
         SSPEC H9,1,C'DAY'                                                      
         SSPEC H10,1,C'---'                                                     
         SSPEC H9,9,C'TIME'                                                     
         SSPEC H10,9,C'----'                                                    
         SSPEC H9,29,C'AUD TYPE'                                                
         SSPEC H10,29,C'--------'                                               
         SSPEC H9,42,C'JAN     FEB     MAR     APR     MAY     JUN     X        
               JUL     AUG     SEP     OCT     NOV     DEC'                     
         SSPEC H10,42,C'---     ---     ---     ---     ---     ---    X        
                ---     ---     ---     ---     ---     ---'                    
                SPACE                                                           
         DC    X'00'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007SPREP8501 08/29/00'                                      
         END                                                                    
