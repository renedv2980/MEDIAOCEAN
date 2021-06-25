*          DATA SET PPREP4901  AT LEVEL 017 AS OF 02/20/07                      
*PHASE PP4901A,+0                                                               
         TITLE 'PP49-01 - PRINTPAK AGENCY SUMMARY - PPG SPECS'                  
PP4901   CSECT                                                                  
         SPACE 2                                                                
         PRINT NOGEN                                                            
*                                  ALL MODES                                    
         SPROG 10,11,30,40,41,60                                                
         SPACE 1                                                                
         PSPEC H1,1,MEDIA                                                       
         PSPEC H4,99,REPORT                                                     
         PSPEC H5,99,RUN                                                        
         PSPEC H5,125,PAGE                                                      
         SPACE 2                                                                
         SPROG 10,11,30,40,41                                                   
         SPACE 1                                                                
         PSPEC H3,53,C'PERIOD FROM'                                             
         PSPEC H3,72,C'TO'                                                      
         SPACE 2                                                                
         SPROG 10,30                                                            
         PSPEC     H7,1,C'           PCT  GROSS ORDERED  GROSS ORDERED X        
                    CASH      GROSS LESS'                                       
         PSPEC H8,1,CL132'PERIOD    CLRD  INSERT MONTHS    BILL MONTHS X        
                  DISCOUNT  CASH DISCOUNT        CLEARED      UNCLEAREDX        
                        BILLED       BILLABLE '                                 
         PSPEC H9,1,CL132'------    ----  -------------  ------------- X        
                  --------  -------------        -------      ---------X        
                        ------       -------- '                                 
         SPACE 2                                                                
         SPROG 11                                                               
         PSPEC     H7,1,C'           PCT    NET ORDERED    NET ORDERED X        
                    CASH        NET LESS'                                       
         PSPEC H8,1,CL132'PERIOD    CLRD  INSERT MONTHS    BILL MONTHS X        
                  DISCOUNT  CASH DISCOUNT        CLEARED      UNCLEAREDX        
                        BILLED       BILLABLE '                                 
         PSPEC H9,1,CL132'------    ----  -------------    ----------- X        
                  --------  -------------        -------      ---------X        
                        ------       -------- '                                 
         SPACE 2                                                                
         SPROG 40                                                               
         PSPEC     H7,1,C'           PCT  GROSS PLN/ORD  GROSS PLN/ORD X        
                    CASH    PLN/ORD GROSS'                                      
         PSPEC H8,1,CL132'PERIOD    CLRD  INSERT MONTHS    BILL MONTHS X        
                  DISCOUNT  LESS CSH DISC        CLEARED      UNCLEAREDX        
                        BILLED       BILLABLE '                                 
         PSPEC H9,1,CL132'------    ----  -------------  ------------- X        
                  --------  -------------        -------      ---------X        
                        ------       -------- '                                 
         SPACE 2                                                                
         SPROG 41                                                               
         PSPEC     H7,1,C'           PCT    NET PLN/ORD    NET PLN/ORD X        
                    CASH    PLN/ORD NET'                                        
         PSPEC H8,1,CL132'PERIOD    CLRD  INSERT MONTHS    BILL MONTHS X        
                  DISCOUNT  LESS CSH DISC        CLEARED      UNCLEAREDX        
                        BILLED       BILLABLE '                                 
         PSPEC H9,1,CL132'------    ----  -------------    ----------- X        
                  --------  -------------        -------      ---------X        
                        ------       -------- '                                 
         SPACE 2                                                                
*                                  AGENCY SUMMARY                               
         SPROG 10,11,40,41                                                      
         SPACE 1                                                                
         PSPEC H1,60,C'AGENCY SUMMARY'                                          
         PSPEC H2,60,C'--------------'                                          
         SPACE 2                                                                
         SPROG 10,11,20,40,41,50,60                                             
         SPACE 1                                                                
         PSPEC H1,99,AGYNAME                                                    
         PSPEC H2,99,AGYADD                                                     
         SPACE 2                                                                
         SPACE 2                                                                
*                                  DDS SUMMARY                                  
         SPROG 30                                                               
         SPACE 1                                                                
         PSPEC H1,61,C'DDS SUMMARY'                                             
         PSPEC H1,99,C'DONOVAN DATA SYSTEMS, INC.'                              
         PSPEC H2,61,C'-----------'                                             
         SPACE 2                                                                
         SPROG 60                                                               
         PSPEC H1,51,C'INSERTION ORDER REQUEST LIST'                            
         PSPEC H2,51,C'----------------------------'                            
         SPACE 2                                                                
         DC    X'0000'                                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017PPREP4901 02/20/07'                                      
         END                                                                    
