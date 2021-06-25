*          DATA SET ACBIL40    AT LEVEL 002 AS OF 03/19/08                      
*PHASE T60E40A                                                                  
         TITLE 'ACBIL40 - CREATIVE BILLING - ADDTRN I/O AREAS'                  
         SPACE 1                                                                
T60E40   CSECT                                                                  
         PRINT NOGEN                                                            
         DC    A(MYTABLE-T60E40)   ADDRESS OF   FIRST ENTRY                     
         DC    A(MYTABLE#)         NUMBER  OF   ENTRIES                         
*                                                                               
MYTABLE  DS    0A                                                               
         DC    A(BTKEY-T60E40)                                                  
         DC    A(ABTKEY-GWS)                                                    
MYTABLNQ EQU   *-MYTABLE                                                        
*                                                                               
         DC    A(BTPPK-T60E40)                                                  
         DC    A(ABTPP-GWS)                                                     
*                                                                               
         DC    A(BTDIO-T60E40)                                                  
         DC    A(ABTDIO-GWS)                                                    
*                                                                               
         DC    A(DMWORK-T60E40)                                                 
         DC    A(ADMWORK-GWS)                                                   
*                                                                               
         DC    A(TRNIO-T60E40)                                                  
         DC    A(ATIO-GWS)                                                      
*                                                                               
         DC    A(BATIO-T60E40)                                                  
         DC    A(ABIO-GWS)                                                      
*                                                                               
         DC    A(ACTIO-T60E40)                                                  
         DC    A(AACTIO-GWS)                                                    
*                                                                               
         DC    A(BUKIO-T60E40)                                                  
         DC    A(ABUKIO-GWS)                                                    
*                                                                               
         DC    A(CNTIO-T60E40)                                                  
         DC    A(ACNTIO-GWS)                                                    
*                                                                               
         DC    A(OFFIO-T60E40)                                                  
         DC    A(AOFFIO-GWS)                                                    
*                                                                               
MYTABLE# EQU   (*-MYTABLE)/MYTABLNQ                                             
*                                                                               
         DS    0D                                                               
BTKEY    DC    XL(L'TBAKEY)'00'    BATCH     KEY  AREA                          
*                                  PASSIVE   POINTER     KEY     AREA           
BTPPK    DC    XL(TBAKDA-TBARECD+L'TBAKDA)'00'                                  
*                                  DIRECTORY I/O  KEY    AREA                   
BTDIO    DC    XL(TBAKDA-TBARECD+L'TBAKDA)'00'                                  
*                                                                               
DMWORK   DC    XL96'00'            DATA MANAGER   WORK   AREA                   
*                                                                               
         DS    0D                                                               
         DC    CL8'ADDTRNIO'                                                    
TRNIO    DC    (RECLNQ)X'00'       I/O  AREA FOR  TRANSACTION    I/O            
BATIO    DC    (RECLNQ)X'00'       I/O  AREA FOR  BATCH  HEADER  I/O            
ACTIO    DC    (RECLNQ)X'00'       I/O  AREA FOR  ACCOUNT        I/O            
BUKIO    DC    (RECLNQ)X'00'       I/O  AREA FOR  BUCKET         I/O            
CNTIO    DC    (RECLNQ)X'00'       I/O  AREA FOR  CONTRA         I/O            
OFFIO    DC    (RECLNQ)X'00'       I/O  AREA FOR  OFFICE ACCOUNT I/O            
*                                                                               
         LTORG                                                                  
         EJECT ,                                                                
         SPACE 1                                                                
*                                                                               
*ACBILDSECT                                                                     
       ++INCLUDE ACBILDSECT                                                     
*ACBILWORK                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACBILWORK                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002ACBIL40   03/19/08'                                      
         END                                                                    
