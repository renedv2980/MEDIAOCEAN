*          DATA SET SPINF41    AT LEVEL 001 AS OF 07/16/79                      
*PHASE T21A41A,+0,NOAUTO                                                        
         TITLE 'T21A41 - SPOTPAK INFO SPILL DEFINITION'                         
*                                                                               
*        SVKEY HAS FOLLOWING UPON ENTRY                                         
*              0-1  X'0D13'                                                     
*              2-3  ALPHA AGENCY                                                
*                5  0=BBM,1=CSI                                                 
*              6-10 STATION / 0 FOR ALL STATIONS                                
*            11-13 CLIENT/ ALL=ALL CLIENTS / 0=DEFAULT RECORDS                  
*                                                                               
T21A41   CSECT                                                                  
         PRINT NOGEN                                                            
