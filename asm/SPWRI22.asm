*          DATA SET SPWRI22    AT LEVEL 003 AS OF 08/11/00                      
*PHASE T20422A                                                                  
         TITLE 'T20422 - SPOTPAK WRITER SUMMARY RECORD OVERLAY'                 
T20422   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T20422                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R9,ASYSD                                                         
         USING SYSD,R9             SYSTEM SPECIFIC WORK                         
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
*                                                                               
         CLI   MODE,VALREC         VALIDATE REQUEST                             
         BE    VREC                                                             
*                                                                               
XIT      XIT1  ,                                                                
         EJECT                                                                  
* VALIDATE RECORD                                                               
*                                                                               
VREC     LA    R2,SUMHEADH         HEADERS                                      
         MVI   MAX,5                                                            
         GOTO1 VALHEAD                                                          
*                                                                               
         LA    R2,SUMMIDH          MIDLINE                                      
         MVI   MAX,1                                                            
         GOTO1 VALMID                                                           
*                                                                               
         LA    R2,SUMROWSH         ROWS                                         
         MVI   MAX,6                                                            
         GOTO1 VALROWS                                                          
*                                  COLUMNS                                      
         LA    R2,SUMCOLSH                                                      
         MVI   MAX,14                                                           
         GOTO1 VALCOLS                                                          
*                                                                               
         GOTO1 WRAPDRON            WRAP UP DRONE                                
*                                                                               
VRECX    B     XIT                                                              
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
* OTHER DSECTS ARE HIDDEN IN HERE                                               
*                                                                               
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*DDMASTD                                                                        
*DRGLOBAL                                                                       
*DRIVETABLE                                                                     
*DRINTRECD                                                                      
*SPWRIWORKD                                                                     
*SPWRIFFD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DRGLOBAL                                                       
       ++INCLUDE DRIVETABLE                                                     
       ++INCLUDE DRINTRECD2                                                     
       ++INCLUDE SPWRIWORKD                                                     
       ++INCLUDE SPWRIFFD                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE SPWRIF3D                                                       
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDTWADCOND                                                     
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003SPWRI22   08/11/00'                                      
         END                                                                    
