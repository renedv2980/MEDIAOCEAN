*          DATA SET TAGENC5    AT LEVEL 006 AS OF 05/01/02                      
*PHASE T702C5A                                                                  
         TITLE 'T702C5 - W4OLD DISPLAY'                                         
T702C5   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T702C5,R7                                                      
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         EJECT                                                                  
*                                                                               
*              MODE CONTROLLED ROUTINES                                         
*                                                                               
         GOTO1 INITIAL,DMCB,0                                                   
*                                                                               
         CLI   MODE,VALKEY         FIRST TIME IN                                
         BNE   W4O10                                                            
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'40',SWOSSNH)                              
         B     XIT                                                              
*                                                                               
         SPACE 3                                                                
W4O10    CLI   MODE,DISPREC        IF MODE IS DISPLAY                           
         BNE   XIT                                                              
         BAS   RE,DISPLAY          DISPLAY THE RECORD                           
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*              DISPLAY THE RECORD                                               
*                                                                               
DISPLAY  NTR1                                                                   
         TWAXC SWOSSNNH                                                         
*                                                                               
         L     R4,AIO              GET W4 ELEMENT                               
         USING TAW4D,R4                                                         
         MVI   ELCODE,TAW4ELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         GOTO1 CHAROUT,DMCB,TANAELQ,SWOSSNNH      NAME                          
         SPACE                                                                  
         GOTO1 CHAROUT,DMCB,TAADELQ,(4,SWOADDRH)  ADDRESS                       
         SPACE                                                                  
         B     XIT                                                              
         EJECT                                                                  
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
*                                                                               
XIT      XIT1                                                                   
         SPACE 3                                                                
*                                                                               
MISSERR  MVI   ERROR,MISSING       MISSING INPUT                                
         B     ERRXIT                                                           
*                                                                               
ERRXIT   XC    DMCB,DMCB                                                        
*                                                                               
ERRXIT2  GOTO1 EXIT,DMCB                                                        
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*                                                                               
*              CONSTANTS, ETC.                                                  
*                                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRC5D                                                       
         EJECT                                                                  
*                                                                               
         SPACE 5                                                                
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TASYSEQUS                                                                     
* TASYSDSECT                                                                    
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* TAGENEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE TAGENEQUS                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006TAGENC5   05/01/02'                                      
         END                                                                    
