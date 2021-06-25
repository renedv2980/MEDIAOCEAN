*          DATA SET TAGENBF    AT LEVEL 003 AS OF 11/26/97                      
*PHASE T702BFA,*                                                                
         TITLE 'T702BF - DISPLAY STANDARD RATE CARD'                            
T702BF   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T702BF                                                         
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
         SPACE 1                                                                
         GOTO1 INITIAL,DMCB,0                                                   
         SPACE 1                                                                
         CLI   MODE,VALKEY                                                      
         BNE   XIT                                                              
         TWAXC SRCFRSTH,PROT=Y     CLEAR SCREEN                                 
*                                                                               
         L     R2,TGABTYP          R2=A(BILLING TYPE TABLE)                     
         USING BTYPTABD,R2                                                      
         LA    R3,SRCFRSTH         R3=A(FIRST LINE)                             
         USING LINED,R3                                                         
*                                  DISPLAY BILLING TYPE                         
SRC10    EDIT  (1,BTYPCDE),(2,LINBTYP)                                          
*                                                                               
         LA    R4,BTYPSRC          R4 = A(RATES)                                
         LA    R5,LINRATES         R5 = A(FIRST SCREEN FIELD)                   
         LA    RF,7                DISPLAY THE 1ST SEVEN                        
*                                                                               
SRC20    EDIT  (2,0(R4)),(5,(R5)),2,ZERO=NOBLANK                                
*                                                                               
         LA    R4,2(R4)            BUMP TO NEXT RATE                            
         LA    R5,8(R5)            AND TO NEXT FIELD IN LINE                    
         LA    R1,LINWCORP                                                      
         CR    R5,R1               SKIP WC FOR COPRS FOR NOW                    
         BE    *-10                                                             
         BCT   RF,SRC20                                                         
*                                                                               
         EDIT  (2,BTYPWCRP),(5,LINWCORP),2,ZERO=NOBLANK                         
*                                                                               
         LA    R2,BTYPNEXT         BUMP TO NEXT ENTRY IN TABLE                  
         LA    R3,LINNEXT          AND TO NEXT LINE IN SCREEN                   
         CLI   0(R2),X'FF'                                                      
         BNE   SRC10               CONTINUE IF NOT END OF TABLE                 
         B     DISPLYD                                                          
         EJECT                                                                  
*              EXITS                                                            
         SPACE 2                                                                
DISPLYD  MVI   MYMSGNO1,78         STANDARD RATE CARD DISPLAYED                 
         OI    GENSTAT2,USGETTXT                                                
         L     R2,EFHREC                                                        
         B     THEEND                                                           
         SPACE 1                                                                
THEEND   GOTO1 EXIT,DMCB,0                                                      
         SPACE 2                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              DSECT TO COVER SCREEN LINE                                       
         SPACE 2                                                                
LINED    DSECT                                                                  
         DS    CL8                                                              
         DS    CL2                                                              
LINBTYP  DS    CL2                 BILLING TYPE                                 
         DS    CL4                                                              
LINRATES DS    CL70                BILLING RATES                                
         ORG   LINRATES+32                                                      
LINWCORP DS    CL5                 WC FOR CORPS IS STUCK IN THE MIDDLE          
         ORG                                                                    
LINNEXT  EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRBFD                                                       
         EJECT                                                                  
* DDGENTWA                                                                      
* TAGENWORKD                                                                    
* TASYSDSECT                                                                    
* TAGENEQUS                                                                     
* TAGENFILE                                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TAGENEQUS                                                      
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003TAGENBF   11/26/97'                                      
         END                                                                    
