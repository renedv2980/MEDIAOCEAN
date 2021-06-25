*          DATA SET NEMED42    AT LEVEL 019 AS OF 04/15/99                      
*PHASE T31E42A,+0                                                               
T31E42   TITLE '-  EDIT FOR HUTLIST'                                            
T31E42   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**HUED**                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T31EFFD,RA                                                       
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS2          W/S IN W/S AREA 2                            
         USING HUTCOM,R7                                                        
         EJECT                                                                  
*              EDIT FIELDS                                                      
         SPACE 3                                                                
         LA    R2,HUTSRCEH         SOURCE                                       
         MVI   SOURCE,C'N'                                                      
         MVI   SCHEME,0                                                         
         CLC   8(3,R2),=C'NTI'     DEFAULT IS POCKET-PIECE                      
         BE    EDIT2                                                            
         CLI   5(R2),0                                                          
         BE    EDIT2                                                            
         MVI   SOURCE,C'-'         - MEANS NO HUTLIST                           
         CLI   8(R2),C'-'                                                       
         BE    ED8                                                              
         MVI   SOURCE,C'C'                                                      
         CLC   8(3,R2),=C'CON'     CONTROL FOR TESTING                          
         BE    EDIT2                                                            
         MVI   SOURCE,C'A'         OTHERWISE IT'S AGENCY SCHEME                 
         MVC   SCHEME,8(R2)                                                     
         SPACE 1                                                                
EDIT2    LA    R2,HUTDAYH                                                       
         NETGO NVDAY,DMCB,DAYFILT  VALIDATE DAY                                 
*                                                                               
         LA    R2,HUTTIMEH                                                      
         MVC   TIMFILT(2),=H'30'                                                
         MVC   TIMFILT+2(2),=H'2400'                                            
         CLI   5(R2),0                                                          
         BE    ED2B                                                             
         CLC   8(3,R2),=C'ALL'                                                  
         BE    ED2B                                                             
         ZIC   R4,5(R2)                                                         
         GOTO1 TIMVAL,PARAS,((R4),8(R2)),TIMFILT                                
         CLI   0(R1),X'FF'                                                      
         BE    ERRTIME                                                          
         B     ED2B                                                             
ERRTIME  MVI   ERROR,INVTIME                                                    
         B     EDERR                                                            
*                                                                               
EDERR    GOTO1 ERREX,DMCB                                                       
*                                                                               
ED2B     LA    R2,HUTYEARH                                                      
         GOTO1 ANY                                                              
         MVI   ACTUAL,0            CLEAR ACTUAL                                 
         CLC   =X'F0F0',HUTYEAR    IF 00                                        
         BE    ED2C                YES                                          
         GOTO1 VALINUM             NO                                           
         CLI   ACTUAL,50           YY < 50 ADD 100 FOR Y2K                      
         BNL   ED2D                                                             
ED2C     ZIC   R1,ACTUAL                                                        
         LA    R1,100(R1)                                                       
         STC   R1,ACTUAL                                                        
ED2D     MVC   YEARFILT,ACTUAL                                                  
         MVC   ENDYEAR,ACTUAL                                                   
         LA    R2,HUTYENDH                                                      
         CLI   5(R2),0                                                          
         BE    ED3                                                              
         MVI   ACTUAL,0                                                         
         GOTO1 ANY                                                              
         CLC   =X'F0F0',HUTYEND                                                 
         BE    ED2E                                                             
         GOTO1 VALINUM                                                          
         CLI   ACTUAL,50                                                        
         BNL   ED2F                                                             
ED2E     ZIC   R1,ACTUAL                                                        
         LA    R1,100(R1)                                                       
         STC   R1,ACTUAL                                                        
ED2F     MVC   ENDYEAR,ACTUAL                                                   
         CLC   YEARFILT,ENDYEAR    START YEAR MUST BE LOWER THAN                
         BNH   ED3                      OR EQUAL TO END YEAR                    
         MVI   ERROR,INVALID                                                    
         B     EDERR                                                            
         EJECT                                                                  
ED3      LA    R2,HUTQRTH                                                       
         MVC   QRTFILT,=C'YYYY'                                                 
         CLC   8(3,R2),=C'ALL'                                                  
         BE    ED6                                                              
         CLI   5(R2),0                                                          
         BE    ED6                                                              
         MVC   QRTFILT,=C'NNNN'                                                 
         LA    R4,8(R2)                                                         
         ZIC   R3,5(R2)                                                         
         SPACE 2                                                                
ED4      CLI   0(R4),C'1'                                                       
         BNE   *+8                                                              
         MVI   QRTFILT,C'Y'                                                     
         CLI   0(R4),C'2'                                                       
         BNE   *+8                                                              
         MVI   QRTFILT+1,C'Y'                                                   
         CLI   0(R4),C'3'                                                       
         BNE   *+8                                                              
         MVI   QRTFILT+2,C'Y'                                                   
         CLI   0(R4),C'4'                                                       
         BNE   *+8                                                              
         MVI   QRTFILT+3,C'Y'                                                   
         LA    R4,1(R4)                                                         
         BCT   R3,ED4                                                           
         SPACE 2                                                                
ED6      LA    R2,HUTTYPEH                                                      
         MVI   TYPE,C'W'           WEEK IS DEFAULT                              
         CLI   5(R2),0                                                          
         BE    ED8                                                              
         CLI   8(R2),C'W'                                                       
         BE    ED8                                                              
         MVI   TYPE,C'M'           MONTH IS OPTIONAL                            
         CLI   8(R2),C'M'                                                       
         BE    ED8                                                              
         MVI   ERROR,INVALID                                                    
         B     EDERR                                                            
         SPACE 1                                                                
ED8      MVC   HOLIOPT,HUTHOLI                                                  
         MVC   OPT52,HUT52                                                      
         MVC   SCHMOPT,HUTSCHM                                                  
         LA    R2,HUTHTYPH                                                      
         MVI   BOOKTYPE,C'A'       BOOK TYPE                                    
         CLI   5(R2),0             DEFAULT IS ASCRIBED                          
         BE    ED10                                                             
         MVI   BOOKTYPE,0          OPTIONAL DIARY                               
         CLI   8(R2),C'D'                                                       
         BE    ED10                                                             
         MVI   BOOKTYPE,C'I'       OPTIONALLY INTEGRATED                        
         CLI   8(R2),C'I'                                                       
         BE    ED10                                                             
         MVI   BOOKTYPE,C'A'       OPTIONALLY ASCRIBED                          
         CLI   8(R2),C'A'                                                       
         BE    ED10                                                             
         MVI   BOOKTYPE,C'C'       OPTIONALLY CONFORMED                         
         CLI   8(R2),C'C'                                                       
         BE    ED10                                                             
         MVI   ERROR,INVALID                                                    
         B     EDERR                                                            
         SPACE 1                                                                
ED10     LA    R2,HUTDAYH                                                       
         SPACE 2                                                                
EXIT     XIT1  REGS=(R2)                                                        
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE NETINCLS                                                       
         PRINT ON                                                               
*                                                                               
HUTCOM   DSECT                                                                  
*                                                                               
*** PASSED TO PRINT MODULE                                                      
DAYFILT  DS    CL1                                                              
QRTFILT  DS    CL4                                                              
YEARFILT DS    CL1                                                              
ENDYEAR  DS    CL1                                                              
TIMFILT  DS    CL4                                                              
TYPE     DS    CL1                                                              
SOURCE   DS    CL1                                                              
SCHEME   DS    CL1                                                              
HOLIOPT  DS    CL1                                                              
OPT52    DS    CL1                                                              
SCHMOPT  DS    CL1                                                              
BOOKTYPE DS    CL1                                                              
*                                                                               
       ++INCLUDE NEMEDFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDE2D                                                       
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'019NEMED42   04/15/99'                                      
         END                                                                    
