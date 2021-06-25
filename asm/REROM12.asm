*          DATA SET REROM12    AT LEVEL 053 AS OF 02/07/03                      
*          DATA SET REROM10    AT LEVEL 114 AS OF 05/31/94                      
*PHASE T82F12A                                                                  
*                                                                               
         TITLE 'T82F12 - REROM12 - DARE REVISION OFFLINE REPORT EDIT'           
***********************************************************************         
*                                                                     *         
*  REROM12 (T82F12) --- DARE REVISION OFFLINE REPORT EDIT             *         
*                                                                     *         
* ------------------------------------------------------------------- *         
* UPDATE HISTORY:                                                     *         
*                                                                     *         
* 23APR97 SKU INITIAL RELEASE                                         *         
*                                                                     *         
***********************************************************************         
T82F12   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T82F12*,R7,RR=R3                                              
         L     RC,0(R1)            STANDARD CODING                              
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN + OUR SCREEN                     
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         LA    R5,SYSSPARE         R5 = A(OVERLAY STORAGE AREA)                 
         USING MYAREAD,R5                                                       
         ST    R3,RELO                                                          
                                                                                
         MVI   MYSCRNUM,X'FA'                                                   
         MVC   DRPLAST+1(2),=X'0101' RETRANSMIT ENTIRE SCREEN                   
                                                                                
         CLI   MODE,VALKEY         VALIDATE KEY?                                
         BE    VK                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* EXITS                                                                         
***********************************************************************         
EXITOK   CR    RB,RB                                                            
         B     EXIT                                                             
EXITL    CLI   *,X'FF'                                                          
         B     EXIT                                                             
EXITH    CLI   *,0                                                              
         B     EXIT                                                             
EXIT     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE KEY                                                                  
***********************************************************************         
VK       DS    0H                                                               
         LA    R2,DRPHDLNH                                                      
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
*                                                                               
         GOTO1 VALICON,DMCB,(R2)                                                
         BNZ   INVLFLD                                                          
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RAGY2KEY,R6                                                      
         MVI   RAGK2TYP,X'1A'                                                   
         MVC   RAGK2AGY,CCONKAGY                                                
         MVC   RAGK2AOF,CCONKAOF                                                
         MVC   RAGK2REP,AGENCY     REP                                          
         DROP  R6                                                               
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(L'RAGYKEY),KEYSAVE                                           
         BNE   INVLFLD                                                          
*                                                                               
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
*                                                                               
         L     R4,AIO3                                                          
         USING RAGY2REC,R4                                                      
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RDARKEY,R6                                                       
         MVI   RDARKTYP,X'41'                                                   
         MVC   RDARKREP,AGENCY                                                  
         MVC   RDARKSTA(5),CCONKSTA                                             
         CLI   RDARKSTA+4,C' '                                                  
         BNE   *+8                                                              
         MVI   RDARKSTA+4,C'T'     MUST SPECIFY IF TV                           
         OC    RDARKSTA,SPACES                                                  
         MVC   RDARKAGY(5),RAGY2DAR   EQUIVALENCY CODE                          
         MVC   RDARKORD,CDARNUM                                                 
         MVI   RDARKRT,X'10'       AGENCY HEADER ONLY                           
                                                                                
         LA    R4,RAGY2DAR         THERE ARE MAX 4 AGENCY ASSIGNMENT            
         LA    R3,4                COMBINATIONS WE NEED TO CHECK                
*                                                                               
VK10     DS    0H                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'RDARKEY),KEYSAVE                                           
         BE    VK20                                                             
         CLI   RAGY2FXL,RAGY2FLQ   ONLY NEWER AGENCY RECORDS                    
         BNL   *+6                 HAS MULTI-DARE AGENCY ASSIGNMENTS            
         DC    H'0'                                                             
         MVC   KEY,KEYSAVE                                                      
         LA    R4,5(R4)                                                         
         MVC   RDARKAGY(5),0(R4)   EQUIVALENCY CODE                             
         BCT   R3,VK10                                                          
         B     INVLFLD                                                          
         DROP  R4,R6                                                            
*                                                                               
VK20     DS    0H                                                               
         MVC   AIO,AIO1                                                         
*                                                                               
         MVC   SELECTKY,KEY                                                     
*                                                                               
VKX      DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
RELO     DS    A                                                                
*                                                                               
* ERROR MESSAGES                                                                
*                                                                               
MISSFLD  MVC   RERROR,=AL2(1)                                                   
         B     ERREND                                                           
*                                                                               
INVLFLD  MVC   RERROR,=AL2(2)                                                   
         B     ERREND                                                           
*                                                                               
INVLRCAC MVC   RERROR,=AL2(INVRCACT)                                            
         B     ERREND                                                           
*                                                                               
RECNTFND MVI   GERROR1,53          RECORD NOT FOUND                             
         B     ERREND                                                           
*                                                                               
ERREND   DS    0H                                                               
         MVI   RMSGTYPE,C'E'                                                    
         GOTO1 MYERROR                                                          
*                                                                               
INFEND   DS    0H                                                               
         LA    R2,DRPHDLNH                                                      
         MVI   RMSGTYPE,C'I'                                                    
         GOTO1 MYERROR             DO A GETTXT CALL                             
*                                                                               
         GETEL R6,DATADISP,ELCODE  USED FOR THE GETEL OPERATIONS                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
       ++INCLUDE FATIOB                                                         
       ++INCLUDE REROMFFD                                                       
       ++INCLUDE DDGENTWA                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE REROMF6D                                                       
       ++INCLUDE REDARWORKD                                                     
       ++INCLUDE REDARDSECT                                                     
         PRINT ON                                                               
*                                                                               
* APPLICATION STORAGE AREA                                                      
*                                                                               
MYAREAD  DSECT                                                                  
WORKLQ   EQU   *-MYAREAD                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'053REROM12   02/07/03'                                      
         END                                                                    
