*          DATA SET TAGEN56    AT LEVEL 032 AS OF 01/25/16                      
*PHASE T70256A                                                                  
         TITLE 'T70256 - PAY - CAST SELECT'                                     
T70256   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70256                                                         
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         LA    R8,TWAHOLE          R8=A(PAY STORAGE)                            
         USING PAYD,R8                                                          
         EJECT                                                                  
*              ROUTINES TO CONTROL PROGRAM                                      
         SPACE 1                                                                
         OI    CONRECH+1,X'20'     PROTECT REC/ACT                              
         OI    CONRECH+6,X'80'                                                  
         OI    CONACTH+1,X'20'                                                  
         OI    CONACTH+6,X'80'                                                  
         SPACE 1                                                                
         OC    SELRHS,SELRHS       IF NOTHING DISPLAYED YET (1ST TIME)          
         BNZ   *+14                                                             
         MVC   SELCMNT,COMCMNT     MOVE COMMERCIAL COMMENT TO SCREEN            
         B     MAIN7                                                            
         SPACE 1                                                                
         BAS   RE,SAVSCRN          ELSE SAVE SCREEN                             
         CLI   PFAID,24            TEST IF ABORT PFKEY PRESSED                  
         BNE   MAIN7                                                            
         OI    LCLSTAT2,ABORT      SET TO ABORT                                 
         B     NO                  RETURN CC NOT EQUAL                          
         SPACE 1                                                                
MAIN7    TM    LCLSTAT,FINISHED    IF WE HAVEN'T FINISHED ALREADY               
         BO    MAIN10                                                           
         BAS   RE,DISPLAY          DISPLAY (FIRST/)NEXT PAGE                    
         OC    SELRHS,SELRHS       IF NO ONE DISPLAYED                          
         BZ    NO                  RETURN CC NOT EQUAL                          
         B     DSPLYMSG            ELSE GIVE USER CHANCE TO SELECT              
         SPACE 1                                                                
MAIN10   OI    PAYSTAT1,CASTSELD   SET CAST SELECTED                            
         MVI   LCLSTAT6,0          CLEAR GUARANTEE BITS                         
         MVI   LCLSTAT7,0                                                       
         NI    LCLSTAT,ALL-FINISHED                                             
         B     YES                 RETURN TO PAY CONTROLLER                     
         EJECT                                                                  
*              ROUTINE TO SAVE A SCREEN                                         
         SPACE 1                                                                
SAVSCRN  NTR1                                                                   
         GOTO1 PUTSCRN,DMCB,0,0,0  WRITE THE SCREEN RECORD                      
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO DISPLAY A SCREEN                                      
         SPACE 1                                                                
DISPLAY  NTR1                                                                   
         LA    R2,SELFRSTH         R2=A(FIRST LINE)                             
         SPACE 1                                                                
         TWAXC (R2),SELLSTH,PROT=Y CLEAR SCREEN                                 
         SPACE 1                                                                
         MVI   DMCB,X'80'          FORCE GETCAST TO READ HIGH 1ST TIME          
         B     *+8                                                              
DISP2    MVI   DMCB,0              CLEAR PARAMETER TO GETCAST                   
         SPACE 1                                                                
         GOTO1 GETCAST,DMCB        GET (1ST/)NEXT CAST MEMBER                   
         BNE   XIT                 NO MORE                                      
         SPACE 1                                                                
         LA    RF,SELCMNTH         TEST PAST END OF SCREEN                      
         CR    R2,RF                                                            
         BNL   XIT                 EXIT TO USER NOW                             
         SPACE 1                                                                
         USING SELD,R2                                                          
         MVC   SELSSN(3),TGSSN     S/S NUMBER                                   
         MVI   SELSSN+3,C'-'                                                    
         MVC   SELSSN+4(2),TGSSN+3                                              
         MVI   SELSSN+6,C'-'                                                    
         MVC   SELSSN+7(4),TGSSN+5                                              
*                                                                               
         TM    TGSYSTAT,TASYSPID                                                
         BZ    DISP5                                                            
         MVC   SELSSN(11),SPACES                                                
         GOTO1 SSNPACK,DMCB,TGSSN,SELSSN                                        
*                                                                               
DISP5    MVC   SELCAT,TGCAT        CATEGORY                                     
         MVC   SELNAME,CASTNAME    NAME                                         
         MVC   SELCAM,TCCAONOF     ON/OFF CAMERA                                
         MVC   SELDBL,TCCADBL      N'DOUBLES                                    
         MVC   SELUNI,TGUNI        UNION                                        
         MVC   SELLCL,TGLCL        LOCAL                                        
         MVC   SELYR,TGYRCDE       YEAR                                         
*                                                                               
         LA    R5,SELOV1                                                        
         ICM   RF,15,TCOV1                                                      
         TM    TCOV1,X'80'         PERCENT SCALE USED?                          
         BZ    DISP10                                                           
         MVI   SELOV1,C'%'                                                      
         LA    R5,1(R5)                                                         
         N     RF,=X'7FFFFFFF'                                                  
*                                                                               
DISP10   EDIT  (RF),(6,(R5)),2,ZERO=BLANK       1ST OVERSCALE                   
         CLC   3(3,R5),=C'.00'                                                  
         BNE   *+10                                                             
         MVC   3(3,R5),SPACES                                                   
*                                                                               
         EDIT  (4,TCOV2),(6,SELOV2),2,ZERO=BLANK  2ND OVERSCALE                 
         CLC   SELOV2+3(3),=C'.00'                                              
         BNE   *+10                                                             
         MVC   SELOV2+3(3),SPACES                                               
         SPACE 1                                                                
         GOTO1 HEXOUT,DMCB,TGCSORT+4,SELSEQ,2,0  CAST SEQ. NUMBER               
         SPACE 1                                                                
         LA    R2,SELNEXT          BUMP TO NEXT LINE                            
         B     DISP2                                                            
         EJECT                                                                  
*              ERRORS/EXITS                                                     
         SPACE 2                                                                
DSPLYMSG MVI   MYMSGNO1,3          CAST DISPLAYED - SELECT AS DESIRED           
         TM    LCLSTAT,FINISHED                                                 
         BZ    *+8                                                              
         MVI   MYMSGNO1,10         NO MORE                                      
         OI    GENSTAT2,USGETTXT                                                
         LA    R2,SELFRSTH         CURSOR TO FIRST SELECT FIELD                 
         B     THEEND                                                           
         SPACE 1                                                                
THEEND   GOTO1 EXIT,DMCB,0                                                      
         SPACE 2                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TAPYS78D                                                       
         EJECT                                                                  
       ++INCLUDE TAGENPAYD                                                      
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TAPYS96D                                                       
         EJECT                                                                  
* DDGENTWA    *** MUST FOLLOW LAST SCREEN ***                                   
* TAGENWORKD                                                                    
* TASYSEQUS                                                                     
* TAGENFILE                                                                     
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'032TAGEN56   01/25/16'                                      
         END                                                                    
