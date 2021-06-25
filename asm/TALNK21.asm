*          DATA SET TALNK21    AT LEVEL 001 AS OF 02/11/14                      
*PHASE T70421A                                                                  
TALNK21  TITLE '- TALENT - PAYMENT SIMULATOR DOWNLOAD'                          
         PRINT NOGEN                                                            
SVRDEF   CSECT                                                                  
         LKSVR TYPE=D,CODE=CODE,REQUEST=*,SYSTEM=TALSYSQ,              *        
               BLOCKS=(B#WORKD,WORKD,B#SAVED,SAVED)                             
                                                                                
ERRTAB   EQU   7500                                                             
WORKLNQ  EQU   ERRTAB                                                           
                                                                                
CODE     NMOD1 WORKLNQ,**TA21**,RR=RE                                           
         LR    RF,RC                                                            
         LR    R5,R1                                                            
         USING LP_D,R5             R5=A(LP_D)                                   
         L     R7,LP_ARUNP                                                      
         USING RUNPARMD,R7         R7=A(RUNPARMS)                               
                                                                                
         L     R9,LP_ABLK1         ROOT PASSES A(WORKD) IN BLOCK 1              
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
         L     R8,LP_ABLK2         ROOT PASSES A(SAVED) IN BLOCK 2              
         USING SAVED,R8            R8=A(SAVE W/S)                               
         L     RA,ATWA                                                          
         USING TWAD,RA             RA=A(TWA)                                    
                                                                                
         ST    RE,SRVRRELO         SAVE PROGRAM RELOCATION FACTOR               
         STM   R2,RB,LP_R2RB       SAVE REGISTERS FOR SUB-ROUTINES              
         MVC   RUNMODE,RUNPMODE    EXTRACT CALLING MODE                         
                                                                                
         ST    RF,AERRTAB          SAVE A(ERROR TABLE)                          
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
*        INITIALIZE FOR RUNNING                                       *         
***********************************************************************         
                                                                                
RUNSTR   CLI   RUNMODE,RRUNSTRQ    TEST 'FIRST FOR RUN' MODE                    
         JNE   PRCWRK                                                           
         J     YES                                                              
                                                                                
***********************************************************************         
*        FIRST FOR NEW WORK                                           *         
***********************************************************************         
                                                                                
PRCWRK   CLI   RUNMODE,RPRCWRKQ    TEST 'PROCESS WORK' MODE                     
         BNE   RUNREQ                                                           
         LA    R0,REQVALS                                                       
         LHI   R1,REQVALL                                                       
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
*        RUN A DOWNLOAD REQUEST                                       *         
***********************************************************************         
                                                                                
RUNREQ   CLI   RUNMODE,RRUNREQQ    TEST 'RUN REQUEST' MODE                      
         JNE   YES                                                              
                                                                                
         LA    R0,OUTVALS          CLEAR OUTPUT VALUES                          
         LHI   R1,OUTVALL                                                       
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         MVC   COMPANY,LP_AGYB     SET COMPANY CODE                             
         MVC   CPYALPH,LP_AGY      SET COMPANY ID                               
         MVC   MAP,LP_QMAPN        SET PROCESSING MAP NUMBER                    
                                                                                
         GOTOR LP_APUTO,LP_D       CALL DDLINK OUTPUT PROCESSOR                 
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        OUTPUT PAYMENT SIMULATOR STATUS RECORD                       *         
***********************************************************************         
                                                                                
NXTPYS   J     *+12                                                             
         DC    C'*NXTPYS*'                                                      
         LR    RB,RF                                                            
         USING NXTPYS,RB                                                        
                                                                                
         CLI   LP_RMODE,LP_RFRST                                                
         JNE   NOMORE                                                           
                                                                                
         BAS   RE,PYVALREQ         VALIDATE REQUEST                             
         JNE   NPYSX                                                            
                                                                                
         GOTOR (#VALSTF,AVALSTF),DMCB,(X'80',RQPYSSTF)                          
         JNE   NPYSX                                                            
                                                                                
NPYSX    MVI   LP_RMODE,LP_RNEXT                                                
         LA    R0,PYSVALS                                                       
         STCM  R0,15,LP_ADATA                                                   
         J     YES                                                              
                                                                                
***********************************************************************         
*        ROUTINE ENSURES THAT REQUEST IS VALID                        *         
***********************************************************************         
                                                                                
PYVALREQ NTR1                                                                   
         OC    RQPYSSTF,RQPYSSTF   ASSERT THAT STAFF ID IS PROVIDED             
         JZ    NO                                                               
                                                                                
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        PROCESS RELEASE ERRRORS                                      *         
***********************************************************************         
                                                                                
NXTERR   J     *+12                                                             
         DC    C'*NXTERR*'                                                      
         LR    RB,RF                                                            
         USING NXTERR,RB                                                        
                                                                                
         CLI   LP_RMODE,LP_RFRST   INITIALIZE VARIABLES                         
         JNE   NERR10                                                           
         L     RE,AERRTAB                                                       
         ST    RE,ANXTERR                                                       
         XC    ERRVALS(ERRVALL),ERRVALS                                         
                                                                                
         USING ERRENTD,R4                                                       
NERR10   L     R4,ANXTERR          R4=A(CURRENT ERROR TABLE ENTRY)              
                                                                                
         CLI   0(R4),X'FF'         EXIT IF ALL ERRORS HAVE BEEN                 
         JE    NOMORE              PROCESSED                                    
                                                                                
         MVC   ERRNUMB,EENUMB                                                   
         MVC   ERRCATY,EECATY                                                   
         MVC   ERRFILD,EEFIELD                                                  
                                                                                
         ZIC   R0,EELEN                                                         
                                                                                
         MVC   ERREMSG,SPACES                                                   
         LR    RE,R0                                                            
         SHI   RE,6                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   ERREMSG(0),EEMSG                                                 
                                                                                
         AR    R4,R0               SAVE ADDRESS OF NEXT ERROR TABLE             
         ST    R4,ANXTERR          ENTRY                                        
                                                                                
         MVI   LP_RMODE,LP_RNEXT                                                
         LA    R0,ERRVALS                                                       
         STCM  R0,15,LP_ADATA                                                   
         J     YES                                                              
         DROP  R4,RB                                                            
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        OUTPUT PAYMENT SIMULATOR PAYMENT DETAILS RECORD              *         
***********************************************************************         
                                                                                
NXTPYP   J     *+12                                                             
         DC    C'*NXTPYP*'                                                      
         LR    RB,RF                                                            
         USING NXTPYP,RB                                                        
                                                                                
         CLI   LP_RMODE,LP_RFRST                                                
         JNE   NOMORE                                                           
                                                                                
NPYPX    MVI   LP_RMODE,LP_RNEXT                                                
         LA    R0,PYOVALS                                                       
         STCM  R0,15,LP_ADATA                                                   
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        OUTPUT PAYMENT SIMULATOR BILLING DETAILS RECORD              *         
***********************************************************************         
                                                                                
NXTPYB   J     *+12                                                             
         DC    C'*NXTPYB*'                                                      
         LR    RB,RF                                                            
         USING NXTPYB,RB                                                        
                                                                                
         CLI   LP_RMODE,LP_RFRST                                                
         JNE   NOMORE                                                           
                                                                                
NPYBX    MVI   LP_RMODE,LP_RNEXT                                                
         LA    R0,PYOVALS                                                       
         STCM  R0,15,LP_ADATA                                                   
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         GETEL R4,DATADISP,ELCODE                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* EXIT CONDITIONS                                                     *         
***********************************************************************         
                                                                                
NOMORE   MVI   LP_RMODE,LP_RLAST   SET NO MORE RECORDS AND EXIT                 
YES      LHI   RE,1                                                             
         J     *+8                                                              
NO       LHI   RE,0                                                             
         CHI   RE,1                                                             
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* REQUEST MAP FOR PAYMENT SIMULATOR                                   *         
***********************************************************************         
                                                                                
REQPYS   LKREQ H,I#PYSDLD,OUTPYS                                                
RqStf    LKREQ F,D#PYSSTF,(D,B#SAVED,RQPYSSTF),CHAR,TEXT=TA#STAFF,COL=*         
RqCom    LKREQ F,D#PYSCOM,(D,B#SAVED,RQPYSCOM),HEXD,TEXT=TA#COMCD,COL=*         
RqSeq    LKREQ F,D#PYSSEQ,(D,B#SAVED,RQPYSSEQ),HEXD,TEXT=TA#CSTSQ,COL=*         
RqUse    LKREQ F,D#PYSUSE,(D,B#SAVED,RQPYSUSE),CHAR,TEXT=TA#USECD,COL=*         
RqUTy    LKREQ F,D#PYSVER,(D,B#SAVED,RQPYSUTY),UBIN,TEXT=TA#PYTYP,COL=*         
RqCyS    LKREQ F,D#PYSCYS,(D,B#SAVED,RQPYSCYS),PDAT,TEXT=TA#CYCST,COL=*         
RqCyE    LKREQ F,D#PYSCYE,(D,B#SAVED,RQPYSCYE),PDAT,TEXT=TA#CYCED,COL=*         
         LKREQ E                                                                
         EJECT                                                                  
***********************************************************************         
* OUTPUT MAP - PAYMENT SIMULATOR                                      *         
***********************************************************************         
                                                                                
OUTPYS   LKOUT H                                                                
                                                                                
PYSREC   LKOUT R,O#PYSSTA                   ** STATUS VALUES **                 
Array    LKOUT C,O#PYSSTA,(A,ARYPYS)                                            
         LKOUT E                                                                
                                                                                
ERRREC   LKOUT R,O#PYSERR                   ** ERROR VALUES **                  
Array    LKOUT C,O#PYSERR,(A,ARYERR)                                            
         LKOUT E                                                                
                                                                                
PYPREC   LKOUT R,O#PYSPDE                   ** PAYMENT DETAILS **               
Array    LKOUT C,O#PYSPDE,(A,ARYPYO)                                            
         LKOUT E                                                                
                                                                                
PYBREC   LKOUT R,O#PYSBDE                   ** BILLING DETAILS **               
Array    LKOUT C,O#PYSBDE,(A,ARYPYO)                                            
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
         EJECT                                                                  
***********************************************************************         
* ARRAY DEFINITION FOR PAYMENT SIMULATOR STATUS RECORDS               *         
***********************************************************************         
                                                                                
ARYPYS   LKOUT A,(R,NXTPYS),ROWNAME=PYSVALS                                     
                                                                                
RtSta    LKOUT C,1,(D,,PYSSTAT),UBIN,ND=Y                                       
                                                                                
         LKOUT E                                                                
         EJECT                                                                  
***********************************************************************         
* ARRAY DEFINITION FOR ERROR MESSAGE RECORDS                          *         
***********************************************************************         
                                                                                
ARYERR   LKOUT A,(R,NXTERR),MULTIROW=Y,ROWNAME=ERRVALS                          
                                                                                
RtNum    LKOUT C,1,(D,,ERRNUMB),UBIN,ND=Y                                       
RtCat    LKOUT C,2,(D,,ERRCATY),UBIN,ND=Y                                       
RtFld    LKOUT C,3,(D,,ERRFILD),UBIN,ND=Y                                       
RtErM    LKOUT C,4,(D,,ERREMSG),CHAR,ND=Y                                       
                                                                                
         LKOUT E                                                                
         EJECT                                                                  
***********************************************************************         
* ARRAY DEFINITION FOR PAYMENT/BILLING DETAILS RECORDS                *         
***********************************************************************         
                                                                                
ARYPYO   LKOUT A,(R,NXTPYP),MULTIROW=Y,ROWNAME=PYOVALS                          
                                                                                
RtDec    LKOUT C,1,(D,,PYODES),CHAR,ND=Y                                        
RtAmt    LKOUT C,2,(D,,PYOAMT),CBIN,ND=Y                                        
                                                                                
         LKOUT E                                                                
         EJECT                                                                  
***********************************************************************         
* SAVED STORAGE                                                       *         
***********************************************************************         
                                                                                
SAVED    DSECT                     ** DSECT TO COVER SAVED STORAGE **           
                                                                                
*** REQUEST VALUES ***                                                          
                                                                                
* PAYMENT SIMULATOR REQUEST *                                                   
                                                                                
REQVALS  DS    0F                  ** REQUEST VALUES **                         
RQPYSSTF DS    CL8                 STAFF                                        
RQPYSCOM DS    XL4                 INTERNAL COMMERCIAL NUMBER                   
RQPYSSEQ DS    XL2                 CAST SEQUENCE NUMBER                         
RQPYSUSE DS    CL3                 USE                                          
RQPYSUTY DS    XL1                 USE TYPE                                     
RQPYSCYS DS    XL3                 CYCLE START DATE                             
RQPYSCYE DS    XL3                 CYCLE END DATE                               
                                                                                
REQVALL  EQU   *-REQVALS                                                        
                                                                                
*** OUTPUT VALUES ***                                                           
                                                                                
OUTVALS  DS    0X                  ** OUTPUT VALUES **                          
                                                                                
* ** PAYMENT SIMULATOR STATUS **                                                
                                                                                
         ORG   OUTVALS             RESET DISPLACEMENT                           
PYSVALS  DS    0X                                                               
PYSSTAT  DS    CL1                 STATUS                                       
PYSSSUC  EQU   1                   SUCCESSFUL                                   
PYSSUNS  EQU   2                   UNSUCCESSFUL                                 
PYSSINV  EQU   3                   UNSUCCESSFUL - INVALID REQUEST               
PYSVALL  EQU   *-PYSVALS                                                        
                                                                                
* ** ERROR VALUES *                                                             
                                                                                
         ORG   OUTVALS                                                          
ERRVALS  DS    0X                                                               
ERRNUMB  DS    CL2                 NUMBER                                       
ERRCATY  DS    CL1                 CATEGORY                                     
ERRFILD  DS    CL1                 FIELD                                        
ERREMSG  DS    CL60                ERROR MESSAGE                                
ERRVALL  EQU   *-ERRVALS                                                        
                                                                                
* ** PAYMENT SIMULATOR PAYMENT DETAILS **                                       
                                                                                
         ORG   OUTVALS                                                          
PYOVALS  DS    0X                                                               
PYODES   DS    CL50                DESCRIPTION                                  
PYOAMT   DS    F                   AMOUNT                                       
                                                                                
         ORG                                                                    
OUTVALL  EQU   *-OUTVALS                                                        
                                                                                
*** REGULAR STORAGE **                                                          
                                                                                
RUNMODE  DS    XL(L'RUNPMODE)      RUNNER/DDLINK MODE                           
USERID   DS    XL2                 HEX USER ID                                  
COMPANY  DS    XL(L'LP_AGYB)       COMPANY CODE                                 
CPYALPH  DS    CL(L'LP_AGY)        COMPANY ALPHA ID                             
                                                                                
MAP      DS    XL2                                                              
                                                                                
ANXTERR  DS    A                   A(LAST PROCESSED ERROR TABLE ENTRY           
         EJECT                                                                  
* INCLUDED DSECTS                                                               
         PRINT OFF                                                              
       ++INCLUDE TALNKWRK                                                       
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE TAUNIEQUS                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001TALNK21   02/11/14'                                      
         END                                                                    
