*          DATA SET ACREPDT02  AT LEVEL 007 AS OF 07/31/03                      
*PHASE ACDT02A                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE PRINT                                                                  
         TITLE 'FIND/DELETE EXPIRED DRAFT TRANSACTIONS'                         
*                                                                               
***********************************************************************         
* THIS PROGRAM WILL READ DRAFT TRANSACTIONS ONLY.                     *         
* THE NUMBER OF DAYS SPECIFIED IN THE SELECT FIELD WILL BE ADDED      *         
* TO THE EFFECTIVE DATE IN THE X'60' ELEMENT. THE DEFAULT IS 15.      *         
* IF THE RESULTING DATE IS LESS THAN "TODAY", THE TRANACTIONS IS      *         
* PRINTED AND OPTIONALLY, MARKED FOR DELETION.                        *         
*                                                                     *         
* IF THE EFFECTIVE DATE FIELD IS ZERO, THE SELECT FIELD WILL BE USED  *         
* TO COMPARE AGAINST THE DATE ADDED TO THE FILE. THE DEFAULT IS 0.    *         
*                                                                     *         
* ASTDRAFT WILL BE UPDATED ACCORDINGLY.                               *         
***********************************************************************         
*                                                                               
*                                                                               
ACDT02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACDT**,R9                                                    
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACDTD,RC                                                         
*-------------------------------------------------------------------*           
*        FIRST FOR RUN                                                          
*-------------------------------------------------------------------*           
         CLI   MODE,RUNFRST                                                     
         BNE   REQF00                                                           
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(2,TODAY2)                                     
*                                                                               
         L     RF,GETOPT           DISABLE GETOPT                               
         MVC   0(2,RF),=X'07FE'                                                 
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        FIRST FOR REQUEST                                                      
*-------------------------------------------------------------------*           
REQF00   CLI   MODE,REQFRST                                                     
         BNE   LDGF00                                                           
*                                                                               
         ZAP   TDAMNT,=P'0'                                                     
         ZAP   TCAMNT,=P'0'                                                     
         ZAP   PDUMP,=P'0'                                                      
         ZAP   TDFTREC,=P'0'                                                    
         MVI   FCSEQ,FCSEQNEW      BYPASS BLDBUF ROUTINE                        
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        SET NECESSARY SWITCHES                                                 
*-------------------------------------------------------------------*           
LDGF00   CLI   MODE,LEDGFRST                                                    
         BNE   PROCA0                                                           
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVI   FCRDTRNS,C'Y'                                                    
         MVI   FCRNTIME,C'Y'                                                    
         MVI   FCRESET,C'Y'                                                     
         MVI   FCSUPOFC,C'Y'                                                    
         MVI   FCDRAOVR,FCDRATRY   READ DRAFT TRANSACTIONS                      
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        GET ASTDRAFT                                                           
*-------------------------------------------------------------------*           
PROCA0   CLI   MODE,PROCACC                                                     
         BNE   PTRN00                                                           
         ZAP   DFTREC,=P'0'        CLEAR DRAFT COUNTER                          
         MVC   SVDRAFT,SPACES                                                   
*                                                                               
         L     R2,ADACC            GET DRAFT COUNTS FROM ACC RECOR              
         USING ACTRECD,R2                                                       
         LA    RE,ACTRECD+ACCORFST                                              
         USING ASTEL,RE                                                         
         SR    RF,RF                                                            
*                                                                               
PROCA2   CLI   ASTEL,0                                                          
         BE    PROCAX              E-O-R                                        
         CLI   ASTEL,ASTELQ                                                     
         BE    PROCA4                                                           
         IC    RF,ASTLN                                                         
         AR    RE,RF                                                            
         B     PROCA2                                                           
*                                                                               
PROCA4   MVC   SVDRAFT,ASTDRAFT    KEEP DRAFT COUNT                             
*                                                                               
PROCAX   B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        PROCESS TRANSACTIONS                                                   
*-------------------------------------------------------------------*           
PTRN00   CLI   MODE,PROCTRNS                                                    
         BNE   ACCL00                                                           
*                                                                               
         USING TRNRECD,R3                                                       
         USING TRNELD,R5                                                        
         L     R5,ADTRANS                                                       
         LR    R3,R5                                                            
         SH    R3,DATADISP                                                      
*                                                                               
         CLI   TRNRSTYP,12         SKIP ORDERS                                  
         BE    PTRNX                                                            
*                                                                               
         TM    TRNRSTA,TRNSDRFT    MUST BE LOOKING AT DRAFTS ONLY               
         BO    *+6                                                              
         DC    H'0'                DIE IF NOT PASSED ONLY THESE                 
         SR    R0,R0                                                            
         L     R4,ADTRANS                                                       
*                                                                               
PTRN02   IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0                                                          
         BE    PTRNX                                                            
         CLI   0(R4),TRSELQ                                                     
         BNE   PTRN02                                                           
*                                                                               
*                                  ADD QSELECT (15) TO EFEECTIVE DATE           
         USING TRSELD,R4                                                        
         OC    TRSEFDT,TRSEFDT     ANY EFFECTIVE DATE?                          
         BZ    PTRN04              NO, USE THE ADDED DATE                       
         GOTO1 DATCON,DMCB,(2,TRSEFDT),WORK                                     
         LA    R2,15               SET DEFAULT AT 15 DAYS                       
         B     PTRN06                                                           
*                                                                               
*                                  ADD QSELECT (0) TO ADDED DATE                
PTRN04   GOTO1 DATCON,DMCB,(2,TRSDATE),WORK                                     
         LA    R2,0                                                             
*                                                                               
PTRN06   CLC   QSELECT(2),SPACES                                                
         BNH   PTRN08                                                           
         PACK  DUB,QSELECT(2)                                                   
         CVB   R2,DUB                                                           
*                                                                               
PTRN08   GOTO1 ADDAY,DMCB,(C'D',WORK),WORK+8,(R2)                               
         GOTO1 DATCON,DMCB,WORK+8,(2,WORK)                                      
*                                                                               
         CLC   WORK(2),TODAY2                                                   
         BNL   PTRNX                                                            
*                                                                               
PTRN10   CLI   QOPT1,C'Y'          MARK ITEM FOR DELETION?                      
         BNE   *+8                 NO, JUST PRINT IT                            
         OI    TRNRSTA,TRNSDELT    YES, MARK IT FIRST                           
*                                                                               
         TM    TRNSTAT,TRNSDR      DEBIT?                                       
         BZ    PTRN12              NO, CREDIT                                   
*                                                                               
         ZAP   DAMNT,TRNAMNT                                                    
         EDIT  DAMNT,(15,P+69),2,CR=YES                                         
         AP    TDAMNT,DAMNT                                                     
         B     PTRN14                                                           
*                                                                               
PTRN12   ZAP   CAMNT,TRNAMNT                                                    
         EDIT  CAMNT,(15,P+86),2,CR=YES                                         
         AP    TCAMNT,CAMNT                                                     
*                                                                               
PTRN14   MVC   P+1(14),TRNKULA                                                  
         MVC   P+18(2),TRNKOFF                                                  
         MVC   P+23(14),TRNKULC                                                 
         MVC   P+39(6),TRNREF                                                   
         GOTO1 DATCON,DMCB,(1,TRNDATE),(X'20',P+47)                             
         MVC   P+55(2),TRNANAL                                                  
         MVC   P+59(4),TRNBREF                                                  
         EDIT  (B1,TRNTYPE),(2,P+65)                                            
         AP    DFTREC,=P'1'                                                     
         AP    TDFTREC,=P'1'                                                    
*                                                                               
         GOTO1 ACREPORT                                                         
*                                                                               
*        BAS   RE,DMPGET                                                        
*                                                                               
         CLI   RCWRITE,C'Y'                                                     
         BNE   PTRNX                                                            
         MVI   MODE,WRITRANS                                                    
*                                                                               
PTRNX    B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        ACCOUNT LAST                                                           
*-------------------------------------------------------------------*           
ACCL00   CLI   MODE,ACCLAST                                                     
         BNE   REQL00                                                           
*                                                                               
         CP    DFTREC,=P'0'                                                     
         BE    ACCLX               NO DRAFTS, NOT CHANGING ASTDRAFT             
*                                                                               
         ZAP   DUB,DFTREC                                                       
         CVB   RF,DUB                                                           
*                                                                               
         SR    R2,R2                                                            
         ICM   R2,7,SVDRAFT                                                     
*                                                                               
         SR    R2,RF                                                            
         STCM  R2,7,SVDRAFT                                                     
*                                                                               
         L     R2,ADACC                                                         
         USING ACTRECD,R2                                                       
         LA    RE,ACTRECD+ACCORFST                                              
         USING ASTEL,RE                                                         
         SR    RF,RF                                                            
*                                                                               
ACCL2    CLI   ASTEL,0                                                          
         BE    ACCLX               E-O-R                                        
         CLI   ASTEL,ASTELQ                                                     
         BE    ACCL4                                                            
         IC    RF,ASTLN                                                         
         AR    RE,RF                                                            
         B     ACCL2                                                            
*                                                                               
ACCL4    MVC   ASTDRAFT,SVDRAFT    UPDATE THE COUNT                             
         CLI   RCWRITE,C'Y'                                                     
         BNE   ACCLX                                                            
         MVI   MODE,WRITACC                                                     
*                                                                               
ACCLX    B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        REQUEST LAST                                                           
*-------------------------------------------------------------------*           
REQL00   CLI   MODE,REQLAST                                                     
         BNE   RUNL00                                                           
*                                                                               
         EDIT  (P6,TDFTREC),(14,P+16),MINUS=YES                                 
         EDIT  TDAMNT,(15,P+69),2,CR=YES                                        
         EDIT  TCAMNT,(15,P+86),2,CR=YES                                        
         MVC   P+1(14),=C'DRAFT RECORDS '                                       
         MVI   SPACING,2                                                        
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        RUNLAST                                                                
*-------------------------------------------------------------------*           
RUNL00   CLI   MODE,RUNLAST                                                     
         BNE   XIT                                                              
         GOTO1 ACREPORT                                                         
         MVC   P+1(5),=C'TOTAL'                                                 
         EDIT  TDAMNT,(15,P+69),2,CR=YES                                        
         EDIT  TCAMNT,(15,P+86),2,CR=YES                                        
         GOTO1 ACREPORT                                                         
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        ROUTINES TO DUMP OUT RECORDS                                           
*-------------------------------------------------------------------*           
DMPGET   NTR1                                                                   
         AP    DUMPCNT,=P'1'                                                    
         ZAP   DUB,DUMPCNT                                                      
         DP    DUB,EVERY                                                        
         CP    DUB+4(4),=P'0'                                                   
         BNE   XIT                                                              
         AP    PDUMP,=P'1'                                                      
         CP    PDUMP,MAXDUMP                                                    
         BH    XIT                                                              
         LA    R6,=C'GET'                                                       
*                                                                               
         SR    R8,R8                                                            
         ICM   R8,3,TRNKEY+ACCORLEN                                             
         GOTO1 PRNTBL,DMCB,(3,(R6)),(R3),C'DUMP',(R8),=C'2D'                    
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        CONSTANTS                                                              
*-------------------------------------------------------------------*           
PRNTBL   DC    V(PRNTBL)                                                        
HELLO    DC    V(HELLO)                                                         
         DC    X'FF'                                                            
*                                                                               
DAMNT    DC    PL6'0'                                                           
TDAMNT   DC    PL6'0'                                                           
CAMNT    DC    PL6'0'                                                           
TCAMNT   DC    PL6'0'                                                           
TDFTREC  DC    PL6'0'                                                           
*                                                                               
DFTREC   DC    PL6'0'                                                           
*                                                                               
*                                                                               
DUMPCNT  DC    PL4'0'                                                           
EVERY    DC    PL4'1'                                                           
PDUMP    DC    PL4'0'                                                           
MAXDUMP  DC    PL4'5'                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
ACDTD    DSECT                                                                  
TODAY2   DS    CL2                                                              
SVDRAFT  DS    XL3                                                              
*                                                                               
*  ACREPWORKD                                                                   
*  ACGENFILE                                                                    
*  ACGENMODES                                                                   
*  ACMASTD                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007ACREPDT02 07/31/03'                                      
         END                                                                    
