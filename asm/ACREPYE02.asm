*          DATA SET ACREPYE02  AT LEVEL 038 AS OF 08/17/00                      
*PHASE ACYE02A                                                                  
*INCLUDE ACLDTPTR                                                               
*INCLUDE ACRECTYP                                                               
ACYE02   TITLE '- RECOVERY REC TYPE COUNT/PRINT'                                
ACYE02   CSECT                                                                  
         NMOD1 0,**ACYE**,R9,R8,R7                                              
         L     RA,0(R1)                                                         
         USING ACWORKD,RA          RA=A(GLOBAL W/S)                             
         LA    RC,SPACEND                                                       
         USING WORKD,RC            RC=A(LOCAL W/S)                              
         SPACE 1                                                                
         CLI   MODE,PROCRCVR       PROCESS A RECOVERY RECORD                    
         BE    DCCOUNT                                                          
         CLI   MODE,RUNLAST        LAST FOR RUN                                 
         BE    DCPRINT                                                          
         SPACE 1                                                                
XIT      XIT1  ,                                                                
         EJECT                                                                  
**********************************************************************          
* COUNT ALL RECORDS BY TYPE AND STATUS FOR EACH COMPANY              *          
**********************************************************************          
         SPACE 1                                                                
DCCOUNT  L     R2,ADTRANS                                                       
         USING RCVRECD,R2                                                       
         MVI   WORK,RCVFADIR                                                    
         CLI   RCFFPARM,C'D'                                                    
         BE    *+8                                                              
         MVI   WORK,RCVFAMST                                                    
         MVI   WORK+1,RCVRADDQ                                                  
         CLC   RCVFILTY(L'RCVFILTY+L'RCVRECTY),WORK                             
         BNE   XIT                                                              
         LA    R2,RCVRECRD                                                      
         USING TRNRECD,R2                                                       
         CLI   WORK,RCVFADIR                                                    
         BNE   DCNT01                                                           
         GOTO1 VLDTPTR,DMCB,TRNRECD,WORK                                        
         CLI   0(R1),X'FF'         IS THIS A PASSIVE POINTER?                   
         BE    XIT                                                              
*                                                                               
DCNT01   DS    0H                                                               
         GOTO1 VRECTYP,DMCB,(C'D',TRNRECD)                                      
         MVC   WRECTYPE,0(R1)                                                   
         MVC   WCPYCODE,1(R1)                                                   
*                                                                               
         L     R3,ARECTAB          FIND ENTRY FOR RECORD TYPE                   
         LA    RE,RECTABL                                                       
         L     RF,ARECTABU                                                      
         USING RECTABD,R3                                                       
         XR    R4,R4               R4=DISPLACEMENT INTO COUNTER TABLE           
DCNT02   CLI   RECTABD,EOT                                                      
         BE    DCNT06                                                           
         CLC   RECTYPE,WRECTYPE    MATCH ON RECORD TYPE                         
         BNE   DCNT04                                                           
DCNT03   CLI   RECTYPE,ACRTUNKN    TEST FOR UNKNOWN TYPE ENTRY                  
         BNE   DCNT10                                                           
         CLC   RECUNKEY,TRNKEY     MATCH ON FIST BYTE OF KEY                    
         BE    DCNT10                                                           
DCNT04   LA    R4,CNTL(R4)                                                      
         BXLE  R3,RE,DCNT02                                                     
*                                                                               
         MVC   RECNAME,T@UNKOTH                                                 
         MVI   RECINDS1,RECIDIR+RECIUNK+RECIFILE                                
         B     DCNT10                                                           
*                                                                               
DCNT06   MVC   RECTYPE,WRECTYPE    ADD ENTRY FOR UNKNOWN                        
         CLI   RECTYPE,ACRTUNKN    ACRECTYP RETURNED UNKNOWN                    
         BNE   DCNT08                                                           
         MVC   RECNAME,T@UNKTYP                                                 
         GOTO1 HEXOUT,DMCB,TRNRECD,RECNAME+2,1,=C'TOG'                          
         MVI   RECINDS1,RECIDIR+RECIUNK+RECIFILE                                
         MVC   RECUNKEY,TRNKEY                                                  
         B     DCNT10                                                           
*                                                                               
DCNT08   MVC   RECNAME,T@UNKEQU    ACRECTYP EQUATE IS UNKNOWN                   
         XR    R0,R0                                                            
         IC    R0,RECTYPE                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  RECNAME+1(3),DUB                                                 
         MVI   RECINDS1,RECIDIR+RECIUNK                                         
*                                                                               
DCNT10   TM    RECINDS1,RECIFILE   TEST FOR FILE TOTALS ONLY                    
         BZ    *+12                                                             
         LA    R4,FILECNTS(R4)     R4=A(COUNTERS FOR FILE)                      
         B     DCNT12                                                           
*                                                                               
         XR    R1,R1                                                            
         IC    R1,WCPYCODE                                                      
         MH    R1,=Y(CPYTABL)                                                   
         A     R1,ACPYTAB                                                       
         USING CPYTABD,R1                                                       
         MVC   CPYCODE,WCPYCODE                                                 
         OI    CPYINDS,CPYICNT                                                  
         LA    R4,CPYCNTS(R4)      R4=A(COUNTERS FOR COMPANY)                   
         DROP  R1                                                               
*                                                                               
         USING CNTD,R4             R4=A(COUNTERS)                               
DCNT12   LA    R1,CNTOUT           INCREMENT OUTPUT                             
         CLI   PCODE,X'41'         TEST FOR PURGED RECORD                       
         BE    *+12                                                             
         TM    TRNKSTAT,TRNSDELT                                                
         BZ    *+8                                                              
         LA    R1,CNTPUR             OR PURGED TOTAL                            
         ICM   RE,15,0(R1)                                                      
         TM    PCODE,X'80'                                                      
         BZ    *+10                                                             
         BCTR  RE,0                                                             
         B     *+8                                                              
         AH    RE,HONE                                                          
         STCM  RE,15,0(R1)                                                      
         B     XIT                                                              
         DROP  R4,R3,R2                                                         
         EJECT                                                                  
**********************************************************************          
* PRINT COMPANY TOTALS BY RECTYPE/STATUS FROM ACCUMBUF,              *          
* ACCUMULATE AND PRINT RECTYPE/STATUS FILE TOATLS                    *          
**********************************************************************          
         SPACE 1                                                                
DCPRINT  L     R8,ADBXAREA                                                      
         USING BOXD,R8                                                          
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+5,C'T'                                                   
         MVI   BOXROWS+7,C'M'                                                   
         MVI   BOXROWS+99,C'B'                                                  
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXOFF,C'N'                                                      
         MVI   BOXINIT,0                                                        
         MVC   BOXCOLS,SPACES                                                   
         MVI   BOXCOLS+(PBXL-P),C'L'                                            
         MVI   BOXCOLS+(PBXC1-P),C'C'                                           
         MVI   BOXCOLS+(PBXC2-P),C'C'                                           
         MVI   BOXCOLS+(PBXC3-P),C'C'                                           
         MVI   BOXCOLS+(PBXR-P),C'R'                                            
*                                                                               
         L     R4,ACPYTAB                                                       
         USING CPYTABD,R4          R4=A(COMPANY TABLE)                          
         LA    R3,CPYTABN                                                       
*                                                                               
DCPRT02  TM    CPYINDS,CPYICNT     TEST ANYTHING FOR COMPANY                    
         BZ    DCPRT08                                                          
         GOTO1 HEXOUT,DMCB,CPYCODE,HEAD5+12,L'CPYCODE,=C'TOG'                   
*                                                                               
         GOTO1 PRNTCPY,CPYCNTS                                                  
*                                                                               
DCPRT08  LA    R4,CPYTABL(R4)                                                   
         BCT   R3,DCPRT02                                                       
         DROP  R4                                                               
*                                                                               
         MVI   RCSUBPRG,1                                                       
         GOTO1 PRNTCPY,FILECNTS                                                 
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PRINT TOTALS FOR COMPANY                                 *         
*                                                                     *         
* NTRY: R1=A(COUNTERS TABLE)                                          *         
***********************************************************************         
         SPACE 1                                                                
PRNTCPY  NTR1  ,                                                                
         LR    R3,R1               R3=A(COUNTRES)                               
         USING CNTD,R3                                                          
         L     R2,ARECTAB                                                       
         USING RECTABD,R2          R2=A(RECORD TYPE TABLE)                      
         LA    R4,FILECNTS         R4=A(FILE TOTAL COUNTERS)                    
*                                                                               
PCPY02   OC    CNTD(CNTL),CNTD                                                  
         BZ    PCPY08              TEST ANY COUNTED                             
         MVC   PREC(L'RECNAME),RECNAME  OUTPUT ROW                              
         GOTO1 PRNTROW,DMCB,CNTD,RECTABD                                        
         GOTO1 ADDCNT,DMCB,(R4),CNTD                                            
*                                                                               
PCPY08   LA    R3,CNTL(R3)                                                      
         LA    R4,CNTL(R4)                                                      
         LA    R2,RECTABL(R2)                                                   
         CLI   RECTABD,EOT                                                      
         BNE   PCPY02                                                           
         DROP  R2,R3                                                            
*                                                                               
         IC    RE,LINE                                                          
         LA    RE,1(RE)                                                         
         CLM   RE,1,MAXLINES                                                    
         BH    PCPY10                                                           
         BE    *+8                                                              
         MVI   BOXREQ,C'B'                                                      
         GOTO1 ACREPORT                                                         
*                                                                               
PCPY10   LA    R2,TOT              PRINT TOTALS                                 
         USING TOTD,R2                                                          
PCPY12   OC    TOTCNT,TOTCNT                                                    
         BZ    PCPY18                                                           
         MVC   PREC,TOTNAME                                                     
         GOTO1 PRNTROW,DMCB,TOTCNT,0                                            
         XC    TOTCNT,TOTCNT                                                    
PCPY18   LA    R2,TOTL(R2)                                                      
         CLI   TOTD,EOT                                                         
         BNE   PCPY12                                                           
         DROP  R2                                                               
*                                                                               
         CLC   LINE,MAXLINES                                                    
         BNL   PCPY20                                                           
         MVI   BOXREQ,C'C'         CLOSE BOX                                    
         GOTO1 ACREPORT                                                         
PCPY20   MVC   LINE,MAXLINES       SET NEW PAGE                                 
         MVC   PAGE,HONE                                                        
         B     XIT                                                              
         SPACE 1                                                                
***********************************************************************         
*  PRINT TOTALS FOR A ROW                                             *         
*                                                                     *         
* NTRY: P1=A(COUNTER TABLE ENTRY)                                     *         
*       P2=A(FILE TOTAL TABLE ENTRY OR ZERO)                          *         
***********************************************************************         
         SPACE 1                                                                
PRNTROW  NTR1  ,                                                                
         LM    R2,R3,0(R1)                                                      
         USING CNTD,R2             R2=COUNTERS TABLE                            
         USING RECTABD,R3          R3=RECORD TABLE ENTRY                        
*                                                                               
         LTR   R3,R3                                                            
         BZ    PROW10                                                           
         LA    R4,TOT                                                           
         USING TOTD,R4                                                          
PROW02   MVC   BYTE,TOTTYPE                                                     
         NC    BYTE,RECINDS1                                                    
         BZ    PROW08                                                           
         GOTO1 ADDCNT,DMCB,TOTCNT,CNTD                                          
PROW08   LA    R4,TOTL(R4)                                                      
         CLI   TOTD,EOT                                                         
         BNE   PROW02                                                           
         DROP  R4                                                               
*                                                                               
PROW10   ICM   R4,15,CNTOUT        R4=NO. OF RECORDS OUTPUT                     
         BZ    PROW12                                                           
         EDIT  (R4),(10,POUT)                                                   
*                                                                               
PROW12   ICM   R5,15,CNTPUR        R5=NO. OF RECORDS PURGED                     
         BZ    PROW14                                                           
         EDIT  (R5),(10,PPUR)                                                   
*                                                                               
PROW14   AR    R4,R5               R4=NO. OF RECORDS INPUT                      
         EDIT  (R4),(10,PINP)                                                   
*                                                                               
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         DROP  R2,R3                                                            
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO ADD COUNTERS                                             *         
*                                                                     *         
* NTRY: P1=A(COUNTERS TO BE ADDED TO)                                 *         
*       P2=A(COUNTERS TO BE ADDED)                                    *         
***********************************************************************         
         SPACE 1                                                                
ADDCNT   STM   RE,R1,12(RD)                                                     
         LM    RE,RF,0(R1)                                                      
         USING CNTD,RE                                                          
         ICM   R0,15,CNTOUT-CNTD(RF)                                            
         BZ    ADDCNT2                                                          
         ICM   R1,15,CNTOUT                                                     
         AR    R1,R0                                                            
         STCM  R1,15,CNTOUT                                                     
ADDCNT2  ICM   R0,15,CNTPUR-CNTD(RF)                                            
         BZ    ADDCNTX                                                          
         ICM   R1,15,CNTPUR                                                     
         AR    R1,R0                                                            
         STCM  R1,15,CNTPUR                                                     
         DROP  RE                                                               
ADDCNTX  LM    RE,R1,12(RD)                                                     
         BR    RE                                                               
         EJECT                                                                  
         EJECT                                                                  
EOT      EQU   X'00'                                                            
         SPACE 1                                                                
         LTORG                                                                  
VLDTPTR  DC    V(LDTPTR)                                                        
         SPACE 1                                                                
ARECTAB  DC    A(RECTAB)                                                        
ARECTABU DC    A(RECTABU-1)                                                     
ACPYTAB  DC    A(CPYTAB)                                                        
         SPACE 1                                                                
VRECTYP  DC    V(ACRECTYP)                                                      
         SPACE 1                                                                
HONE     DC    H'1'                                                             
         SPACE 1                                                                
T@UNKEQU DC    CL(L'RECNAME)'''000'' Unknown record equate'                     
T@UNKTYP DC    CL(L'RECNAME)'X''00'' Unknown record type'                       
T@UNKOTH DC    CL(L'RECNAME)'.......All other unknowns'                         
         SPACE 1                                                                
FILECNTS DC    (RECTABN)XL(CNTL)'00' FILE TOTAL COUNTERS                        
         EJECT                                                                  
***********************************************************************         
* TOTALS TABLE (FOR COMPANY)                                          *         
***********************************************************************         
         SPACE 1                                                                
TOTD     DSECT                                                                  
TOTNAME  DS    CL(L'RECNAME)       TOTAL NAME                                   
TOTTYPE  DS    XL1                 TOTAL TYPE (SEE RECINDS1)                    
TOTCNT   DS    XL(CNTL)            COUNTERS                                     
TOTL     EQU   *-TOTD                                                           
         SPACE 1                                                                
ACYE02   CSECT                                                                  
TOT      DS    0XL(TOTL)                                                        
*                                                                               
         DC    CL(L'TOTNAME)'* Total of Unknowns'                               
         DC    AL1(RECIUNK),XL(CNTL)'00'                                        
*                                                                               
         DC    CL(L'TOTNAME)'* Total of ACCARC records'                         
         DC    AL1(RECIARC),XL(CNTL)'00'                                        
*                                                                               
         DC    CL(L'TOTNAME)'* Total of ACCMST records'                         
         DC    AL1(RECIMST),XL(CNTL)'00'                                        
*                                                                               
         DC    CL(L'TOTNAME)'* Total of Records on File'                        
         DC    AL1(RECIMST+RECIARC),XL(CNTL)'00'                                
*                                                                               
         DC    CL(L'TOTNAME)'* Total of ACCDIR records'                         
         DC    AL1(RECIDIR),XL(CNTL)'00'                                        
*                                                                               
TOTX     DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* RECORD TABLE                                                        *         
***********************************************************************         
         SPACE 1                                                                
RECTABD  DSECT                                                                  
RECNAME  DS    CL30                RECORD NAME                                  
RECTYPE  DS    AL1                 RECORD TYPE                                  
RECINDS1 DS    XL1                 INDICATORS                                   
RECIFILE EQU   X'80'               ADD INTO FILE TOTALS ONLY                    
RECIDIR  EQU   X'40'               ADD INTO ACCDIR TOTAL                        
RECIMST  EQU   X'20'               ADD INTO ACCMST TOTAL                        
RECIARC  EQU   X'10'               ADD INTO ACCARC TOTAL                        
RECIUNK  EQU   X'08'               ADD INTO UNKNOWN TOTAL                       
RECIPLD  EQU   X'04'               PEELED TRANSACTIONS                          
         DS    XL6                 N/D                                          
RECUNKEY DS    XL1                 FIRST BYTE OF KEY OF UNKNOWN TYPE            
RECTABL  EQU   *-RECTABD                                                        
         SPACE 1                                                                
ACYE02   CSECT                                                                  
RECTAB   DS    0X                                                               
*                                                                               
         DC    CL(L'RECNAME)'ACCMST Header'                                     
         DC    AL1(ACRTHDRA)                                                    
         DC    AL1(RECIFILE+RECIDIR+RECIMST,0,0,0,0,0,0,0)                      
*                                                                               
         DC    CL(L'RECNAME)'ACCARC Header'                                     
         DC    AL1(ACRTHDRB)                                                    
         DC    AL1(RECIFILE+RECIDIR+RECIARC,0,0,0,0,0,0,0)                      
*                                                                               
         DC    CL(L'RECNAME)'ACCMST Trailer'                                    
         DC    AL1(ACRTTRLA)                                                    
         DC    AL1(RECIFILE+RECIDIR+RECIMST,0,0,0,0,0,0,0)                      
*                                                                               
         DC    CL(L'RECNAME)'ACCARC Trailer'                                    
         DC    AL1(ACRTTRLB)                                                    
         DC    AL1(RECIFILE+RECIDIR+RECIARC,0,0,0,0,0,0,0)                      
*                                                                               
         DC    CL(L'RECNAME)'Company'                                           
         DC    AL1(ACRTCPY)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'Unit'                                              
         DC    AL1(ACRTUNT)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'Ledger'                                            
         DC    AL1(ACRTLDG)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'High Account'                                      
         DC    AL1(ACRTACTH)                                                    
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'Low Account'                                       
         DC    AL1(ACRTACTL)                                                    
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'Account Office'                                    
         DC    AL1(ACRTOFA)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'Contra-account Passive'                            
         DC    AL1(ACRTCHDP)                                                    
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'Contra-account Header'                             
         DC    AL1(ACRTCHDH)                                                    
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'History Bucket'                                    
         DC    AL1(ACRTCAC)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'ACCMST Transaction'                                
         DC    AL1(ACRTTRN)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'Time Management   '                                
         DC    AL1(ACRTTIM)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'Person Time'                                       
         DC    AL1(ACRTTRN)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'ACCARC Transaction'                                
         DC    AL1(ACRTTRNA)                                                    
         DC    AL1(RECIDIR+RECIARC,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''01'' Office'                                    
         DC    AL1(ACRTOFF)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''02'' Invoice# Passive'                          
         DC    AL1(ACRTINV)                                                     
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''03'' New Batch'                                 
         DC    AL1(ACRTNBT)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''04'' New Batch Passive'                         
         DC    AL1(ACRTNBP)                                                     
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''05'' Tax Rules'                                 
         DC    AL1(ACRTTAX)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''06'' Billing Source'                            
         DC    AL1(ACRTBSC)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''07'' Dutch Reconcile Passive'                   
         DC    AL1(ACRTMRH)                                                     
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''08'' Media Interface'                           
         DC    AL1(ACRTPMD)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''09'' Production Media'                          
         DC    AL1(ACRTMIN)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''0A'' Analysis Code'                             
         DC    AL1(ACRTWCO)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''0B'' Batch'                                     
         DC    AL1(ACRTBAT)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''0C'' Comment'                                   
         DC    AL1(ACRTSCM)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''0F'' Person'                                    
         DC    AL1(ACRTPER)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''10'' Check AuthoriYEtion'                       
         DC    AL1(ACRTCKA)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''11'' Advertiser'                                
         DC    AL1(ACRTADV)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''11'' Advertiser Passive'                        
         DC    AL1(ACRTACP)                                                     
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''12'' Account Group'                             
         DC    AL1(ACRTAGR)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''13'' Account Group Passive'                     
         DC    AL1(ACRTAGP)                                                     
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''14'' Activity Passive'                          
         DC    AL1(ACRTRAP)                                                     
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''16'' Override control'                          
         DC    AL1(ACRTOCO)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''17'' Trans./Contra Passive'                     
         DC    AL1(ACRTTCP)                                                     
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''19'' Adjustment Rate'                           
         DC    AL1(ACRTPAR)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''1A'' Order'                                     
         DC    AL1(ACRTORD)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''1B'' Budget'                                    
         DC    AL1(ACRTBUD)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''1C'' Price List'                                
         DC    AL1(ACRTPRL)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''1D'' List'                                      
         DC    AL1(ACRTLST)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''1E'' Group Invoice'                             
         DC    AL1(ACRTGIN)                                                     
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''1F'' Old Artist Fee Control'                    
         DC    AL1(ACRTFEEC)                                                    
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''20'' Old Artist Fee Area'                       
         DC    AL1(ACRTFEEA)                                                    
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''21'' Old Artist Fee Percent'                    
         DC    AL1(ACRTFEEP)                                                    
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''22'' Order Reservation'                         
         DC    AL1(ACRTORES)                                                    
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''23'' German Bank/Client ptr'                    
         DC    AL1(ACRTGBC)                                                     
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''29'' Charge Rate (TMS)'                         
         DC    AL1(ACRTPCRT)                                                    
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''2A'' Charge Rate'                               
         DC    AL1(ACRTPCR)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''2B'' Bill/Debtor passives'                      
         DC    AL1(ACRTBDP)                                                     
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''2C02'' Office Group'                            
         DC    AL1(ACRTOGRG)                                                    
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''2C04'' Office'                                  
         DC    AL1(ACRTOGRO)                                                    
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''2C06'' Media Group'                             
         DC    AL1(ACRTMGR)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''2C08'' Workcode Group'                          
         DC    AL1(ACRTWGR)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''2C10'' User Field'                              
         DC    AL1(ACRTUFS)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''2C20'' Options'                                 
         DC    AL1(ACRTPOP)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''2C22'' Auto Job Number'                         
         DC    AL1(ACRTAJN)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''2C23'' Studio Type'                             
         DC    AL1(ACRTSTU)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''2C24'' Unit Pricing'                            
         DC    AL1(ACRTPRC)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''2C30'' Scheme Header'                           
         DC    AL1(ACRTSCH)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''2C32'' Category'                                
         DC    AL1(ACRTCAT)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''2C34'' Panel'                                   
         DC    AL1(ACRTPAN)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''2C36'' Estimate Version'                        
         DC    AL1(ACRTEVE)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''2C38'' Text'                                    
         DC    AL1(ACRTTXT)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''2C3A'' Group Bill'                              
         DC    AL1(ACRTGRB)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''2C3C'' Session Estimate'                        
         DC    AL1(ACRTSES)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''2C3E'' Job Cycle Bill'                          
         DC    AL1(ACRTJCB)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''2C40'' Project Date'                            
         DC    AL1(ACRTDAT)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''2C42'' Time Sheet List'                         
         DC    AL1(ACRTTSL)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''2D01'' Sales Tax'                               
         DC    AL1(ACRTSUT)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''2D02'' Scribe Format'                           
         DC    AL1(ACRTRES)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''2D03'' Intagy Estimate'                         
         DC    AL1(ACRTINT)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''2D04'' Intagy Journal Passive'                  
         DC    AL1(ACRTIDJ)                                                     
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''2D05'' Interest Rate'                           
         DC    AL1(ACRTRAT)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''2D06'' Scribe Keyword'                          
         DC    AL1(ACRTKWD)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''2D40'' Date Scheme'                             
         DC    AL1(ACRTDAT)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''2E'' Split Billing'                             
         DC    AL1(ACRTSBL)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''2F00'' Media Detail'                            
         DC    AL1(ACRTMPD)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''2F01'' Media Rules'                             
         DC    AL1(ACRTMPR)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''30'' Office/Account Passive'                    
         DC    AL1(ACRTOAP)                                                     
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''31'' Acct/Name Change Passive'                  
         DC    AL1(ACRTANC)                                                     
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''32'' Name Search Passive'                       
         DC    AL1(ACRTSRC)                                                     
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''3300'' Prod. Bill Control'                      
         DC    AL1(ACRTPBC)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''3301'' Active Prod. Bills'                      
         DC    AL1(ACRTPBA)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''3302'' Passive Prod. Bills'                     
         DC    AL1(ACRTPBP)                                                     
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''34'' Prod. Trans. Activity'                     
         DC    AL1(ACRTPTA)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''3502'' Studio PO Passive'                       
         DC    AL1(ACRTSPO)                                                     
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''3504'' Agency PO Passive'                       
         DC    AL1(ACRTAPO)                                                     
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''36'' Filter Name/Value'                         
         DC    AL1(ACRTRSF)                                                     
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''3D01'' Account Contract Record'                 
         DC    AL1(ACRTCONT)                                                    
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''3D02'' Account Contract Passive'                
         DC    AL1(ACRTCONP)                                                    
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''3E01'' Cost Allocation Hist'                    
         DC    AL1(ACRTCAH)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''3E02'' Cost Allocation Method'                  
         DC    AL1(ACRTCMT)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''3E03'' Cost Payroll Code'                       
         DC    AL1(ACRTPAY)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''3E05'' Cost Payroll History'                    
         DC    AL1(ACRTPHI)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''3E07'' Cost Personal Rates'                     
         DC    AL1(ACRTCPR)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''3E09'' Cost Profile'                            
         DC    AL1(ACRTCAP)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''3E0A'' Costing Client Profile'                  
         DC    AL1(ACRTCCP)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''3E0B'' Cost Calendar '                          
         DC    AL1(ACRTCAS)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''3E0C'' Cost Calendar Passive'                   
         DC    AL1(ACRTCASP)                                                    
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''3E0D'' Cost Standard Hours'                     
         DC    AL1(ACRTSTD)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''3E0E'' Time Total         '                     
         DC    AL1(ACRTTTH)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''3E0F'' Timesheet Wkly Passive'                  
         DC    AL1(ACRTTSW)                                                     
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''3E10'' Edit Hours            '                  
         DC    AL1(ACRTEDT)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''3E11'' Timesheet Save Record '                  
         DC    AL1(ACRTSSAV)                                                    
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''3E12'' Cost Person Id Passive'                  
         DC    AL1(ACRTPID)                                                     
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''3E13'' Timesheet Tempo X-Ref'                   
         DC    AL1(ACRTTPOX)                                                    
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''3F01'' Stored Request'                          
         DC    AL1(ACRTSRM)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    20XL(RECTABL)'00'   FOR UNKNOWN RECORD TYPES/EQUATES             
RECTABU  DC    XL(RECTABL)'00'     IN CASE TOO MANY UNKNOWN TYPES               
*                                                                               
RECTABN  EQU   (*-RECTAB)/RECTABL                                               
RECTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* COUNTER TABLE                                                       *         
***********************************************************************         
         SPACE 1                                                                
CNTD     DSECT                                                                  
CNTOUT   DS    XL4                 NUMBER OF RECORDS OUTPUT                     
CNTPUR   DS    XL4                 NUMBER OF RECORDS PURGED                     
CNTL     EQU   *-CNTD                                                           
         SPACE 1                                                                
***********************************************************************         
* COMPANY TABLE                                                       *         
***********************************************************************         
         SPACE 1                                                                
CPYTABD  DSECT                                                                  
CPYCODE  DS    XL1                 COMPANY CODE                                 
CPYINDS  DS    XL1                 INDICATORS                                   
CPYICNT  EQU   X'80'               RECORD COUNTED FOR THIS COMPANY              
CPYCNTS  DS    (RECTABN)XL(CNTL)   COUNTERS FOR RECORD TYPES                    
CPYTABL  EQU   *-CPYTABD                                                        
         SPACE 1                                                                
ACYE02   CSECT                                                                  
CPYTAB   DC    (256*CPYTABL)X'00'                                               
         DS    0X                                                               
CPYTABN  EQU   (*-CPYTAB)/CPYTABL                                               
         EJECT                                                                  
***********************************************************************         
* LOCAL WORKING STORAGE                                               *         
***********************************************************************         
         SPACE 1                                                                
WORKD    DSECT                                                                  
PARMS    DS    0XL4                                                             
PCODE    DS    XL1                 INPUT CODE                                   
PAREC    DS    AL3                 A(RECORD)                                    
WRECTYPE DS    XL1                                                              
WCPYCODE DS    XL1                                                              
WORKL    EQU   *-WORKD                                                          
         SPACE 1                                                                
* ACREPWORKD                                                                    
       ++INCLUDE ACREPWORKD                                                     
         ORG   P                   ** PRINT LINE **                             
PBXL     DS    CL1                                                              
         DS    CL2                                                              
PREC     DS    CL(L'RECNAME)                                                    
         DS    CL1                                                              
PBXC1    DS    CL1                                                              
         DS    CL3                                                              
PINP     DS    CL10                                                             
         DS    CL3                                                              
PBXC2    DS    CL1                                                              
         DS    CL3                                                              
PPUR     DS    CL10                                                             
         DS    CL3                                                              
PBXC3    DS    CL1                                                              
         DS    CL3                                                              
POUT     DS    CL10                                                             
         DS    CL3                                                              
PBXR     DS    CL1                                                              
PBXLEN   EQU   *-PBXL                                                           
         SPACE 1                                                                
* DDBIGBOX                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* DDLOGOD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDLOGOD                                                        
         PRINT ON                                                               
         SPACE 1                                                                
* ACRECEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACRECEQUS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* ACRCVRECD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACRCVRECD                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* ACGENMODES                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'038ACREPYE02 08/17/00'                                      
         END                                                                    
