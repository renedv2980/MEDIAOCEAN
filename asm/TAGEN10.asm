*          DATA SET TAGEN10    AT LEVEL 042 AS OF 03/20/15                      
*PHASE T70210C,*                                                                
         TITLE 'T70210 - SYSTEM MAINTENANCE'                                    
T70210   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70210                                                         
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
         CLI   MODE,VALKEY         FIRST TIME IN                                
         BNE   SYS10                                                            
         GOTO1 RECVAL,DMCB,TLSYCDQ,(X'40',0)                                    
         B     XIT                                                              
         SPACE 3                                                                
SYS10    CLI   MODE,DISPREC        IF MODE IS DISPLAY                           
         BE    SYS15                                                            
         CLI   MODE,XRECPUT        OR RECORD CHANGED                            
         BNE   SYS20                                                            
         SPACE 1                                                                
SYS15    BAS   RE,DISPLAY          (RE-)DISPLAY THE RECORD                      
         B     XIT                                                              
         SPACE 3                                                                
SYS20    CLI   MODE,VALREC         VALIDATE THE RECORD                          
         BNE   XIT                                                              
         CLC   SSYLAST+1(2),=X'0101'  IF SCREEN JUST LOADED                     
         BNE   SYS30                                                            
         GOTO1 RECVAL,DMCB,TLSYCDQ,(X'20',0)  GET THE RECORD                    
         BAS   RE,DISPLAY                     DISPLAY RECORD FIRST              
         MVC   TWAKEYSV,KEY        NEED BECAUSE DON'T GET VALKEY MODE           
*                                  WHEN DO ACTION CHANGE FROM OTHER REC         
         B     DISPLYD             (NEED THIS WHEN DON'T HAVE KEY FLDS)         
         SPACE 1                                                                
SYS30    BAS   RE,BLDREC                                                        
         B     XIT                                                              
         EJECT                                                                  
*              DISPLAY THE RECORD                                               
         SPACE 1                                                                
         USING TASYD,R4                                                         
DISPLAY  NTR1                                                                   
         TWAXC SSYUSBKH                                                         
         MVI   ELCODE,TASYELQ      GET SYSTEM CONTROL ELEMENT                   
         L     R4,AIO                                                           
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         SPACE                                                                  
         MVC   SSYUSBK,TASYUSBK                  USA BANK A/C                   
         MVC   SSYUSST,TASYUSST                  NEXT CHECK                     
         MVC   SSYCNBK,TASYCNBK                  CANADA BANK A/C                
         MVC   SSYCNST,TASYCNST                  NEXT CHECK                     
         MVC   SSYEUBK,TASYEUBK                  EURO BANK A/C                  
         MVC   SSYEUST,TASYEUST                  NEXT CHECK                     
         MVC   SSYPCBK,TASYPCBK                  PRINT BANK A/C                 
         MVC   SSYPCST,TASYPCST                  NEXT CHECK                     
         MVC   SSYLCBK,TASYLCBK                  P+ BANK A/C                    
         MVC   SSYLCST,TASYLCST                  NEXT CHECK                     
         SPACE 1                                                                
         MVC   SVSYLWSU,TASYLWSU   SAVE LAST WEB SUMMARY RUN DATE               
         SPACE 1                                                                
*        OC    TASYLPMU,TASYLPMU                                                
*        BZ    DISP10                                                           
*        GOTO1 HEXOUT,DMCB,TASYLPMU,SSYLPMU,4    LAST PMUSIC #                  
         EDIT  TASYLPMU,SSYLPMU,ALIGN=RIGHT,FILL=0                              
         SPACE 1                                                                
DISP10   OC    TASYLCOM,TASYLCOM                                                
         BZ    DISP12                                                           
         GOTO1 HEXOUT,DMCB,TASYLCOM,SSYLCOM,4    LAST COMMERCIAL #              
         SPACE 1                                                                
DISP12   OC    TASYLMKT,TASYLMKT                                                
         BZ    DISP14                                                           
         GOTO1 HEXOUT,DMCB,TASYLMKT,SSYLMKT,4    LAST MARKET #                  
         SPACE 1                                                                
DISP14   OC    TASYLAGT,TASYLAGT                                                
         BZ    DISP16                                                           
         GOTOR TRNSAGT,DMCB,(X'40',TASYLAGT),SSYLAGT                            
         SPACE 1                                                                
DISP16   EDIT  TASYCCVT,(6,SSYCCVT),2,ALIGN=LEFT                                
         EDIT  TASYECVT,(6,SSYECVT),2,ALIGN=LEFT                                
         SPACE 1                                                                
         MVC   SSYEMP,TASYEMP      DEFAULT EMPLOYER                             
         MVI   SSYEMPH+5,3         SET LENGTH FOR RECVAL                        
         MVC   AIO,AIO2            MAKE SURE DON'T CREAM RECORD                 
         GOTO1 RECVAL,DMCB,TLEMCDQ,(4,SSYEMPH)                                  
         MVC   AIO,AIO1            RESTORE AIO                                  
         SPACE 1                                                                
         MVC   SSYID,TASYIDCD      CONTROL FILE ID                              
         MVC   AIO,AIO2            MAKE SURE DON'T CREAM RECORD                 
         GOTO1 USERVAL,DMCB,(X'80',TASYIDCD)                                    
         MVC   AIO,AIO1            RESTORE AIO                                  
         SPACE 1                                                                
         OC    TASYLKDT,TASYLKDT   LOCK DATE                                    
         BZ    DISP20                                                           
         GOTO1 DATCON,DMCB,(1,TASYLKDT),(8,SSYLKDT)                             
         SPACE 1                                                                
DISP20   OC    TASYLKST,TASYLKST   LOCK START TIME                              
         BZ    DISP24                                                           
         GOTO1 TIMECON,DMCB,TASYLKST,TASYLKDT,(8,SSYLKST)                       
         SPACE 1                                                                
DISP24   OC    TASYLKEN,TASYLKEN   LOCK END TIME                                
         BZ    DISP28                                                           
         GOTO1 TIMECON,DMCB,TASYLKEN,TASYLKDT,(8,SSYLKEN)                       
         SPACE 1                                                                
DISP28   OC    TASYPLDT,TASYPLDT   PRINT LOCK DATE                              
         BZ    DISP30                                                           
         GOTO1 DATCON,DMCB,(1,TASYPLDT),(8,SSYPLDT)                             
         SPACE 1                                                                
DISP30   OC    TASYPLST,TASYPLST   PRINT LOCK START TIME                        
         BZ    DISP34                                                           
         GOTO1 TIMECON,DMCB,TASYPLST,TASYPLDT,(8,SSYPLST)                       
         SPACE 1                                                                
DISP34   OC    TASYPLEN,TASYPLEN   PRINT LOCK END TIME                          
         BZ    DISP35                                                           
         GOTO1 TIMECON,DMCB,TASYPLEN,TASYPLDT,(8,SSYPLEN)                       
         SPACE 1                                                                
DISP35   CLI   TASYLEN,TASYLNQ                                                  
         BL    DISP38                                                           
         OC    TASYLLDT,TASYLLDT   P+ LOCK DATE                                 
         BZ    DISP36                                                           
         GOTO1 DATCON,DMCB,(1,TASYLLDT),(8,SSYLLDT)                             
         SPACE 1                                                                
DISP36   OC    TASYLLST,TASYLLST   P+ LOCK START TIME                           
         BZ    DISP37                                                           
         GOTO1 TIMECON,DMCB,TASYLLST,TASYLLDT,(8,SSYLLST)                       
         SPACE 1                                                                
DISP37   OC    TASYLLEN,TASYLLEN   P+ LOCK END TIME                             
         BZ    DISP38                                                           
         GOTO1 TIMECON,DMCB,TASYLLEN,TASYLLDT,(8,SSYLLEN)                       
         SPACE 1                                                                
DISP38   MVI   SSYLYR,C'N'                                                      
         TM    TASYSTAT,TASYSLYR   ALLOW ADJ TO POST TO LAST YEAR               
         BZ    *+8                                                              
         MVI   SSYLYR,C'Y'                                                      
         SPACE 1                                                                
         MVI   SSYBRB,C'N'                                                      
         TM    TASYSTAT,TASYSBRT   BILLING READS BRATE?                         
         BZ    *+8                                                              
         MVI   SSYBRB,C'Y'                                                      
         SPACE 1                                                                
         OC    TASYITCD,TASYITCD   GRT INSTALLMENT TIMING CHANGE DATE           
         BZ    DISP39                                                           
         GOTO1 DATCON,DMCB,(1,TASYITCD),(8,SSYITCD)                             
         SPACE 1                                                                
DISP39   MVI   SSYCRD,C'N'                                                      
         TM    TASYSTAT,TASYSCRD   NEW CREDITING RULES ENABLED?                 
         BZ    *+8                                                              
         MVI   SSYCRD,C'Y'                                                      
         SPACE 1                                                                
         MVI   SSYNCS,C'N'                                                      
         TM    TASYSTA2,TASYSNCS   CHARGE NY/CA SURCHARGE?                      
         BZ    *+8                                                              
         MVI   SSYNCS,C'Y'                                                      
         SPACE 1                                                                
         MVI   SSYULA,C'N'                                                      
         TM    TASYSTA2,TASYSULA   PRINT CA URGENTS IN LA?                      
         BZ    *+8                                                              
         MVI   SSYULA,C'Y'                                                      
         SPACE 1                                                                
         MVI   SSYFLT,C'N'                                                      
         TM    TASYSTAT,TASYSFLT   NEW FLAT TAX RULES ENABLED?                  
         BZ    *+8                                                              
         MVI   SSYFLT,C'Y'                                                      
         SPACE 1                                                                
         MVI   SSYROI,C'N'                                                      
         TM    TASYSTA2,TASYSROI   RETAIN ORDER OF INPUT?                       
         BZ    *+8                                                              
         MVI   SSYROI,C'Y'                                                      
         SPACE 1                                                                
         MVI   SSYNDC,C'N'                                                      
         TM    TASYSTA2,TASYSNDC   NEW DUE COMPANY RULES ENABLED?               
         BZ    *+8                                                              
         MVI   SSYNDC,C'Y'                                                      
         SPACE 1                                                                
         MVI   SSYMQR,C'N'                                                      
         TM    TASYSTA2,TASYSMQR   MQ MESSAGE FOR HF REISSUE?                   
         BZ    *+8                                                              
         MVI   SSYMQR,C'Y'                                                      
         SPACE 1                                                                
         MVI   SSYVHW,C'N'                                                      
         TM    TASYSTA2,TASYSVHW   ALLOW HI/WA WITH OTHER STATES?               
         BZ    *+8                                                              
         MVI   SSYVHW,C'Y'                                                      
         SPACE 1                                                                
         MVI   SSYECL,C'N'                                                      
         TM    TASYSTAT,TASYSECL   ENFORCE LOCKING RULES AT CRP LVL?            
         BZ    *+8                                                              
         MVI   SSYECL,C'Y'                                                      
         SPACE 1                                                                
         L     R2,TASYSTBL                                                      
         AH    R2,=Y(TGAGRACT-TGTABLES)                                         
                                                                                
         GOTO1 HEXOUT,DMCB,2(R2),SSYGEN+2,2,0                                   
         L     R2,0(R2)                                                         
         A     R2,TASYSTBL                                                      
         USING RACTD,R2                                                         
         MVI   SSYGEN,C'N'                                                      
         GOTO1 TSTLCKT,DMCB,=CL10'TAL_GNRACT'                                   
         BNE   *+8                                                              
         MVI   SSYGEN,C'Y'                                                      
         SPACE 1                                                                
         L     R2,TASYSTBL                                                      
         AH    R2,=Y(TGARRACT-TGTABLES)                                         
                                                                                
         GOTO1 HEXOUT,DMCB,2(R2),SSYREP+2,2,0                                   
         L     R2,0(R2)                                                         
         A     R2,TASYSTBL                                                      
         MVI   SSYREP,C'N'                                                      
         GOTO1 TSTLCKT,DMCB,=CL10'TAL_RPRACT'                                   
         BNE   *+8                                                              
         MVI   SSYREP,C'Y'                                                      
         SPACE 1                                                                
         GOTO1 ACTVOUT,DMCB,(X'80',SSYLCHGH)  LAST CHANGED                      
         B     XIT                                                              
         EJECT                                                                  
*              BUILD THE RECORD                                                 
         SPACE 1                                                                
         USING TASYD,R4                                                         
BLDREC   NTR1                                                                   
         MVI   ELCODE,TASYELQ      SYSTEM CONTROL ELEMENT                       
         GOTO1 REMELEM             DELETE CURRENT                               
         SPACE                                                                  
         LA    R4,ELEMENT          ELEMENT AS EXISTED                           
*                                  SAVE URGENT CHECK LOCKOUT FIELDS             
         MVC   WORK(TASYLKLQ),TASYLKDT                                          
         MVC   WORK+TASYLKLQ(TASYPLLQ),TASYPLDT                                 
         MVC   WORK+TASYLKLQ+TASYPLLQ(TASYLLLQ),TASYLLDT                        
         SPACE                                                                  
         XC    ELEMENT,ELEMENT     START NEW ELEMENT                            
         MVI   TASYEL,TASYELQ                                                   
         MVI   TASYLEN,TASYLNQ                                                  
*                                  RESTORE URGENT CHECK LOCKOUT FIELDS          
         MVC   TASYLKDT(TASYLKLQ),WORK                                          
         MVC   TASYPLDT(TASYPLLQ),WORK+TASYLKLQ                                 
         MVC   TASYLLDT(TASYLLLQ),WORK+TASYLKLQ+TASYPLLQ                        
         SPACE 1                                                                
         MVC   TASYLWSU,SVSYLWSU   RESTORE LAST WEB SUMMARY RUN DATE            
         SPACE 1                                                                
         CLI   SSYUSBKH+5,0        IF THERE IS INPUT                            
         BE    *+16                                                             
         MVC   TASYUSBK,SSYUSBK    MOVE INTO ELEMENT                            
         OC    TASYUSBK,SPACES     PAD WITH SPACES                              
         SPACE 1                                                                
         CLI   SSYUSSTH+5,0                                                     
         BE    *+16                                                             
         MVC   TASYUSST,SSYUSST                                                 
         OC    TASYUSST,SPACES                                                  
         SPACE 1                                                                
         CLI   SSYCNBKH+5,0                                                     
         BE    *+16                                                             
         MVC   TASYCNBK,SSYCNBK                                                 
         OC    TASYCNBK,SPACES                                                  
         SPACE 1                                                                
         CLI   SSYCNSTH+5,0                                                     
         BE    *+16                                                             
         MVC   TASYCNST,SSYCNST                                                 
         OC    TASYCNST,SPACES                                                  
         SPACE 1                                                                
         CLI   SSYEUBKH+5,0                                                     
         BE    *+16                                                             
         MVC   TASYEUBK,SSYEUBK                                                 
         OC    TASYEUBK,SPACES                                                  
         SPACE 1                                                                
         CLI   SSYEUSTH+5,0                                                     
         BE    *+16                                                             
         MVC   TASYEUST,SSYEUST                                                 
         OC    TASYEUST,SPACES                                                  
         SPACE 1                                                                
         CLI   SSYPCBKH+5,0                                                     
         BE    *+16                                                             
         MVC   TASYPCBK,SSYPCBK                                                 
         OC    TASYPCBK,SPACES                                                  
         SPACE 1                                                                
         CLI   SSYPCSTH+5,0                                                     
         BE    *+16                                                             
         MVC   TASYPCST,SSYPCST                                                 
         OC    TASYPCST,SPACES                                                  
         SPACE 1                                                                
         CLI   SSYLCBKH+5,0                                                     
         BE    *+16                                                             
         MVC   TASYLCBK,SSYLCBK                                                 
         OC    TASYLCBK,SPACES                                                  
         SPACE 1                                                                
         CLI   SSYLCSTH+5,0                                                     
         BE    *+16                                                             
         MVC   TASYLCST,SSYLCST                                                 
         OC    TASYLCST,SPACES                                                  
         SPACE 1                                                                
         LA    R2,SSYLPMUH                                                      
         ZIC   R3,5(R2)                                                         
         LTR   R3,R3                                                            
         BZ    BLDR05                                                           
         CH    R3,=H'8'                                                         
         BL    FLDINV              INPUT LENGTH MUST BE 8                       
*        GOTO1 HEXIN,DMCB,SSYLPMU,TASYLPMU,(R3) LAST PMUSIC #                   
*        OC    DMCB+12(4),DMCB+12                                               
*        BZ    FLDINV              INVALID IF 4TH PARAMETER IS 0                
         PACK  DUB,SSYLPMU                                                      
         CVB   RE,DUB                                                           
         STCM  RE,15,TASYLPMU                                                   
         SPACE 1                                                                
BLDR05   LA    R2,SSYLCOMH                                                      
         ZIC   R3,5(R2)                                                         
         LTR   R3,R3                                                            
         BZ    BLDR10                                                           
         CH    R3,=H'8'                                                         
         BL    FLDINV              INPUT LENGTH MUST BE 8                       
         GOTO1 HEXIN,DMCB,SSYLCOM,TASYLCOM,(R3) LAST COMMERCIAL #               
         OC    DMCB+12(4),DMCB+12                                               
         BZ    FLDINV              INVALID IF 4TH PARAMETER IS 0                
         SPACE 1                                                                
BLDR10   LA    R2,SSYLMKTH                                                      
         ZIC   R3,5(R2)                                                         
         LTR   R3,R3                                                            
         BZ    BLDR12                                                           
         CH    R3,=H'8'                                                         
         BL    FLDINV              INPUT LENGTH MUST BE 8                       
         GOTO1 HEXIN,DMCB,SSYLMKT,TASYLMKT,(R3) LAST MARKET #                   
         OC    DMCB+12(4),DMCB+12                                               
         BZ    FLDINV              INVALID IF 4TH PARAMETER IS 0                
         SPACE 1                                                                
BLDR12   LA    R2,SSYLAGTH                                                      
         CLI   5(R2),0             LAST AGENT #                                 
         BE    BLDR20                                                           
         GOTOR TRNSAGT,DMCB,(X'80',SSYLAGT),TASYLAGT                            
         BNE   FLDINV                                                           
         SPACE 1                                                                
BLDR20   LA    R2,SSYCCVTH         CANADIAN CONVERSION RATE                     
         CLI   5(R2),0                                                          
         BNE   BLDR30                                                           
         ZAP   TASYCCVT,=P'0'      INSURE PACKED FIELD                          
         B     BLDR40                                                           
         SPACE 1                                                                
BLDR30   ZIC   R3,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,8(R2),(R3)                                          
         CLI   0(R1),0                                                          
         BNE   FLDINV                                                           
         L     R2,4(R1)                                                         
         CVD   R2,DUB                                                           
         ZAP   TASYCCVT,DUB                                                     
         SPACE 1                                                                
BLDR40   LA    R2,SSYECVTH         EUROPEAN CONVERSION RATE                     
         CLI   5(R2),0                                                          
         BNE   BLDR40A                                                          
         ZAP   TASYECVT,=P'0'      INSURE PACKED FIELD                          
         B     BLDR40B                                                          
         SPACE 1                                                                
BLDR40A  ZIC   R3,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,8(R2),(R3)                                          
         CLI   0(R1),0                                                          
         BNE   FLDINV                                                           
         L     R2,4(R1)                                                         
         CVD   R2,DUB                                                           
         ZAP   TASYECVT,DUB                                                     
         SPACE 1                                                                
BLDR40B  LA    R2,SSYEMPH          DEFAULT EMPLOYER REQUIRED                    
         CLI   5(R2),0                                                          
         BE    FLDMISS                                                          
         TM    4(R2),X'20'         IF DEFAULT EMPR NOT PREV VALIDATED           
         BO    BLDR50                                                           
         MVC   AIO,AIO2            MAKE SURE DON'T CREAM RECORD                 
         GOTO1 RECVAL,DMCB,TLEMCDQ,(R2) VALIDATE DEFAULT EMPLOYER               
         MVC   AIO,AIO1            RESTORE AIO                                  
BLDR50   MVC   TASYEMP,SSYEMP                                                   
         OC    TASYEMP,SPACES                                                   
         SPACE 1                                                                
         TM    SSYIDH+4,X'20'      IF CONTROL FILE ID NOT VALIDATED             
         BO    BLDR60                                                           
         MVC   AIO,AIO2            MAKE SURE DON'T CREAM RECORD                 
         GOTO1 USERVAL,DMCB,SSYIDH VALIDATE CONTROL FILE ID                     
         MVC   AIO,AIO1            RESTORE AIO                                  
BLDR60   MVC   TASYIDCD,SSYID                                                   
         OC    TASYIDCD,SPACES                                                  
         SPACE 1                                                                
         LA    R2,SSYLKDTH         URGENT CHECK RUN LOCKOUT DATE                
         CLI   5(R2),0                                                          
         BNE   BLDR64                                                           
*                                  IF NO INPUT, CLEAR DATE, TIMES               
         XC    TASYLKDT(L'TASYLKDT+L'TASYLKST+L'TASYLKEN),TASYLKDT              
         B     BLDR68                                                           
         SPACE 1                                                                
BLDR64   GOTO1 DATVAL,DMCB,8(R2),WORK                                           
         OC    0(4,R1),0(R1)                                                    
         BZ    FLDINV                                                           
         GOTO1 DATCON,DMCB,(0,WORK),(1,DUB)                                     
         SPACE 1                                                                
         CLC   TASYLKDT,DUB        IF DATE HAS CHANGED                          
         BE    BLDR65                                                           
         GOTO1 PROTOFF             TURN OFF STORAGE PROTECTION                  
         L     R1,TGACKLK          R1=A(LOCKOUT STATUS BYTE IN CORE)            
         MVI   0(R1),CKLKINIT      SET STATUS TO RE-INITIALIZE                  
         GOTO1 PROTON              TURN ON STORAGE PROTECTION                   
         SPACE 1                                                                
BLDR65   MVC   TASYLKDT,DUB        SET LOCKOUT DATE                             
         SPACE 1                                                                
BLDR68   LA    R2,SSYPLDTH         PRINT URGENT CHECK RUN LOCKOUT DATE          
         CLI   5(R2),0                                                          
         BNE   *+14                                                             
*                                  IF NO INPUT, CLEAR PRINT DATE, TIMES         
         XC    TASYPLDT(L'TASYPLDT+L'TASYPLST+L'TASYPLEN),TASYPLDT              
         B     BLDR70                                                           
         SPACE 1                                                                
         GOTO1 DATVAL,DMCB,8(R2),WORK                                           
         OC    0(4,R1),0(R1)                                                    
         BZ    FLDINV                                                           
         GOTO1 DATCON,DMCB,(0,WORK),(1,TGDUB)                                   
         SPACE 1                                                                
         CLC   TASYPLDT,TGDUB      IF PRINT DATE HAS CHANGED                    
         BE    BLDR69                                                           
         GOTO1 PROTOFF             TURN OFF STORAGE PROTECTION                  
         L     R1,TGACKLK          R1=A(LOCKOUT STATUS BYTE IN CORE)            
         MVI   0(R1),CKLKINIT      SET STATUS TO RE-INITIALIZE                  
         GOTO1 PROTON              TURN ON STORAGE PROTECTION                   
         SPACE 1                                                                
BLDR69   MVC   TASYPLDT,TGDUB      SET PRINT LOCKOUT DATE                       
         SPACE 1                                                                
BLDR70   LA    R2,SSYLLDTH         P+ URGENT CHECK RUN LOCKOUT DATE             
         CLI   5(R2),0                                                          
         BNE   *+14                                                             
*                                  IF NO INPUT, CLEAR P+ DATE, TIMES            
         XC    TASYLLDT(L'TASYLLDT+L'TASYLLST+L'TASYLLEN),TASYLLDT              
         B     BLDR75                                                           
         SPACE 1                                                                
         GOTO1 DATVAL,DMCB,8(R2),WORK                                           
         OC    0(4,R1),0(R1)                                                    
         BZ    FLDINV                                                           
         GOTO1 DATCON,DMCB,(0,WORK),(1,TGDUB)                                   
         SPACE 1                                                                
         CLC   TASYLLDT,TGDUB      IF P+ DATE HAS CHANGED                       
         BE    BLDR71                                                           
         GOTO1 PROTOFF             TURN OFF STORAGE PROTECTION                  
         L     R1,TGACKLK          R1=A(LOCKOUT STATUS BYTE IN CORE)            
         MVI   0(R1),CKLKINIT      SET STATUS TO RE-INITIALIZE                  
         GOTO1 PROTON              TURN ON STORAGE PROTECTION                   
         SPACE 1                                                                
BLDR71   MVC   TASYLLDT,TGDUB      SET PRINT LOCKOUT DATE                       
         SPACE 1                                                                
BLDR75   LA    R2,SSYLYRH          ALLOW ADJ TO POST TO LAST YEAR               
         CLI   5(R2),0                                                          
         BE    BLDR80                                                           
         CLI   8(R2),C'N'                                                       
         BE    BLDR80                                                           
         CLI   8(R2),C'Y'                                                       
         BNE   FLDINV                                                           
         OI    TASYSTAT,TASYSLYR                                                
         SPACE 1                                                                
BLDR80   OI    TASYSTAT,TASYSPID   OK TO USE PID# OVER SS#                      
         OI    TASYSTAT,TASYS3VR   USE 3 CHARACTER VERSION CODES                
         OI    TASYSTAT,TASYSMUS   NEW MUSIC RULES ENABLED                      
         OI    TASYSTA2,TASYSPDJ   PROHIBIT DRAFT JOBS                          
         OI    TASYSTA2,TASYSEEN   SAVE NAMES FOR P+ ENABLED                    
         SPACE 1                                                                
BLDR90   LA    R2,SSYBRBH          BILLING READS BRATE ENABLED?                 
         CLI   5(R2),0                                                          
         BE    BLDR100                                                          
         CLI   8(R2),C'N'                                                       
         BE    BLDR100                                                          
         CLI   8(R2),C'Y'                                                       
         BNE   FLDINV                                                           
         OI    TASYSTAT,TASYSBRT                                                
         SPACE 1                                                                
BLDR100  LA    R2,SSYITCDH         GRT INSTALLMENT TIME CHANGE DATE             
         GOTO1 DTVAL,DMCB,TASYITCD                                              
         SPACE 1                                                                
         LA    R2,SSYCRDH          NEW CREDITING RULES ENABLE?                  
         CLI   5(R2),0                                                          
         BE    BLDR110                                                          
         CLI   8(R2),C'N'                                                       
         BE    BLDR110                                                          
         CLI   8(R2),C'Y'                                                       
         BNE   FLDINV                                                           
         OI    TASYSTAT,TASYSCRD                                                
         SPACE 1                                                                
BLDR110  LA    R2,SSYNCSH          CHARGE NY/CA SURCHARGE?                      
         CLI   5(R2),0                                                          
         BE    BLDR120                                                          
         CLI   8(R2),C'N'                                                       
         BE    BLDR120                                                          
         CLI   8(R2),C'Y'                                                       
         BNE   FLDINV                                                           
         OI    TASYSTA2,TASYSNCS                                                
         SPACE 1                                                                
BLDR120  LA    R2,SSYULAH          PRINT CA URGENTS IN LA?                      
         CLI   5(R2),0                                                          
         BE    BLDR130                                                          
         CLI   8(R2),C'N'                                                       
         BE    BLDR130                                                          
         CLI   8(R2),C'Y'                                                       
         BNE   FLDINV                                                           
         OI    TASYSTA2,TASYSULA                                                
         SPACE 1                                                                
BLDR130  LA    R2,SSYFLTH          NEW FLAT TAX RULES ENABLED?                  
         CLI   5(R2),0                                                          
         BE    BLDR140                                                          
         CLI   8(R2),C'N'                                                       
         BE    BLDR140                                                          
         CLI   8(R2),C'Y'                                                       
         BNE   FLDINV                                                           
         OI    TASYSTAT,TASYSFLT                                                
         SPACE 1                                                                
BLDR140  LA    R2,SSYROIH          RETAIN ORDER OF INPUT?                       
         CLI   5(R2),0                                                          
         BE    BLDR160                                                          
         CLI   8(R2),C'N'                                                       
         BE    BLDR160                                                          
         CLI   8(R2),C'Y'                                                       
         BNE   FLDINV                                                           
         OI    TASYSTA2,TASYSROI                                                
         SPACE 1                                                                
BLDR160  LA    R2,SSYNDCH          NEW DUE COMPANY RULES ENABLED?               
         CLI   5(R2),0                                                          
         BE    BLDR170                                                          
         CLI   8(R2),C'N'                                                       
         BE    BLDR170                                                          
         CLI   8(R2),C'Y'                                                       
         BNE   FLDINV                                                           
         OI    TASYSTA2,TASYSNDC                                                
         SPACE 1                                                                
BLDR170  LA    R2,SSYMQRH          MQ MESSAGE FOR HF REISSUE?                   
         CLI   5(R2),0                                                          
         BE    BLDR180                                                          
         CLI   8(R2),C'N'                                                       
         BE    BLDR180                                                          
         CLI   8(R2),C'Y'                                                       
         BNE   FLDINV                                                           
         OI    TASYSTA2,TASYSMQR                                                
         SPACE 1                                                                
BLDR180  LA    R2,SSYVHWH          ALLOW HI/WA WITH OTHER STATES?               
         CLI   5(R2),0                                                          
         BE    BLDR190                                                          
         CLI   8(R2),C'N'                                                       
         BE    BLDR190                                                          
         CLI   8(R2),C'Y'                                                       
         BNE   FLDINV                                                           
         OI    TASYSTA2,TASYSVHW                                                
         SPACE 1                                                                
BLDR190  LA    R2,SSYECLH          ENFORCE LOCKING RULES AT CRP LVL?            
         CLI   5(R2),0                                                          
         BE    BLDR200                                                          
         CLI   8(R2),C'N'                                                       
         BE    BLDR200                                                          
         CLI   8(R2),C'Y'                                                       
         BNE   FLDINV                                                           
         OI    TASYSTAT,TASYSECL                                                
         SPACE 1                                                                
BLDR200  GOTO1 ADDELEM             ADD NEW ELEMENT                              
         SPACE 1                                                                
         GOTO1 ACTVIN,DMCB,(X'80',0)  LAST CHANGED                              
         B     XIT                                                              
         EJECT                                                                  
*              LOCAL ERROR/EXIT ROUTINES                                        
         SPACE                                                                  
FLDINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     THEEND                                                           
         SPACE                                                                  
FLDMISS  MVI   ERROR,MISSING       MISSING INPUT FIELD                          
         B     THEEND                                                           
         SPACE                                                                  
NUMINV   MVI   ERROR,3             FIELD NOT NUMERIC                            
         B     THEEND                                                           
         SPACE                                                                  
DISPLYD  MVI   MYMSGNO1,4          RECORD DISPLAYED - ENTER CHANGES             
         MVI   MYMSYS,X'FF'                                                     
         OI    GENSTAT2,USGETTXT                                                
         L     R2,AFRSTREC                                                      
         B     THEEND                                                           
         SPACE                                                                  
THEEND   GOTO1 EXIT,DMCB,0                                                      
         SPACE 2                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         SPACE 2                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR10D                                                       
         EJECT                                                                  
*              LOCAL VARIABLES                                                  
*                                                                               
         DS    D                                                                
SVSYLWSU DS    A                   SAVED LAST WEB SUMMARY RUN DATE              
         EJECT                                                                  
* DDGENTWA                                                                      
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TASYSEQUS                                                                     
* TASYSDSECT                                                                    
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'042TAGEN10   03/20/15'                                      
         END                                                                    
