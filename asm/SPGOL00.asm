*          DATA SET SPGOL00    AT LEVEL 065 AS OF 12/18/17                      
*PHASE T20200B                                                                  
         TITLE 'SPGOL00 - SPOTPAK GOALS - BASE'                                 
T20200   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 GENOLDX-GENOLD,T20200,RR=R9,CLEAR=Y                              
*                                                                               
         LA    R7,2048(RC)                                                      
         LA    R7,2048(R7)                                                      
         USING GENOLD,RC,R7                                                     
*                                                                               
         ST    RB,BASERB                                                        
         ST    RD,BASERD                                                        
         ST    R9,RELO                                                          
         ST    R1,SYSPARMS         SAVE A(SYSPARMS)                             
         B     GOINIT                                                           
RELO     DC    F'0'                                                             
*                                                                               
GOINIT   BRAS  RE,INITL                                                         
*                                                                               
         L     RA,VTWA                                                          
         USING T202FFD,RA                                                       
*                                                                               
         MVI   GOLMSGH+7,60        SET MSG LEN TO MAX                           
         XC    GOLMSG,GOLMSG                                                    
         FOUT  GOLMSGH                                                          
*                                                                               
         BRAS  RE,CKDDLINK         COMING FROM DDLINK?                          
         JNE   GOINITX                                                          
*                                                                               
* CALL SPGOL39 TO DEAL WITH DDLINK                                              
*                                                                               
         GOTO1 VCALLOV,DMCB,(X'39',0),VTWA                                      
         CLI   4(R1),X'FF'                                                      
         JE    *+2                                                              
*                                                                               
         L     RF,DMCB             GET OVLY ADDRESS                             
         GOTO1 (RF),DMCB,(RC),(RA)                                              
*                                                                               
         LA    R2,GOLMDH           POINT R2 AT SOME FIELD                       
         J     EXIT                                                             
                                                                                
* CLEAR FORMATTED DISPLAY AREAS IF NEEDED *                                     
                                                                                
GOINITX  CLI   SVSCRN,X'F4'        TEST TRANSFER                                
         BE    TSTX                YES - DIFFERENT SCREEN                       
*                                                                               
         MVC   ELEM(GOLACT1H-GOLACT0H),GOLACT0H   SAVE DATA FOR 'NEXT'          
         LA    R2,GOLACT0H                                                      
         BAS   RE,TSTCLR                                                        
         LA    R2,GOLMKT0H                                                      
         BAS   RE,TSTCLR                                                        
         LA    R2,GOLDPT0H                                                      
         BAS   RE,TSTCLR                                                        
         LA    R2,GOLPER0H                                                      
         BAS   RE,TSTCLR                                                        
*                                                                               
TSTX     B     TESTHL                                                           
*                                                                               
TSTCLR   SR    R5,R5                                                            
         IC    R5,0(R2)                                                         
         SH    R5,=H'9'                                                         
         EX    R5,TSTOC                                                         
         BER   RE                                                               
         EX    R5,TSTXC                                                         
         FOUT  (R2)                                                             
         BR    RE                                                               
TSTOC    OC    8(0,R2),8(R2)                                                    
TSTXC    XC    8(0,R2),8(R2)                                                    
         EJECT                                                                  
* TEST FOR NEW MED/CLT/PRD/EST *                                                
         SPACE 1                                                                
*                                                                               
TESTHL   TM    GOLMDH+4,X'20'                                                   
         BZ    EDITHL                                                           
         TM    GOLCLH+4,X'20'                                                   
         BZ    EDITHL                                                           
         TM    GOLPRH+4,X'20'                                                   
         BZ    EDITHL                                                           
         TM    GOLESH+4,X'20'                                                   
         BZ    EDITHL                                                           
         TM    GOLTGH+4,X'20'                                                   
         BZ    EDITHL                                                           
         TM    GOLOPTSH+4,X'20'                                                 
         BZ    EDITHL                                                           
         CLI   SDAGY,C'Y'          TEST SUPERDESK AGY                           
         BNE   EDITDATA                                                         
* FOR SUPERDESK, REREAD ESTIMATE TO SET FLAG IF ACTION NOT RECALL               
         CLI   GOLACT1,C'R'        TEST RECALL                                  
         BE    EDITDATA                                                         
         CLI   GOLACT1,C'N'        TEST NEXT                                    
         BE    EDITDATA                                                         
         CLI   GOLACT1,C'T'        TEST TOT                                     
         BE    EDITDATA                                                         
         BRAS  RE,GETSDEST         GET SUPERDESK ESTIMATE                       
         B     EDITDATA                                                         
         SPACE 1                                                                
EDITHL   LA    R3,1                OVERLAY                                      
         BAS   R9,GETOVLY                                                       
         SPACE 1                                                                
* HEADLINE EDIT CLEARS ALL FIELDS ON LOWER SCREENS SO ALWAYS HAVE               
* TO RELOAD FA SCREEN SINCE IT CONTAINS MONTH NAMES                             
*                                                                               
         CLI   SVSCRN,X'FA'                                                     
         BNE   *+8                                                              
         MVI   SVSCRN,0                                                         
*                                                                               
         XC    ELEM,ELEM           CLEAR SAVED DATA AREA                        
         SPACE 1                                                                
* CHECK FOR RIGHT SCREEN *                                                      
         SPACE 1                                                                
         CLI   SVSCRN,0                                                         
         BNE   *+8                                                              
         MVI   SVSCRN,X'FD'                                                     
*                                                                               
         MVI   BYTE,X'F4'                                                       
         CLI   SVOPT1,C'X'         TEST TRANSFER OR                             
         BE    EDITHL1X                                                         
         CLI   SVOPT1,C'C'           COPY ACTIVE                                
         BE    EDITHL1X                                                         
*                                                                               
         MVI   BYTE,X'FD'                                                       
         CLI   SVPRD,X'FF'         TEST POL                                     
         BNE   *+8                                                              
         MVI   BYTE,X'FE'                                                       
* CHECK FOR PRE-BUY INPUT                                                       
         CLC   =C'BRDDOL',GOLTG                                                 
         BE    *+14                                                             
         CLC   =C'BD',GOLTG                                                     
         BNE   *+8                                                              
         MVI   BYTE,X'FC'                                                       
*                                                                               
         CLC   =C'BP',GOLTG        IF BRAND PERCENT                             
         BNE   EDITHL1                                                          
         MVI   BYTE,X'FB'                                                       
         CLI   GOLTG+2,C'0'        AND NO TIER #S -> ERROR                      
         BNL   EDITHL1                                                          
         LA    R2,GOLTGH                                                        
         MVI   ERRCD,INVERR                                                     
         GOTO1 ERROR                                                            
*                                                                               
EDITHL1  CLI   SVESTYP,1                                                        
         BL    *+16                                                             
         CLI   SVESTYP,4           QTRLY CPP (TYPE 5) USES NORMAL SCRN          
         BH    *+8                                                              
         MVI   BYTE,X'FA'                                                       
*                                                                               
EDITHL1X CLC   SVSCRN,BYTE                                                      
         BE    EDITHL2                                                          
*                                                                               
         MVC   SVSCRN,BYTE                                                      
         MVI   FULL,C'R'                                                        
         MVC   FULL+1(2),=X'0202'                                               
         MVC   FULL+3(1),BYTE                                                   
         MVC   DMCB+4(4),FULL                                                   
*                                                                               
         LA    R1,GOLOPTSH         POINT TO OPTIONS                             
         ZIC   R0,0(R1)                                                         
         AR    R0,R1               AND POINT TO END                             
*                                                                               
         GOTO1 VCALLOV,DMCB,(R0)                                                
*                                                                               
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R1,GOLOPTSH                                                      
         SR    R0,R0                                                            
*                                                                               
         IC    R0,0(R1)                                                         
         AR    R1,R0                                                            
         CLI   0(R1),0             TEST E-O-S                                   
         BNE   *-10                                                             
         MVI   1(R1),X'01'         SET BI/AI                                    
         MVI   2(R1),X'01'                                                      
*                                                                               
         CLI   SVSCRN,X'F4'        TEST TRANSFER SCREEN                         
         BE    EDITHLX                                                          
*                                                                               
         LA    R2,GOLACT1H                                                      
         MVI   5(R2),0                                                          
         SPACE 1                                                                
EDITHL2  CLI   SVSCRN,X'F4'        TEST TRANSFER SCREEN                         
         BNE   EDITHL4                                                          
*                                                                               
EDITHL2X LA    R3,4                                                             
         BAS   R9,GETOVLY                                                       
         B     EXIT                                                             
*                                                                               
EDITHL4  BRAS  RE,GLSETHL                                                       
         LA    R2,GOLACT1H         CHECK FOR INPUT DATA                         
*                                                                               
         CLI   5(R2),0                                                          
         BNE   EDITDATA                                                         
*                                                                               
EDITHLX  MVC   GOLMSG(17),=C'ENTER RECORD DATA'                                 
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
EDITDATA DS    0H                                                               
         CLI   SVSCRN,X'F4'        TEST TRANSFER SCREEN                         
         BE    EDITHL2X                                                         
*                                                                               
         BRAS  RE,GLSETHL                                                       
         CLI   GOLACT1,C'R'        TEST RECALL                                  
         BE    DATA2                                                            
         CLI   GOLACT1,C'T'                                                     
         BE    DATA2                                                            
         CLI   GOLACT1,C'N'        TEST 'NEXT'                                  
         BE    DATA2                                                            
         B     DATA4                                                            
DATA2    LA    R3,2                                                             
         CLI   SVOPT1,C'L'                                                      
         BNE   *+8                                                              
DATA3    LA    R3,X'12'            SET CORRECT OVERLAY                          
         CLI   SVSCRN,X'FB'        TEST PRE-BUY INPUT                           
         BNE   *+8                                                              
         LA    R3,X'22'                                                         
         CLI   SVSCRN,X'FC'                                                     
         BNE   *+8                                                              
         LA    R3,X'22'                                                         
         CLI   SVESTYP,1                                                        
         BL    *+16                                                             
         CLI   SVESTYP,5           QTRLY CPP IS IN OVLY 24                      
         BH    *+8                                                              
         LA    R3,X'24'                                                         
         BAS   R9,GETOVLY                                                       
         MVC   GOLMSG(24),=C'REQUESTED DATA DISPLAYED'                          
         LA    R2,GOLACT1H                                                      
         CLI   SVSCRN,X'FA'                                                     
         BE    EXIT                                                             
         CLI   GOLACT1,C'*'        DID WE DISPLAY ANYTHING                      
         BE    EXIT                YES                                          
         MVI   ERRCD,NODTAERR                                                   
         GOTO1 ERROR                                                            
         B     EXIT                                                             
*                                                                               
DATA4    MVI   ERRCD,NOADVERR                                                   
         MVC   BYTE,SVAGYMD                                                     
         NI    BYTE,X'F0'                                                       
         CLC   BYTE,SVADVAGY       TEST ADD/DEL ON ADV AGY                      
         BNE   DATA6                                                            
         LA    R2,GOLCLH                                                        
         GOTO1 ERROR                                                            
         B     EXXMOD                                                           
         EJECT                                                                  
DATA6    LA    R3,3                                                             
*                                                                               
         CLI   SVPPROF+6,C'Y'      TEST ALLOWED TO CHANGE GLOCK                 
         BE    DATA6X                                                           
         CLI   SVOPT1,C'G'                                                      
         BNE   DATA6X                                                           
         CLI   GOLACT1,C'A'                                                     
         BE    DATA6A                                                           
         CLI   GOLACT1,C'D'                                                     
         BNE   DATA6X                                                           
DATA6A   MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(NOCHGLOK)                                              
         LA    R2,GOLACT1H                                                      
         GOTO1 ERROR                                                            
         B     EXXMOD                                                           
*                                                                               
DATA6X   CLI   SVSCRN,X'FB'        TEST PRE-BUY INPUT                           
         BNE   *+8                                                              
         LA    R3,X'23'                                                         
         CLI   SVSCRN,X'FC'                                                     
         BNE   *+8                                                              
         LA    R3,X'23'                                                         
         CLI   SVESTYP,1                                                        
         BL    *+16                                                             
         CLI   SVESTYP,5           QTRLY CPP IS IN OVLY 24                      
         BH    *+8                                                              
         LA    R3,X'24'                                                         
***      BE    DATA7               THIS JUST LOOKS WRONG (EJOR 05MAR08)         
         CLI   SVOPT1,C'L'                                                      
         BNE   *+8                                                              
DATA7    LA    R3,X'13'            SET CORRECT OVERLAY                          
*                                                                               
DATA8    DS    0H                                                               
         BAS   R9,GETOVLY                                                       
         MVC   GOLMSG(22),=C'** ACTION COMPLETED **'                            
         LA    R2,GOLACT1H                                                      
         TM    VCALLBAS,X'80'                                                   
         JZ    EXIT                                                             
         NI    VCALLBAS,X'7F'                                                   
         L     RD,GL39RD                                                        
         J     EXXMOD                                                           
         EJECT                                                                  
*============================*                                                  
* GOALS OVERLAY ACCESS TABLE *                                                  
*============================*                                                  
         SPACE 1                                                                
MAINT    EQU   1                                                                
RCL      EQU   2                                                                
         SPACE 1                                                                
*============================================================*                  
ACCSTAB  DS    0XL2                                          *                  
         DC    X'01',AL1(0)        HEADLINE EDITS            *                  
         DC    X'02',AL1(RCL)      GOALS RECALL              *                  
         DC    X'03',AL1(MAINT)    GOALS ADD/DEL             *                  
         DC    X'04',AL1(MAINT)    GOALS XFR                 *                  
         DC    X'12',AL1(RCL)      LOCKIN RECALL             *                  
         DC    X'13',AL1(MAINT)    LOCKIN ADD/DEL            *                  
         DC    X'22',AL1(0)        CSO                       *                  
         DC    X'23',AL1(MAINT)    CSO                       *                  
         DC    X'24',AL1(0)        CPP (TEST IN OVERLAY)     *                  
ACCSTABX EQU   *                                             *                  
*============================================================*                  
         SPACE 2                                                                
GETOVLY  DS    0H                                                               
         LA    R1,ACCSTAB                                                       
         LA    R0,(ACCSTABX-ACCSTAB)/L'ACCSTAB                                  
*                                                                               
GETOV2   CLM   R3,1,0(R1)                                                       
         BE    GETOV4                                                           
         LA    R1,L'ACCSTAB(R1)                                                 
         BCT   R0,GETOV2                                                        
         DC    H'0'                                                             
*                                                                               
GETOV4   CLI   1(R1),0                                                          
         BE    GETOV10                                                          
         CLI   1(R1),RCL                                                        
         BE    GETOV10                                                          
         CLI   1(R1),MAINT                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   SVPPROF+1,C'Y'      TEST LIMIT ACCESS CODE REQD FOR UPD          
         BNE   GETOV6                                                           
         CLC   T202FFD+6(2),SVCLT  DOES LIMIT ACCESS MATCH CLT                  
         BE    GETOV6                                                           
*                                                                               
GETOVERR MVI   ERRCD,ACCSERR                                                    
         GOTO1 ERROR                                                            
         B     EXIT                                                             
*                                                                               
GETOV6   CLI   0(R1),X'13'         TEST LOCKIN UPDATE                           
         BNE   GETOV8                                                           
         CLI   T202FFD+1,C'*'      TEST DDS TERMINAL                            
         BE    GETOV10                                                          
         CLI   SVPPROF+2,C'Y'      TEST AGY AUTH TO UPDATE LOCKIN               
         BNE   GETOVERR                                                         
         EJECT                                                                  
GETOV8   CLI   T202FFD+13,X'0F'    TEST ACCESS VALUE < 15                       
         BL    GETOVERR                                                         
*                                                                               
GETOV10  GOTO1 VCALLOV,DMCB,((R3),0),VTWA                                       
*                                                                               
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,DMCB             GET OVLY ADDRESS                             
         GOTO1 (RF),DMCB,(RC),(RA)                                              
*                                                                               
         CLI   ERRAREA,0           TEST ERROR IN OVERLAY                        
         BCR   8,R9                NO-RETURN                                    
         B     EXXMOD                                                           
         SPACE 2                                                                
EXIT     OI    6(R2),X'40'         POSITION CURSOR                              
*                                                                               
EXXMOD   XIT1                                                                   
         SPACE 2                                                                
SPCOMMON NTR1  BASE=BASERB                                                      
*                                                                               
         SRL   RF,24                                                            
         B     SPCOMTAB(RF)                                                     
*                                                                               
SPCOMTAB B     SPERROR             X'00'                                        
         B     SPANY               X'04'                                        
         B     SPMOVE              X'08'                                        
         B     SPPACK              X'0C'                                        
         B     SPREAD              X'10'                                        
         B     SPSEQ               X'14'                                        
         B     SPHIGH              X'18'                                        
         B     SPADD               X'1C'                                        
         B     SPDIR               X'20'                                        
         B     SPRDSTA             X'24'                                        
         B     SPSTA               X'28'                                        
         B     SPGETREC            X'2C'                                        
         B     SPPUTREC            X'30'                                        
         B     SPADDREC            X'34'                                        
         B     SPFIL               X'3C'                                        
         DC    5AL4(0)             X'40/44/48/4C/50' RESERVED                   
SPCOMUSR DC    9AL4(0)   ** USER ROUTINES ORIGIN HERE WITH X'54' **             
SPCOMCNT EQU   (*-SPCOMTAB)/4      NUMBER OF ENTRIES                            
         SPACE 2                                                                
SPCOMXIT XIT1                                                                   
         EJECT                                                                  
SPERROR  L     R4,ERRAREA                                                       
         CLI   ERRAREA,0           TEST FOR PRESET MESSAGE                      
         BNE   SPERR20                                                          
         CLI   ERRCD,NEWERRS       IS THIS A 2 CHAR ERROR                       
         BNE   SPERR10                                                          
*                                                                               
         XC    WORK,WORK           DEFINE CONTROL BLOCK                         
         LA    R1,WORK                                                          
         USING GETTXTD,R1                                                       
         MVI   GTMTYP,GTMERR       SET MESSAGE TYPE TO ERRO                     
         MVC   GTMSGNO,NERRCD      AND MESSAGE NUMBER                           
         MVI   GTMSYS,2            AND MESSAGE SYSTEM                           
         LA    RE,REC2             IN CASE IT'S DEFINED                         
         STCM  RE,7,GTASUBST       SET A(SUBSTITUTION BLOCK)                    
         DROP  R1                                                               
         L     RF,VCOMFACS                                                      
         L     RF,CGETTXT-COMFACSD(RF)                                          
         GOTO1 (RF),WORK           PUT OUT SYSTEM MESSAGE                       
         B     SPERR20                                                          
         EJECT                                                                  
*                                                                               
SPERR10  L     R4,ERRAREA                                                       
         MVI   ERRAREA,X'FF'                                                    
         MVC   DMCB+20(4),VDATAMGR                                              
         MVI   DMCB+20,2                                                        
         GOTO1 VGETMSG,DMCB+12,(ERRCD,8(R4)),(X'FF',DMCB)                       
*                                                                               
SPERR20  FOUT  (R4)                                                             
*                                                                               
         OI    6(R2),X'40'         POSITION CURSOR                              
*                                                                               
         L     RD,BASERD           RETURN TO * BASE *                           
         TM    VCALLBAS,X'80'      TEST CALLBASE ACTIVE                         
         JZ    *+12                                                             
         NI    VCALLBAS,X'7F'                                                   
         L     RD,GL39RD           RETURN TO CALLBASE                           
         B     SPCOMXIT                                                         
         SPACE 2                                                                
SPANY    CLI   5(R2),0                                                          
         BNE   ANY2                                                             
         MVI   ERRCD,1                                                          
         B     SPERROR                                                          
*                                                                               
ANY2     TM    4(R2),X'10'                                                      
         BZ    SPCOMXIT                                                         
         MVI   ERRCD,3                                                          
         B     SPERROR                                                          
         EJECT                                                                  
SPPACK   SR    R1,R1                                                            
         SR    R0,R0                                                            
         ZAP   DUB,=P'0'                                                        
         IC    R1,5(R2)                                                         
         LTR   R1,R1               EXIT ON ZERO LENGTH                          
         BZ    PACKX                                                            
         TM    4(R2),X'08'         OR NON-NUMERIC                               
         BZ    PACKX                                                            
         BCTR  R1,0                                                             
         EX    R1,*+12                                                          
         CVB   R0,DUB                                                           
         B     PACKX                                                            
         PACK  DUB,8(0,R2)   * EXECUTED *                                       
*                                                                               
PACKX    XIT1  REGS=(R0,R1)                                                     
         SPACE 2                                                                
SPMOVE   MVI   WORK,C' '                                                        
         MVC   WORK+1(L'WORK-1),WORK                                            
         SR    R1,R1                                                            
         IC    R1,5(R2)                                                         
         LTR   R1,R1                                                            
         BZ    SPCOMXIT                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     PACKX                                                            
*                                                                               
         MVC   WORK(0),8(R2) * EXECUTED *                                       
         EJECT                                                                  
SPREAD   MVC   COMMAND,=C'DMREAD'                                               
         B     SPDIR                                                            
SPSEQ    MVC   COMMAND,=C'DMRSEQ'                                               
         B     SPDIR                                                            
SPHIGH   MVC   COMMAND,=C'DMRDHI'                                               
         MVC   KEYSAVE,KEY                                                      
         B     SPDIR                                                            
SPADD    MVC   COMMAND,=C'DMADD'                                                
         B     SPDIR                                                            
SPWRITE  MVC   COMMAND,=C'DMWRT'                                                
*                                                                               
SPDIR    GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'SPTDIR',KEY,KEY               
*                                                                               
SPDIRX   TM    8(R1),X'FD'         TEST ALL BUT DELETED                         
         BZ    SPDIRX2             NO ERROR                                     
         CLI   COMMAND+2,C'R'      TEST READ COMMAND                            
         BE    *+6                                                              
         DC    H'0'                FORCE RECOVERY ON ADD/WRITE ERROR            
*                                                                               
SPDIRX2  MVC   BYTE,DMOUTBTS                                                    
         NC    BYTE,8(R1)                                                       
         BZ    SPCOMXIT                                                         
* DATAMGR ERROR HAS OCCURRED                                                    
         MVI   ERRCD,0                                                          
         B     SPERROR                                                          
         EJECT                                                                  
SPRDSTA  MVC   COMMAND,=C'DMREAD'                                               
*                                                                               
SPSTA    GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'STATION',KEY,AREC             
*                                                                               
         B     SPDIRX                                                           
         EJECT                                                                  
SPGETREC MVC   COMMAND,=C'GETREC'                                               
         B     SPFIL                                                            
SPPUTREC MVC   COMMAND,=C'PUTREC'                                               
         B     SPFIL                                                            
SPADDREC MVC   COMMAND,=C'ADDREC'                                               
*                                                                               
SPFIL    GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'SPTFILE',KEY+14,AREC,X        
               DMWORK                                                           
*                                                                               
         CLI   COMMAND,C'G'        TEST GETREC                                  
         BE    SPDIRX2                                                          
         TM    8(R1),X'D0'         TEST EOF OR ERROR                            
         BZ    SPDIRX2                                                          
         DC    H'0'                FORCE RECOVERY ON ADD/WRITE ERRORS           
         EJECT                                                                  
         ORG   SPCOMUSR                                                         
*                                                                               
         B     EDTMKT              USER1                                        
         B     EDTDPTLN            USER2                                        
         B     EDTPER              USER3                                        
         B     FIXREC              USER4                                        
         B     GOCHKSLN            USER5                                        
*                                                                               
         ORG                                                                    
         SPACE 1                                                                
** USER1 **                                                                     
*                                                                               
EDTMKT   GOTO1 ANY                                                              
         GOTO1 PACK                                                             
         STH   R0,BMKT                                                          
         MVI   ERRCD,INVERR                                                     
*                                                                               
         CLI   SVCXTRA+8,C'P'      TEST P&G GOALS                               
         BNE   MKT8                                                             
*                                                                               
         LA    R0,4                                                             
         LA    R1,SVPGPROF+2       MAKE SURE PROFILE MKT NUMERIC                
*                                                                               
MKT2     CLI   0(R1),C'0'                                                       
         BL    BADPGMKT                                                         
         CLI   0(R1),C'9'                                                       
         BH    BADPGMKT                                                         
         LA    R1,1(R1)                                                         
         BCT   R0,MKT2                                                          
*                                                                               
         PACK  DUB,SVPGPROF+2(4)                                                
         CVB   R0,DUB                                                           
         CH    R0,BMKT                                                          
         BNE   BADPGMKT                                                         
*                                                                               
MKT8     CLI   SVAPROF+7,C'C'      TEST CANADA                                  
         BNE   *+12                                                             
         CLI   GOLMD,C'N'          TEST NTWK                                    
         BE    MKT10               YES - ALLOW MKT 0                            
         LTR   R0,R0                                                            
         BZ    SPERROR                                                          
* READ MARKET RECORD                                                            
MKT10    TM    4(R2),X'20'                                                      
         BO    SPCOMXIT                                                         
         CLI   SVSCRN,X'FB'         TEST BRDPCT                                 
         BNE   MKT20                                                            
         CHI   R0,9999                                                          
         BNE   MKT20                                                            
         MVC   SVMKTNAM,SPACES                                                  
         MVC   SVMKTNAM(13),=C'** DEFAULT **'                                   
         B     SPCOMXIT                                                         
*                                                                               
MKT20    MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),GOLMD                                                   
         OI    DUB+7,X'0F'                                                      
         UNPK  KEY+2(4),DUB                                                     
         MVC   KEY+6(2),AGYALPHA                                                
         L     R6,AREC3                                                         
         ST    R6,AREC                                                          
         USING MKTRECD,R6                                                       
         GOTO1 RDSTA                                                            
         MVC   SVMACCS,MKTLTACC    SAVE LIMIT ACCESS CODES                      
         MVC   SVMKTNAM,MKTNAME                                                 
         DROP  R6                                                               
*                                                                               
         BRAS  RE,CALLOFCR         CHECK FOR MARKET LIMIT ACCESS                
         B     SPCOMXIT                                                         
*                                                                               
BADPGMKT MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(PGMKTERR)                                              
         B     SPERROR                                                          
         EJECT                                                                  
** USER2 **                                                                     
         SPACE 1                                                                
EDTDPTLN DS    0H                                                               
         MVI   ERRCD,DPTERR                                                     
         CLI   8(R2),C'Z'                                                       
         BE    SPERROR                                                          
* ALLOW * FOR DAYPART FOR RM ACTION                                             
         CLI   8(R2),C'*'                                                       
         BNE   DPTLN0                                                           
         CLC   =C'RM',GOLACT1                                                   
         BE    DPTLN4                                                           
*                                                                               
DPTLN0   L     RE,AREC2                                                         
         CLC   =X'0D40',0(RE)      TEST SPLIT REC IN CORE                       
         BNE   *+12                                                             
         CLI   8(R2),C'*'          IF SO,DPT * IS VALID                         
         BE    DPTLN5                                                           
*                                                                               
         LA    R1,SVMENU                                                        
DPTLN2   CLC   8(1,R2),0(R1)                                                    
         BE    DPTLN4                                                           
         LA    R1,1(R1)                                                         
         CLI   0(R1),0                                                          
         BNE   DPTLN2                                                           
         B     SPERROR                                                          
*                                                                               
DPTLN4   MVC   BDPT,8(R2)                                                       
         CLI   SVCXTRA+8,C'P'      TEST P&G GOALS                               
         BNE   DPTLN4A                                                          
         CLC   BDPT,SVPGPROF+1     TEST RIGHT DPT                               
         BNE   BADPGDPT                                                         
         B     DPTLN5                                                           
*                                                                               
DPTLN4A  CLI   BDPT,C'$'           SPECIAL FOR WESTERN                          
         BNE   DPTLN4X                                                          
         MVI   BSLN,1              FORCE SLN=01                                 
         MVI   BTLN,1              FORCE TOTLEN=01                              
         MVI   ERRCD,SLNERR                                                     
         CLI   5(R2),1                                                          
         BNE   DPTLN5              NOP BE SPCOMXIT                              
         B     SPCOMXIT            NOP B  SPERROR                               
*                                                                               
DPTLN4X  CLI   5(R2),1                                                          
         BE    SPCOMXIT                                                         
         SPACE 1                                                                
* EDIT SECONDS *                                                                
         SPACE 1                                                                
DPTLN5   SR    R0,R0                                                            
         IC    R0,5(R2)                                                         
         BCTR  R0,0                ADJUST FOR DPT                               
         LA    R4,9(R2)            POINT TO START OF DATA                       
         LR    R5,R4               SAVE START ADDRESS                           
         MVI   ERRCD,SLNERR                                                     
         B     DPTLN6A                                                          
*                                                                               
DPTLN6   CLI   0(R4),C'-'                                                       
         BE    DPTLN8                                                           
DPTLN6A  CLI   0(R4),C'0'                                                       
         BL    SPERROR                                                          
         CLI   0(R4),C'9'                                                       
         BH    SPERROR                                                          
         LA    R4,1(R4)                                                         
         BCT   R0,DPTLN6                                                        
*                                                                               
DPTLN8   LR    RE,R4               GET DATA POINTER                             
         SR    RE,R5               GET LEN OF DATA                              
         BNP   SPERROR                                                          
         BCTR  RE,0                SET FOR EX                                   
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R5)    * EXECUTED *                                      
         CVB   R1,DUB                                                           
         CHI   R1,255                                                           
         BH    SPERROR                                                          
         BRAS  RE,CHKSLN                                                        
         BNE   SPERROR                                                          
         STC   R1,BSLN             SET AS PRODUCT SLN                           
         STC   R1,BTLN             AND TOTAL SLN                                
*                                                                               
         CLI   0(R4),C'-'          TEST PIGGYBACK ENTERED                       
         BE    DPTLN12                                                          
         SPACE 1                                                                
* NO PARTNER LENGTH ENTERED - TEST FOR DEFAULT PIGGYBACK                        
         SPACE 1                                                                
         CLI   SVPRD2,0            ARE THERE 2 PRODUCTS                         
         BE    SPCOMXIT            NO                                           
         SRL   R1,1                HALVE THE SLN                                
         STC   R1,BSLN                                                          
         BRAS  RE,CHKSLN                                                        
         BNE   SPERROR                                                          
*                                                                               
         TM    BTLN,1              TEST ODD SLN ENTERED                         
         BZ    SPCOMXIT                                                         
         MVI   ERRCD,BADPR2LN      YES - THAT'S AN ERROR                        
         B     SPERROR                                                          
*                                                                               
DPTLN12  MVI   ERRCD,BADPR2LN                                                   
         CLI   SVPRD2,0            TEST 2 PRODUCTS                              
         BE    SPERROR             NO - NO PIGGYBACK LENS                       
         LA    R4,1(R4)            SKIP PAST -                                  
         BCTR  R0,R0                                                            
         LTR   R0,R0                                                            
         BZ    SPERROR                                                          
         LR    R5,R4               SAVE START ADDRESS                           
*                                                                               
DPTLN14  CLI   0(R4),C'0'                                                       
         BL    SPERROR                                                          
         CLI   0(R4),C'9'                                                       
         BH    SPERROR                                                          
         LA    R4,1(R4)                                                         
         BCT   R0,DPTLN14                                                       
*                                                                               
         SR    R4,R5               GET LENGTH OF DATA                           
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R5) *EXECUTED*                                           
*                                                                               
         CVB   R1,DUB                                                           
         CHI   R1,255                                                           
         BH    SPERROR                                                          
         STC   R1,BSLN2                                                         
         BRAS  RE,CHKSLN                                                        
         BNE   SPERROR                                                          
*                                                                               
         ZIC   RE,BSLN                                                          
         AR    R1,RE               GET TOTAL LEN                                
         BRAS  RE,CHKSLN           AND MAKE SURE ITS VALID                      
         BNE   SPERROR                                                          
         STC   R1,BTLN             SET TOTAL LENGTH                             
         B     SPCOMXIT                                                         
*                                                                               
BADPGDPT MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(PGDPTERR)                                              
         B     SPERROR                                                          
         EJECT                                                                  
** USER3 **                                                                     
         SPACE 1                                                                
EDTPER   XC    BWEEKS,BWEEKS                                                    
         MVC   WORK2(12),SVSTART   MOVE EST START/END                           
         CLI   5(R2),0                                                          
         BE    PER30                                                            
*                                                                               
         GOTO1 MOVE                MOVE DATA TO WORK                            
         LA    R4,WORK                                                          
* 3270'S LEAVE INPUT LEN 13 WITH NO DATA - SO CHECK FOR IT                      
         OC    WORK(13),SPACES                                                  
         CLC   WORK(13),SPACES                                                  
         BE    PER30                                                            
*                                                                               
         CLC   =C'S-',0(R4)                                                     
         BNE   PER2                                                             
         LA    R4,2(R4)                                                         
         B     PER4                                                             
*                                                                               
PER2     GOTO1 VDATVAL,DMCB,(1,(R4)),WORK2                                      
*                                                                               
         MVI   ERRCD,SDTERR                                                     
         OC    0(4,R1),0(R1)                                                    
         BZ    SPERROR                                                          
*                                                                               
         A     R4,0(R1)            POINT TO SEPARATOR                           
         CLI   0(R4),C'-'                                                       
         BE    PER3                                                             
         CLI   SVEDAILY,C'Y'                                                    
         BNE   SPERROR                                                          
         MVC   WORK2+6(6),WORK2    MOVE START DATE TO END DATE                  
         B     PER30                                                            
*                                                                               
PER3     LA    R4,1(R4)                                                         
*                                                                               
PER4     CLC   =C'E ',0(R4)                                                     
         BE    PER30                                                            
*                                                                               
PER10    GOTO1 VDATVAL,DMCB,(1,(R4)),WORK2+6                                    
*                                                                               
         OC    0(4,R1),0(R1)       IF NOT VALID,                                
         BZ    PER12                 CHECK FOR OTHER EXPRESSIONS                
         B     PER30                                                            
*                                                                               
PER12    MVI   ERRCD,EDTERR                                                     
         CLI   SVEDAILY,C'Y'       OTHER FORMATS NOT VALID FOR DAILY            
         BE    SPERROR                                                          
         LA    R0,3                CHECK FOR 'NW OR NNW'                        
         LR    R1,R4                                                            
PER13    CLI   0(R1),C' '                                                       
         BE    SPERROR                                                          
         CLI   0(R1),C'W'                                                       
         BE    PER14                                                            
         CLI   0(R1),C'0'                                                       
         BL    SPERROR                                                          
         CLI   0(R1),C'9'                                                       
         BH    SPERROR                                                          
         LA    R1,1(R1)                                                         
         BCT   R0,PER13                                                         
         B     SPERROR                                                          
PER14    LA    R5,2                SET MAX LEN-1                                
         SR    R5,R0               SET FOR EX                                   
         BM    SPERROR                                                          
         EX    R5,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R4)    * EXECUTED *                                      
         CVB   R0,DUB                                                           
         STC   R0,BYTE             SAVE NUMBER OF WEEKS                         
         LA    R0,7                                                             
         CLI   1(R1),C' '                                                       
         BE    PER16                                                            
         CLI   GOLACT0,0           TEST DISPLAY FUNCTION                        
         BE    *+12                NO                                           
         MVI   ERRCD,INVERR        MUST DISPLAY CONSECUTIVE WEEKS               
         B     SPERROR                                                          
*                                                                               
         LA    R0,14                                                            
         CLI   1(R1),C'A'                                                       
         BE    PER16                                                            
         LA    R0,21                                                            
         CLI   1(R1),C'T'                                                       
         BE    PER16                                                            
         LA    R0,28                                                            
         CLI   1(R1),C'F'                                                       
         BE    PER16                                                            
         MVI   ERRCD,DTERR                                                      
         B     SPERROR                                                          
PER16    ST    R0,FULL             SAVE DAYS SKIPPED                            
         CLC   WORK2(2),=C'00'     DO WE HAVE START YEAR                        
         BNE   PER18               YES                                          
         MVC   WORK2(2),SVSTART    MOVE EST START YEAR                          
         CLC   SVSTART(2),SVEND    TEST EST ALL IN ONE YEAR                     
         BE    PER18                                                            
         CLC   WORK2+2(4),SVSTART+2  INPUT MONTH TO EST START MONTH             
         BNL   *+10                  IF HI OR EQ USE START YEAR                 
         MVC   WORK2(2),SVEND        ELSE USE END YEAR                          
*                                                                               
PER18    MVI   ERRCD,PERERR                                                     
         CLC   WORK2(6),SVSTART                                                 
         BL    SPERROR                                                          
         CLC   WORK2(6),SVEND                                                   
         BH    SPERROR                                                          
         MVC   WORK(6),WORK2                                                    
* FIND START DAY                                                                
         GOTO1 VGETDAY,DMCB,WORK2,WORK2+12                                      
         MVI   ERRCD,SDAYERR                                                    
         CLI   SVEOWSDY,0          TEST OUT-OF-WEEK DATA                        
         BE    PER19               NO                                           
*                                                                               
         CLC   0(1,R1),SVEOWSDY    YES- MUST MATCH EST START DAY                
         BE    PER20                                                            
         B     SPERROR             OR IT IS AN ERROR                            
*                                                                               
PER19    CLI   0(R1),1             TEST MONDAY                                  
         BE    PER20                                                            
         CLC   WORK2(6),SVSTART                                                 
         BNE   SPERROR                                                          
* GET PREVIOUS MONDAY IN 'WORK'                                                 
         SR    R0,R0                                                            
         IC    R0,0(R1)                                                         
         BCTR  R0,0                                                             
         LCR   R0,R0                                                            
         GOTO1 VADDAY,DMCB,WORK2,WORK,(R0)                                      
*                                                                               
* BUILD DATES - FIRST DATE FROM WORK2, REST FROM WORK                           
*                                                                               
PER20    GOTO1 VDATCON,DMCB,WORK2,(2,BWEEKS)                                    
*                                                                               
         LA    R5,BWEEKS+2                                                      
         SR    R0,R0                                                            
         IC    R0,BYTE             GET NUMBER OF WEEKS                          
         MVI   ERRCD,PERERR                                                     
         B     PER24                                                            
PER22    MVC   DMCB+8(4),FULL                                                   
         GOTO1 VADDAY,DMCB,WORK,WORK                                            
         CLC   WORK(6),SVEND                                                    
         BH    SPERROR                                                          
         GOTO1 VDATCON,DMCB,WORK,(2,(R5))                                       
         LA    R5,2(R5)                                                         
PER24    BCT   R0,PER22                                                         
         B     SPCOMXIT                                                         
         EJECT                                                                  
PER30    CLC   WORK2(2),=C'00'     DO WE HAVE START YEAR                        
         BNE   PER32                                                            
         MVC   WORK2(2),SVSTART    MOVE EST START YEAR                          
         CLC   SVSTART(2),SVEND    TEST EST ALL IN ONE YEAR                     
         BE    PER32                                                            
         CLC   WORK2+2(4),SVSTART+2  INPUT MONTH TO EST START MONTH             
         BNL   *+10                  IF HI OR EQ USE START YEAR                 
         MVC   WORK2(2),SVEND        ELSE USE END YEAR                          
*                                                                               
PER32    CLC   WORK2+6(2),=C'00'   DO WE HAVE END YEAR                          
         BNE   PER34                                                            
         MVC   WORK2+6(2),SVSTART                                               
         CLC   SVSTART(2),SVEND                                                 
         BE    PER34                                                            
         CLC   WORK2+8(4),SVSTART+2                                             
         BNL   *+10                                                             
         MVC   WORK2+6(2),SVEND                                                 
*                                                                               
PER34    MVI   ERRCD,STENDERR                                                   
         CLC   WORK2(6),WORK2+6                                                 
         BH    SPERROR                                                          
         MVI   ERRCD,PERERR                                                     
         CLC   WORK2(6),SVSTART                                                 
         BL    SPERROR                                                          
         CLC   WORK2+6(6),SVEND                                                 
         BH    SPERROR                                                          
         CLI   SVPRD2,0            TEST PIGGYBACK                               
         BE    PER34X                                                           
         SPACE 1                                                                
* TEST DATES IN PARTNER ESTIMATE PERIOD *                                       
         SPACE 1                                                                
         MVI   ERRCD,BADPR2DT                                                   
         CLC   WORK2(6),SVSTART2                                                
         BL    SPERROR                                                          
         CLC   WORK2+6(6),SVEND2                                                
         BH    SPERROR                                                          
*                                                                               
PER34X   MVC   WORK(6),WORK2       ASSUME MONDAY START                          
         CLI   SVEDAILY,C'Y'                                                    
         BE    PER38                                                            
*                                                                               
         GOTO1 VGETDAY,DMCB,WORK2,WORK2+12                                      
         MVI   ERRCD,SDAYERR                                                    
         CLI   SVEOWSDY,0          TEST OUT-OF-WEEK FLAG                        
         BE    PER35                                                            
         CLC   SVEOWSDY,0(R1)      MATCH OOW START DAY                          
         BNE   SPERROR                                                          
         B     PER36                                                            
*                                                                               
PER35    CLI   0(R1),1             NOT OOW MUST BE MONDAY                       
         BE    PER36                                                            
         CLC   WORK2(6),SVSTART                                                 
         BNE   SPERROR                                                          
* GET PREVIOUS MONDAY IN WORK                                                   
         SR    R0,R0                                                            
         IC    R0,0(R1)                                                         
         BCTR  R0,0                                                             
         LCR   R0,R0                                                            
         GOTO1 VADDAY,DMCB,WORK2,WORK,(R0)                                      
*                                                                               
* NOW CHECK END DAY IS MONDAY OR EST END OR SAME AS START !                     
*                                                                               
PER36    CLC   WORK2(6),WORK2+6                                                 
         BE    PER38                                                            
         CLC   WORK2+6(6),SVEND                                                 
         BE    PER38                                                            
         GOTO1 VGETDAY,DMCB,WORK2+6,WORK2+12                                    
         MVI   ERRCD,EDAYERR                                                    
         CLI   SVEOWSDY,0          TEST OUT-OF-WEEK DATA                        
         BE    PER37                                                            
         CLC   SVEOWSDY,0(R1)      CHECK OOW START DAY                          
         BNE   SPERROR                                                          
         B     PER38                                                            
*                                                                               
PER37    CLI   0(R1),1                                                          
         BE    PER38                                                            
         CLC   WORK2+6(6),SVEND                                                 
         BNE   SPERROR                                                          
*                                                                               
PER38    DS    0H                  SET DAYS BETWEEN ENTRIES                     
         LA    R0,1                                                             
         CLI   SVEDAILY,C'Y'                                                    
         BE    *+8                                                              
         LA    R0,7                                                             
*                                                                               
         GOTO1 VDATCON,DMCB,WORK2,(2,BWEEKS)                                    
*                                                                               
         LA    R5,BWEEKS+2                                                      
PER40    GOTO1 VADDAY,DMCB,WORK,WORK,(R0)                                       
         CLC   WORK(6),WORK2+6                                                  
         BH    PER42                                                            
         GOTO1 VDATCON,DMCB,WORK,(2,(R5))                                       
         LA    R5,2(R5)                                                         
         B     PER40                                                            
*                                                                               
PER42    B     SPCOMXIT                                                         
         EJECT                                                                  
** USER4 **                                                                     
                                                                                
FIXREC   CLI   SVEDAILY,C'Y'                                                    
         BE    SPCOMXIT                                                         
         BRAS  RE,FIXIT                                                         
         B     SPCOMXIT                                                         
                                                                                
** USER5 **                                                                     
                                                                                
GOCHKSLN BRAS  RE,CHKSLN                                                        
         BNE   SPERROR                                                          
         B     SPCOMXIT                                                         
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
CKDDLINK NTR1  BASE=*,LABEL=*      CHECK FOR DDLINK CALL                        
         L     RF,VCOMFACS         RF=A(COMFACS)                                
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,=C'GETD',ELEM,10,GLVDLUWF                              
         CLI   8(R1),0             WORKER FILE GLOBAL NOT PRESENT               
         JNE   SETCCNEQ                                                         
*                                                                               
         GOTO1 (RF),DMCB,=C'GETD',ELEM,GLVXLENQ,GLVXCTL                         
         CLI   DMCB+8,GLEGNF                                                    
         JE    SETCCNEQ                                                         
         CLC   ELEM(12),=C'SPOLINSPOGOA'                                        
         JNE   SETCCNEQ                                                         
*                                                                               
SETCCEQ  CR    RB,RB               EQUAL                                        
         J     *+6                                                              
SETCCNEQ LTR   RB,RB               NOT EQUAL                                    
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
*==================================================================             
* PROVIDE ENTRY POINT FOR SPGOL39 TO CALL FOR PROCESSING                        
*==================================================================             
                                                                                
CALLBASE NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    CL8'CALLBASE'                                                    
         OI    VCALLBAS,X'80'      SET FLAG THAT CALLBASE ACTIVE                
*                                                                               
         CLC   0(8,R1),=C'*T20239*'                                             
         JNE   *+8                                                              
         ST    RD,GL39RD           SET TO RETURN TO CALLER                      
*                                                                               
         L     RA,VTWA                                                          
         L     RB,BASERB                                                        
*                                                                               
         LARL  RF,TESTHL                                                        
         BASR  RE,RF                                                            
* NOTE NOTE NOTE!!! NO RETURN HERE. EXXMOD TEST VCALLBAS!                       
* NOTE NOTE NOTE!!! NO RETURN HERE. EXXMOD TEST VCALLBAS!                       
         LTORG                                                                  
         EJECT                                                                  
*=================================================================              
* VALIDATE SLN IN R1 AGAINST NEW CORE-RESIDENT PHASE                            
*=================================================================              
                                                                                
CHKSLN   NTR1  BASE=*,LABEL=*                                                   
         STC   R1,BYTE             SAVE SLN                                     
*                                                                               
         L     R1,VSLNTAB                                                       
         LH    RE,0(R1)            GET ENTRY LENGTH                             
         L     RF,2(R1)            GET DSPL TO EOT                              
         AR    RF,R1               RELOCATE EOT ADDRESS                         
         AHI   R1,6                POINT TO FIRST ENTRY                         
*                                                                               
         SR    R0,R0                                                            
         LA    R0,C'T'                                                          
         CLI   GOLMD,C'T'                                                       
         BE    CHKSLN2                                                          
         CLI   GOLMD,C'N'                                                       
         BE    CHKSLN2                                                          
         CLI   GOLMD,C'C'                                                       
         BE    CHKSLN2                                                          
*                                                                               
         LA    R0,C'R'                                                          
         CLI   GOLMD,C'R'                                                       
         BE    CHKSLN2                                                          
         CLI   GOLMD,C'X'                                                       
         BE    CHKSLN2                                                          
         DC    H'0'                                                             
*                                                                               
CHKSLN2  CLC   =C'00',0(R1)        FIND DEFAULT ENTRY                           
         BE    CHKSLN4                                                          
         CLC   AGYALPHA,0(R1)      MATCH AGY                                    
         BNE   *+12                                                             
CHKSLN4  CLM   R0,1,2(R1)          MATCH MEDIA                                  
         BE    CHKSLN6                                                          
*                                                                               
         BXLE  R1,RE,CHKSLN2       NEXT ENTRY                                   
         DC    H'0'                                                             
*                                                                               
CHKSLN6  MVI   ERRCD,SLNERR                                                     
         AHI   R1,4                POINT BEYOND TABLE ID                        
         SR    RE,RE                                                            
         IC    RE,BYTE             GET SLN                                      
         AR    RE,RE               X 2                                          
         AR    RE,R1               POINT TO ENTRY                               
         CLI   1(RE),0             TEST SLN VALID                               
         JE    CHKSLNNO            NOT VALID IF NOTHING HERE                    
*                                                                               
CHKSLNYS SR    RC,RC               YES, IT IS VALID                             
CHKSLNNO LTR   RC,RC                                                            
CHKSLNX  XIT1                      EXIT WITH CC SET                             
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================          
* RESET SAVED EFLAG1 FOR SUPERDESK AGENCIES WHENEVER ACTION NOT RECALL          
*=====================================================================          
                                                                                
GETSDEST NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),SVAGYMD                                                 
         MVC   KEY+2(2),SVCLT                                                   
         MVC   KEY+4(3),QPRD                                                    
         MVC   KEY+7(1),SVEST                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AREC3                                                         
         ST    R6,AREC                                                          
         GOTO1 GETREC                                                           
*                                                                               
         USING ESTHDRD,R6                                                       
         OC    SVEFLAG1,EFLAG1                                                  
         DROP  R6                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
* THIS ROUTINE SWITCHES TO LOCKIN HEADLINES                                     
* OR BACK TO NORMAL DEPENDING ON PLANNER                                        
*                                                                               
GLSETHL  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R4,GOLOPTSH                                                      
         SR    R5,R5                                                            
         IC    R5,0(R4)                                                         
         AR    R4,R5               POINT TO HEADLINE FIELD                      
*                                                                               
         CLC   =C'LOCKIN',GOLPLNR                                               
         BE    GLSETHL2                                                         
         CLI   SVOPT1,C'L'                                                      
         BE    GLSETHL2                                                         
* MAKE SURE LOCKIN HEADLINES NOT PRESENT                                        
         CLC   LKINHL(12),GOLDOLL                                               
         BNE   GLSETHLX                                                         
         MVC   GOLDOLL,NLKINHL                                                  
         MVC   GOLPTS(12),NLKINHL+15                                            
         OI    GOLPTSH+6,X'80'     TRANSMIT                                     
         OI    GOLDOLLH+6,X'80'    TRANSMIT                                     
         B     GLSETHLX                                                         
         SPACE 1                                                                
* MAKE SURE LOCKIN HEADLINES ARE THERE                                          
*                                                                               
GLSETHL2 CLC   LKINHL(12),GOLDOLL                                               
         BE    GLSETHLX                                                         
         CLI   SVPRD,X'FF'         POL LOCKIN IS INVALID                        
         BNE   GLSETHL4                                                         
         MVI   ERRCD,INVERR                                                     
         LA    R2,GOLPRH                                                        
         GOTO1 ERROR                                                            
         DC    H'0'                NO RETURN HERE                               
*                                                                               
GLSETHL4 DS    0H                                                               
         MVC   GOLDOLL,LKINHL                                                   
         MVC   GOLPTS(12),LKINHL+15                                             
         OI    GOLDOLLH+6,X'80'                                                 
         OI    GOLPTSH+6,X'80'                                                  
*                                                                               
GLSETHLX J     EXXMOD                                                           
*                                                                               
LKINHL   DC    CL27'---DOLLARS---  SPOTS/POINTS'                                
NLKINHL  DC    CL27'DOLLARS/WEEK   POINTS/WEEK '                                
         LTORG                                                                  
         EJECT                                                                  
*===============================================================                
* NOTE THAT ON ENTRY, R9 CONTAINS RELO FACTOR                                   
*===============================================================                
                                                                                
INITL    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
*                                                                               
         LM    R3,R4,4(R1)                                                      
         ST    R3,VTWA                                                          
*                                                                               
         MVC   AGYALPHA,14(R3)                                                  
*                                                                               
         LA    R3,64(R3)           PRESET ERROR MSG ADDRESS                     
         ST    R3,ERRAREA                                                       
*                                                                               
         MVC   VDATAMGR(80),0(R4)  FACILITY LIST                                
         MVC   VCOMFACS,16(R1)     DO THIS AFTER THE PREVIOUS MVC               
*                                                                               
         L     R2,12(R1)                                                        
         ST    R2,VTIA                                                          
*                                                                               
         MVI   DMINBTS,X'C0'                                                    
         MVI   DMOUTBTS,X'FD'                                                   
         LA    R0,REC                                                           
         ST    R0,AREC                                                          
* SET UP COMMON FACILITY LINKAGES                                               
         L     R6,=A(SPCOMMON)                                                  
         AR    R6,R9               ADD RELO VALUE                               
         SR    R1,R1                                                            
         LA    R8,ERROR                                                         
         LA    R0,SPCOMCNT                                                      
*                                                                               
INIT10   ST    R6,0(R8)                                                         
         STC   R1,0(R8)                                                         
         LA    R1,4(R1)                                                         
         LA    R8,4(R8)                                                         
         BCT   R0,INIT10                                                        
*                                                                               
         LA    R0,REC                                                           
         ST    R0,AREC1                                                         
         AHI   R0,REC2-REC                                                      
         ST    R0,AREC2                                                         
         AHI   R0,REC3-REC2                                                     
         ST    R0,AREC3                                                         
*                                                                               
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(3),=X'D9000A'   GET SLNTAB  ADDRESS                       
         MVI   DMCB+7,QSPSLNTB                                                  
         GOTO1 VCALLOV,DMCB                                                     
         CLI   4(R1),X'FF'                                                      
         JE    *+2                                                              
         MVC   VSLNTAB,0(R1)                                                    
*                                                                               
         MVC   DMCB+4(3),=X'D9000A'                                             
         MVI   DMCB+7,QLINKIO                                                   
         GOTO1 VCALLOV,DMCB                                                     
         CLI   4(R1),X'FF'                                                      
         JE    *+2                                                              
         MVC   LINKIO,0(R1)                                                     
*                                                                               
         LARL  RE,CALLBASE                                                      
         ST    RE,VCALLBAS                                                      
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*==============================================================*                
* IF ANY OF THE DATES IN THE GOAL RECORD REGULAR ELEMENTS DO   *                
* NOT AGREE WITH THE DATES IN BWEEKS, FIX THE DATES IN THE     *                
* ELEMENTS                                                     *                
*==============================================================*                
         DS    0D                                                               
FIXIT    NTR1  BASE=*,LABEL=*                                                   
         ST    R6,FULL            SAVE R6 POINTER                               
         XC    BPERS,BPERS                                                      
         LA    R5,BPERS                                                         
         MVC   WORK(6),SVSTART                                                  
         GOTO1 VGETDAY,DMCB,WORK,DUB                                            
         CLI   0(R1),1            IS IT MONDAY?                                 
         BE    FIXREC5            YES                                           
         ZIC   R0,0(R1)           NO - BACK UP TO PREVIOUS MON                  
         BCTR  R0,0                                                             
         LCR   R0,R0                                                            
         GOTO1 VADDAY,DMCB,WORK,WORK,(R0)                                       
*                                                                               
FIXREC5  GOTO1 VDATCON,DMCB,(0,WORK),(2,0(R5))                                  
         LA    R5,2(R5)                                                         
         GOTO1 VADDAY,DMCB,WORK,WORK+6,F'7'                                     
         MVC   WORK(6),WORK+6                                                   
         CLC   WORK(6),SVEND                                                    
         BNH   FIXREC5                                                          
         GOTO1 VDATCON,DMCB,(0,SVEND),(2,0(R5))  ESTIMATE END                   
*                                                                               
         LA    R5,BPERS                                                         
         CLC   2(2,R6),0(R5)      FIRST TIME ONLY                               
         BL    FIXREC45           DELETE THE ELEMENT                            
FIXREC10 CLC   2(2,R6),2(R5)      CHECK WITHIN THIS WEEK PERIOD                 
         BL    FIXREC40             YES - IN THIS WEEK                          
         LA    R5,2(R5)             --   NO  - TRY NEXT WEEK                    
         CLC   0(2,R5),=X'0000'                                                 
         BE    FIXREC45           DELETE THE ELEMENT                            
         B     FIXREC10                                                         
*                                 WITHIN THIS WEEKS PERIOD                      
FIXREC40 GOTO1 VDATCON,DMCB,(2,0(R5)),(0,WORK)                                  
         ZIC   R0,SVEOWSDY                                                      
         BCTR  R0,0                                                             
         GOTO1 VADDAY,DMCB,WORK,WORK+6,(R0)                                     
         GOTO1 VDATCON,DMCB,(0,WORK+6),(2,2(R6))                                
         B     FIXREC50           GET'S NEXT ELEMENT                            
*                                                                               
FIXREC45 GOTO1 VRECUP,DMCB,(X'00',AREC),(R6),0                                  
         B     FIXREC55                                                         
*                                                                               
FIXREC50 ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
FIXREC55 CLI   0(R6),0                                                          
         BE    FIXREC60                                                         
         CLI   0(R6),X'21'                                                      
         BNE   FIXREC50                                                         
         B     FIXREC10           FIX ELEMENT DATE                              
*                                                                               
FIXREC60 GOTO1 PUTREC                                                           
         L     R6,FULL            RESTORE R6                                    
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*=========================================================                      
* CALL OFFICER TO VALIDATE LIMIT ACCESS                                         
*=========================================================                      
         SPACE 1                                                                
CALLOFCR NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A38' GET OFFICER ADDRESS                       
         GOTO1 VCALLOV,DMCB                                                     
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    WORK2,WORK2                                                      
         LA    R1,WORK2                                                         
         USING OFFICED,R1                                                       
*                                                                               
         MVI   OFCSYS,C'S'                                                      
         MVC   OFCAUTH,T202FFD+6                                                
         MVC   OFCAGY,AGYALPHA                                                  
         MVC   OFCOFC,SVOFFC                                                    
         MVC   OFCCLT,QCLT                                                      
         MVC   OFCSAGMD,SVAGYMD                                                 
         MVC   OFCLMT(4),T202FFD+6                                              
         MVC   OFCACCSC(3),SVCACCS    ACCESS LIST FROM CLTHDR                   
         MVC   OFCACCSM(3),SVMACCS    ACCESS LIST FROM MKTREC                   
         LA    R0,SECBLK                                                        
         ST    R0,OFCSECD                                                       
         DROP  R1                                                               
*                                                                               
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(C'N',WORK2),VCOMFACS                                  
         MVI   ERRCD,ERRLOCK                                                    
         CLI   0(R1),0                                                          
         JE    EXXMOD                                                           
         GOTO1 ERROR                                                            
         LTORG                                                                  
*                                                                               
SLNTABC  DS    0H                                                               
       ++INCLUDE SPSLNTAB                                                       
         DC    5X'00'                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE SPGOLWRK                                                       
         PRINT OFF                                                              
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDOFFICED                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDGLVXCTLD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'065SPGOL00   12/18/17'                                      
         END                                                                    
