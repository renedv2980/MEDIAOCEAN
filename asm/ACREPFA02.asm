*          DATA SET ACREPFA02  AT LEVEL 068 AS OF 05/01/02                      
*PHASE ACFA02A                                                                  
*INCLUDE CASHVAL                                                                
*INCLUDE COVAIL                                                                 
*INCLUDE SCANNER                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE UNDERLIN                                                               
*INCLUDE ACSLRY                                                                 
*INCLUDE ACSALHST                                                               
*INCLUDE ACLIST                                                                 
         TITLE 'ACFA02 - FEE ALLOCATION SYSTEM  '                               
ACFA02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACFA**,RA,R9,R7                                              
         L     RC,0(R1)                                                         
         USING ACWORKD,RC                                                       
         LA    R8,SPACEND                                                       
         USING MYD,R8                                                           
         SPACE 1                                                                
         L     R2,=V(CASHVAL)      RELOCATE V AND A TYPES                       
         ST    R2,CASHVAL                                                       
         L     R2,=V(COVAIL)                                                    
         ST    R2,COVAIL                                                        
         L     R2,=V(SCANNER)                                                   
         ST    R2,SCANNER                                                       
         L     R2,=V(UNDERLIN)                                                  
         ST    R2,UNDERLIN                                                      
         L     R2,=V(ACSLRY)                                                    
         ST    R2,ACSLRY                                                        
         L     R2,=V(ACSALHST)                                                  
         ST    R2,ACSALHST                                                      
         L     R2,=V(ACLIST)                                                    
         ST    R2,ACLIST                                                        
         L     R2,=V(PRNTBL)                                                    
         ST    R2,PRNTBL                                                        
         L     R2,=A(CLIMEMO)                                                   
         ST    R2,ACLIMEMO                                                      
         L     R2,=A(COSTCLI)                                                   
         ST    R2,ACOSTCLI                                                      
         SPACE 1                                                                
         L     R2,=A(RULEPOOL)                                                  
         ST    R2,ARULES                                                        
         ST    R2,APOOL                                                         
         MVC   LRULE,8(R2)                                                      
         L     R2,=A(MYIO)                                                      
         ST    R2,AMYIO                                                         
         L     R2,=A(MYIO2)                                                     
         ST    R2,AMYIO2                                                        
         LA    R2,HOOK                                                          
         ST    R2,HEADHOOK                                                      
*                                                                               
         CLI   MODE,RUNFRST        RUN FIRST INITIALIZATION                     
         BNE   FACON                                                            
         LA    RF,SALAREA                                                       
         USING SALARYD,RF                                                       
         XC    SALBLOCK(SALLNQ),SALBLOCK                                        
         BAS   RE,IDBILD                                                        
         B     XIT                                                              
         DROP  RF                                                               
         EJECT                                                                  
*              CONTROL FEE ALLOCATION                                           
         SPACE 3                                                                
FACON    CLI   MODE,REQFRST                                                     
         BNE   RUNEND                                                           
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         BAS   RE,REQINIT                                                       
         L     RF,VEXTRAS                                                       
         USING RUNXTRAD,RF                                                      
         L     R4,ADMASTD                                                       
         USING MASTD,R4                                                         
         L     R4,MCBXAREA                                                      
         ST    R4,SAVEABOX                                                      
*                                                                               
         MVI   NBOX,0              PHASE 1 IS TO READ RULES                     
         MVI   RCSUBPRG,1                  PRINT RULE LISTING                   
         MVC   PRINTOPT,QOPT1              OUTPUT TO SORT                       
         BAS   RE,RULIO                                                         
         CLI   QOPT3,C'Y'                                                       
         BNE   XIT                                                              
*                                                                               
*                                  PHASE 2 IS TO READ PERSONAL RECORDS          
*                                                                               
         LA    RF,SALAREA                                                       
         USING SALARYD,RF                                                       
         XC    WORK,WORK                                                        
         MVC   WORK(8),SALBLOCK+8   ADDR AND LENGTH OF ACQUIRED BUFFER          
         XC    SALBLOCK(SALLNQ),SALBLOCK                                        
         MVC   SALBLOCK+8(8),WORK   RESTORE ADDR/LEN OF BUFFER                  
         MVC   SALCMPY,RCCOMPFL                                                 
         MVI   SALMETHD,C'1'        SET DEFAULT TO BE METHOD 1                  
         MVC   SALSTART(2),MNTHLIST+22                                          
         MVI   SALSTART+2,X'01'                                                 
         MVC   SALEND(2),SALSTART                                               
         MVI   SALEND+2,X'31'                                                   
         MVC   SALACOVL,COVAIL                                                  
         OI    SALSTAT1,SALHRRAT   TURN ON INCLUDE HOURLY RATES                 
         LA    R0,SALACTM#         NUMBER OF MONTHLY HRLY ACCUMS                
         LA    R1,SALACTM1                                                      
         ZAP   0(L'SALACTM1,R1),=P'0'                                           
         LA    R1,L'SALACTM1(R1)                                                
         BCT   R0,*-10                                                          
*                                                                               
         SR    R1,R1                                                            
         LA    R0,NULL             CLEAR TABLE                                  
         L     RE,ACOSTCLI                                                      
         L     RF,=A(COSTCLLN)     LENGTH OF TABLE                              
         MVCL  RE,R0                                                            
         L     RF,ACOSTCLI                                                      
         USING COSTCD,RF                                                        
         MVI   COSTCODE,X'FF'      SET END OF TABLE                             
         DROP  RF                                                               
*                                                                               
         MVI   RCSUBPRG,2                  REPORT HOURLY DISTRIBUTION           
         MVC   PRINTOPT,QOPT2              OUTPUT TO SORT                       
         BAS   RE,PERSIO                                                        
*                                                                               
* PHASE 3 IS TO READ SORT                                                       
*                                                                               
         MVI   RCSUBPRG,3                  PRINT FEE ALLOCATION REPORT          
         MVC   PRINTOPT,QOPT3              MAKE POSTINGS BACK TO 1R             
         BAS   RE,CLIREPS                                                       
         B     XIT                                                              
*                                                                               
RUNEND   CLI   MODE,RUNLAST                                                     
         BNE   CHREQLST                                                         
         BAS   RE,EOFPOST                                                       
         BAS   RE,CLOSWRK                                                       
         B     XIT                                                              
*                                                                               
CHREQLST CLI   MODE,REQLAST                                                     
         BNE   XIT                                                              
         GOTO1 ADSORTER,DMCB,=C'END'                                            
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
* INITIALIZATION                                                                
*                                                                               
REQINIT  NTR1                                                                   
         MVC   WORK,QSTART                                                      
         MVC   WORK+4(2),=C'15'                                                 
         LA    R2,MNTHLIST+22                                                   
         LA    R3,12                                                            
*                                                                               
         ZAP   PRSSALY,=P'0'                                                    
         LA    R1,PKFLDS           INITIALIZE PACKED FIELDS                     
         LA    R0,PKFLDSQ                                                       
         ZAP   0(L'PKFLDS,R1),=P'0'                                             
         LA    R1,L'PKFLDS(R1)                                                  
         BCT   R0,*-10                                                          
*                                                                               
RQ2      PACK  DUB(3),WORK(5)      BUILD LIST OF PACKED Y/M                     
         MVC   0(2,R2),DUB                                                      
         MVC   DMCB+8(4),=F'-30'                                                
         GOTO1 ADDAY,DMCB,WORK,WORK+6                                           
         MVC   WORK(6),WORK+6                                                   
         BCTR  R2,0                                                             
         BCTR  R2,0                                                             
         BCT   R3,RQ2                                                           
         SPACE 1                                                                
         PACK  DUB,QSTART+2(2)     SET UP MMM/YY FOR HEADLINES                  
         CVB   R1,DUB                                                           
         MH    R1,=H'3'                                                         
         LA    R1,MONTHS-3(R1)                                                  
         MVC   ALLMONTH(3),0(R1)                                                
         MVI   ALLMONTH+3,C'/'                                                  
         GOTO1 DATCON,DMCB,(0,QSTART),(X'20',WORK)                              
         MVC   ALLMONTH+4(2),WORK                                               
         BAS   RE,WORKMON          FIGURE OUT WORKING DAYS IN MONTH             
*                                                                               
         L     R2,=A(SORTPOOL)     INITIALIZE SORT                              
         GOTO1 ADSORTER,DMCB,SORTCARD,RECCARD,(40,(R2))                         
         BAS   RE,MOS                                                           
         EJECT                                                                  
*                                                                               
* INITIAL READ OF COMPANY/LEDGERS ETC                                           
*                                                                               
         LA    R4,KEY                                                           
         USING ACKEYD,R4                                                        
         MVC   KEY,SPACES                                                       
         MVC   ACKEYACC(1),QCOMPANY                                             
         BAS   RE,READ                                                          
         BAS   RE,GETNAME                                                       
         MVC   COMPNAME,WORK                                                    
*                                                                               
         MVC   ACKEYACC+1(2),=C'1F'                                             
         BAS   RE,READ                                                          
         MVI   ELCODE,X'16'                                                     
         BAS   RE,GETEL                                                         
         USING ACHEIRD,R6                                                       
         MVC   LCLICODE,ACHRLEVA   GET LENGTH OF CLIENT AND DIVISION            
         ZIC   R1,ACHRLEVB                                                      
         ZIC   R0,ACHRLEVA                                                      
         SR    R1,R0                                                            
         STC   R1,LDIVCODE                                                      
         CLI   LDIVCODE,6                                                       
         BL    *+6                                                              
         MVI   LDIVCODE,6                                                       
*                                                                               
         MVC   ACKEYACC+1(2),=C'1C'                                             
         BAS   RE,READ                                                          
         MVI   ELCODE,X'16'                                                     
         BAS   RE,GETEL                                                         
         USING ACHEIRD,R6                                                       
         MVI   OFFSW,C'Y'                                                       
*                                                                               
         MVI   ACKEYACC+2,C'R'                                                  
         BAS   RE,READ                                                          
         BAS   RE,GETEL                                                         
         MVC   LDPTCODE,ACHRLEVA   GET LENGTH OF DPT SUB AND PERSON             
         ZIC   R1,ACHRLEVB                                                      
         ZIC   R0,ACHRLEVA                                                      
         SR    R1,R0                                                            
         STC   R1,LSUBCODE                                                      
         ZIC   R1,ACHRLEVC                                                      
         ZIC   R0,ACHRLEVB                                                      
         SR    R1,R0                                                            
         STC   R1,LPRSCODE                                                      
         CLI   ACHRLEVD,0                                                       
         BE    XIT                                                              
         MVC   LOFFCODE,ACHRLEVA   4 LEVELS - 1ST MUST BE OFFICE                
         MVC   LDPTCODE,ACHRLEVB   COMBINE 1&2 OFF/DPT AS "DEPT"                
         ZIC   R1,ACHRLEVB                                                      
         ZIC   R0,LOFFCODE                                                      
         SR    R1,R0                                                            
         STC   R1,LDPTCOD2                                                      
         ZIC   R1,ACHRLEVC                                                      
         ZIC   R0,LDPTCODE                                                      
         SR    R1,R0                                                            
         STC   R1,LSUBCODE                                                      
         ZIC   R1,ACHRLEVD                                                      
         ZIC   R0,LSUBCODE                                                      
         SR    R1,R0                                                            
         STC   R1,LPRSCODE                                                      
*                                                                               
         LA    RF,SALAREA                                                       
         USING SALARYD,RF                                                       
         MVC   SALLEVS,LENLEVLS            SET 1R LEVELS FOR SALHST             
         B     XIT                                                              
         DROP  RF                                                               
         EJECT                                                                  
*              ROUTINE TO CONTROL RULE I/O                                      
         SPACE 3                                                                
RULIO    NTR1                                                                   
         SR    R1,R1                                                            
         LA    R0,NULL             CLEAR TABLE                                  
         L     RE,ACLIMEMO                                                      
         L     RF,=A(CLIMEMOQ*6)   LENGTH OF TABLE                              
         MVCL  RE,R0                                                            
         MVI   FRSTCLI,C'Y'                                                     
*                                                                               
         LA    R4,KEY                                                           
         USING ACKEYD,R4                                                        
         MVC   KEY,SPACES                                                       
         MVC   ACKEYACC(1),QCOMPANY                                             
         MVC   ACKEYACC+1(2),=C'1F'                                             
         MVC   ACKEYACC+3(12),QACCOUNT                                          
         MVI   KEY+14,0                                                         
         CLC   QACCOUNT,SPACES                                                  
         BNE   *+8                                                              
         MVI   KEY+14,X'41'                                                     
         BAS   RE,HIGH                                                          
         B     RU4                                                              
*                                                                               
RU2      MVC   KEYSAVE,KEY                                                      
         BAS   RE,SEQ                                                           
*                                                                               
RU4      CLC   KEY(3),KEYSAVE      END OF RULES                                 
         BE    RU6                                                              
         BAS   RE,DUMPRULE                                                      
         B     XIT                                                              
*                                                                               
RU6      ZIC   R1,LCLICODE         CHANGE OF CLIENT                             
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   CLICODE(0),KEY+3     SAVE CLIENT CODE FOR LIST CHECK             
         CLI   FRSTCLI,C'Y'                                                     
         BE    RU7                                                              
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   KEY+3(0),KEYSAVE+3                                               
         BE    RU12                                                             
         BAS   RE,DUMPRULE         RULES TO SORT                                
         CLC   QACCOUNT,SPACES                                                  
         BE    RU7                                                              
         CLC   KEY+3(12),QACCOUNT                                               
         BNE   XIT                                                              
RU7      L     RF,VEXTRAS                                                       
         USING RUNXTRAD,RF                                                      
         MVI   EXCLUDE,C'I'        ASSUME ALL ARE INCLUDED                      
         OC    VLISTREC,VLISTREC                                                
         BZ    RU7B                NO LIST RECORD                               
         GOTO1 ACLIST,DMCB,VLISTREC,CLICODE                                     
         MVC   EXCLUDE,DMCB                                                     
         CLI   EXCLUDE,C'E'                                                     
         BE    RU2                 EXCLUDE THIS CLIENT                          
*                                                                               
RU7B     BAS   RE,GETNAME                                                       
         MVC   CLINAME,WORK        SAVE CLIENT NAME                             
         MVI   FORCEHED,C'Y'                                                    
         MVI   FRSTCLI,C'N'                                                     
         L     R2,ACLIMEMO                                                      
*                                                                               
RU8      OC    0(6,R2),0(R2)       MAKE A MEMO ITEM OF CLIENT                   
         BZ    RU10                                                             
         CLI   0(R2),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         LA    R2,6(R2)                                                         
         B     RU8                                                              
*                                                                               
RU10     MVC   0(6,R2),KEY+3                                                    
         B     RU2                                                              
*                                                                               
RU12     CLI   EXCLUDE,C'E'                                                     
         BE    RU2                 EXCLUDE THIS CLIENT                          
         CLC   ACKEYCON,SPACES     DIVISION                                     
         BNE   RU14                                                             
         BAS   RE,GETNAME          SAVE NAME                                    
         MVC   DIVNAME,WORK                                                     
         B     RU2                                                              
*                                                                               
RU14     BAS   RE,RULES            RULE                                         
         B     RU2                                                              
         EJECT                                                                  
**********************************************************************          
* ROUTINES TO HANDLE RULE RECORDS                                    *          
**********************************************************************          
         SPACE 1                                                                
RULES    NTR1                                                                   
         L     R4,AMYIO            HANDLE THE PRINT LINE                        
         USING ACKEYD,R4                                                        
         LA    R1,ACKEYACC+3                                                    
         BAS   RE,SPLITCLI                                                      
         LA    R1,ACKEYCON+3                                                    
         BAS   RE,SPLITDPT                                                      
         MVC   P+1(6),DIVCODE                                                   
         CLC   P+1(3),=C'***'                                                   
         BE    RULES2                                                           
         GOTO1 CHOPPER,DMCB,(36,DIVNAME),(29,P+8),1                             
*                                                                               
RULES2   MVC   P+38(6),DPTCODE                                                  
         MVC   P+45(6),SUBCODE                                                  
         MVC   P+52(8),PRSCODE                                                  
         BAS   RE,GETNAME                                                       
         GOTO1 CHOPPER,DMCB,(36,WORK),(29,P+61),1                               
*                                                                               
         L     R2,ARULES           ADDRESS RULES ENTRY                          
         L     R1,4(R2)                                                         
         LA    R1,1(R1)            BUMP N'ITEMS                                 
         ST    R1,4(R2)                                                         
         C     R1,0(R2)                                                         
         BNH   *+6                                                              
         DC    H'0'                                                             
         BCTR  R1,0                                                             
         M     R0,LRULE                                                         
         LA    R2,16(R1,R2)                                                     
         USING RULED,R2                                                         
         L     R1,LRULE                                                         
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         XC    0(0,R2),0(R2)                                                    
         MVC   RUCLI,CLICODE                                                    
         MVC   RUDIV,DIVCODE                                                    
         MVC   RUDPT,DPTCODE                                                    
         MVC   RUSUB,SUBCODE                                                    
         MVC   RUPRS,PRSCODE                                                    
         EJECT                                                                  
*              NOW HANDLE RULE ELEMENTS                                         
         SPACE 3                                                                
         USING ACRULD,R6                                                        
         LR    R6,R4                                                            
         MVI   ELCODE,X'69'                                                     
         BAS   RE,GETEL                                                         
         B     RULES6                                                           
*                                                                               
RULES4   BAS   RE,NEXTEL                                                        
*                                                                               
RULES6   BE    RULES8                                                           
         CLC   P,SPACES                                                         
         MVC   P,SPACES                                                         
         BNE   XIT                                                              
         BAS   RE,SPLAT                                                         
         B     XIT                                                              
*                                                                               
RULES8   CLC   ACRULEFF,MNTHLIST+22                                             
         BH    RULES4          SKIP RULES EFFECTIVE AFTER CURRENT MONTH         
         ZIC   R1,ACRULEN                                                       
         SH    R1,=H'5'                                                         
         MVC   P+92(30),SPACES                                                  
         EX    R1,*+4                                                           
         MVC   P+92(0),ACRULDET                                                 
         MVC   CARD(30),P+92                                                    
         OC    ACRULEFF,ACRULEFF                                                
         BZ    RULES10             NO EFFECTIVE DATE                            
         LA    RF,P+92                                                          
         LA    RF,1(R1,RF)                                                      
         MVI   0(RF),C','                                                       
         MVC   WORK(2),ACRULEFF    FOEMAT EFFECTIVE DATE                        
         MVI   WORK+2,1                                                         
         GOTO1 DATCON,DMCB,(1,WORK),(6,1(RF))                                   
         EJECT                                                                  
*              INTERPRET RULES ELEMENT AND POST                                 
         SPACE 3                                                                
RULES10  MVC   CARD+30(50),SPACES                                               
         GOTO1 SCANNER,DMCB,(C'C',CARD),(1,SCAN)                                
         LA    R1,RULETAB                                                       
*                                                                               
RULES12  CLC   SCAN+12(10),0(R1)                                                
         BE    RULES14                                                          
         LA    R1,12(R1)                                                        
         CLI   0(R1),X'FF'                                                      
         BNE   RULES12                                                          
         MVC   P,SPACES                                                         
         B     RULES4              IF NOT IN TABLE IGNORE IT                    
*                                                                               
RULES14  BAS   RE,SPLAT            PRINT THE RULE                               
         ZIC   R3,10(R1)           DISPLACEMENT INTO RULED                      
         AR    R3,R2                                                            
         CLI   11(R1),1            ROUTINE NUMBER                               
         BL    RULES16                                                          
         BE    RULES18                                                          
         CLI   11(R1),3                                                         
         BL    RULES20                                                          
         BE    RULES22                                                          
         BH    RULES26                                                          
*                                                                               
RULES16  MVC   0(2,R3),SCAN+10     H                                            
         B     RULES21                                                          
*                                                                               
RULES18  MVC   0(4,R3),SCAN+22     CL4                                          
         B     RULES4                                                           
*                                                                               
RULES20  ZIC   R5,SCAN+1           NN.NNN                                       
         GOTO1 CASHVAL,DMCB,(3,SCAN+22),(R5)                                    
         CLI   DMCB,X'FF'                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   0(2,R3),DMCB+6                                                   
         CLC   SCAN+12(4),=C'RATE'                                              
         BNE   RULES21             CARRY RATE TO 2 DECIMAL PLACES               
         L     R1,DMCB+4                                                        
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
         STH   R1,0(R3)                                                         
*                                                                               
RULES21  OC    0(2,R3),0(R3)                                                    
         BNZ   RULES4                                                           
         OI    0(R3),X'80'         OVERRIDES OF ZERO = X'80'                    
         B     RULES4                                                           
*                                                                               
RULES22  LA    R1,MONTHS           MONTH                                        
         LA    RF,1                                                             
         SPACE 1                                                                
RULES24  STC   RF,0(R3)                                                         
         CLC   0(1,R1),SCAN+22                                                  
         BE    RULES4                                                           
         LA    R1,3(R1)                                                         
         LA    RF,1(RF)                                                         
         B     RULES24                                                          
*                                                                               
RULES26  MVC   0(2,R3),SCAN+22     CL2                                          
         B     RULES4                                                           
         EJECT                                                                  
**********************************************************************          
* ROUTINES TO DUMP RULES TO SORT                                     *          
**********************************************************************          
         SPACE 1                                                                
DUMPRULE NTR1                                                                   
         L     R2,ARULES                                                        
         L     R3,4(R2)                                                         
         LTR   R3,R3               ANY RULES                                    
         BZ    XIT                                                              
         XC    4(4,R2),4(R2)       SET N'RULES USED BACK TO ZERO                
         LA    R2,16(R2)                                                        
         USING RULED,R2                                                         
*                                                                               
         LA    RE,SRTREC           RE=A(SORT BUFFER)                            
         LA    RF,SRTLNQ           RF=LENGTH OF SORT BUFFER                     
         SR    R1,R1                                                            
         MVCL  RE,R0               CLEAR SORT BUFFER                            
*                                                                               
         L     R1,LRULE                                                         
         LA    R1,40(R1)                                                        
         STH   R1,SRTRECL                                                       
         MVC   SRTCLI,RUCLI                                                     
         MVI   SRTTYPE,1                                                        
         LA    R4,1                                                             
         L     R5,LRULE                                                         
         BCTR  R5,0                                                             
*                                                                               
DMPR10   ST    R4,SRTRULES-4       RULE SEQ NO. AT END OF KEY                   
         EX    R5,*+4                                                           
         MVC   SRTRULES(0),0(R2)                                                
         GOTO1 ADSORTER,DMCB,=C'PUT',SRTREC                                     
         A     R2,LRULE                                                         
         LA    R4,1(R4)                                                         
         BCT   R3,DMPR10                                                        
         B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* ROUTINES TO READ RULES FROM SORT                                   *          
**********************************************************************          
         SPACE 1                                                                
LOADRULE NTR1                                                                   
         L     R2,ARULES                                                        
         L     R3,SRTRULES-4                                                    
         ST    R3,4(R2)                                                         
         BCTR  R3,0                                                             
         MH    R3,LRULE+2                                                       
         LA    R2,16(R2,R3)                                                     
         L     R5,LRULE                                                         
         BCTR  R5,0                                                             
         EX    R5,*+4                                                           
         MVC   0(0,R2),SRTRULES                                                 
         B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* ROUTINES TO CONTROL READING OF 1R LEDGER                           *          
**********************************************************************          
         SPACE 1                                                                
PERSIO   NTR1                                                                   
         BAS   RE,CLEARPOL                                                      
         LA    R4,KEY                                                           
         USING ACKEYD,R4                                                        
         MVC   KEY,SPACES                                                       
         MVC   ACKEYACC(1),QCOMPANY                                             
         MVC   ACKEYACC+1(2),=C'1R'                                             
         MVI   KEY+14,1                                                         
         BAS   RE,HIGH                                                          
         B     PR4                                                              
*                                                                               
PR2      BAS   RE,SEQ                                                           
*                                                                               
PR4      CLC   KEY(3),KEYSAVE                                                   
         BE    PR6                                                              
         BAS   RE,PERSEND                                                       
         B     XIT                                                              
*                                                                               
PR6      CLC   ACKEYCON,SPACES     ACCOUNT RECORD                               
         BNE   PR12                                                             
         BAS   RE,PERSEND                                                       
         LA    R1,ACKEYACC+3                                                    
         BAS   RE,SPLITDPT                                                      
         BAS   RE,GETNAME                                                       
         CLC   SUBCODE,SPACES                                                   
         BNE   PR8                                                              
         MVC   DPTNAME,WORK        DEPARTMENT                                   
         B     PR2                                                              
*                                                                               
PR8      CLC   PRSCODE,SPACES                                                   
         BNE   PR10                                                             
         MVC   SUBNAME,WORK        SUB-DEPT                                     
         MVI   FORCEHED,C'Y'                                                    
         B     PR2                                                              
*                                                                               
PR10     MVC   PRSNAME,WORK        PERSON                                       
         MVC   P+1(8),PRSCODE                                                   
         MVC   P+10(34),PRSNAME                                                 
         BAS   RE,COMPSAL          GO AND FIGURE OUT SALARY                     
         B     PR2                                                              
*                                                                               
PR12     MVI   ELCODE,X'45'                                                     
         BAS   RE,GETEL                                                         
         BNE   PR16                                                             
         MVC   OFFCODE,SPACES      CLEAR OFFICE CODE                            
         SR    R1,R1                                                            
         IC    R1,LOFFCODE         SAVE OFFICE CODE                             
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   OFFCODE(0),ACKEYCON+3                                            
         LA    R1,ACKEYCON+3       POINT R1 TO CONTRA ACCOUNT                   
         SR    R0,R0                                                            
         IC    R0,LOFFCODE                                                      
         AR    R1,R0               POINT R1 PAST OFFICE CODE TO CLIENT          
         BAS   RE,SPLITCLI                                                      
         CLI   ACKEYCON+2,C'N'     SPECIAL FOR NON-CLIENT TIME                  
         BNE   PR14                                                             
         MVC   CLICODE,=6X'FF'                                                  
         MVC   DIVCODE,CLICODE                                                  
*                                                                               
PR14     BAS   RE,POST                                                          
         BAS   RE,NEXTEL                                                        
         BE    PR14                                                             
         B     PR2                                                              
*                                                                               
PR16     L     R4,AMYIO                                                         
         MVC   ACKEYDTE,=X'FFFFFF' SKIP PAST TRANSACTIONS                       
         BAS   RE,SKIP                                                          
         LA    R4,KEY                                                           
         B     PR4                                                              
         SPACE 3                                                                
*              ROUTINE TO FIGURE OUT SALARY                                     
         SPACE 2                                                                
COMPSAL  NTR1                                                                   
         L     RE,AMYIO                                                         
         MVC   SAVEKEY,0(RE)       TO RESTORE READS                             
         LA    R2,SALAREA2                                                      
         CLI   MNTHLIST+22,X'95'                                                
         BNL   COMPS02                                                          
         USING SLRD,R2                                                          
         MVC   DUB(2),MNTHLIST+22                                               
         MVC   DUB+2(2),DUB                                                     
*MN      GOTO1 ACSLRY,DMCB,(R6),DUB,(R2)                                        
         GOTO1 ACSLRY,DMCB,(X'80',(R6)),DUB,(R2),ADCOMFAC                       
         ZAP   PRSSALP,SLRTOT                                                   
         B     XIT                                                              
*                                                                               
         USING SALARYD,R2                                                       
COMPS02  DS    0H                                                               
         LA    R2,SALAREA                                                       
         MVC   SALOFFC(L'SALOFFC+L'SALDEPT+L'SALSDPT+L'SALPRSN),SPACES          
         MVC   SALOFFC,OFFCOD2                                                  
         MVC   SALDEPT,DPTCOD2                                                  
         MVC   SALSDPT,SUBCODE                                                  
         MVC   SALPRSN,PRSCODE                                                  
         L     R3,AMYIO2                                                        
         GOTO1 ACSALHST,DMCB,ACWORKD,(R2),(R3)                                  
         OC    SALSTAT2,SALSTAT2   ANY ERRORS?                                  
         BZ    COMPS04             NO                                           
         TM    SALSTAT2,X'FF'-SALINVPR ANY ERROR BUT PERSON MISSING             
         BZ    *+6                                                              
         DC    H'0'                DIE FOR NOW                                  
COMPS04  ZAP   PRSSALP,SALSALRY                                                 
         MVC   KEY,SAVEKEY                                                      
         BAS   RE,READ             RESTORE READ SEQUENCE                        
         B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* ROUTINE TO POST BUCKETS INTO POOL                                  *          
**********************************************************************          
         SPACE 1                                                                
         USING TRHISTD,R6                                                       
         USING POOLD,R2                                                         
POST     NTR1                                                                   
         L     R2,APOOL                                                         
         LA    R2,16(R2)           BUMP TO BINTABLE                             
         LA    R3,1800             MAX NUMBER OF RULES 547200/304               
         L     R4,AMYIO                                                         
*                                                                               
POST10   CLI   0(R2),0             FIND A NEW ENTRY                             
         BE    POST30                                                           
         CLC   POOLCLI,CLICODE     OR EXISTING ONE FOR CLI/DIV                  
         BNE   POST20                                                           
         CLC   POOLDIV,DIVCODE                                                  
         BE    POST30                                                           
*                                                                               
POST20   LA    R2,POOLNQ(R2)                                                    
         BCT   R3,POST10                                                        
         DC    H'0'                                                             
*                                                                               
POST30   MVC   POOLCLI,CLICODE                                                  
         MVC   POOLOFF,OFFCODE                                                  
         MVC   POOLDIV,DIVCODE                                                  
*                                                                               
         LA    R3,POOLHOUR         IS THIS AN HOUR (CR)                         
         ZAP   DUB,TRHSCR                                                       
         CLI   ACKEYREF,C'H'                                                    
         BE    POST40                                                           
         ZAP   DUB,TRHSDR                                                       
         LA    R3,POOLFEES                                                      
         CLI   ACKEYREF,C'F'       OR FEES                                      
         BE    POST40                                                           
         LA    R3,POOLCOST         MUST BE COST                                 
*                                                                               
POST40   BAS   RE,POST50                                                        
         B     XIT                                                              
*                                                                               
POST50   NTR1                                                                   
         LA    R2,MNTHLIST         NOW LOCATE CORRECT MONTH                     
         LA    R5,12                                                            
*                                                                               
POST60   CLC   0(2,R2),TRHSYEAR                                                 
         BE    POST70                                                           
         LA    R2,2(R2)                                                         
         LA    R3,L'PLBUK(R3)      BUMP TO THE NEXT BUCKET                      
         BCT   R5,POST60                                                        
         B     XIT                                                              
POST70   AP    0(L'PLBUK,R3),DUB   FOUND - ADD IN                               
         B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* CLEAR POOL                                                         *          
**********************************************************************          
         SPACE 1                                                                
CLEARPOL NTR1                                                                   
         L     R2,APOOL                                                         
         LA    R2,16(R2)           BUMP TO BINTABLE                             
         LA    R3,1800             MAX NUMBER OF RULES 547200/304               
*                                                                               
CLRP10   XC    POOLCLI(L'POOLCLI+L'POOLDIV),POOLCLI                             
         LA    R1,PLBUK            START OF BUCKETS                             
         LA    R0,PLBUKNUM         NUMBER OF BUCKETS                            
         ZAP   0(L'PLBUK,R1),=P'0'                                              
         LA    R1,L'PLBUK(R1)                                                   
         BCT   R0,*-10                                                          
         XC    POOLOFF(4),POOLOFF                                               
         LA    R2,POOLNQ(R2)                                                    
         BCT   R3,CLRP10                                                        
         B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* CONTROL REPORT AND OUTPUT OF SORT RECORDS                          *          
**********************************************************************          
         SPACE 1                                                                
PERSEND  NTR1                                                                   
         L     R3,ACLIMEMO                                                      
         L     R2,APOOL                                                         
         LA    R2,16(R2)           BUMP TO BINTABLE                             
         OC    0(12,R2),0(R2)      ANY ENTRIES                                  
         BZ    XIT                                                              
*                                                                               
PE2      OC    0(6,R3),0(R3)       FOR EACH ACTIVE CLIENT                       
         BZ    PE8                                                              
         BAS   RE,PE4                                                           
         LA    R3,6(R3)                                                         
         B     PE2                                                              
*                                                                               
PE4      NTR1                                                                   
         LA    R0,LINELNQ          # OF ACCUMULATORS                            
         LA    R1,CLILINE          CLEAR ACCUMES                                
         ZAP   0(L'PLBUK,R1),=P'0'                                              
         LA    R1,L'PLBUK(R1)                                                   
         BCT   R0,*-10                                                          
         MVC   ACTCLI,0(R3)                                                     
*                                                                               
         USING POOLD,R2                                                         
PE6      LA    R4,CLILINE                                                       
         CLC   POOLCLI,ACTCLI      ADD TO CLIENT LINE IF IT MATCHES             
         BNE   *+8                                                              
         BAS   RE,ADDLINE                                                       
         LA    R4,ALLLINE          AND TO ALL CLIENTS                           
         CLI   POOLCLI,X'FF'       IF THIS IS A CLIENT                          
         BE    *+8                                                              
         BAS   RE,ADDLINE                                                       
         LA    R4,TOTLINE                                                       
         BAS   RE,ADDLINE          AND TO TOTAL LINE                            
*                                                                               
         LA    R2,POOLNQ(R2)                                                    
         OC    0(12,R2),0(R2)                                                   
         BNZ   PE6                                                              
*                                                                               
         LA    R1,CLILINE                                                       
         LA    R0,PLBUKNUM         # OF BUCKETS TO LOOP                         
         CP    0(L'PLBUK,R1),=P'0'                                              
         BNE   PE10                                                             
         LA    R1,L'PLBUK(R1)                                                   
         BCT   R0,*-14                                                          
         B     XIT                                                              
*                                                                               
PE8      BAS   RE,CLEARPOL                                                      
         B     XIT                                                              
*                                                                               
ADDLINE  NTR1                                                                   
         LA    R2,POOLHOUR         ADD LINE (R2) TO LINE (R4)                   
         LA    R3,PLBUKNUM         NUMBER OF ACCUMS                             
*                                                                               
ADDLINE2 AP    0(L'PLBUK,R4),0(L'PLBUK,R2)                                      
         LA    R2,L'PLBUK(R2)                                                   
         LA    R4,L'PLBUK(R4)                                                   
         BCT   R3,ADDLINE2                                                      
         B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* REPORT AND OUTPUT RECORDS FOR EACH DIVISION                        *          
**********************************************************************          
         SPACE 1                                                                
         USING POOLD,R2                                                         
PE10     L     R2,APOOL                                                         
         LA    R2,16(R2)                                                        
         MVI   NONSW,C'N'                                                       
*                                                                               
PE12     DS    0H                                                               
         OC    0(12,R2),0(R2)                                                   
         BNZ   PE14                                                             
         MVC   P,SPACES                                                         
         B     XIT                                                              
*                                                                               
PE14     CLC   POOLCLI,ACTCLI                                                   
         BNE   PE18                                                             
*        OC    POOLCOST+44(4),POOLCOST+44    ONLY PRINT IF MONTH ACTIVE         
         CP    POOLCOST+88,=P'0'             ONLY PRINT IF MONTH ACTIVE         
         BNE   PE15                                                             
         CLI   NONSW,C'Y'                                                       
         BE    PE16                                                             
         CLC   ALLLINE+92(8),CLILINE+92                                         
         BE    PE16                                                             
*                                                                               
PE15     MVC   P+46(12),POOLCLI                                                 
         GOTO1 ADSQUASH,DMCB,P+46,12                                            
         EDIT  POOLHOUR+88,(8,P+59),2   EDIT HOURS                              
         ZAP   PACK8,ALLLINE+88                                                 
         SP    PACK8,CLILINE+88         NON-CLIENT HOURS                        
         EDIT  PACK8,(8,P+85),2                                                 
         EDIT  POOLCOST+88,(9,P+72),2                                           
         ZAP   PACK8,ALLLINE+184                                                
         SP    PACK8,CLILINE+184        NON-CLIENT COST                         
         EDIT  PACK8,(9,P+98),2                                                 
         CLI   NONSW,C'Y'          ENSURE NON-CLIENT DATA PRINTS ONCE           
         BNE   *+16                                                             
         MVC   P+85(8),SPACES                                                   
         MVC   P+98(9),SPACES                                                   
         MVI   NONSW,C'Y'                                                       
         BAS   RE,SPLAT                                                         
         EJECT                                                                  
**********************************************************************          
* NOW HANDLE THE SORT RECORD                                         *          
**********************************************************************          
         SPACE 1                                                                
PE16     MVC   P,SPACES                                                         
         LA    RE,SRTREC           RE=A(SORT BUFFER)                            
         LA    RF,SRTLNQ           RF=LENGTH OF SORT BUFFER                     
         SR    R1,R1                                                            
         MVCL  RE,R0               CLEAR SORT BUFFER                            
*                                                                               
         MVC   SRTRECL,=Y(SRTLNQ)                                               
         MVC   SRTCLI,POOLCLI                                                   
         MVI   SRTTYPE,2                                                        
         MVC   SRTDIV,POOLDIV                                                   
         MVC   SRTDPT,DPTCODE                                                   
         MVC   SRTSUB,SUBCODE                                                   
         MVC   SRTPRS,PRSCODE                                                   
         ZAP   SRTSALY,PRSSALP                                                  
         MVC   SRTOFF,POOLOFF                                                   
*                                                                               
         LA    RE,SRTHOURS                                                      
         LA    RF,PLBUKQ                                                        
         LA    R0,TOTLINE                                                       
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
         GOTO1 ADSORTER,DMCB,=C'PUT',SRTREC                                     
*                                                                               
         MVI   SRTALL,1                                                         
         LA    RE,SRTHOURS                                                      
         LA    RF,PLBUKQ                                                        
         LA    R0,ALLLINE                                                       
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
         GOTO1 ADSORTER,DMCB,=C'PUT',SRTREC                                     
*                                                                               
         MVI   SRTALL,2                                                         
         LA    RE,SRTHOURS                                                      
         LA    RF,PLBUKQ                                                        
         LA    R0,CLILINE                                                       
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
         GOTO1 ADSORTER,DMCB,=C'PUT',SRTREC                                     
*                                                                               
         MVI   SRTALL,3                                                         
         LA    RE,SRTHOURS                                                      
         LA    RF,PLBUKQ                                                        
         LA    R0,POOLHOUR                                                      
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
         GOTO1 ADSORTER,DMCB,=C'PUT',SRTREC                                     
*                                                                               
PE18     LA    R2,POOLNQ(R2)                                                    
         B     PE12                                                             
         EJECT                                                                  
*              CONTROL SORT READING FOR CLIENT REPORTS - TOTALLING              
         SPACE 3                                                                
CLIREPS  NTR1                                                                   
         XC    LKEY,LKEY                                                        
         LA    RE,SRTREC           RE=A(SORT BUFFER)                            
         LA    RF,SRTLNQ           RF=LENGTH OF SORT BUFFER                     
         SR    R1,R1                                                            
         MVCL  RE,R0               CLEAR SORT BUFFER                            
         B     CR4                                                              
*                                                                               
CR2      MVC   LKEY,SRTCLI                                                      
*                                                                               
CR4      GOTO1 ADSORTER,DMCB,=C'GET'                                            
         L     R2,DMCB+4                                                        
         LTR   R2,R2                                                            
         BZ    CR5                                                              
         XR    R3,R3                                                            
         ICM   R3,3,0(R2)                                                       
         LA    R4,SRTREC                                                        
         LA    R5,SRTLNQ                                                        
         MVCL  R4,R2                                                            
*                                                                               
         CLI   SRTTYPE,1           HANDLE RULES                                 
         BNE   CR6                                                              
         BAS   RE,LOADRULE                                                      
         XC    LKEY,LKEY                                                        
         B     CR4                                                              
*                                                                               
CR5      BAS   RE,PRSTOT           EOF - ALL TOTALS ETC                         
         BAS   RE,SUBTOT                                                        
         BAS   RE,DPTTOT                                                        
         BAS   RE,DIVTOT                                                        
         BAS   RE,CLITOT                                                        
         BAS   RE,AGYTOT                                                        
         GOTO1 PRINT,DMCB,P,=C'BC01'                                            
         B     XIT                                                              
         SPACE 1                                                                
CR6      CLC   SRTCLI,LKEY         CLIENT C/B                                   
         BE    CR7                                                              
         BAS   RE,PRSTOT                                                        
         BAS   RE,SUBTOT                                                        
         BAS   RE,DPTTOT                                                        
         BAS   RE,DIVTOT                                                        
         BAS   RE,CLITOT                                                        
         B     CR14                                                             
         SPACE 1                                                                
CR7      CLC   SRTDIV,LKEY+7       DIVISION C/B                                 
         BE    CR8                                                              
         BAS   RE,PRSTOT                                                        
         BAS   RE,SUBTOT                                                        
         BAS   RE,DPTTOT                                                        
         BAS   RE,DIVTOT                                                        
         B     CR14                                                             
         SPACE 1                                                                
CR8      CLC   SRTDPT,LKEY+13      DEPARTMENT C/B                               
         BE    CR10                                                             
         BAS   RE,PRSTOT                                                        
         BAS   RE,SUBTOT                                                        
         BAS   RE,DPTTOT                                                        
         B     CR14                                                             
         SPACE 1                                                                
CR10     CLC   SRTSUB,LKEY+19      SUB-DEPT C/B                                 
         BE    CR12                                                             
         BAS   RE,PRSTOT                                                        
         BAS   RE,SUBTOT                                                        
         SPACE 1                                                                
CR12     CLC   SRTPRS,LKEY+25      PERSON C/B                                   
         BE    CR14                                                             
         BAS   RE,PRSTOT                                                        
         EJECT                                                                  
*              CLIENT REPORTS - FIRST TIME HANDLING                             
         SPACE 3                                                                
CR14     MVC   CLICODE,SRTCLI      PICK UP CODES                                
         MVC   DIVCODE,SRTDIV                                                   
         MVC   DPTCODE,SRTDPT                                                   
         MVC   SUBCODE,SRTSUB                                                   
         MVC   PRSCODE,SRTPRS                                                   
         MVC   OFFCODE,SRTOFF                                                   
         BAS   RE,REFRESH          GET ANY NAMES THAT HAVE CHANGED              
         BAS   RE,COMPRULE         ESTABLISH RULES (INTO EFFRULE)               
*                                                                               
         CLC   SRTCLI(13),LKEY     NEW CLIENT/DIV                               
         BE    CR16                                                             
         MVI   FORCEHED,C'Y'                                                    
         LA    R2,EFFRULE          SET UP START OF CLIENT FISCAL                
         USING RULED,R2                                                         
         ZIC   R1,RUMONTH                                                       
         MH    R1,=H'3'                                                         
         LA    R1,MONTHS-3(R1)                                                  
         MVC   CLIMONTH(3),0(R1)                                                
         MVI   CLIMONTH+3,C'/'                                                  
         GOTO1 DATCON,DMCB,(0,QSTART),(X'20',WORK)                              
         MVC   CLIMONTH+4(2),WORK                                               
         PACK  DUB,QSTART+2(2)     SEE IF CLIENT'S FISCAL STARTS IN             
         CVB   R1,DUB                                                           
         STC   R1,DUB                                                           
         CLC   RUMONTH,DUB                                                      
         BNH   CR16                                                             
*                                                                               
         GOTO1 ADDAY,DMCB,(C'Y',QSTART),(X'20',WORK),-1                         
         MVC   CLIMONTH+4(2),WORK                                               
*        PACK  DUB,QSTART(2)       PREVIOUS YEAR                                
*        CVB   R1,DUB                                                           
*        BCTR  R1,0                                                             
*        EDIT  (R1),(2,DMCB)                                                    
*        MVC   CLIMONTH+4(2),DMCB                                               
*                                                                               
CR16     CLC   SRTCLI(19),LKEY     NEW DEPARTMENT                               
         BE    CR18                                                             
         MVC   WORK(6),SRTDPT                                                   
         MVC   WORK+7(36),DPTNAME                                               
         MVC   DPTMID,WORK                                                      
         MVI   DPTPEND,C'Y'                                                     
         SPACE 1                                                                
CR18     CLC   SRTCLI(25),LKEY     NEW SUB-DEPARTMENT                           
         BE    CR20                                                             
         MVC   WORK(6),SRTSUB                                                   
         MVC   WORK+7(36),SUBNAME                                               
         MVC   SUBMID,WORK                                                      
         MVI   SUBPEND,C'Y'                                                     
*                                                                               
* APPLY NUMBERS                                                                 
*                                                                               
CR20     CLI   SRTALL,2                                                         
         BE    CR22                                                             
         BH    CR24                                                             
         CLI   SRTALL,1                                                         
         BE    CR21                                                             
         LA    RE,TOTLINE                                                       
         LA    RF,PLBUKQ                                                        
         LA    R0,SRTHOURS                                                      
         LR    R1,RF                                                            
         MVCL  RE,R0               SAVE CLIENT AND NON-CLIENT                   
         B     CR2                                                              
*                                                                               
CR21     LA    RE,ALLLINE                                                       
         LA    RF,PLBUKQ                                                        
         LA    R0,SRTHOURS                                                      
         LR    R1,RF                                                            
         MVCL  RE,R0               SAVE ALL CLIENT HOURS/COST                   
         B     CR2                                                              
*                                                                               
CR22     LA    RE,CLILINE                                                       
         LA    RF,PLBUKQ                                                        
         LA    R0,SRTHOURS                                                      
         LR    R1,RF                                                            
         MVCL  RE,R0               SAVE TOTAL CLIENT HOURS/COST                 
         B     CR2                                                              
*                                                                               
CR24     MVC   PRSSALP,SRTSALY     PICK UP SALARY                               
         BAS   RE,RULEDEV          FIGURE OUT HOW RULES APPLY                   
         LA    R2,PRSACC           ADD PERSONAL CALCULATIONS                    
         LA    R3,SUBACC           TO UPPER LEVELS                              
         LA    R4,5                                                             
*                                                                               
CR26     LR    RE,R2                                                            
         LR    RF,R3                                                            
         LA    R0,12                                                            
*                                                                               
CR28     AP    0(L'PKFLDS,RF),0(L'PKFLDS,RE)                                    
         LA    RE,L'PKFLDS(RE)                                                  
         LA    RF,L'PKFLDS(RF)                                                  
         BCT   R0,CR28                                                          
*                                                                               
         LA    R3,PKFLDLNQ(R3)                                                  
         BCT   R4,CR26                                                          
         BAS   RE,FEEPOST                                                       
         B     CR2                                                              
         EJECT                                                                  
**********************************************************************          
* FIGURE OUT VALUES FROM RULES - SALARY COSTS                        *          
**********************************************************************          
         SPACE 1                                                                
RULEDEV  NTR1                                                                   
         ZAP   PRSSALY,=P'0'                                                    
         BAS   RE,YTD                                                           
         LA    R2,EFFRULE                                                       
         USING RULED,R2                                                         
         LA    R3,PRSACC                                                        
         USING ACCUMD,R3                                                        
         CLC   RUTYPE,=C'ZERO'                                                  
         BE    XIT                                                              
         ZAP   YTDFEES,PRSYFEES                                                 
         CP    PRSMHOUR,=P'0'                                                   
         BE    RD6                                                              
         ZAP   PACK16,PRSSALP      MONTHLY SALARY                               
         MP    PACK16,=P'12'                                                    
         AP    PACK16,=P'50'       ROUND TO NEAREST DOLLAR                      
         SRP   PACK16,64-2,0                                                    
         SRP   PACK16,2,0                                                       
         ZAP   PRSSALY,PACK16                                                   
         LH    R4,RUHOURS                                                       
         CVD   R4,DUB                                                           
         DP    PACK16,DUB          DERIVE CLIENT HOURLY RATE                    
         ZAP   PACK8,PACK16(L'PACK16-L'DUB)   GET RID OF REM IF ANY             
         ZAP   PACK16,PACK8                                                     
         ZAP   HOURS,PRSMHOUR                                                   
         BAS   RE,HOURADJ          CHECK FOR MAXIMUMS ETC                       
         MP    PACK16,HOURS        NOW HAVE HOURS                               
         SRP   PACK16,64-2,0                                                    
         ZAP   SALARY,PACK16       SO GET SALARY COST                           
*                                                                               
         OC    RURATE,RURATE       RATE TAKES PREFERENCE OVER TYPE              
         BNZ   RD5                                                              
         CLC   RUTYPE(3),=C'100'   100% PEOPLE                                  
         BNE   RD2                                                              
         ZAP   PACK16,PRSSALY                                                   
         ZAP   PACK8,=P'12'        GET 1/12 ANNUAL SALARY                       
         DP    PACK16,PACK8                                                     
         ZAP   SALARY,PACK16(L'PACK16-L'PACK8)                                  
*                                                                               
RD2      CLC   RUTYPE(2),=C'66'    2/3 PEOPLE                                   
         BNE   RD2B                                                             
         ZAP   PACK16,PRSSALY                                                   
         ZAP   PACK8,=P'18'        GET 1/18 ANNUAL SALARY                       
         DP    PACK16,PACK8                                                     
         ZAP   SALARY,PACK16(L'PACK16-L'PACK8)                                  
         B     RD4                                                              
*                                                                               
RD2B     CLC   RUTYPE(2),=C'50'    1/2 PEOPLE                                   
         BNE   RD3                                                              
         ZAP   PACK16,PRSSALY                                                   
         ZAP   PACK8,=P'24'        GET 1/24 ANNUAL SALARY                       
         DP    PACK16,PACK8                                                     
         ZAP   SALARY,PACK16(L'PACK16-L'PACK8)                                  
         B     RD4                                                              
*                                                                               
RD3      CLC   RUTYPE(2),=C'33'    1/3 PEOPLE                                   
         BNE   RD3B                                                             
         ZAP   PACK16,PRSSALY                                                   
         ZAP   PACK8,=P'36'        GET 1/36 ANNUAL SALARY                       
         DP    PACK16,PACK8                                                     
         ZAP   SALARY,PACK16(L'PACK16-L'PACK8)                                  
         B     RD4                                                              
*                                                                               
RD3B     CLC   RUTYPE(2),=C'90'    90 PEOPLE                                    
         BNE   RD4                                                              
         ZAP   PACK16,PRSSALY                                                   
         MP    PACK16,=P'9'                                                     
         ZAP   PACK8,=P'120'       GET 9/120 ANNUAL SALARY                      
         DP    PACK16,PACK8                                                     
         ZAP   SALARY,PACK16(L'PACK16-L'PACK8)                                  
         B     RD4                                                              
*                                                                               
* APPLY OVERHEADS AND ADJUSTMENTS TO GET FEES                                   
*                                                                               
RD4      ZAP   PACK16,SALARY       APPLY OVERHEAD FACTOR A                      
         LH    R0,RUOVA                                                         
         CVD   R0,DUB                                                           
         MP    PACK16,DUB                                                       
         SRP   PACK16,64-3,0                                                    
         ZAP   OVA,PACK16                                                       
*                                                                               
         ZAP   PACK16,SALARY       APPLY OVERHEAD FACTOR A                      
         LH    R0,RUOVB            AND FACTOR B                                 
         CVD   R0,DUB                                                           
         MP    PACK16,DUB                                                       
         SRP   PACK16,64-3,0                                                    
         ZAP   OVB,PACK16                                                       
*                                                                               
         AP    PACK16,OVA                                                       
         LH    R0,RUOVC            APPLY FACTOR C TO A PLUS B                   
         CVD   R0,DUB                                                           
         MP    PACK16,DUB                                                       
         SRP   PACK16,64-3,0                                                    
         ZAP   OVC,PACK16                                                       
*                                                                               
RD5      OC    RURATE,RURATE       FIGURE OUT FLAT RATE                         
         BZ    RD6                                                              
         ZAP   PACK16,HOURS                                                     
         LH    R0,RURATE                                                        
         CVD   R0,DUB                                                           
         MP    PACK16,DUB                                                       
         DP    PACK16,=PL8'50'                                                  
         ZAP   FLAT,PACK16(8)                                                   
         AP    FLAT,=P'1'                                                       
         ZAP   PACK16,FLAT                                                      
         DP    PACK16,=PL8'2'                                                   
         ZAP   FLAT,PACK16(8)                                                   
*                                                                               
RD6      BAS   RE,EOYADJ           CHECK ANY END OF YEAR ADJUSTMENTS            
         ZAP   PACK16,=P'0'                                                     
         LA    RE,OVA                                                           
         LA    RF,8                                                             
*                                                                               
RD8      AP    PACK16,0(L'PLBUK,RE)   DEVELOP TOTAL FEES FOR MONTH              
         LA    RE,L'PLBUK(RE)                                                   
         BCT   RF,RD8                                                           
         ZAP   MONFEES,PACK16                                                   
         AP    PACK16,YTDFEES                                                   
         ZAP   YTDFEES,PACK16         UPDATE YTD FEES                           
         B     XIT                                                              
         EJECT                                                                  
*              ADJUST FOR MAXIMUM HOURS                                         
         SPACE 2                                                                
HOURADJ  NTR1                                                                   
         LA    R2,EFFRULE                                                       
         USING RULED,R2                                                         
         LH    R1,RUMAXMON                                                      
         LTR   R1,R1                                                            
         BZ    HADJ2                                                            
         MH    R1,=H'100'                                                       
         C     R1,HOURS            SEE IF MAX IN MONTH EXCEEDED                 
         BH    HADJ2                                                            
         ST    R1,HOURS            ALLOW ONLY MAX                               
         SPACE 1                                                                
HADJ2    LH    R1,RUMAXDAY                                                      
         LTR   R1,R1                                                            
         BZ    HADJ4                                                            
         M     R0,WORKDAYS                                                      
         C     R1,HOURS                                                         
         BH    HADJ4                                                            
         ST    R1,HOURS                                                         
         SPACE 1                                                                
HADJ4    CLI   RUPART,C' '         RULE TO BILL ONLY PART OF HOURS              
         BNH   XIT                                                              
         L     R1,HOURS                                                         
         LA    R0,2                                                             
         LA    RE,3                                                             
         CLC   RUPART,=C'33'       33=1/3                                       
         BE    HADJ6                                                            
         LA    R0,4                                                             
         CLC   RUPART,=C'66'       66=2/3                                       
         BE    HADJ6                                                            
         PACK  DUB,RUPART          OTHERWISE ITS NN PERCENT                     
         CVB   R0,DUB                                                           
         LA    RE,50                                                            
         SPACE 1                                                                
HADJ6    MR    R0,R0               FIGURE OUT HOURS (ROUNDED)                   
         DR    R0,RE                                                            
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         SR    R0,R0                                                            
         LA    R1,12(R1)           TO NEAREST QUARTER HOUR                      
         D     R0,=F'25'                                                        
         M     R0,=F'25'                                                        
         ST    R1,HOURS                                                         
         B     XIT                                                              
         EJECT                                                                  
*              FIGURE OUT WORKING DAYS IN MONTH                                 
         SPACE 1                                                                
WORKMON  NTR1                                                                   
         MVC   WORK,QSTART                                                      
         MVC   WORK+4(2),=C'01'                                                 
         SR    R2,R2                                                            
WM2      GOTO1 GETDAY,DMCB,WORK,DUB                                             
         CLI   DUB,C'S'                                                         
         BE    *+8                                                              
         LA    R2,1(R2)            ADD NON SAT/SUN                              
         SPACE 1                                                                
         GOTO1 ADDAY,DMCB,WORK,WORK+6,1                                         
         MVC   WORK(6),WORK+6                                                   
         CLC   WORK(4),QSTART                                                   
         BE    WM2                                                              
         PACK  DUB,QSTART+2(2)                                                  
         CVB   R1,DUB                                                           
         LA    R1,HOLTAB-1(R1)                                                  
         ZIC   R0,0(R1)                                                         
         SR    R2,R0               SUBTRACT HOLIDAYS                            
         ST    R2,WORKDAYS                                                      
         B     XIT                                                              
         SPACE 1                                                                
HOLTAB   DC    AL1(0,1,0,0,1,0,1,0,1,0,1,1)                                     
         EJECT                                                                  
**********************************************************************          
* END OF YEAR ADJUSTMENTS - 100 PCT                                  *          
**********************************************************************          
         SPACE 1                                                                
EOYADJ   NTR1                                                                   
         LA    R2,EFFRULE                                                       
         USING RULED,R2                                                         
         CLC   RUADJ(2),=C'YE'                                                  
         BNE   XIT                                                              
         UNPK  WORK(5),MNTHLIST(3) ARE WE HANDLING THE LAST MONTH               
         PACK  DUB,WORK+2(2)       OF CLIENT FISCAL YEAR                        
         CVB   R1,DUB                                                           
         STC   R1,DUB                                                           
         CLC   RUMONTH,DUB         (SEE IF FIRST MONTH = FISCAL START)          
         BNE   XIT                                                              
         CLC   RUTYPE(3),=C'100'   FOR 100% PEOPLE                              
         BNE   EOY2                                                             
         CP    CLIYHOUR,ALLYHOUR   DID THEY WORK ON OTHER CLIENTS               
         BE    XIT                                                              
         ZAP   PACK8,OVA           YES SO TOTAL FEES FOR YEAR                   
         AP    PACK8,YTDFEES                                                    
         ZAP   PACK16,CLIYHOUR                                                  
         SP    PACK16,ALLYHOUR                                                  
         MP    PACK16,PACK8        ARE ADJUSTED BY PROPORTION OF TIME           
         DP    PACK16,ALLYHOUR     SPENT ON OTHER CLIENTS                       
         ZAP   ADJUST1,PACK16(L'PACK16-L'ALLYHOUR)                              
*                                                                               
         ZAP   PACK8,CLIYHOUR     REFLECT THIS IN COMMENTS                      
         SP    PACK8,ALLYHOUR                                                   
         NI    PACK8+15,X'FC'     GET ABSOLUTE VALUE                            
         SRP   PACK8,64-2,0                                                     
         EDIT  PACK8,(4,ADJSTUB),ALIGN=LEFT                                     
         MVC   ADJSTUB+4(21),=CL21'HOURS ON OTHER CLIENTS'                      
         GOTO1 ADSQUASH,DMCB,ADJSTUB,25                                         
         B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* END OF YEAR ADJUSTMENTS - NON 100 PCT                              *          
**********************************************************************          
         SPACE 1                                                                
EOY2     LH    R1,RUMONTH                                                       
         CVD   R1,DUB                                                           
         MP    DUB,=P'100'        MULTIPLY BY 100                               
         CP    DUB,ALLYHOUR                                                     
         BNL   XIT                                                              
         SP    DUB,ALLYHOUR                                                     
         ZAP   PACK8,DUB                                                        
         NI    PACK8+7,X'FC'       GET ABSOLUTE VALUE                           
         ZAP   PACK16,OVA                                                       
         AP    PACK16,YTDFEES                                                   
         MP    PACK16,DUB                                                       
         DP    PACK16,ALLYHOUR                                                  
         ZAP   ADJUST1,PACK16(L'PACK16-L'ALLYHOUR)                              
*                                                                               
         SRP   PACK8,64-2,0                                                     
         MVC   ADJSTUB(25),=C'LESS 1234 HOURS OVER MAX '                        
         LA    R4,ADJSTUB+5                                                     
         EDIT  PACK8,(4,(R4)),ALIGN=LEFT                                        
         GOTO1 ADSQUASH,DMCB,ADJSTUB,25                                         
         B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* FIGURE OUT YTD VALUES FROM INDIVIDUAL MONTHS                       *          
**********************************************************************          
         SPACE 1                                                                
YTD      NTR1                                                                   
         LA    R2,EFFRULE                                                       
         USING RULED,R2                                                         
         ZIC   R3,RUMONTH          START OF FISCAL YEAR                         
         PACK  DUB,QSTART+2(2)                                                  
         CVB   R4,DUB              PRESENT MONTH                                
         CR    R3,R4               IF FISCAL GREATER THAN PRESENT               
         BNH   *+8                 ADJUST PRESENT                               
         LA    R4,12(R4)                                                        
         LA    R6,1(R4)            NUMBER OF MONTHS IS PRESENT+1                
         SR    R6,R3                                   -FISCAL                  
         LA    R5,12               FIGURE OUT DISPLACEMENT                      
         SR    R5,R6               INTO MONTHLY ACCUMS FOR                      
         SLL   R5,3                START OF FISCAL YEAR                         
         LA    R2,SRTHOURS                                                      
         LA    R3,PRSMHOUR                                                      
         BAS   RE,YTD2                                                          
         LA    R2,CLILINE                                                       
         LA    R3,CLIMHOUR                                                      
         BAS   RE,YTD2                                                          
         LA    R2,ALLLINE                                                       
         LA    R3,ALLMHOUR                                                      
         BAS   RE,YTD2                                                          
         LA    R2,TOTLINE                                                       
         LA    R3,TOTMHOUR                                                      
         BAS   RE,YTD2                                                          
         B     XIT                                                              
*                                                                               
YTD2     NTR1                                                                   
         LA    R0,3                DO THIS FOR HOURS/COST/FEES                  
*                                                                               
YTD4     ZAP   0(L'PLBUK,R3),88(L'PLBUK,R2)     MOVE OUT MONTHLY                
         LA    RE,0(R2,R5)                      DISPLACE INTO MONTHS            
         LR    RF,R6                                                            
         ZAP   PACK16,=P'0'                                                     
*                                                                               
YTD6     AP    PACK16,0(L'PLBUK,RE)     ADD UP YTD                              
         LA    RE,L'PLBUK(RE)                                                   
         BCT   RF,YTD6                                                          
         ZAP   8(L'PLBUK,R3),PACK16     AND STORE                               
         LA    R2,96(R2)                                                        
         LA    R3,16(R3)                                                        
         BCT   R0,YTD4                                                          
         SH    R3,=H'16'              THIS MONTH'S FEES SHOULDNT BE             
         ZAP   PACK8,8(L'PLBUK,R3)    INCLUDED IN THE YTD FEES                  
         SP    PACK8,0(L'PLBUK,R3)    SO WE'LL ADJUST YTD                       
         ZAP   8(L'PLBUK,R3),PACK8                                              
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*              CONTROL TOTALLING ROUTINES                                       
*                                                                               
PRSTOT   NTR1                                                                   
         LA    R0,12                                                            
         LA    R1,PRSACC                                                        
         CP    0(L'PKFLDS,R1),=P'0'      CHECK FOR ACTIVITY                     
         BNE   PRST10                                                           
         LA    R1,L'PKFLDS(R1)                                                  
         BCT   R0,*-14                                                          
         B     XIT                                                              
*                                                                               
PRST10   BAS   RE,ANYMID                                                        
         MVC   P+1(8),PRSCODE                                                   
         MVC   P+10(36),PRSNAME                                                 
         GOTO1 ADSQUASH,DMCB,P+1,46                                             
         ZAP   PACK8,PRSSALY                                                    
         AP    PACK8,=P'50'                                                     
         SRP   PACK8,64-2,0                                                     
         EDIT  PACK8,(7,P+35)                                                   
         MVI   LEVEL,1                                                          
         B     AT2                                                              
*                                                                               
SUBTOT   NTR1                                                                   
         LA    R0,12                                                            
         LA    R1,SUBACC                                                        
         CP    0(L'PKFLDS,R1),=P'0'      CHECK FOR ACTIVITY                     
         BNE   *+16                                                             
         LA    R1,L'PKFLDS(R1)                                                  
         BCT   R0,*-14                                                          
         BZ    XIT                                                              
*                                                                               
         BAS   RE,SPLAT                                                         
         MVI   LEVEL,2                                                          
         B     ALLTOT                                                           
*                                                                               
DPTTOT   NTR1                                                                   
         MVI   LEVEL,3                                                          
         B     ALLTOT                                                           
*                                                                               
DIVTOT   NTR1                                                                   
         MVI   LEVEL,4                                                          
         B     ALLTOT                                                           
*                                                                               
CLITOT   NTR1                                                                   
         MVI   LEVEL,5                                                          
         B     ALLTOT                                                           
*                                                                               
AGYTOT   NTR1                                                                   
         MVI   FORCEHED,C'Y'                                                    
         MVI   LEVEL,6                                                          
*                                                                               
ALLTOT   MVC   P+10(10),=C'TOTALS FOR'                                          
         ZIC   R1,LEVEL                                                         
         SH    R1,=H'2'                                                         
         MH    R1,=H'14'                                                        
         LA    R1,ALLTAB(R1)                                                    
         MVC   P+21(14),0(R1)                                                   
         B     AT2                                                              
*                                                                               
ALLTAB   DC    CL14'SUB-DEPARTMENT'                                             
         DC    CL14'DEPARTMENT'                                                 
         DC    CL14'DIVISION'                                                   
         DC    CL14'CLIENT'                                                     
         DC    CL14'ALL CLIENTS'                                                
         EJECT                                                                  
**********************************************************************          
* NOW HANDLE THE NUMBERS - HOURS, SALARY AND FEES YTD                *          
**********************************************************************          
         SPACE 1                                                                
AT2      SR    R2,R2                                                            
         IC    R2,LEVEL            POSITION TO ACCUMULATORS FOR LEVEL           
         BCTR  R2,0                                                             
         MHI   R2,PKFLDLNQ                                                      
         LA    R2,PRSACC(R2)                                                    
*                                                                               
         LA    R0,12                                                            
         LR    R1,R2                                                            
         CP    0(L'PKFLDS,R1),=P'0'      CHECK FOR ACTIVITY                     
         BNE   AT4                                                              
         LA    R1,L'PKFLDS(R1)                                                  
         BCT   R0,*-14                                                          
*                                                                               
         MVC   P,SPACES                                                         
         B     XIT                                                              
         DROP  R3                                                               
*                                                                               
         USING ACCUMD,R2                                                        
AT4      EDIT  HOURS,(7,P+43),2,ZERO=BLANK                                      
*                                                                               
AT6      EDIT  SALARY,(11,P+50),2,ZERO=BLANK       SALARY COST                  
         CLI   P+50,C' '                                                        
         BE    AT8                                                              
         EDIT  SALARY,(13,P+50)                                                 
         MVC   P+61(2),SPACES                                                   
*                                                                               
AT8      DS    0H                                                               
         EDIT  YTDFEES,(12,P+98),2,MINUS=YES       YTD FEES                     
         CLI   P+98,C' '                                                        
         BE    AT10                                                             
         EDIT  YTDFEES,(15,P+98),2,MINUS=YES                                    
         MVC   P+109(4),P+112                                                   
         EJECT                                                                  
**********************************************************************          
* BUILD UP TOWARDS FEES                                              *          
**********************************************************************          
         SPACE 1                                                                
AT10     LA    R1,OVA                                                           
         LA    R4,8                                                             
         SR    R5,R5                                                            
*                                                                               
AT12     CP    0(L'PLBUK,R1),=P'0'     COUNT ACTIVE LINES IN R5                 
         BE    *+8                                                              
         LA    R5,1(R5)                                                         
         LA    R1,L'PLBUK(R1)                                                   
         BCT   R4,AT12                                                          
         MVI   TOTSW,C'N'                                                       
         CH    R5,=H'1'                                                         
         BE    AT16                                                             
         BH    AT14                                                             
         BAS   RE,SPLAT            NONE - WE'RE DONE                            
         B     AT28                                                             
*                                                                               
AT14     MVI   TOTSW,C'Y'          MORE THAN 1 SO WE'LL NEED TOTALS             
         LA    R5,1(R5)            LATER WHICH TAKES 1 MORE LINE                
         ZIC   R1,LINE                                                          
         LA    R5,1(R1,R5)                                                      
         ZIC   R1,MAXLINES                                                      
         CR    R5,R1               SEE IF CHUNK WILL FIT                        
         BNH   AT16                                                             
         MVI   FORCEHED,C'Y'                                                    
         SPACE 1                                                                
AT16     CLI   LEVEL,1             SEED THE TABLE                               
         BE    AT18                                                             
         MVC   OVASTUB+18(5),=C'TOTAL'                                          
         MVC   OVBSTUB+18(5),=C'TOTAL'                                          
         MVC   OVCSTUB+18(5),=C'TOTAL'                                          
         MVC   OVDSTUB+18(5),=C'TOTAL'                                          
         MVC   FLTSTUB+10(5),=C'TOTAL'                                          
         MVC   FLTSTUB+15(10),SPACES                                            
         B     AT20                                                             
         SPACE 1                                                                
AT18     LA    R2,EFFRULE          (BORROW R2)                                  
         USING RULED,R2                                                         
         EDIT  (2,RUOVA),(5,STUB+18),3                                          
         LA    R1,STUB+18                                                       
         BAS   RE,TRIM                                                          
         EDIT  (2,RUOVB),(5,STUB+43),3                                          
         LA    R1,STUB+43                                                       
         BAS   RE,TRIM                                                          
         EDIT  (2,RUOVC),(5,STUB+68),3                                          
         LA    R1,STUB+68                                                       
         BAS   RE,TRIM                                                          
         MVC   FLTSTUB+10(14),SPACES                                            
         MVI   FLTSTUB+10,C'$'                                                  
         LH    R1,RURATE                                                        
         EDIT  (R1),(9,DMCB),2,ALIGN=LEFT                                       
         MVC   FLTSTUB+11(9),DMCB                                               
         LA    R2,PRSACC                                                        
         USING ACCUMD,R2                                                        
         B     AT20                                                             
*                                                                               
TRIM     CLI   4(R1),C'0'          REMOVE TRAILING ZEROS                        
         BNER  RE                                                               
         MVI   4(R1),C' '                                                       
         CLI   3(R1),C'0'                                                       
         BNER  RE                                                               
         MVI   3(R1),C' '                                                       
         BR    RE                                                               
         EJECT                                                                  
*              NOW PRINT OUT THE FEES                                           
         SPACE 3                                                                
AT20     LA    R3,OVA                                                           
         LA    R4,STUB                                                          
         LA    R5,8                                                             
*                                                                               
AT22     CP    0(L'PLBUK,R3),=P'0'       PRINT ACTIVE LINES                     
         BE    AT24                                                             
         MVC   P+62(25),0(R4)                                                   
         EDIT  (P8,(R3)),(12,P+86),2,MINUS=YES                                  
         BAS   RE,SPLAT                                                         
*                                                                               
AT24     LA    R3,L'PLBUK(R3)                                                   
         LA    R4,25(R4)                                                        
         BCT   R5,AT22                                                          
         CLI   TOTSW,C'Y'                                                       
         BNE   AT26                                                             
         MVC   P+80(5),=C'TOTAL'                                                
         EDIT  (P8,(R3)),(12,P+86),2,MINUS=YES                                  
         BAS   RE,SPLAT                                                         
*                                                                               
AT26     CLI   LEVEL,1             EXTRA LINE FOR HIGH LEVELS                   
         BE    AT28                                                             
         BAS   RE,SPLAT                                                         
*                                                                               
AT28     LA    R0,12               CLEAR LINE OF ACCUMS                         
         LR    R1,R2                                                            
         ZAP   0(L'PKFLDS,R1),=P'0'                                             
         LA    R1,L'PKFLDS(R1)                                                  
         BCT   R0,*-10                                                          
         B     XIT                                                              
*                                                                               
STUB     DS    0D                                                               
OVASTUB  DC    C'OVERHEAD FACTOR A        '                                     
OVBSTUB  DC    C'    PLUS FACTOR B        '                                     
OVCSTUB  DC    C'    PLUS FACTOR C        '                                     
OVDSTUB  DC    C'    PLUS FACTOR D        '                                     
FLTSTUB  DC    C'FLAT RATE $85   PER HOUR '                                     
         DC    C'SPARE                    '                                     
ADJSTUB  DC    C'1234 HOURS OTHER CLIENTS '                                     
         DC    C'MISCELLANEOUS ADJUSTMENT '                                     
         EJECT                                                                  
*              HANDLE MID-LINES PENDING                                         
         SPACE 3                                                                
ANYMID   NTR1                                                                   
         MVC   WORK,DPTMID                                                      
         CLI   DPTPEND,C'Y'                                                     
         BNE   *+8                                                              
         BAS   RE,DOMID                                                         
         MVI   DPTPEND,C'N'                                                     
         MVC   WORK,SUBMID                                                      
         CLI   SUBPEND,C'Y'                                                     
         BNE   *+8                                                              
         BAS   RE,DOMID                                                         
         MVI   SUBPEND,C'N'                                                     
         B     XIT                                                              
         SPACE 2                                                                
DOMID    NTR1                                                                   
         MVC   P+1(6),WORK                                                      
         MVC   P+8(36),WORK+7                                                   
         GOTO1 ADSQUASH,DMCB,P+1,43                                             
         GOTO1 UNDERLIN,DMCB,(43,P+1),PSECOND+1                                 
         MVI   SPACING,2                                                        
         BAS   RE,SPLAT                                                         
         B     XIT                                                              
         EJECT                                                                  
*              ESTABLISH EFFECTIVE RULE FOR PRESENT RECORD                      
         SPACE 3                                                                
COMPRULE NTR1                                                                   
         XC    EFFRULE,EFFRULE     SET DEFAULTS                                 
         LA    R2,EFFRULE                                                       
         USING RULED,R2                                                         
         MVC   RUTYPE,=C'NON '                                                  
         MVC   RUHOURS,=H'1540'    HOURS OF 1540                                
         MVI   RUMONTH,1           FISCAL JAN                                   
*                                                                               
         L     R2,ARULES                                                        
         L     R3,4(R2)                                                         
         LA    R2,16(R2)                                                        
         LTR   R3,R3                                                            
         BZ    XIT                                                              
*                                                                               
COMP2    CLI   RUDIV,C'*'          SEE IF RULE APPLIES                          
         BE    *+14                                                             
         CLC   RUDIV,SRTDIV        FOR DIVISION                                 
         BNE   COMP4                                                            
*                                                                               
         CLI   RUDPT,C'*'          DEPARTMENT                                   
         BE    *+14                                                             
         CLC   RUDPT,SRTDPT                                                     
         BNE   COMP4                                                            
*                                                                               
         CLI   RUSUB,C'*'          SUB-DEPARTMENT                               
         BE    *+14                                                             
         CLC   RUSUB,SRTSUB                                                     
         BNE   COMP4                                                            
*                                                                               
         CLI   RUPRS,C'*'          PERSON                                       
         BE    *+14                                                             
         CLC   RUPRS,SRTPRS                                                     
         BNE   COMP4                                                            
*                                                                               
         BAS   RE,APLRULE          THEN WE CAN APPLY THIS RULE                  
*                                                                               
COMP4    A     R2,LRULE                                                         
         BCT   R3,COMP2                                                         
         B     XIT                                                              
         EJECT                                                                  
*              APPLY RULE TO EFFRULE                                            
         SPACE 3                                                                
APLRULE  NTR1                                                                   
         LA    R4,APLTAB           ADDRESS DISPLACEMENT TABLE                   
*                                                                               
APL2     CLI   0(R4),X'FF'                                                      
         BE    XIT                                                              
         ZIC   R1,0(R4)            DISPLACEMENT                                 
         LA    RE,0(R1,R2)         INTO POOL ENTRY                              
         LA    RF,EFFRULE(R1)      AND INTO EFFECTIVE RULE                      
         ZIC   R1,1(R4)            LENGTH                                       
         BCTR  R1,R0               -1                                           
         EX    R1,*+8              ANY VALUES THERE                             
         B     *+10                                                             
         OC    0(0,RE),0(RE)                                                    
         BZ    APL4                                                             
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),SPACES                                                   
         BE    APL4                                                             
*                                                                               
         EX    R1,*+4              YES - SO MOVE IN OVERRIDE VALUE              
         MVC   0(0,RF),0(RE)                                                    
         CLI   0(RE),X'80'         SPECIAL FOR OVERRIDE OF ZERO                 
         BNE   *+8                                                              
         MVI   0(RF),0                                                          
*                                                                               
APL4     LA    R4,2(R4)                                                         
         B     APL2                                                             
*                                                                               
APLTAB   DC    AL1(RUTYPE-RULED,4)   TYPE                                       
         DC    AL1(RUADJ-RULED,4)    ADJUSTMENT                                 
         DC    AL1(RUHOURS-RULED,2)  HOURS                                      
         DC    AL1(RURATE-RULED,2)   RATE                                       
         DC    AL1(RUOVA-RULED,2)    OVA                                        
         DC    AL1(RUOVB-RULED,2)    OVB                                        
         DC    AL1(RUOVC-RULED,2)    OVC                                        
         DC    AL1(RUMAXMON-RULED,2) MAXMON                                     
         DC    AL1(RUMAXDAY-RULED,2) MAXDAY                                     
         DC    AL1(RUMONTH-RULED,1)  FISCAL                                     
         DC    AL1(RUPART-RULED,2)   PART                                       
         DC    X'FF'                                                            
         EJECT                                                                  
*              DATA MANAGER SUPPORT                                             
         SPACE 3                                                                
READ     NTR1                                                                   
         MVC   MYCOM,=CL8'DMREAD'                                               
         B     DMALL                                                            
         SPACE 1                                                                
HIGH     NTR1                                                                   
         MVC   MYCOM,=CL8'DMRDHI'                                               
         MVC   KEYSAVE,KEY                                                      
         B     DMALL                                                            
         SPACE 1                                                                
SEQ      NTR1                                                                   
         MVC   MYCOM,=CL8'DMRSEQ'                                               
         B     DMALL                                                            
         SPACE 1                                                                
SKIP     NTR1                                                                   
         MVC   MYCOM,=CL8'DMRDHI'                                               
         L     R6,AMYIO                                                         
         B     DMALL2                                                           
         SPACE 1                                                                
DMALL    L     R6,AMYIO                                                         
         MVC   0(42,R6),SPACES                                                  
         MVC   0(32,R6),KEY                                                     
         SPACE 1                                                                
DMALL2   GOTO1 DATAMGR,DMCB,MYCOM,=C'ACCOUNT',(R6),(R6),0                       
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   KEY,0(R6)                                                        
         SPACE 1                                                                
XIT6     XIT1  REGS=(R6)                                                        
         SPACE 1                                                                
XIT      XIT1                                                                   
         SPACE 1                                                                
         GETEL (R6),DATADISP,ELCODE                                             
         EJECT                                                                  
*              ROUTINES TO SPLIT DIVISION AND PERSON CODES                      
         SPACE 3                                                                
SPLITCLI NTR1                                                                   
         MVC   CLICODE,SPACES      R1 A(CL12 CLI/DIV)                           
         MVC   DIVCODE,SPACES                                                   
         ZIC   R2,LCLICODE                                                      
         BCTR  R2,0                                                             
         EX    R2,SPLIT2                                                        
         LA    R1,1(R1,R2)                                                      
         ZIC   R2,LDIVCODE                                                      
         BCTR  R2,0                                                             
         EX    R2,SPLIT4                                                        
         B     XIT                                                              
         SPACE 1                                                                
SPLITDPT NTR1                                                                   
         LR    R3,R1                                                            
         MVC   OFFCOD2,SPACES                                                   
         MVC   DPTCOD2,SPACES                                                   
         MVC   DPTCODE,SPACES      R1 A(CL12 DPT/SUB/PRS)                       
         MVC   SUBCODE,SPACES                                                   
         MVC   PRSCODE,SPACES                                                   
         ZIC   R2,LDPTCODE                                                      
         BCTR  R2,0                                                             
         EX    R2,SPLIT6                                                        
         LA    R1,1(R1,R2)                                                      
         ZIC   R2,LSUBCODE                                                      
         BCTR  R2,0                                                             
         EX    R2,SPLIT8                                                        
         LA    R1,1(R1,R2)                                                      
         ZIC   R2,LPRSCODE                                                      
         BCTR  R2,0                                                             
         EX    R2,SPLIT10                                                       
         ZIC   R2,LOFFCODE         SPLIT OFFICE AND DEPT FOR SALHST             
         BCTR  R2,0                                                             
         EX    R2,SPLIT12                                                       
         LA    R3,1(R3,R2)                                                      
         ZIC   R2,LDPTCOD2                                                      
         BCTR  R2,0                                                             
         EX    R2,SPLIT14                                                       
         B     XIT                                                              
         SPACE 1                                                                
SPLIT2   MVC   CLICODE(0),0(R1)                                                 
SPLIT4   MVC   DIVCODE(0),0(R1)                                                 
SPLIT6   MVC   DPTCODE(0),0(R1)                                                 
SPLIT8   MVC   SUBCODE(0),0(R1)                                                 
SPLIT10  MVC   PRSCODE(0),0(R1)                                                 
SPLIT12  MVC   OFFCOD2(0),0(R3)                                                 
SPLIT14  MVC   DPTCOD2(0),0(R3)                                                 
         EJECT                                                                  
*              REFRESH NAMES                                                    
         SPACE 3                                                                
REFRESH  NTR1                                                                   
         LA    R4,KEY                                                           
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),QCOMPANY                                                  
         MVC   KEY+1(2),=C'1F'                                                  
         MVC   KEY+3(6),CLICODE    CLIENT                                       
         CLC   CLICODE,LASTCLI                                                  
         BE    REFRESH2                                                         
         BAS   RE,READ                                                          
         BAS   RE,GETNAME                                                       
         MVC   CLINAME,WORK                                                     
         MVC   LASTCLI,CLICODE                                                  
         XC    LASTDIV,LASTDIV                                                  
         SPACE 1                                                                
REFRESH2 ZIC   R1,LCLICODE         DIVISION                                     
         LA    R1,KEY+3(R1)                                                     
         MVC   0(6,R1),DIVCODE                                                  
         CLC   DIVCODE,LASTDIV                                                  
         BE    REFRESH4                                                         
         MVC   DIVNAME,SPACES                                                   
         BAS   RE,HIGH                                                          
         CLC   KEY(15),KEYSAVE                                                  
         BNE   REFRESH4                                                         
         BAS   RE,GETNAME                                                       
         MVC   DIVNAME,WORK                                                     
         MVC   LASTDIV,DIVCODE                                                  
*                                                                               
REFRESH4 MVC   KEY,SPACES                                                       
         MVC   KEY(1),QCOMPANY                                                  
         MVC   KEY+1(2),=C'1R'                                                  
         MVC   KEY+3(6),DPTCODE    DEPARTMENT                                   
         CLC   DPTCODE,LASTDPT                                                  
         BE    REFRESH6                                                         
         BAS   RE,READ                                                          
         BAS   RE,GETNAME                                                       
         MVC   DPTNAME,WORK                                                     
         MVC   LASTDPT,DPTCODE                                                  
         XC    LASTSUB,LASTSUB                                                  
*                                                                               
REFRESH6 ZIC   R1,LDPTCODE         SUB-DEPT                                     
         LA    R1,KEY+3(R1)                                                     
         MVC   0(6,R1),SUBCODE                                                  
         CLC   SUBCODE,LASTSUB                                                  
         BE    REFRESH8                                                         
         BAS   RE,READ                                                          
         BAS   RE,GETNAME                                                       
         MVC   SUBNAME,WORK                                                     
         MVC   LASTSUB,SUBCODE                                                  
         XC    LASTPRS,LASTPRS                                                  
*                                                                               
REFRESH8 ZIC   R0,LSUBCODE         PERSON                                       
         AR    R1,R0                                                            
         MVC   0(8,R1),PRSCODE                                                  
         CLC   PRSCODE,LASTPRS                                                  
         BE    XIT                                                              
         BAS   RE,READ                                                          
         BAS   RE,GETNAME                                                       
         MVC   PRSNAME,WORK                                                     
         MVC   LASTPRS,PRSCODE                                                  
         B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* ROUTINES TO DIG OUT NAMES FROM RECORDS                             *          
**********************************************************************          
         SPACE 1                                                                
GETNAME  NTR1                                                                   
         MVC   WORK,SPACES                                                      
         L     R6,AMYIO                                                         
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   GETNAME2                                                         
         USING ACNAMED,R6                                                       
         ZIC   R1,ACNMLEN                                                       
         SH    R1,=H'3'                                                         
         EX    R1,*+4                                                           
         MVC   WORK(0),ACNMNAME                                                 
         B     XIT                                                              
*                                                                               
GETNAME2 L     R6,AMYIO                                                         
         MVI   ELCODE,X'43'                                                     
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING TRSUBHD,R6                                                       
         ZIC   R1,TRSBLEN                                                       
         SH    R1,=H'18'                                                        
         EX    R1,*+4                                                           
         MVC   WORK(0),TRSBNAME                                                 
         B     XIT                                                              
*                                                                               
SPLAT    NTR1                                                                   
         CLI   PRINTOPT,C'Y'       OPTIONAL PRINTING                            
         BNE   XIT                                                              
         STM   RE,RC,BASEREGS                                                   
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         EJECT                                                                  
*              POST TO WORKER FILE                                              
         SPACE 3                                                                
FEEPOST  NTR1                                                                   
         XC    AREA-4(254),AREA-4                                               
         XC    AREA+250(250),AREA+250                                           
*                                                                               
         MVC   WORK(15),=CL15'C1R'                                              
         MVC   WORK(1),QCOMPANY                                                 
         MVC   WORK+3(6),SRTDPT    POST TO DPT/SUB/PERSON                       
         LA    R2,WORK+3                                                        
         ZIC   R1,LDPTCODE                                                      
         AR    R2,R1                                                            
         MVC   0(6,R2),SRTSUB                                                   
         ZIC   R1,LSUBCODE                                                      
         AR    R2,R1                                                            
         MVC   0(6,R2),SRTPRS                                                   
         MVC   ACCOUNT,WORK                                                     
         SPACE 1                                                                
         MVC   WORK+1(14),=CL14'1C'                                             
         SR    R1,R1                                                            
         IC    R1,LOFFCODE                                                      
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   WORK+3(0),SRTOFF    OFFICE                                       
         LA    R2,WORK+3           CLI/DIV                                      
         SR    R1,R1                                                            
         IC    R1,LOFFCODE                                                      
         AR    R2,R1               POINT R2 PAST OFFICE                         
         MVC   0(6,R2),SRTCLI                                                   
         ZIC   R1,LCLICODE                                                      
         AR    R2,R1                                                            
         MVC   0(6,R2),SRTDIV                                                   
         MVC   SUBAC,WORK                                                       
         MVC   CONNAME,SPACES                                                   
         BAS   RE,COSTIT           GET COST NAME                                
         SPACE 1                                                                
         MVI   DRORCR,C'D'                                                      
         MVC   NARR,SPACES                                                      
         MVI   NARRLEN,1                                                        
         LA    R2,MEMO                                                          
         USING TRCASHD,R2                                                       
         MVC   TRCSEL(2),=X'5009'  SUBSIDIARY CASH ELEMENT                      
         MVI   TRCSTYPE,C'F'                                                    
         LA    R3,PRSACC                                                        
         USING ACCUMD,R3                                                        
         ZAP   PACK8,MONFEES                                                    
         SP    PACK8,PRSMFEES                                                   
         BZ    XIT                                                              
         ZAP   TRCSAMNT,PACK8                                                   
         BAS   RE,POSTEM                                                        
         B     XIT                                                              
         EJECT                                                                  
*              LOOKUP 1C COSTING NAME                                           
*              NOTE ASSUME WORK HAS 1C CODE                                     
         SPACE 3                                                                
COSTIT   NTR1                                                                   
         MVC   CONNAME,SPACES                                                   
         L     R3,ACOSTCLI                                                      
         USING COSTCD,R3                                                        
         LA    R1,COSTCLIQ-1       LEAVE ROOM FOR END OF TAB MARK               
COST02   CLI   COSTCODE,X'FF'                                                   
         BE    COST04                                                           
         CLC   COSTCODE,WORK+3                                                  
         BE    COST06                                                           
         LA    R3,COSTLEN(R3)                                                   
         BCT   R1,COST02                                                        
         DC    H'0'                TABLE FULL                                   
COST04   MVC   KEY,SPACES          ADD NEW TABLE ENTRY                          
         MVC   KEY(15),WORK                                                     
         MVC   COSTCODE,WORK+3                                                  
         BAS   RE,READ                                                          
         BAS   RE,GETNAME                                                       
         MVC   COSTNAME,WORK                                                    
         LA    R1,COSTLEN(R3)      USE R1 TO MARK NEW END OF TAB                
         MVI   0(R1),X'FF'                                                      
COST06   MVC   CONNAME,COSTNAME    PASS BACK NAME                               
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              POSTING ROUTINES - SUBSIDIARY                                    
         SPACE 3                                                                
IDBILD   NTR1                                                                   
         XC    ID,ID               BUILD WORKER KEY                             
         MVC   ID(2),ORIGINUM                                                   
         MVC   ID+2(3),=C'AFA'                                                  
         MVI   ID+5,C'R'                                                        
         PACK  DUB(2),RCDATE+3(3)  DAY (PWOS)                                   
         MVC   ID+6(1),DUB                                                      
         MVI   ID+7,C'P'                                                        
         B     XIT                                                              
         SPACE 1                                                                
MOS      NTR1                                                                   
         MVC   MNTH(1),QSTART+1    BUILD MOS IN MNTH                            
         MVC   MNTH+1(1),QSTART+3                                               
         CLI   QSTART+2,C'1'                                                    
         BNE   XIT                                                              
         ZIC   R1,MNTH+1           CONVERT 0-2 TO A-C                           
         SLL   R1,28                                                            
         SRL   R1,28                                                            
         LA    R1,MOSLIST(R1)                                                   
         MVC   MNTH+1(1),0(R1)                                                  
         B     XIT                                                              
         SPACE 1                                                                
MOSLIST  DC    C'ABC'                                                           
         SPACE 1                                                                
EOFPOST  NTR1                                                                   
         LA    R2,AREA             TRAILER                                      
         USING PSSUBFD,R2                                                       
         MVC   PSSBEL(2),=X'521D'                                               
         MVC   PSSBDESC,=CL15'FEES-'                                            
         L     R4,ADCMPEL                                                       
         USING ACCOMPD,R4                                                       
         MVC   PSSBDESC+7(7),ACMPABBR                                           
         ZAP   PSSBRECS,TOTRECS                                                 
         ZAP   PSSBCASH,TOTCASH                                                 
         MVI   PSSBCASH+6,0                                                     
         BAS   RE,ADDWRK                                                        
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO COMPLETE POSTINGS                                     
         SPACE 3                                                                
POSTEM   NTR1                                                                   
         LA    R2,AREA                                                          
         USING PSHEADD,R2                                                       
         MVI   PSHDEL,X'50'        HEAD ELEMENT                                 
         MVI   PSHDLEN,70                                                       
         MVC   PSHDACC,ACC                                                      
         MVC   PSHDANAL,SPACES                                                  
         CLC   SUBAC(2),=C'99'                                                  
         BNE   *+10                                                             
         MVC   PSHDANAL,=C'99'                                                  
         CLC   SUBWORK,SPACES                                                   
         BE    *+10                                                             
         MVC   PSHDANAL,SUBWORK                                                 
         MVC   PSHDSBAC,SUBAC                                                   
         MVC   PSHDSBNM,CONNAME                                                 
         SPACE 1                                                                
         LA    R2,70(R2)                                                        
         USING TRANSD,R2                                                        
         MVI   TRNSEL,X'44'                                                     
         ZIC   R1,NARRLEN                                                       
         LA    R1,28(R1)                                                        
         STC   R1,TRNSLEN                                                       
         GOTO1 DATCON,DMCB,(4,RCDATE),(1,TRNSDATE)                              
         ZAP   DUB,=P'1'                                                        
         UNPK  TRNSREF,DUB                                                      
         OI    TRNSREF+5,X'F0'                                                  
         MVI   TRNSSBRF,0                                                       
         MVI   TRNSTYPE,3                                                       
         MVI   TRNSSTAT,0                                                       
         CLI   DRORCR,C'C'                                                      
         BE    *+8                                                              
         MVI   TRNSSTAT,X'80'                                                   
         MVC   TRNSBTCH,SPACES                                                  
         MVC   TRNSBTCH(2),MNTH                                                 
         ZAP   TRNSAMNT,=P'0'                                                   
         MVC   TRNSANAL,SPACES                                                  
         MVC   TRNSANAL,SRTOFF     OFFICE                                       
         ZIC   R1,NARRLEN                                                       
         EX    R1,*+4                                                           
         MVC   TRNSNARR,NARR       MOVE IN VARIABLE NARRATIVE                   
         CLI   DRORCR,C'D'                                                      
         BNE   *+10                                                             
         AP    TOTCASH,TRNSAMNT                                                 
         AP    TOTRECS,=P'1'                                                    
         ZIC   R1,1(R2)                                                         
         AR    R2,R1                                                            
         MVI   0(R2),0                                                          
         CLI   MEMO+1,0                                                         
         BE    *+10                                                             
         MVC   0(200,R2),MEMO                                                   
         BAS   RE,ADDWRK                                                        
         B     XIT                                                              
*                                                                               
ACCOUNT  DS    0CL15                                                            
ACC      DS    CL15                THESE FIELDS FILLED IN BEFORE                
SUBWORK  DC    C'  '                                                            
SUBAC    DS    CL15                GOING TO POST ROUTINE                        
CONNAME  DS    CL36                                                             
DRORCR   DS    CL1                                                              
NARR     DS    CL48                                                             
NARRLEN  DS    CL1                                                              
MEMO     DS    CL250                                                            
         SPACE 2                                                                
TOTRECS  DC    PL6'0'                                                           
TOTCASH  DC    PL6'0'                                                           
         EJECT                                                                  
*              WORKER INTERFACE                                                 
         SPACE 3                                                                
ADDWRK   NTR1                                                                   
         MVC   COMMAND,=CL6'ADD'                                                
         LA    R2,AREA                                                          
         SR    R3,R3                                                            
         SPACE 2                                                                
ADDWRK2  CLI   0(R2),0             FIND EOR                                     
         BE    ADDWRK4                                                          
         IC    R3,1(R2)                                                         
         AR    R2,R3                                                            
         B     ADDWRK2                                                          
         SPACE 2                                                                
ADDWRK4  LA    R6,AREA-4           COMPUTE L'RECORD +4                          
         XC    0(4,R6),0(R6)                                                    
         LA    R2,1(R2)                                                         
         SR    R2,R6                                                            
         STH   R2,0(R6)                                                         
         B     WRKALL                                                           
         SPACE 2                                                                
CLOSWRK  NTR1                                                                   
         MVC   COMMAND,=CL6'CLOSE'                                              
         SPACE 2                                                                
WRKALL   LA    R3,AREA-4                                                        
         CLI   RCWRITE,C'N'                                                     
         BE    XIT                                                              
         L     R4,=A(POSTBUFF)                                                  
         GOTO1 WORKER,DMCB,COMMAND,(R4),ID,(R3)                                 
         TM    DMCB+8,X'C0'                                                     
         BZ    XIT                                                              
         DC    H'0'                                                             
         SPACE 3                                                                
         SPACE 2                                                                
COMMAND  DS    CL6                                                              
ID       DS    XL16                                                             
         EJECT                                                                  
*              HEADLINE ROUTINES (HOOK)                                         
         SPACE 3                                                                
         DS    0H                                                               
         USING *,RF                                                             
         SPACE 1                                                                
HOOK     NTR1                                                                   
         LM    RE,RC,BASEREGS                                                   
         DROP  RF                                                               
         L     R4,SAVEABOX                                                      
         USING BOXD,R4                                                          
         MVC   HEAD4+10(36),COMPNAME                                            
         MVC   HEAD5+101(6),ALLMONTH                                            
         CLI   RCSUBPRG,2                                                       
         BE    HOOK2                                                            
         CLI   RCSUBPRG,4                                                       
         BE    HOOK2                                                            
         MVC   HEAD5+10(6),CLICODE                                              
         MVC   HEAD5+17(36),CLINAME                                             
         CLI   RCSUBPRG,1                                                       
         BE    HOOK4                                                            
         MVC   HEAD6+10(6),DIVCODE                                              
         MVC   HEAD6+17(36),DIVNAME                                             
         MVC   HEAD6+101(6),CLIMONTH                                            
         CLI   NBOX,3                                                           
         BE    XIT                                                              
         MVI   NBOX,3                                                           
         MVC   BOXCOLS,MYCOLS3                                                  
         MVI   BOXINIT,0                                                        
         B     XIT                                                              
         SPACE 1                                                                
HOOK2    MVC   HEAD5+10(6),DPTCODE                                              
         MVC   HEAD5+17(36),DPTNAME                                             
         MVC   HEAD6+10(6),SUBCODE                                              
         MVC   HEAD6+17(36),SUBNAME                                             
         CLI   NBOX,2                                                           
         BE    XIT                                                              
         MVI   NBOX,2                                                           
         MVC   BOXCOLS,MYCOLS2                                                  
         MVI   BOXINIT,0                                                        
         B     XIT                                                              
*                                                                               
HOOK4    CLI   NBOX,1                                                           
         BE    XIT                                                              
         MVI   NBOX,1                                                           
         MVC   0(200,R4),MYBOX                                                  
         MVC   200(200,R4),MYBOX+200                                            
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DUMP RECORDS                                                        *         
***********************************************************************         
         SPACE 1                                                                
DUMP     NTR1                                                                   
         CLI   QOPT7,C'Y'                                                       
         BNE   DUMPX                                                            
         LA    R0,L'MSG                                                         
         LA    R2,MSG                                                           
         L     R3,0(R1)                                                         
         L     R4,4(R1)                                                         
*                                                                               
         LA    R5,=C'2D'                                                        
         GOTO1 PRNTBL,DMCB,((R0),(R2)),(R3),C'DUMP',(R4),(R5),         X        
               (C'P',PRINT)                                                     
*                                                                               
DUMPX    XIT1                                                                   
*        MVC   MSG,=CL10'TRNS  REC'                                             
*        GOTO1 DUMP,DMCB,(R2),(R6)                                              
*                                                                               
********************************************************************            
* BOXES AND OTHER CONSTANTS                                        *            
********************************************************************            
         SPACE 3                                                                
BASEREGS DS    16F                                                              
MYBOX    DS    0D                                                               
         DC    C'Y'                                                             
         DC    X'01'                                                            
         DC    6X'00'                                                           
MYCOLS   DC    C'L                                    C      '                  
         DC    C'C      C        C                           '                  
         DC    C'  C                 R                       '                  
         DC    CL132' '                                                         
MYROWS   DC    C'        T  M                  '                                
         DC    C'                          B   '                                
         DC    CL40' '                                                          
         DC    28X'00'                                                          
         SPACE 1                                                                
MYCOLS2  DC    C'L                                           '                  
         DC    C'C            C            C            C    '                  
         DC    C'        C            R                      '                  
         SPACE 1                                                                
MYCOLS3  DC    C'L                                  C      C '                  
         DC    C'      C          C                        C '                  
         DC    C'          C          R                      '                  
         SPACE 1                                                                
RULETAB  DS    0CL12                                                            
         DC    C'TYPE      ',AL1(RUTYPE-RULED,1)                                
         DC    C'ADJUST    ',AL1(RUADJ-RULED,1)                                 
         DC    C'HOURS     ',AL1(RUHOURS-RULED,0)                               
         DC    C'RATE      ',AL1(RURATE-RULED,2)                                
         DC    C'OVA       ',AL1(RUOVA-RULED,2)                                 
         DC    C'OVB       ',AL1(RUOVB-RULED,2)                                 
         DC    C'OVC       ',AL1(RUOVC-RULED,2)                                 
         DC    C'MAX       ',AL1(RUMAXMON-RULED,0)                              
         DC    C'WMAX      ',AL1(RUMAXDAY-RULED,0)                              
         DC    C'START     ',AL1(RUMONTH-RULED,3)                               
         DC    C'PART      ',AL1(RUPART-RULED,4)                                
         DC    X'FF'                                                            
*                                                                               
RELOC    DC    A(*)                                                             
NULL     DC    X'00'                                                            
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(5,36,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=V,LENGTH=(340,,,340,340) '                      
FRSTCLI  DC    C'Y'                                                             
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* DSECT FOR FEE ALLOCATION SYSTEM                                    *          
**********************************************************************          
         SPACE 1                                                                
MYD      DSECT                                                                  
SRTREC   DS    0C                  SORT RECORD                                  
SRTRECL  DS    H                   4+36+300=340                                 
         DS    H                                                                
SRTCLI   DS    CL6                                                              
SRTTYPE  DS    CL1                 1=RULES 2=HOURS/COST                         
SRTDIV   DS    CL6                                                              
SRTDPT   DS    CL6                                                              
SRTSUB   DS    CL6                                                              
SRTPRS   DS    CL8                                                              
SRTALL   DS    CL1                 1=ALL CLI  2=THIS CLI  3=THIS DIV            
*                                  0=ALL CLI AND NON-CLI                        
         DS    CL2                                                              
SRTRULES DS    0C                                                               
SRTHOURS DS    (PLBUKLNQ)P         HOURS (2 DEC) X 12 MONTHS                    
SRTCOST  DS    (PLBUKLNQ)P         COST  (CENTS) X 12 MONTHS                    
SRTFEES  DS    (PLBUKLNQ)P         FEES  (CENTS) X 12 MONTHS                    
SRTSALY  DS    PL6                                                              
SRTOFF   DS    CL2                                                              
         DS    CL4                                                              
SRTLNQ   EQU   *-SRTREC                                                         
*                                                                               
ARULES   DS    A                                                                
APOOL    DS    A                                                                
LRULE    DS    F                                                                
AMYIO    DS    F                                                                
AMYIO2   DS    F                                                                
SCANNER  DS    V                                                                
CASHVAL  DS    V                                                                
COVAIL   DS    V                                                                
UNDERLIN DS    V                                                                
ACSLRY   DS    V                                                                
ACSALHST DS    V                                                                
ACLIST   DS    V                                                                
PRNTBL   DS    V                                                                
ACLIMEMO DS    A                                                                
ACOSTCLI DS    A                                                                
*                                                                               
PRINTOPT DS    CL1                                                              
CARD     DS    CL80                                                             
MYCOM    DS    CL8                                                              
*                                                                               
COMPNAME DS    CL36                                                             
SAVEKEY  DS    CL42                SAVE TO RESTORE READS                        
LCLICODE DS    CL1                                                              
LDIVCODE DS    CL1                                                              
LDPTCODE DS    CL1                 COMBINED OFFICE/DPT LEN                      
LENLEVLS DS    0CL4                                                             
LOFFCODE DS    CL1                 LENGTH OF OFFICE                             
LDPTCOD2 DS    CL1                                                              
LSUBCODE DS    CL1                                                              
LPRSCODE DS    CL1                                                              
ELCODE   DS    CL1                                                              
CLICODE  DS    CL6                                                              
DIVCODE  DS    CL6                                                              
CLINAME  DS    CL36                                                             
DIVNAME  DS    CL36                                                             
DPTCOD2  DS    CL6                                                              
DPTCODE  DS    CL6                                                              
SUBCODE  DS    CL6                                                              
PRSCODE  DS    CL8                                                              
DPTNAME  DS    CL36                                                             
SUBNAME  DS    CL36                                                             
PRSNAME  DS    CL36                                                             
DPTMID   DS    CL64                                                             
SUBMID   DS    CL64                                                             
DPTPEND  DS    CL1                                                              
SUBPEND  DS    CL1                                                              
SCAN     DS    CL64                                                             
MSG      DS    CL10                MESSAGE FOR PRINTABLE                        
*                                                                               
ALLMONTH DS    CL6                                                              
CLIMONTH DS    CL6                                                              
MNTHLIST DS    CL24                                                             
MNTH     DS    CL2                                                              
OFFSW    DS    CL1                                                              
*                                                                               
LASTCLI  DS    CL6                                                              
LASTDIV  DS    CL6                                                              
LASTDPT  DS    CL6                                                              
LASTSUB  DS    CL6                                                              
LASTPRS  DS    CL8                                                              
ACTCLI   DS    CL6                                                              
         DS    0D                                                               
CLILINE  DS    (PLBUKNUM)PL8                                                    
ALLLINE  DS    (PLBUKNUM)PL8                                                    
TOTLINE  DS    (PLBUKNUM)PL8                                                    
LINELNQ  EQU   (*-CLILINE)/L'CLILINE       # OF ACCUMS                          
*                                                                               
EFFRULE  DS    CL60                                                             
NONSW    DS    CL1                                                              
LKEY     DS    CL36                                                             
*                                                                               
PKFLDS   DS    0PL8                                                             
PRSACC   DS    12PL8                                                            
PKFLDLNQ EQU   *-PKFLDS                                                         
SUBACC   DS    12PL8                                                            
DPTACC   DS    12PL8                                                            
DIVACC   DS    12PL8                                                            
CLIACC   DS    12PL8                                                            
AGYACC   DS    12PL8                                                            
PKFLDSQ  EQU   (*-PKFLDS)/L'PKFLDS                                              
*                                                                               
TOTSW    DS    CL1                                                              
LEVEL    DS    CL1                                                              
PRSSALY  DS    PL8                                                              
PRSSALP  DS    PL6                                                              
SAVEABOX DS    A                                                                
NBOX     DS    CL1                                                              
OFFCODE  DS    CL2                 UP TO TWO BYTE OFFICE CODE                   
OFFCOD2  DS    CL2                                                              
WORKDAYS DS    F                                                                
EXCLUDE  DS    CL1                                                              
*                                  VALUES EXTRACTED BY YTD                      
PACK16   DS    PL16                TEMPORARY ACCUMULATOR                        
PACK8    DS    PL8                 TEMPORARY ACCUMULATOR                        
PRSMHOUR DS    PL8                 FOR PERSON                                   
PRSYHOUR DS    PL8                                                              
PRSMCOST DS    PL8                                                              
PRSYCOST DS    PL8                                                              
PRSMFEES DS    PL8                                                              
PRSYFEES DS    PL8                                                              
CLIMHOUR DS    PL8                 CLIENT                                       
CLIYHOUR DS    PL8                                                              
CLIMCOST DS    PL8                                                              
CLIYCOST DS    PL8                                                              
CLIMFEES DS    PL8                                                              
CLIYFEES DS    PL8                                                              
ALLMHOUR DS    PL8                 ALL CLIENTS                                  
ALLYHOUR DS    PL8                                                              
ALLMCOST DS    PL8                                                              
ALLYCOST DS    PL8                                                              
ALLMFEES DS    PL8                                                              
ALLYFEES DS    PL8                                                              
TOTMHOUR DS    PL8                 CLIENT AND NON CLIENT                        
TOTYHOUR DS    PL8                                                              
TOTMCOST DS    PL8                                                              
TOTYCOST DS    PL8                                                              
TOTMFEES DS    PL8                                                              
TOTYFEES DS    PL8                                                              
*                                                                               
         DS    F                                                                
AREA     DS    500C                                                             
SALAREA  DS    CL(SALLNQ)          SALARY BLOCK                                 
SALAREA2 DS    100C                SALARY BLOCK                                 
         SPACE 1                                                                
         SPACE 1                                                                
         EJECT                                                                  
**********************************************************************          
* DSECT TO COVER RULE ENTRY                                          *          
**********************************************************************          
         SPACE 1                                                                
RULED    DSECT                                                                  
RUCLI    DS    CL6                 CLIENT                                       
RUDIV    DS    CL6                 DIVISION                                     
RUDPT    DS    CL6                 DEPARTMENT                                   
RUSUB    DS    CL6                 SUB-DEPARTMENT                               
RUPRS    DS    CL8                 PERSON                                       
RUTYPE   DS    CL4                 TYPE ZERO NON 66 100 ETC                     
RUADJ    DS    CL4                 ADJUSTMENT DETAILS                           
RUHOURS  DS    CL2                 MAXIMUM HOURS                                
RURATE   DS    CL2                 HOURLY RATE                                  
RUOVA    DS    CL2                 OVERHEAD FACTOR A                            
RUOVB    DS    CL2                 OVERHEAD FACTOR B                            
RUOVC    DS    CL2                 OVERHEAD FACTOR C                            
RUMAXMON DS    CL2                 MAX HOURS IN MONTH                           
RUMAXDAY DS    CL2                 MAX HOURS IN WORKING DAY                     
RUMONTH  DS    CL1                 FISCAL MONTH                                 
RUPART   DS    CL2                 BILL NN PERCENT OF HOURS                     
         DS    CL3                 SPARE                                        
         SPACE 2                                                                
         EJECT                                                                  
**********************************************************************          
* DSECT TO COVER 1C TABLE                                            *          
**********************************************************************          
         SPACE 1                                                                
COSTCD   DSECT                                                                  
COSTCODE DS    CL12                1C CODE                                      
COSTNAME DS    CL36                NAME                                         
COSTLEN  EQU   *-COSTCD                                                         
         SPACE 2                                                                
**********************************************************************          
* DSECT TO COVER POOL ENTRY                                          *          
**********************************************************************          
         SPACE 1                                                                
POOLD    DSECT                                                                  
POOLCLI  DS    CL6                 CLIENT                                       
POOLDIV  DS    CL6                 DIVISION                                     
PLBUK    DS    0PL8                START OF BUCKETS                             
POOLHOUR DS    12PL8               MONTHLY HOUR BUCKETS                         
PLBUKLNQ EQU   *-PLBUK                                                          
POOLCOST DS    12PL8               MONTHLY COST BUCKETS                         
POOLFEES DS    12PL8               MONTHLY FEE BUCKETS                          
PLBUKQ   EQU   *-PLBUK             TOTAL LENGTH OF BUCKETS                      
PLBUKNUM EQU   PLBUKQ/L'PLBUK      TOTAL NUMBER OF BUCKETS                      
POOLOFF  DS    CL2                 OFFICE                                       
         DS    CL2                                                              
POOLNQ   EQU   *-POOLD                                                          
         SPACE 2                                                                
**********************************************************************          
* DSECT TO COVER A LINE OF ACCUMULATORS                              *          
**********************************************************************          
         SPACE 1                                                                
ACCUMD   DSECT                                                                  
HOURS    DS    PL8                 HOURS                                        
SALARY   DS    PL8                 SALARY COST                                  
OVA      DS    PL8                 OVERHEAD A                                   
OVB      DS    PL8                          B                                   
OVC      DS    PL8                          C                                   
OVD      DS    PL8                          D                                   
FLAT     DS    PL8                 FLAT RATE                                    
         DS    PL8                 SPARE                                        
ADJUST1  DS    PL8                 ADJUSTMENT 1                                 
ADJUST2  DS    PL8                            2                                 
MONFEES  DS    PL8                 MONTHLY FEES                                 
YTDFEES  DS    PL8                 YTD FEES                                     
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE DDREPXTRAD                                                     
       ++INCLUDE DDREPMASTD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDSLRD                                                         
       ++INCLUDE ACSALHSTD                                                      
       ++INCLUDE ACGENPOST                                                      
         PRINT ON                                                               
         SPACE 2                                                                
ACFA02   CSECT                                                                  
         ENTRY RULEPOOL                                                         
         DS    0D                                                               
         DC    C'*RULES**'                                                      
RULEPOOL DC    F'9120'             NUMBER OF RULES MAX                          
         DC    F'0'                NUMBER OF RULES SO FAR                       
         DC    F'60'               WIDTH OF RULE                                
         DC    F'0'                SPARE                                        
         DC    547200X'00'         POOL                                         
         SPACE 1                                                                
         ENTRY SORTPOOL                                                         
         DS    0D                                                               
         DC    C'**SORT**'                                                      
SORTPOOL DC    41000X'00'                                                       
         SPACE 1                                                                
         ENTRY MYIO                                                             
         DS    0D                                                               
         DC    C'**MYIO**'                                                      
MYIO     DC    2000X'00'                                                        
         SPACE 1                                                                
         ENTRY MYIO2                                                            
         DS    0D                                                               
         DC    C'**MYIO2*'                                                      
MYIO2    DC    2000X'00'                                                        
         SPACE 1                                                                
         ENTRY POSTBUFF                                                         
         DS    0D                                                               
         DC    C'**POST**'                                                      
POSTBUFF DC    4500X'00'                                                        
         SPACE 1                                                                
CLIMEMOQ EQU   500                 500 CLIENT IN TABLE                          
CLIMEMO  DC    (CLIMEMOQ*6)X'00'                                                
         SPACE 1                                                                
COSTCLIQ EQU   800                 800 CLIENT IN TABLE                          
COSTCLI  DC    (COSTCLIQ*COSTLEN)X'00'                                          
COSTCLLN EQU   *-COSTCLI                                                        
         DC    X'FF'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'068ACREPFA02 05/01/02'                                      
         END                                                                    
