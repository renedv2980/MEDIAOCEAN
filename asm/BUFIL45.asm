*          DATA SET BUFIL45    AT LEVEL 075 AS OF 05/01/02                      
*PHASE T50245A                                                                  
*INCLUDE ACJOBCOL                                                               
T50245   TITLE 'BUFIL45 - INTERAGENCY ESTIMATE BUDGET EXTRACT - ACCPAK'         
T50245   CSECT                                                                  
         PRINT NOGEN                                                            
         SPACE 2                                                                
         NMOD1 (ACWKX-ACWKD),T50245,RA,RR=R8,CLEAR=YES                          
         ST    R8,RELO                                                          
*                                                                               
         USING ACWKD,R7                                                         
         USING GEND,RC                                                          
         USING SYSD,R9                                                          
         USING SPOOLD,R8                                                        
         LR    R7,RC                                                            
         L     RC,0(R1)                                                         
         L     R9,ASYSD                                                         
         L     R8,ASPOOLD                                                       
*                                                                               
         L     R0,VADUMMY                                                       
         ST    R0,NEXTADDR         SET NEXT AVAILABLE CORE ADDRESS              
*                                                                               
         L     R3,ATWA                                                          
         USING T502FFD,R3                                                       
         MVC   VBUFFALO,TWAVBUFF                                                
*                                                                               
         L     R0,=A(BUFFALOC)                                                  
         A     R0,RELO                                                          
         ST    R0,BUFFBUFF                                                      
         GOTO1 VBUFFALO,DMCB,=C'SET',BUFFBUFF                                   
*                                                                               
         MVC   AACIOS,=A(ACIOS)                                                 
         L     RE,AACIOS           RE = A(RECORD SAVE AREA)                     
         LA    RF,ADLIST           RF = A(ADCON LIST)                           
         LA    R0,ADCONS           R0 = N'ADCONS                                
         ST    RE,0(RF)                                                         
         LA    RE,L'ACIOS(RE)      NEXT SAVE AREA                               
         LA    RF,4(RF)            NEXT ADCON                                   
         BCT   R0,*-12                                                          
*                                                                               
         XC    DMCB,DMCB           OBTAIN CORE-RESIDENT PHASES                  
         LA    R0,CORES                                                         
         LA    R4,CORETAB                                                       
         LA    R5,COREMODS                                                      
         L     RF,CALLOV                                                        
         LA    R1,DMCB                                                          
         MVC   DMCB+4(3),=X'D9000A'                                             
*                                                                               
GETCORE  MVC   DMCB+7(1),0(R5)                                                  
         GOTO1 (RF),(R1),0                                                      
         MVC   0(4,R4),DMCB        SAVE MODULE ADDRESS                          
         LA    R5,1(R5)            GET NEXT MODULE NUMBER                       
         LA    R4,4(R4)            GET NEXT ADDRESS                             
         BCT   R0,GETCORE          GET THE NEXT ONE                             
         EJECT                                                                  
***********************************************************************         
* FIND DATA TYPES TO PROCESS                                                    
***********************************************************************         
*                                                                               
         LA    R1,SVDTYPES                                                      
         USING SVDTD,R1                                                         
         LA    R0,MAXDTYP          R0=COUNTER                                   
         XC    DATAINDS,DATAINDS   RESET EXTRACT SWITCH                         
         SR    R4,R4               R4=N'EXTRACT TYPES                           
         LA    R5,EXTYPS           R5=A(EXTRACT TYPE TABLE)                     
*                                                                               
IAGY02   CLI   SVDTEX,0            TEST FOR EOT                                 
         BE    IAGY06              YES                                          
         LA    RE,EXTTBL           RE=A(VALID DATA TYPE TABLE)                  
         LA    RF,EXTYPES          RF=N'VALID DATA TYPES                        
*                                                                               
IAGY03   CLC   SVDTEX,0(RE)        TEST IF VALID FOR SPOT EXTRACT               
         BE    IAGY04              YES                                          
         LA    RE,L'EXTTBL(RE)                                                  
         BCT   RF,IAGY03                                                        
         B     IAGY05                                                           
*                                                                               
IAGY04   OC    DATAINDS,1(RE)      UPDATE CUMULATIVE MASK                       
         MVC   0(1,R5),0(RE)       ADD EXTRACT TYPE                             
         LA    R4,1(R4)            INCREMENT EXTRACT TYPE COUNT                 
         LA    R5,1(R5)            AND NEXT TABLE POSITION                      
*                                                                               
IAGY05   LA    R1,SVDTL(R1)        NEXT DATA TYPE                               
         BCT   R0,IAGY02                                                        
         DROP  R1                                                               
*                                                                               
IAGY06   LTR   R4,R4               TEST ANYTHING TO EXTRACT                     
         BZ    XIT                                                              
*                                                                               
         TM    DATAIND,EXORDB      ORDERED DATA TYPE?                           
         BNZ   IAGY07                                                           
         TM    DATAIND,EXBADVB     BILLING BY ADVERTISING MONTH?                
         BNZ   IAGY07                                                           
         TM    DATAIND1,EXBAADVB   ACTUAL BILL BY ADVERTISING MONTH?            
         BZ    XIT                                                              
*                                                                               
IAGY07   STC   R4,NEXTYPS          SAVE COUNTER                                 
         MVI   BITFLAG1,0          RESET FIRST SET OF BIT FLAGS                 
*                                                                               
         L     R2,ARULDATA         R2 = A(FIRST RULE)                           
         USING QRD,R2                                                           
         MVC   DATADISP,=Y(ACCRFST-ACCRECD)                                     
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE COMPANY                                                          
***********************************************************************         
*                                                                               
         MVC   COMPANY,QRAGY       LOOKUP THE COMPANY ON THE FILE               
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         BAS   RE,ACCHIGH                                                       
*                                                                               
         CLC   KEY(L'ACCKEY),KEYSAVE                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R5,ADCOMP           SAVE THE COMPANY RECORD                      
         BAS   RE,SAVREC                                                        
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,CPYELQ                                                    
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING CPYELD,R6                                                        
         MVC   PRODLDGR,CPYPROD    COPY THE PRODUCT AND RECEIVABLE              
         MVC   RECVLDGR,CPYRECV        LEDGERS                                  
         DROP  R6                                                               
*                                                                               
***********************************************************************         
* VALIDATE THE PRODUCT LEDGER                                                   
***********************************************************************         
*                                                                               
         MVC   KEY+1(L'PRODLDGR),PRODLDGR                                       
         BAS   RE,ACCHIGH                                                       
*                                                                               
         CLC   KEY(ACCORLEN),KEYSAVE                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R5,ADPRLDGR         SAVE THE PRODUCT LEDGER RECORD               
         BAS   RE,SAVREC                                                        
*                                                                               
         L     R6,ADPRLDGR                                                      
         MVI   ELCODE,ACLELQ                                                    
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         ST    R6,ADLDGHIR         SAVE A(HEIRARCHY ELEMENT)                    
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE CLIENT                                                           
***********************************************************************         
*                                                                               
         USING ACLELD,R6                                                        
VALCLT00 ZIC   R1,ACLVLEN          CLIENT IS THE FIRST LEVEL                    
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),QRCLT                                                   
         DROP  R6                                                               
*                                                                               
         BAS   RE,ACCHIGH                                                       
*                                                                               
         CLC   KEY(ACCORLEN),KEYSAVE                                            
         BNE   BADCLT                                                           
         L     R5,ADCLTREC         SAVE THE CLIENT RECORD                       
         BAS   RE,SAVREC                                                        
*                                                                               
         MVC   CLTFIL,SPACES                                                    
         MVI   ELCODE,RSTELQ                                                    
         BAS   RE,GETEL                                                         
         BNE   VALCLTX                                                          
*                                                                               
         USING RSTELD,R6                                                        
         MVC   CLTFIL(2),RSTFILT1     EXTRACT CLIENT'S ACCOUNT FILTERS          
         MVC   CLTFIL+2(1),RSTFILT3       ANALYSIS CODE                         
         MVC   CLTFIL+3(1),RSTFILT4       SUB-COMPANY                           
         CLI   RSTLN,RSTLN2Q                                                    
         BL    VALCLTX                                                          
         MVC   CLTFIL+4(1),RSTFILT5                                             
*                                                                               
VALCLTX  DS    0H                                                               
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE PRODUCT                                                          
***********************************************************************         
*                                                                               
VALPRD00 CLC   QRPRD,=C'ALL'       ALL PRODUCT EXTRACT?                         
         BNE   *+12                NO                                           
         OI    BITFLAG1,B1ALLPRD   YES                                          
         B     VALPRDX                                                          
*                                                                               
         L     R6,ADLDGHIR                                                      
         USING ACLELD,R6                                                        
         ZIC   R5,ACLVLEN             CLIENT IS THE FIRST LEVEL                 
         ZIC   R1,ACLVLEN+L'ACLVALS   PRODUCT IS THE SECOND LEVEL               
         SR    R1,R5                                                            
         LA    R5,KEY+3(R5)           BUILD KEY FOR PRODUCT RECORD              
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),QRPRD                                                    
         DROP  R6                                                               
*                                                                               
         BAS   RE,ACCHIGH                                                       
*                                                                               
         CLC   KEY(ACCORLEN),KEYSAVE                                            
         BNE   BADPRD                                                           
         L     R5,ADPRDREC         SAVE THE CLIENT RECORD                       
         BAS   RE,SAVREC                                                        
*                                                                               
         MVC   PRDFIL,SPACES                                                    
         MVI   ELCODE,RSTELQ                                                    
         BAS   RE,GETEL                                                         
         BNE   VALPRDX                                                          
*                                                                               
         USING RSTELD,R6                                                        
         MVC   PRDFIL(2),RSTFILT1    EXTRACT PRODUCT'S ACCOUNT FILTERS          
         MVC   PRDFIL+2(1),RSTFILT3      ANALYSIS CODE                          
         MVC   PRDFIL+3(1),RSTFILT4      SUB-COMPANY                            
         CLI   RSTLN,RSTLN2Q                                                    
         BL    VALPRDX                                                          
         MVC   PRDFIL+4(1),RSTFILT5                                             
*                                                                               
VALPRDX  DS    0H                                                               
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* GET RECORDS REQUESTED                                                         
***********************************************************************         
GTREC00  DS    0H                                                               
         L     R2,ARULDATA                                                      
*                                                                               
         MVC   KEY,SPACES                                                       
         LA    R4,KEY                                                           
         USING INTKEY,R4                                                        
         MVI   INTKTYP,INTKTYPQ                                                 
         MVI   INTKSUB,INTKSUBQ                                                 
         MVC   INTKCPY,COMPANY                                                  
         MVC   INTKUNT(L'RECVLDGR),RECVLDGR                                     
*                                                                               
GTRECHI  BAS   RE,ACCHIGH                                                       
         B     GTREC10                                                          
*                                                                               
GTRECSQ  BAS   RE,ACCSEQ                                                        
         L     R2,ARULDATA         AS PER BOBL SINCE QRJUMP IS (0)              
*                                                                               
GTREC10  CLC   KEY(INTKACT-INTKEY),KEYSAVE   SAME COMPANY/UNIT/LEDGER?          
         BNE   GTRECX                        NO                                 
*                                                                               
         TM    BITFLAG1,B1ALLPRD   YES, ALL PRODUCTS?                           
         BNZ   GTREC20                 YES                                      
*                                                                               
         CLC   INTKCLT,QRCLT       SAME CLIENT?                                 
         BE    GTREC13                                                          
         BL    *+12                                                             
         MVI   INTKCLT,X'FF'       NO                                           
         B     GTRECHI                                                          
         MVC   INTKCLT,QRCLT       YES                                          
         XC    INTKPRD(L'INTKPRD+L'INTKMED+L'INTKEST),INTKPRD                   
         B     GTRECHI                                                          
*                                                                               
GTREC13  CLC   INTKPRD,QRPRD           NO, CHECK AGAINST SPECIFIED PRD          
         BE    GTREC20                     SAME PRD, YES, CHECK FILTERS         
         BL    *+12                                                             
         MVI   INTKPRD,X'FF'       NO                                           
         B     GTRECHI                                                          
         MVC   INTKPRD,QRPRD       YES, SET THE PRODUCT TO FILTER PRD           
         XC    INTKMED(L'INTKMED+L'INTKEST),INTKMED                             
         B     GTRECHI                                                          
*                                                                               
GTREC20  BAS   RE,CKFILTRS         CHECK THE FILTERS TO SEE IF THEY ARE         
*                                                                               
         L     R2,ARULDATA                                                      
GTREC30  CLI   QRPROC,C'Y'         PROCESS THE RECORD FOR THIS RULE?            
         BE    GTREC40                                                          
GTREC30N ICM   R2,15,QRNEXT        NO, CHECK NEXT RULE                          
         BNZ   GTREC30                                                          
         B     GTRECSQ             DON'T PROCESS REC, CHECK NEXT RECORD         
*                                                                               
GTREC40  L     R6,AIO                                                           
         TM    ACCOSTAT(R6),X'80'  RECORD DELETED?                              
         BNZ   GTREC30N                                                         
*                                                                               
         XC    BUFFRULE,BUFFRULE                                                
*                                                                               
GTREC50  CLI   QRPROC,C'Y'         PROCESS THE RECORD?                          
         BNE   GTREC50N            NO                                           
*                                                                               
         BAS   RE,PRCSSREC         YES                                          
*                                                                               
         OC    BUFFRULE,BUFFRULE   DID THAT RULE USE THE RECORD?                
         BZ    GTREC50N            NO, GET NEXT RULE                            
*                                                                               
         XC    BUFFRULE,BUFFRULE   YES, NOW JUMP AHEAD IN RULE TABLE            
         ICM   R2,15,QRJUMP                                                     
         BNZ   GTREC50             BEGIN AGAIN AT JUMP POINT                    
         B     GTRECSQ             GET NEXT RECORD                              
*                                                                               
GTREC50N ICM   R2,15,QRNEXT        ELSE TRY ANOTHER RULE IF ANY                 
         BNZ   GTREC50                                                          
         B     GTRECSQ                                                          
*                                                                               
GTRECX   DS    0H                                                               
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE WRITES WORKER RECORDS                                            
***********************************************************************         
*                                                                               
WRKRRECS L     R2,ARULDATA         R2=A(RULE ENTRY)                             
*                                                                               
WKRREC10 ZIC   R4,NEXTYPS          R4=N'EXTRACT TYPES                           
         LA    R5,EXTYPS           R5=A(EXTRACT TYPE TABLE)                     
         XC    BUFFREC,BUFFREC                                                  
         ST    R2,BUFFRULE                                                      
         GOTO1 VPRTRULE,PARAS,(R2)                                              
         GOTO1 VDATAHD                                                          
*                                                                               
WKRREC20 MVC   BUFFTYPE,0(R5)      SET EXTRACT TYPE                             
         LA    R6,SVEXTDTS                                                      
         USING SVEXD,R6                                                         
*                                                                               
WKRREC30 MVC   BUFFPER,SVEXPER     SET PERIOD                                   
         BAS   RE,BUFFGET                                                       
         CLI   DMCB+8,0            TEST IF ITEM FOUND                           
         BE    WKRREC40            YES                                          
         ZAP   BUFFGRS,=P'0'       NO-MANUFACTURE ZERO RECORD                   
         ZAP   BUFFNET,=P'0'                                                    
         ZAP   BUFFSPTS,=P'0'                                                   
*                                                                               
WKRREC40 MVI   DMCB,BUPPUT                                                      
         TM    WHEN,X'18'          TEST OVERNIGHT                               
         BNZ   WKRREC50            YES                                          
         MVI   DMCB,BUPADD                                                      
         TM    WHEN,X'20'          TEST SOON                                    
         BO    *+6                                                              
         DC    H'0'                                                             
*                                                                               
WKRREC50 GOTO1 VBUPPER                                                          
         LA    R6,SVEXL(R6)        NEXT PERIOD                                  
         OC    SVEXPER,SVEXPER     TEST FOR LAST PERIOD                         
         BNZ   WKRREC30            NO                                           
*                                                                               
         LA    R5,1(R5)            NEXT EXTRACT TYPE                            
         BCT   R4,WKRREC20                                                      
*                                                                               
         MVI   DMCB,BUPFINAL       FINAL CALL FOR OUTLINE                       
         GOTO1 VBUPPER                                                          
         ICM   R2,15,QRNEXT                                                     
         BNZ   WKRREC10                                                         
*                                                                               
WKRRECX  B     XIT                 NO MORE RULES                                
         DROP  R2,R6                                                            
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE CHECKS TO SEE IF THE MEDIA AND ESTIMATE FITLERS WERE             
* SATISFIED                                                                     
***********************************************************************         
*                                                                               
CKFILTRS NTR1                                                                   
         NI    BITFLAG1,X'FF'-B1MEDOKY-B1ESTOKY    MEDIA & EST NOT MET          
*                                                                               
         L     R2,ARULDATA         RESET ALL RULE PROCESS SWITCHES              
         USING QRD,R2                                                           
CFLTR00  MVI   QRSW1,C'N'                                                       
         MVI   QRSW2,C'N'                                                       
         MVI   QRPROC,C'N'                                                      
         ICM   R2,15,QRNEXT                                                     
         BNZ   CFLTR00                                                          
*                                                                               
         LA    R4,KEY                                                           
         USING INTKEY,R4                                                        
*                                                                               
*****                                                                           
* CHECK MEDIA FILTERS FROM ALL THE RULES                                        
*****                                                                           
*                                                                               
         L     R2,ARULDATA         R2 = A(FIRST RULE)                           
CFLTR10  ZICM  R5,QRIMIDSP,2       R5 = DISP TO INTERAGY MEDIA LIST             
         BZ    CFLTR14                 NONE MEANS ALL ACCEPTED                  
*                                                                               
         AR    R5,R2               R5 = A(INTERAGY MEDIA LIST)                  
         ZIC   R3,0(R5)            R3 = NUMBER IN LIST                          
         LA    R5,1(R5)                                                         
*                                                                               
CFLTR12  CLC   INTKMED,0(R5)       MEDIA PART OF THE LIST?                      
         BNE   CFLTR16                                                          
CFLTR14  MVI   QRSW1,C'Y'          YES, SET SWITCH ON FOR THIS RULE             
         B     CFLTR10N                                                         
*                                                                               
CFLTR16  LA    R5,2(R5)            2 BYTE ENTRIES                               
         BCT   R3,CFLTR12                                                       
*                                                                               
CFLTR10N ICM   R2,15,QRNEXT        CHECK ALL THE RULES                          
         BNZ   CFLTR10                                                          
*                                                                               
*****                                                                           
* CHECK ESTIMATE FILTERS FROM ALL THE RULES                                     
*****                                                                           
*                                                                               
         L     R2,ARULDATA         R2 = A(FIRST RULE)                           
CFLTR20  ZICM  R5,QRIESDSP,2       R5 = DISP TO INTERAGY ESTIMATE LIST          
         BZ    CFLTR24                 NONE MEANS ALL ACCEPTED                  
*                                                                               
         AR    R5,R2               R5 = A(INTERAGY ESTIMATE LIST)               
         ZIC   R3,0(R5)            R3 = NUMBER IN LIST                          
         LA    R5,1(R5)                                                         
*                                                                               
CFLTR22  CLC   INTKEST,0(R5)       ESTIMATE PART OF THE LIST?                   
         BNE   CFLTR26                                                          
CFLTR24  MVI   QRSW2,C'Y'          YES, SET SWITCH ON FOR THIS RULE             
         B     CFLTR20N                                                         
*                                                                               
CFLTR26  LA    R5,6(R5)            6 BYTE ENTRIES                               
         BCT   R3,CFLTR22                                                       
*                                                                               
CFLTR20N ICM   R2,15,QRNEXT        CHECK ALL THE RULES                          
         BNZ   CFLTR20                                                          
*                                                                               
*****                                                                           
* COMBINE MEDIA SWITCH AND ESTIMATE SWITCH AND SET PROCESS RULE TO Y/N          
*****                                                                           
*                                                                               
         L     R2,ARULDATA         R2 = A(FIRST RULE)                           
CFLTR30  CLI   QRSW1,C'Y'                                                       
         BNE   CFLTR30N                                                         
         OI    BITFLAG1,B1MEDOKY                                                
         CLI   QRSW2,C'Y'                                                       
         BNE   CFLTR30N                                                         
         OI    BITFLAG1,B1ESTOKY                                                
         MVI   QRPROC,C'Y'         SET TO PROCESS THIS RULE                     
*                                                                               
CFLTR30N ICM   R2,15,QRNEXT                                                     
         BNZ   CFLTR30                                                          
*                                                                               
CFLTRX   B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE PROCESSES THE INTERAGENCY RECORD                                 
***********************************************************************         
*                                                                               
PRCSSREC NTR1                                                                   
         L     R6,AIO              GET THE DATA                                 
         MVI   ELCODE,IESELQ                                                    
         BAS   RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
PRECD10  BAS   RE,NEXTEL                                                        
         BNE   PRECDX                                                           
*                                                                               
         LA    R5,SVEXTDTS                                                      
         USING SVEXD,R5                                                         
         USING IESELD,R6                                                        
*                                                                               
         MVC   FULL,IESMTH                                                      
         MVC   FULL+2,X'01'                                                     
         GOTO1 HEXOUT,DMCB,FULL,DUB,3                                           
         GOTO1 DATCON,DMCB,(0,DUB),(3,FULL)                                     
*                                                                               
PRECD15  DS    0H                                                               
         OC    SVEXPER,SVEXPER     ANY MORE PERIODS?                            
         BZ    PRECD10             NO MORE, CHECK NEXT ELEMENT                  
         CLC   FULL(L'SVEXSTB),SVEXSTB   TEST PRIOR TO PERIOD START             
         BL    PRECD20                                                          
         CLC   FULL(L'SVEXENDB),SVEXENDB     OR AFTER PERIOD END                
         BH    PRECD20                                                          
         B     PRECD30                                                          
*                                                                               
PRECD20  LA    R5,SVEXL(R5)        NEXT PERIOD                                  
         B     PRECD15                                                          
*                                                                               
PRECD30  ST    R2,BUFFRULE         SAVE ADDRESS OF CURRENT RULE                 
*                                                                               
         TM    DATAIND,EXORDB      ORDERED?                                     
         BZ    PRECD40                                                          
         MVI   BUFFTYPE,EXORD      YES                                          
         MVC   BUFFPER,SVEXPER         SET PERIOD                               
         ZAP   BUFFGRS,IESGRS          GROSS BILLING                            
         ZAP   BUFFNET,=P'0'                                                    
         ZAP   BUFFSPTS,=P'0'                                                   
*                                                                               
         BAS   RE,BUFFPUT                                                       
*                                                                               
PRECD40  TM    DATAIND,EXBADVB     BILLING BY ADVERTISING MONTH?                
         BZ    PRECD45                                                          
         MVI   BUFFTYPE,EXBADV     YES                                          
         BAS   RE,PRECDADV                                                      
         BAS   RE,BUFFPUT                                                       
*                                                                               
PRECD45  TM    DATAIND1,EXBAADVB   ACTUAL BILL BY ADVERTISING MONTH?            
         BZ    PRECD50                                                          
         MVI   BUFFTYPE,EXBAADV    YES                                          
         BAS   RE,PRECDADV                                                      
         BAS   RE,BUFFPUT                                                       
*                                                                               
PRECD50  B     PRECD10                                                          
*                                                                               
PRECDX   B     XIT                                                              
*                                                                               
PRECDADV MVC   BUFFPER,SVEXPER         SET PERIOD                               
         ZAP   BUFFGRS,=P'0'           SET UP DEFAULTS FIRST                    
         ZAP   BUFFNET,=P'0'                                                    
         ZAP   BUFFSPTS,=P'0'                                                   
*                                                                               
         CP    IESGRS,=P'0'        IF ANY OF THESE ARE 0, THEN USE              
         BER   RE                     THE DEFAULTS                              
         CP    IESREC,=P'0'                                                     
         BER   RE                                                               
         CP    IESPD,=P'0'                                                      
         BER   RE                                                               
*                                                                               
         ZAP   PL13,IESPD                                                       
         MP    PL13,IESGRS                                                      
         SRP   PL13,1,0            MULTIPLY BY 10 TO GET ROUNDING               
         DP    PL13,IESREC                USE QUOTIENT FOR BILLING              
         ZAP   BUFFGRS,PL13(L'PL13-L'IESREC)                                    
         SRP   BUFFGRS,64-1,5                                                   
         BR    RE                                                               
         DROP  R5,R6                                                            
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE SAVES THE RECORD IN AIO INTO SAVE STORAGE FOR 1000 BYTES         
***********************************************************************         
*                                                                               
SAVREC   NTR1                                                                   
         LR    R0,R5                                                            
         L     RE,AIO                                                           
         LA    R1,L'ACIOS           FULL IO AREA LENGTH                         
         LR    RF,R1                                                            
         MVCL  R0,RE                RECORD FROM IO TO ACIOS                     
         B     XIT                                                              
*                                                                               
***********************************************************************         
*           DATAMGR CALLS ON THE ACCOUNT FILE                                   
***********************************************************************         
*                                                                               
ACCHIGH  NTR1                                                                   
         MVC   KEYSAVE,KEY                                                      
         MVC   COMMAND,=CL8'DMRDHI'                                             
         TM    SVTRACE,X'80'                                                    
         BZ    ACCDRCT                                                          
         LA    R0,KEYSAVE          SET TRACE PARAMETERS                         
         ST    R0,TRIO1                                                         
         MVI   TRIO1,ACCORLEN                                                   
         MVC   TRIO2,AIO                                                        
         MVI   TRIO2,ACCORLEN                                                   
         B     ACCDRCT                                                          
*                                                                               
ACCSEQ   NTR1                                                                   
         MVC   COMMAND,=CL8'DMRSEQ'                                             
         TM    SVTRACE,X'80'                                                    
         BZ    ACCDRCT                                                          
         MVC   TRIO1,AIO                                                        
         MVI   TRIO1,ACCORLEN                                                   
         XC    TRIO2,TRIO2                                                      
*                                                                               
ACCDRCT  DS    0H                                                               
         MVC   DMFILE,=CL8'ACCDIR'                                              
         GOTO1 DATAMGR,DMCB,(DMINBTS,COMMAND),DMFILE,KEY,AIO                    
*                                                                               
         L     R6,AIO                                                           
         USING ACCKEY,R6                                                        
         MVC   KEY(L'ACCKEY),ACCKEY                                             
         MVC   KEYSTATS,ACCKSTA                                                 
         MVC   KEYDSKAD,ACCKDA                                                  
         DROP  R6                                                               
*                                                                               
         TM    SVTRACE,X'80'                                                    
         BZ    *+8                                                              
         BAS   RE,ACCTRACE                                                      
*                                                                               
         TM    DMCB+8,X'02'        RECORD DELETED?                              
         BNZ   ACCD10                                                           
         CLI   DMCB+8,0            NO, SOME OTHER ERROR?                        
         BE    ACCD10                                                           
         DC    H'0'                    YES, THEN DIE                            
*                                                                               
ACCD10   MVC   DMFILE,=CL8'ACCMST'                                              
         MVC   COMMAND,=CL8'GETREC'                                             
         GOTO1 DATAMGR,DMCB,(DMINBTS,COMMAND),DMFILE,KEYDSKAD,AIO,     X        
               DMWORK                                                           
*                                                                               
         TM    DMCB+8,X'02'        RECORD DELETED?                              
         BNZ   ACCFX                                                            
         CLI   DMCB+8,0            NO, SOME OTHER ERROR?                        
         BE    ACCFX                                                            
         DC    H'0'                    YES, THEN DIE                            
*                                                                               
ACCFX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE SHOWS THE COMMAND, FILENAME, KEY, AND KEY FOUND ON A             
* CALL TO DATAMGR                                                               
*                                                                               
* ON ENTRY:    TRIO1 - BYTE 0      LENGTH OF ORIGINAL KEY                       
*                      BYTES 1-3   A(ORIGINAL KEY)                              
*                                                                               
*              TRIO2 - (0)         DOING A DMSEQ, ORIGINAL KEY IS KEY           
*                      BYTE 0      LENGTH OF KEY FOUND                          
*                      BYTES 1-3   A(KEY FOUND BY DATAMGR ON A DMRDHI)          
***********************************************************************         
*                                                                               
ACCTRACE NTR1                                                                   
         MVC   P(6),COMMAND                                                     
         MVC   P+8(7),DMFILE                                                    
*                                                                               
         LA    R4,P+16             KEY                                          
         ZIC   R0,TRIO1                                                         
         GOTO1 HEXOUT,DMCB,TRIO1,(R4),(R0),=C'TOG'                              
*                                                                               
         OC    TRIO2,TRIO2         SEQUENTIAL READ?                             
         BZ    ACCTRAC5            YES, SHOWN KEY ALREADY                       
         LA    R4,P2+16                                                         
         ZIC   R0,TRIO2            NO, SHOW KEY WE FOUND ON DMRDHI              
         GOTO1 HEXOUT,DMCB,TRIO2,(R4),(R0),=C'TOG'                              
*                                                                               
ACCTRAC5 GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
ACCTRACX B     XIT                 RETURN TO CALLER                             
         EJECT                                                                  
***********************************************************************         
*           ERROR ROUTINES                                                      
***********************************************************************         
*                                                                               
BADCLT   GOTO1 VPRTRULE,PARAS,0                                                 
         MVC   P+1(29),=CL29'** ERROR - BAD CLIENT CODE **'                     
         B     ACCTRAC5                                                         
*                                                                               
BADPRD   GOTO1 VPRTRULE,PARAS,0                                                 
         MVC   P+1(30),=CL30'** ERROR - BAD PRODUCT CODE **'                    
         B     ACCTRAC5                                                         
         EJECT                                                                  
***********************************************************************         
*           BUFFALO CALLS                                                       
*                                                                               
* NOTE: R0 GETS CLOBBERED                                                       
***********************************************************************         
*                                                                               
BUFFPUT  MVC   COMMAND(8),=CL8'BPUT'                                            
         B     GOBUFFER                                                         
*                                                                               
BUFFGET  MVC   COMMAND(8),=CL8'BGET'                                            
*                                                                               
GOBUFFER LR    R0,RE               SAVE CALLING REG                             
         GOTO1 VBUFFALO,DMCB,COMMAND+1,BUFFBUFF,BUFFREC,1                       
         LR    RE,R0               RESTORE CALLING REG                          
*                                                                               
         TM    SVTRACE,X'40'       TEST TRACE BUFFALO                           
         BZR   RE                                                               
*                                                                               
BUFFTRC  NTR1                                                                   
         MVC   WORK(16),DMCB       SAVE DMCB                                    
*                                                                               
         MVC   P(5),COMMAND                                                     
*                                                                               
         L     R4,BUFFRULE                                                      
         LA    R5,QRNODE-QRD(R4)                                                
         GOTO1 HEXOUT,DMCB,(R5),P+7,4,=C'TOG'                                   
*                                                                               
         LA    R5,QRCODE-QRD(R4)                                                
         MVC   P+17(8),0(R5)                                                    
*                                                                               
         GOTO1 HEXOUT,DMCB,BUFFREC,P+27,32,=C'TOG'                              
*                                                                               
         GOTO1 HEXOUT,DMCB,WORK+8,P+92,1,=C'TOG'                                
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         MVC   DMCB(16),WORK       RESTORE DMCB                                 
         B     XIT                                                              
         EJECT                                                                  
BUFFBUFF DS    A                                                                
RELO     DS    A                                                                
*                                                                               
XITYES   SR    RC,RC                                                            
XITNO    LTR   RC,RC                                                            
XIT      XIT1                                                                   
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
XFF      DC    16X'FF'                                                          
*                                                                               
COREMODS DS    0C                  TABLE OF CORE-RESIDENT MODULES               
         DC    X'8485'                                                          
CORES    EQU   (*-COREMODS)                                                     
*                                                                               
* TABLE OF VALID EXTRACT TYPES FOR ESTIMATE EXTRACT                             
*                                                                               
EXTTBL   DS    0CL3                                                             
         DC    AL1(EXORD),AL1(EXORDB),AL1(0)                                    
         DC    AL1(EXBADV),AL1(EXBADVB),AL1(0)                                  
         DC    AL1(EXBDATE),AL1(EXBDATB),AL1(0)                                 
         DC    AL1(EXORDN),AL1(EXORDNB),AL1(0)                                  
         DC    AL1(EXBNADV),AL1(EXBNADVB),AL1(0)                                
         DC    AL1(EXBNDATE),AL1(EXBNDATB),AL1(0)                               
         DC    AL1(EXBINV),AL1(EXBINVB),AL1(0)                                  
         DC    AL1(EXBNINV),AL1(EXBNINVB),AL1(0)                                
         DC    AL1(EXBAADV),AL1(0),AL1(EXBAADVB)                                
         DC    AL1(EXBADAT),AL1(0),AL1(EXBADATB)                                
         DC    AL1(EXBAINV),AL1(0),AL1(EXBAINVB)                                
EXTYPES  EQU   (*-EXTTBL)/L'EXTTBL                                              
*                                                                               
VBUFFALO DS    V                                                                
*                                                                               
ACIOS    DS    (ADCONS)CL1000      SAVED RECORDS                                
*                                                                               
GOBUFFL  EQU   400                                                              
GOBUFF   DC    (GOBUFFL)X'00'                                                   
*                                                                               
JOBIO    DC    2000X'00'                                                        
*                                                                               
         BUFF  LINES=250,ROWS=1,COLUMNS=3,FLAVOR=PACKED,               X        
               KEYLIST=(8,A)                                                    
*                                                                               
         EJECT                                                                  
* DSECT TO COVER MODULE WORKING STORAGE                                         
*                                                                               
ACWKD    DSECT                                                                  
ADLIST   DS    0A                  ADCON LIST                                   
ADCOMP   DS    A                   A(COMPANY RECORD)                            
ADPRLDGR DS    A                   A(PRODUCT LEDGER RECORD)                     
ADCLTREC DS    A                   A(CLIENT RECORD)                             
ADPRDREC DS    A                   A(PRODUCT RECORD)                            
ADCONS   EQU   (*-ADLIST)/4                                                     
*                                                                               
CORETAB  DS    0A                  CORE RESIDENT PHASES                         
GETOPT   DS    A                   T00A84                                       
JOBBER   DS    A                   T00A85                                       
*                                                                               
AACIOS   DS    A                   A(ACIOS)                                     
ADLDGHIR DS    A                   A(LEDGER ELEMENT)                            
AFSTRULE DS    A                                                                
AFSTEST  DS    A                   A(FIRST WORKCODE ESTIMATE ELEMENT)           
*                                                                               
KEYSTATS DS    XL(L'ACCKSTA)       THE KEY'S STATUS BYTES                       
KEYDSKAD DS    XL(L'ACCKDA)                  DISK ADDRESS                       
*                                                                               
COMPANY  DS    XL1                 COMPANY CODE                                 
PRODLDGR DS    CL2                 PRODUCT LEDGER                               
RECVLDGR DS    CL2                 RECEIVABLE LEDGER                            
*                                                                               
BITFLAG1 DS    XL1                 VARIOUS BIT FLAGS                            
B1ALLPRD EQU   X'80'                   PRODUCT  = 'ALL'                         
B1MEDOKY EQU   X'40'                   MEDIA FILTER SATISFIED                   
B1ESTOKY EQU   X'20'                   ESTIMATE FILTER SATISFIED                
*                                                                               
PRVACCCD DS    CL12                PREVIOUS ACCOUNT CODE                        
PRVPRDCD DS    CL3                          PRODUCT CODE                        
PRVMEDCD DS    CL2                          MEDIA CODE                          
PRVESTCD DS    CL6                          ESTIMATE CODE                       
*                                                                               
ESTPER   DS    XL3                 JOB OPEN DATE PERIOD (FOR ESTIMATES)         
BATCHPER DS    XL2                 BATCH HEADER MONTH PERIOD                    
DATPER   DS    XL2                 PERIOD FOR BILL RUN ON DATE                  
INVPER   DS    XL2                 INVOICE DATE PERIOD                          
RCCOMPFL DS    CL1                                                              
*                                                                               
MOSTAB   DS    12CL4               2 BYTE BINARY YYMM/2 BYTE MOS                
STARTPER DS    XL2                     START PERIOD (YYMM)                      
ENDPER   DS    XL2                     END PERIOD (YYMM)                        
DATAINDS DS    0XL2                                                             
DATAIND  DS    X                                                                
DATAIND1 DS    X                                                                
NEXTYPS  DS    X                   N'EXTRACT TYPES                              
EXTYPS   DS    CL(MAXDTYP)                                                      
*                                                                               
WKCODE   DS    CL2                                                              
FILTSW   DS    CL1                                                              
SIGN     DS    C                                                                
CLTFIL   DS    CL5                 CLIENT FILTERS (AF1 - AF5)                   
PRDFIL   DS    CL5                 PRODUCT FILTERS                              
JOBFIL   DS    CL5                                                              
COMPFIL  DS    CL5                                                              
PL13     DS    PL13                                                             
ACNET    DS    PL8                                                              
ACGROSS  DS    PL8                                                              
CASHD    DS    PL6                                                              
STKEY    DS    CL15                START KEY C/U/L/CLI/PRD/(M)                  
STKEYLN  DS    CL1                 LENGTH OF STKEYLN FOR EX                     
STJOB    DS    CL1                 DISP. TO JOB FIELD                           
STJOBLN  DS    CL1                 LENGTH OF JOB FILED                          
STPRDLN  DS    CL1                 COMPARE LENGTH FOR CLIENT/PRODUCT            
LENREST  DS    CL1                 L'KEY AFTER MEDIA                            
LCLTPRD  DS    CL6                 LAST CLIENT/PRODUCT                          
*                                                                               
ACOLIST  DS    A                                                                
ACOLTAB  DS    A                                                                
LCOLTAB  DS    F                                                                
AOPVTAB  DS    A                                                                
LOPVTAB  DS    F                                                                
AGOBUFF  DS    A                                                                
LGOBUFF  DS    F                                                                
AJOBIO   DS    A                                                                
AGOBLOCK DS    A                                                                
VJOBCOL  DS    V                                                                
         EJECT                                                                  
       ++INCLUDE ACGOBLOCK                                                      
         EJECT                                                                  
       ++INCLUDE ACJOBBLOCK                                                     
ACWKX    EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE ACJOBBERD                                                      
         EJECT                                                                  
       ++INCLUDE BUFILWORKD                                                     
         EJECT                                                                  
       ++INCLUDE BUEXTWORKD                                                     
         EJECT                                                                  
*  BUPPERD                                                                      
         PRINT OFF                                                              
       ++INCLUDE BUPPERD                                                        
         PRINT ON                                                               
*  DDCOMFACS                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
*  ACGENFILE                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
       ++INCLUDE DDBUFFALOD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'075BUFIL45   05/01/02'                                      
         END                                                                    
