*          DATA SET ACREC02    AT LEVEL 062 AS OF 07/18/19                      
*PHASE T60802A                                                                  
*INCLUDE CATCALL                                                                
ACREC02  TITLE '- U.S. OVERLAY TO HANDLE HEADER SCREEN AND POSTINGS'            
***********************************************************************         
*  ID  LVL   DATE    TICKET            COMMENTS                       *         
* ---- --- ------- ------------ --------------------------------------*         
* JSAY 061 13MAR18 <SPEC-21208> RELINK FOR NEW TSAR FIELD TSARBTYC    *         
* JSAY 062 19JUL19 <SPEC-36076> RELINK FOR NEW TSAR FIELD TSARPOEL    *         
***********************************************************************         
ACREC02  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**REC2**,RA,R9,RR=RE                                           
         LR    RC,R1                                                            
         USING WORKD,RC                                                         
         LH    R8,=Y(OVERWORK-WORKD)                                            
         LA    R8,WORKD(R8)                                                     
         USING OVRWRKD,R8                                                       
         USING SAVED,R7                                                         
         USING TWAD,R6                                                          
         L     R0,=V(CATCALL)      RELOCATE ADDRESS OF CATCALL                  
         AR    R0,RE                                                            
         ST    R0,VCATCALL                                                      
         L     R0,=A(VALHED)                                                    
         AR    R0,RE                                                            
         ST    R0,AVALHED                                                       
         L     R0,=A(UPDATE)                                                    
         AR    R0,RE                                                            
         ST    R0,AUPDATE                                                       
         MVC   DEPTUL,=C'2D'                                                    
         MVC   STAFUL,=C'2P'                                                    
         MVC   EXPAUL,=C'28'                                                    
         MVC   CLIAUL,=C'29'                                                    
         MVC   COST2,=C'12'                                                     
         MVC   COST3,=C'13'                                                     
         MVC   COSTP,=C'1P'                                                     
         MVC   WOFFLST(WOFFLSTL),WOFFULS                                        
         MVC   DISCLST(DISCLSTL),DISCULS                                        
         MVC   NINES,=C'999999999999'                                           
         SPACE 2                                                                
         SR    RF,RF                                                            
         IC    RF,ACTION                                                        
         SLL   RF,2                                                             
         B     *+4(RF)                                                          
         B     SETLST              INITIALISE                                   
         B     NTRVAL              HEADER                                       
         B     EXIT                INPUT                                        
         B     EXIT                SPECIAL                                      
         B     RESHED              RESTORE (HEADER)                             
         B     EXIT                                                             
         B     NTRUPD              UPDATE                                       
         B     NTRUPD              DRAFT UPDATE                                 
         B     NTRUPD              FILTERED DRAFT UPDATE                        
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     NTRVAL              CHANGE (HEADER)                              
         B     RELAST              RESTORE HEADER (AFTER UPDATE)                
*                                                                               
NTRVAL   L     RF,AVALHED          CALL VALHED MODULE                           
         LA    R1,WORKD                                                         
         BASR  RE,RF                                                            
         B     EXIT                                                             
NTRUPD   L     RF,AUPDATE          CALL UPDATE MODULE                           
         LA    R1,WORKD                                                         
         BASR  RE,RF                                                            
         B     EXIT                                                             
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* INITIALISE - SET Y-TYPES FOR VALID LEDGER LISTS                     *         
***********************************************************************         
         SPACE 1                                                                
SETLST   DS    0H                                                               
         GOTO1 VDICTATE,DMCB,C'LU  ',DINLSTU,DOUTLSTU                           
         GOTO1 (RF),(R1),C'LL  ',DINLSTL,DOUTLSTL                               
         GOTO1 (RF),(R1),C'LL  ',TINLST,TOUTLST                                 
         MVI   TOTALSX,EOT                                                      
         MVC   BANKLIST,=Y(BANKULS-ACREC02)                                     
         MVC   DISCLIST,=Y(DISCULS-ACREC02)                                     
         MVC   RECVLIST,=Y(RECVULS-ACREC02)                                     
         MVC   WOFFLIST,=Y(WOFFULS-ACREC02)                                     
         MVC   SHEADTOT,=Y(RECHTOTH-RECOLAYH)                                   
         MVC   SHEADPFK,=Y(RECHPFKH-RECOLAYH)                                   
         MVC   SRECWACT,=Y(RECWACTH-RECOLAYH)                                   
         MVC   SRECWAC,=Y(RECWACH-RECOLAYH)                                     
         MVC   SRECWRFT,=Y(RECWRFTH-RECOLAYH)                                   
         MVC   SRECWRF,=Y(RECWRFH-RECOLAYH)                                     
         MVC   SRECWDTT,=Y(RECWDTTH-RECOLAYH)                                   
         MVC   SRECWDT,=Y(RECWDTH-RECOLAYH)                                     
         MVC   SRECWNRT,=Y(RECWNRTH-RECOLAYH)                                   
         MVC   SRECWNR,=Y(RECWNRH-RECOLAYH)                                     
         MVC   SRECODTT,=Y(RECODTTH-RECOLAYH)                                   
         MVC   SRECODT,=Y(RECODTH-RECOLAYH)                                     
         MVC   SRECDSCT,=Y(RECDSCTH-RECOLAYH)                                   
         MVC   SRECDSC,=Y(RECDSCH-RECOLAYH)                                     
SETLSTX  B     EXIT                                                             
         SPACE 2                                                                
DISCULS  DS    0CL2                VALID LEDGERS FOR DISCOUNT                   
         DC    C'SE'                                                            
         DC    C'SI'                                                            
         DC    AL1(EOT)                                                         
*                                                                               
RECVULS  DC    C'SR'               VALID LEDGERS FOR RECEIVABLES                
         DC    C'SA'                                                            
         DC    C'SB'                                                            
         DC    AL1(EOT)                                                         
*                                                                               
WOFFULS  DS    0CL2                VALID LEDGERS FOR WRITE-OFF                  
         DC    C'SA'                                                            
         DC    C'SB'                                                            
         DC    C'SC'                                                            
         DC    C'SE'                                                            
         DC    C'SF'                                                            
         DC    C'SI'                                                            
         DC    C'SV'                                                            
         DC    C'SW'                                                            
         DC    C'SX'                                                            
         DC    C'SY'                                                            
         DC    AL1(EOT)                                                         
*                                                                               
BANKULS  DC    C'SC'               VALID LEDGERS FOR BANK                       
         DC    C'SB'                                                            
         DC    AL1(EOT)                                                         
*                                                                               
DINLSTU  DS    0C                  ** UPPER CASE DICTIONARY **                  
         DCDDL AC#CTRD,6                                                        
         DCDDL AC#ACC,3                                                         
         DCDDL AC#DATE,4                                                        
         DCDDL AC#WRTFN,11                                                      
         DCDDL AC#INVC2,9                                                       
         DCDDL AC#XFRFR,16                                                      
         DCDDL AC#XFRTO,14                                                      
         DCDDL AC#ON,3                                                          
DINLSTUX DC    AL1(0)                                                           
*                                                                               
DINLSTL  DS    0C                  ** LOWER CASE LIST **                        
         DCDDL AC#BATD,13                                                       
         DCDDL AC#RCVA,14                                                       
         DCDDL AC#BNKA,12                                                       
         DCDDL AC#NRTV,9                                                        
         DCDDL AC#BATTS,12                                                      
         DCDDL AC#ALCTD,9                                                       
         DCDDL AC#SPCLS,8                                                       
         DCDDL AC#CHKP,13                                                       
         DCDDL AC#CHKAM,12                                                      
         DCDDL AC#WRTNF,11                                                      
         DCDDL AC#WRTFA,13                                                      
         DCDDL AC#CTRD,6                                                        
         DCDDL AC#XFRD,11                                                       
         DCDDL AC#DISS,8                                                        
         DCDDL AC#DISAC,12                                                      
         DCDDL AC#TDC,11                                                        
         DCDDL AC#BAL,7                                                         
         DCDDL AC#ITEMS,5                                                       
DINLSTLX DC    AL1(EOT)                                                         
         EJECT                                                                  
TINLST   DS    0C                                                               
         DCDDL AC#BAL,12        ** TOTAL HEADINS INPUT LIST **                  
         DCDDL AC#CHK,12                                                        
         DCDDL AC#ALCTD,12                                                      
         DCDDL AC#SPCLS,12                                                      
         DCDDL AC#DISS,12                                                       
         DCDDL AC#WRTNF,12                                                      
         DCDDL AC#CTRD,12                                                       
         DCDDL AC#XFRD,12                                                       
TINLSTX  DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* RESTORE - RESTORE HEADER SCREEN                                     *         
***********************************************************************         
         SPACE 1                                                                
RESHED   DS    0H                                                               
         OI    RECBNAMH+(FVATRB-FVIHDR),FVAPROT                                 
         OI    RECBMONH+(FVATRB-FVIHDR),FVAPROT                                 
         OI    RECBREFH+(FVATRB-FVIHDR),FVAPROT                                 
         OC    BANK,BANK           PROT BANK A/C IF PRESENT                     
         BZ    *+8                                                              
         OI    RECBNKH+(FVATRB-FVIHDR),FVAPROT                                  
         CLC   BATOFF,SPACES       PROT OFFICE IF PRESENT                       
         BNH   *+8                                                              
         OI    RECFOFFH+(FVATRB-FVIHDR),FVAPROT                                 
         OI    RECRCVH+(FVATRB-FVIHDR),FVAPROT                                  
         OI    RECRCV2H+(FVATRB-FVIHDR),FVAPROT                                 
*                                                                               
         OC    WOFF,WOFF           PROT WRITE-OFF A/C ETC. IF PRESENT           
         BZ    RESHED2                                                          
         OI    RECWACH+(FVATRB-FVIHDR),FVAPROT                                  
         OI    RECWDTH+(FVATRB-FVIHDR),FVAPROT                                  
         OI    RECWRFH+(FVATRB-FVIHDR),FVAPROT                                  
         OI    RECWNRH+(FVATRB-FVIHDR),FVAPROT                                  
         OI    RECFOFFH+(FVATRB-FVIHDR),FVAPROT                                 
         OI    RECDPTH+(FVATRB-FVIHDR),FVAPROT                                  
         OI    RECSTFH+(FVATRB-FVIHDR),FVAPROT                                  
RESHED2  CLI   WCPCNT,0            PROT W-O CLI/PRO IF PRESENT                  
         BE    *+8                                                              
         OI    RECWCPH+(FVATRB-FVIHDR),FVAPROT                                  
*                                                                               
         OI    RECDSCH+(FVATRB-FVIHDR),FVAPROT                                  
*                                                                               
         OI    RECDATH+(FVATRB-FVIHDR),FVAPROT                                  
         OI    RECBNRH+(FVATRB-FVIHDR),FVAPROT                                  
         OI    RECBSOH+(FVATRB-FVIHDR),FVAPROT                                  
RESHEDX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* LAST - RESTORE HEADER SCREEN AFTER UPDATE                           *         
***********************************************************************         
         SPACE 1                                                                
RELAST   DS    0H                                                               
         NI    RECBNAMH+(FVATRB-FVIHDR),X'FF'-FVAPROT                           
         NI    RECBMONH+(FVATRB-FVIHDR),X'FF'-FVAPROT                           
         NI    RECBREFH+(FVATRB-FVIHDR),X'FF'-FVAPROT                           
         NI    RECBNKH+(FVATRB-FVIHDR),X'FF'-FVAPROT                            
         NI    RECFOFFH+(FVATRB-FVIHDR),X'FF'-FVAPROT                           
         NI    RECRCVH+(FVATRB-FVIHDR),X'FF'-FVAPROT                            
         NI    RECRCV2H+(FVATRB-FVIHDR),X'FF'-FVAPROT                           
         NI    RECWACH+(FVATRB-FVIHDR),X'FF'-FVAPROT                            
         NI    RECWDTH+(FVATRB-FVIHDR),X'FF'-FVAPROT                            
         NI    RECWRFH+(FVATRB-FVIHDR),X'FF'-FVAPROT                            
         NI    RECWNRH+(FVATRB-FVIHDR),X'FF'-FVAPROT                            
         NI    RECDPTH+(FVATRB-FVIHDR),X'FF'-FVAPROT                            
         NI    RECSTFH+(FVATRB-FVIHDR),X'FF'-FVAPROT                            
         NI    RECWCPH+(FVATRB-FVIHDR),X'FF'-FVAPROT                            
         NI    RECDSCH+(FVATRB-FVIHDR),X'FF'-FVAPROT                            
         NI    RECDATH+(FVATRB-FVIHDR),X'FF'-FVAPROT                            
         NI    RECBNRH+(FVATRB-FVIHDR),X'FF'-FVAPROT                            
         NI    RECBSOH+(FVATRB-FVIHDR),X'FF'-FVAPROT                            
RELASTX  B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* HEADER - VALIDATE ALL HEADER SCREEN FIELDS                          *         
***********************************************************************         
         SPACE 1                                                                
VALHED   NMOD1 0,**VHED**,RA,R9,RR=RE                                           
         LR    RC,R1                                                            
         USING WORKD,RC                                                         
         SPACE 2                                                                
***********************************************************************         
* VALIDATE BATCH REFERENCE                                            *         
***********************************************************************         
         SPACE 1                                                                
VALBAT   TM    RECBREFH+(FVATRB-FVIHDR),FVAPROT                                 
         BO    VALBATX                                                          
         GOTO1 AVALBAT,RECBREFH                                                 
         BNE   VALHEDXX                                                         
VALBATX  DS    0H                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE BATCH NAME                                                 *         
***********************************************************************         
         SPACE 1                                                                
VALNAM   TM    RECBNAMH+(FVATRB-FVIHDR),FVAPROT                                 
         BO    VALNAMX                                                          
         MVI   FVMINL,1            REQUIRED FIELD                               
         GOTO1 AFVAL,RECBNAMH                                                   
         BNE   VALHEDXX            EXIT WITH ERROR SET                          
         MVC   BATNAME,FVIFLD      SAVE NAME                                    
VALNAMX  DS    0H                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE BATCH MONTH                                                *         
***********************************************************************         
         SPACE 1                                                                
VALMON   TM    RECBMONH+(FVATRB-FVIHDR),FVAPROT                                 
         BO    VALMONX                                                          
         GOTO1 AVALMON,RECBMONH                                                 
         BNE   VALHEDXX                                                         
VALMONX  DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE BANK ACCOUNT                                               *         
***********************************************************************         
         SPACE 1                                                                
VALBNK   NI    TWAMODE2,255-TWA2NALL                                            
         GOTO1 AVALBNK,RECBNKH                                                  
         BH    VALHEDXX            ERROR                                        
         BE    *+12                FIELD IS PRESENT                             
         OI    TWAMODE2,TWA2NALL   ABSENT - SET NO ALLOCATION                   
         B     VALBNKX                                                          
         GOTO1 VACSRCHC,DMCB,FVADDR,TWAD,(C'*',BANKUL),                X        
               (X'C0',RECNDSP),AIO1,(L'BANKNAME,BANKNAME)                       
VALBNKX  DS    0H                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE BANK REFERENCE NUMBER (CHEQUE NUMBER)                      *         
***********************************************************************         
         SPACE 1                                                                
VALREF   XC    BANKREF,BANKREF                                                  
         OC    BANK,BANK           TEST BANK A/C PRESENT                        
         BZ    *+8                                                              
         MVI   FVMINL,1            YES  - REQUIRED FIELD                        
         GOTO1 AFVAL,RECREFH                                                    
         BH    VALHEDXX                                                         
         BE    *+12                                                             
         OI    TWAMODE2,TWA2NALL                                                
         B     *+10                                                             
         MVC   BANKREF,FVIFLD                                                   
VALREFX  DS    0H                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE DATE CHEQUE WAS DEPOSITED                                  *         
***********************************************************************         
         SPACE 1                                                                
VALDEP   OC    BANK,BANK           TEST BANK A/C PRESENT                        
         BZ    *+8                                                              
         MVI   FVMINL,1            YES  - REQUIRED FIELD                        
         GOTO1 AVALDEP,RECDEPH                                                  
         BH    VALHEDXX                                                         
         BE    VALDEPX                                                          
         OI    TWAMODE2,TWA2NALL                                                
VALDEPX  DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE CHEQUE (CONTROL) AMOUNT                                    *         
***********************************************************************         
         SPACE 1                                                                
VALCHQ   ZAP   CHQAMT,PZERO                                                     
         OC    BANK,BANK           TEST BANK A/C PRESENT                        
         BZ    *+8                                                              
         MVI   FVMINL,1            YES  - REQUIRED FIELD                        
         GOTO1 AFVAL,RECCHQH                                                    
         BH    VALHEDXX            ERROR                                        
         BE    *+12                FIELD IS PRESENT                             
         OI    TWAMODE2,TWA2NALL   ABSENT - SET NO ALLOCATION                   
         B     VALCHQ2                                                          
         MVC   FVMSGNO,=AL2(EAAMTINV)                                           
         SR    R0,R0                                                            
         IC    R0,FVILEN                                                        
         GOTO1 VCASHVAL,DMCB,(X'82',RECCHQ),(R0)                                
         CLI   0(R1),0                                                          
         BNE   VALHEDXX                                                         
         ZAP   CHQAMT,4(8,R1)                                                   
VALCHQ2  CURED CHQAMT,(L'RECCHQ,RECCHQ),2,ALIGN=LEFT,FLOAT=-                    
         OI    RECCHQH+(FVOIND-FVIHDR),FVOXMT                                   
VALCHQX  DS    0H                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE CHEQUE DATE                                                *         
***********************************************************************         
         SPACE 1                                                                
VALCDT   OC    BANK,BANK           TEST BANK A/C PRESENT                        
         BZ    *+8                                                              
         MVI   FVMINL,1            YES  - REQUIRED FIELD                        
         GOTO1 AVALCDT,RECCDTH                                                  
         BH    VALHEDXX                                                         
         BE    VALCDTX                                                          
         OI    TWAMODE2,TWA2NALL                                                
VALCDTX  DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE RECEIVING ACCOUNT(S) *NEED NOT BE IN RECEIVABLE LEDGER*              
***********************************************************************         
         SPACE 1                                                                
VALRCV   TM    RECRCVH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALRCVX                                                          
*                                                                               
         NI    OVRBYTE,X'FF'-(OVRCLI+OVRRCVL+OVRCLV+OVRCGD+OVR2NDLN)            
*                                                                               
         L     R0,ASCANOUT         CLEAR SCANNER OUTPUT BLOCK                   
         LA    R1,SCANLTAB                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R0,RCVFLDS          CLEAR TEMPORARY FIELD HEADERS                
         LA    R1,RCVFLDLQ                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   RCVFLDH,RECRCVH     SAVE OFF FIELD HEADER INFO                   
         LA    RE,RCVFLDH                                                       
         MVI   0(RE),RCVFLDLN      SET VALID LENGTH                             
         MVI   5(RE),0             INIT INPUT LENGTH                            
         MVC   RCVFLD2H,RECRCV2H   SAVE OFF FIELD HEADER INFO                   
         LA    RE,RCVFLD2H                                                      
         MVI   0(RE),RCVFLDLN      SET VALID LENGTH                             
         MVI   5(RE),0             INIT INPUT LENGTH                            
*                                                                               
         LA    RE,RCVFLDH                                                       
         ST    RE,SVFADDH          SAVE OFF THE ADDRESS OF THE HEADER           
         LA    RE,RCVFLD                                                        
         ST    RE,SVFADD           SAVE OFF THE ADDRESS OF THE FIELD            
         MVI   SVLEN,0             INITIALIZE SAVE AREA FOR LENGTH              
         MVC   LASTCLI,SPACES                                                   
*                                                                               
         LA    R2,RECRCVH                                                       
         B     VALRC15                                                          
VALRC10  TM    OVRBYTE,OVR2NDLN    HAVE WE DONE THE 2ND LINE ALREADY?           
         BO    VALRC150                                                         
         LA    R2,RECRCV2H                                                      
         OI    OVRBYTE,OVR2NDLN    SHOW THAT WE ARE DOING THE 2ND LINE          
VALRC15  CLI   5(R2),0             ANY DATA                                     
         BE    VALRC150                                                         
         GOTO1 VSCANNER,DMCB,(L'SCANTXT2,(R2)),                        *        
               ('RECVMAXN',ASCANOUT),C',=  '                                    
         SR    R0,R0                                                            
         ICM   R0,1,4(R1)                                                       
         BZ    VALRC150                                                         
*                                                                               
         USING SCANOUTD,R4                                                      
         L     R4,ASCANOUT                                                      
VALRC20  SR    R1,R1                                                            
         IC    R1,SCANLEN1                                                      
         CHI   R1,L'AC@CLI         MAX LENGTH FOR CLC IS L'AC#CLI               
         BNH   *+8                                                              
         LA    R1,L'AC@CLI                                                      
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SCANTXT1(0),AC@CLI                                               
         BNE   VALRC140                                                         
         CLC   SCANTXT2,SPACES     DID THEY ENTER A CLI=STATEMENT?              
         BNH   VALRC160                                                         
         OI    OVRBYTE,OVRCLI+OVRRCVL    SHOW THAT WE HAVE AN OVERRIDDE         
*                                                                               
         NI    OVRBYTE,X'FF'-OVRCLN                                             
         NI    OVRBYT2,X'FF'-OVRDNE                                             
*                                                                               
         USING ACTRECD,R2                                                       
         SR    R1,R1                                                            
         IC    R1,PRODALEN                                                      
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   LASTCLI(0),SCANTXT2    READ FOR DEFAULT CLI LEV ACT              
         BE    VALRC30                                                          
         MVC   LASTCLI,SPACES                                                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LASTCLI(0),SCANTXT2    SAVE OFF CLIENT FOR LATER CLCS            
*                                                                               
         LA    R2,KEY              GET CLIENT LEVEL INFO                        
         MVC   KEY,SPACES                                                       
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(L'PRODUL),PRODUL                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ACTKACT(0),SCANTXT2    READ FOR DEFAULT CLI LEV ACT              
         GOTO1 AIOREAD                                                          
         BE    *+14                                                             
         MVC   LASTCLI,SPACES                                                   
         B     VALRC140                                                         
         OI    OVRBYTE,OVRCLV+OVRCGD      CLI IS GOOD AND DOING CLI LV          
         B     VALRC60                                                          
*                                                                               
VALRC30  LA    R2,KEY              GET CLIENT LEVEL INFO                        
         MVC   KEY,SPACES                                                       
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(L'PRODUL),PRODUL                                         
         SR    R1,R1                                                            
         IC    R1,SCANLEN2                                                      
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ACTKACT(0),SCANTXT2                                              
         CLC   PRODALEN,SCANLEN2                                                
         BL    VALRC50                                                          
         B     *+14                                                             
VALRC40  L     R3,AIO1                                                          
         MVC   KEY(ACTKEND),0(R3)  RESET KEY                                    
         LA    RE,ACTKACT                                                       
         SR    RF,RF                                                            
         IC    RF,PRODBLEN                                                      
         AR    RE,RF                                                            
         MVI   0(RE),X'FF'                                                      
VALRC50  GOTO1 AIOHIGH                                                          
         L     R3,AIO1                                                          
         LA    RE,ACTKACT-ACTRECD  OVERHEAD                                     
         SR    R1,R1                                                            
         IC    R1,PRODALEN         LENGTH OF CLIENT CODE                        
         SR    RF,RF                                                            
         IC    RF,SCANLEN2                                                      
         CR    RF,R1                                                            
         BNH   *+8                                                              
         IC    R1,PRODBLEN         LENGTH OF CLIENT+PRODUCT                     
         AR    R1,RE                                                            
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   KEY(0),0(R3)                                                     
         BE    VALRC55                                                          
         TM    OVRBYTE,OVRCLN      DID WE GET A PRODUCT?                        
         BO    VALRC140            YES - CONTINUE ON                            
         OI    OVRBYT2,OVRDNE      SHOW THAT WE ARE DONE READING                
         B     VALRC97             ADD DEFAULT SR ACCOUNT TO LIST               
*                                                                               
VALRC55  OI    OVRBYTE,OVRCLN      SHOW THAT WE HAVE A PRODUCT                  
VALRC60  L     R3,AIO1             PROCESS LEDGER RECORD & BUILD ENTRY          
         AH    R3,DATADISP                                                      
*                                                                               
VALRC70  CLI   0(R3),0             EOR                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
VALRC80  CLI   0(R3),PPRELQ        X'24' - PRODUCTION PROFILE                   
         BE    VALRC90                                                          
         IC    R1,1(R3)                                                         
         AR    R3,R1                                                            
         B     VALRC70                                                          
*                                                                               
         USING PPRELD,R3                                                        
VALRC90  TM    OVRBYTE,OVRCLV      ARE WE AT THE CLIENT LEVEL                   
         BNO   VALRC95                                                          
         NI    OVRBYTE,X'FF'-OVRCLV                                             
         MVC   SVDFACT,PPRRECVA    SAVE OFF THE RECEIVABLE ACCOUNT              
         B     VALRC30                                                          
*                                                                               
VALRC95  LA    R1,PPRRECVA                                                      
         CLC   PPRRECVA,SPACES                                                  
         BH    VALRC100                                                         
VALRC97  CLC   SVDFACT,SPACES                                                   
         BH    *+6                                                              
         DC    H'0'                                                             
         LA    R1,SVDFACT                                                       
VALRC100 MVC   PRDACT,0(R1)                                                     
*                                                                               
         LA    R1,L'PRDACT                 INIT LENGTH TO MAX                   
         LA    RE,PRDACT+L'PRDACT-1        FIND FIELD LENGTH                    
         CLI   0(RE),X'40'                 ANYTHING SIGNIFICANT?                
         BH    *+12                                                             
         AHI   RE,-1                                                            
         BCT   R1,*-12                                                          
         L     RF,SVFADD                                                        
         SR    RE,RE                                                            
         IC    RE,SVLEN                                                         
         SR    RF,RE               CHECK TO SEE IF ACCOUNT IS IN FIELD          
         AHI   R1,-1                                                            
VALRC110 CLI   0(RF),0                                                          
         BE    VALRC120            NO ACCOUNT FOUND-JUST ADD                    
*        EX    R1,*+8                                                           
*        B     *+10                                                             
*        CLC   0(0,RF),PRDACT                                                   
         CLC   PRDACT,0(RF)                                                     
         BE    VALRC135            ACCOUNT ALREADY IN RCVFLD                    
         LA    RF,1(RF)                                                         
         B     VALRC110                                                         
*                                                                               
VALRC120 DS    0H                                                               
         SR    RE,RE               MAKE SURE THERE'S ENOUGH ROOM                
         IC    RE,SVLEN                                                         
         LA    RF,L'RCVFLD                                                      
         SR    RF,RE                                                            
         CR    R1,RF               IF ENTRY IS TOO BIG CLOSE LINE               
         BL    VALRC130            AND BUMP POINTERS TO NEXT LINE               
         L     RE,SVFADDH                                                       
         MVC   5(L'SVLEN,RE),SVLEN                                              
         LA    RE,RCVFLD2H                                                      
         C     RE,SVFADDH          HAVE WE DONE THIS ALREADY?                   
         BE    VALRC160                                                         
         LA    RE,RCVFLD2H                                                      
         ST    RE,SVFADDH          SAVE OFF THE ADDRESS OF THE HEADER           
         LA    RE,RCVFLD2                                                       
         ST    RE,SVFADD           SAVE OFF THE ADDRESS OF THE FIELD            
         MVI   SVLEN,0             RE-INIT SVLEN                                
*                                                                               
VALRC130 L     RF,SVFADD                                                        
         LA    RE,PRDACT                                                        
*        AHI   R1,-1               DONE ABOVE                                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),0(RE)       RE=A(PPRRECVA OR SVDFACT)                    
         LA    RF,1(R1,RF)                                                      
         MVI   0(RF),C','          ASSIGNED SEPARATOR                           
         LA    RF,1(RF)            BUMP PAST SEPARATOR                          
         ST    RF,SVFADD                                                        
         AHI   R1,2                                                             
         SR    RE,RE                                                            
         IC    RE,SVLEN                                                         
         AR    RE,R1                                                            
         STC   RE,SVLEN                                                         
*                                                                               
VALRC135 TM    OVRBYT2,OVRDNE                                                   
         BNO   VALRC40                                                          
*                                                                               
VALRC140 LA    R4,SCANOUTL(R4)     TAKE NEXT ENTRY                              
         BCT   R0,VALRC20                                                       
         TM    OVRBYTE,OVRRCVL     DID WE GET A CLI FILTER?                     
         BNO   VALRC10                                                          
         L     RE,SVFADDH                                                       
         MVC   5(L'SVLEN,RE),SVLEN                                              
         B     VALRC10                                                          
*                                                                               
VALRC150 LA    R2,RECRCVH          RESET R2                                     
         ST    R2,FVADDR                                                        
         TM    OVRBYTE,OVRCLI      DID WE GET A CLI FILTER?                     
         BO    VALRC160                                                         
*                                                                               
         GOTO1 AVALRCV,DMCB,RECRCVH,RECRCV2H                                    
         BNE   VALHEDXX                                                         
         B     VALRCVX                                                          
*                                                                               
VALRC160 TM    OVRBYTE,OVRCGD      WAS THE CLIENT FILTER GOOD                   
         BO    VALRC170                                                         
         LA    R2,RECRCVH          RESET R2                                     
         ST    R2,FVADDR                                                        
         MVC   FVMSGNO,=AL2(EGINVCLI)                                           
         B     VALHEDE                                                          
VALRC170 GOTO1 AVALRCV,DMCB,RCVFLDH,RCVFLD2H                                    
         BNE   VALHEDXX                                                         
*                                                                               
VALRCVX  DS    0H                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE DISCOUNT ACCOUNT - ALWAYS, IF VALIDATING WRITE-OFF A/C     *         
***********************************************************************         
         SPACE 1                                                                
VALDSC   TM    RECWACH+(FVATRB-FVIHDR),FVAPROT                                  
         BNO   *+12                MUST VALIDATE IF WOFF MAY CHANGE             
         TM    RECDSCH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALDSCX                                                          
         GOTO1 AVALDSC,RECDSCH                                                  
         BH    VALHEDXX                                                         
         BL    VALDSCX                                                          
         CLC   DISCULSI,DISC                                                    
         BNE   VALDSC2                                                          
         OI    DISCINDS,DISCISI    SET DISCOUNT A/C IS INCOME                   
         CLC   SIMP,DISC           TEST SIMP                                    
         BE    *+14                                                             
         CLC   SIMD,DISC           TEST SIMD                                    
         BNE   VALDSC4                                                          
         MVI   DISCCOST,C' '       CLEAR ANY ANALYSIS FLAGS                     
         XC    DISCANAL,DISCANAL   AND ANALYSIS A/C                             
         NI    DISCSTAT,X'FF'-RSTSEADD-RSTSGPEI                                 
         B     VALDSC4                                                          
*                                                                               
VALDSC2  CLC   DISCULSE,DISC                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    DISCINDS,DISCISE    SET DISCOUNT A/C IS EXPENSE                  
*                                                                               
VALDSC4  GOTO1 VACSRCHC,DMCB,FVADDR,TWAD,0,                            X        
               (X'C0',RECNDSP),AIO1,(L'DISCNAME,DISCNAME)                       
         TM    COMPSTA5,CPYSNCST   TEST NEW COSTING                             
         BZ    VALDSCX                                                          
         TM    DISCINDS,DISCISE    TEST DISCOUNT TO EXPENSE                     
         BZ    VALDSCX                                                          
         LA    R1,CATBLK                                                        
         USING CATD,R1                                                          
         XC    CATD(CATLNQ),CATD                                                
         MVC   CATDMGR,VDATAMGR    BUILD CONTROL BLOCK                          
         MVC   CATSEAC+(ACTKCPY-ACTRECD)(L'ACTKCPY),COMPANY                     
         MVC   CATSEAC+(ACTKUNT-ACTRECD)(L'DISC),DISC                           
         MVC   CATOFF,NINES                                                     
         MVC   CATDPT,NINES                                                     
         GOTO1 VCATCALL,CATD                                                    
         CLI   CATERR,0                                                         
         BE    VALDSC6                                                          
         MVC   FVMSGNO,=AL2(EAACCINV)                                           
         MVC   FVXTRA,SPACES                                                    
         MVC   FVXTRA(L'CATACC3-L'ACTKCPY),CATACC3+L'ACTKCPY                    
         B     VALHEDE             SET CC=NOT EQUAL ON EXIT                     
*                                                                               
VALDSC6  CLI   CATPST,C'N'         NO COSTING POSTING                           
         BNE   *+12                                                             
         OI    DISCINDS,DISCINCP                                                
         B     *+8                                                              
         OI    DISCINDS,DISCIYCP                                                
*                                                                               
VALDSCX  OC    DISC,DISC                                                        
         BZ    *+8                                                              
         OI    DISCINDS,DISCIADD   SET DON'T ADD DISCOUNT TO BALANCE            
         EJECT                                                                  
***********************************************************************         
* VALIDATE WRITE-OFF ACCOUNT - ALWAYS IN CONJUNCTION WITH VALDSC      *         
***********************************************************************         
         SPACE 1                                                                
VALWAC   TM    RECWACH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALWACX                                                          
         GOTO1 AVALWAC,RECWACH                                                  
         BH    VALHEDXX                                                         
         BL    VALWACX                                                          
         TM    TWAAUTH,ACTIAUT8    TEST AUTHORISED TO WRITE-OFF                 
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(EASECLOC)                                           
         B     VALHEDE                                                          
         GOTO1 VACSRCHC,DMCB,FVADDR,TWAD,0,                            X        
               (X'C0',RECNDSP),AIO1,(L'WOFFNAME,WOFFNAME)                       
         MVI   WOFFINDS,WOFFISI    SET WRITE-OFF A/C IN SI                      
         CLC   WOFFULSI,WOFF                                                    
         BNE   VALWAC2                                                          
         CLC   SIMP,WOFF           TEST SIMP                                    
         BE    *+14                                                             
         CLC   SIMD,WOFF           TEST SIMD                                    
         BNE   VALWAC4                                                          
         MVI   WOFFCOST,C' '       CLEAR ANY ANALYSIS FLAGS                     
         XC    WOFFANAL,WOFFANAL   AND ANALYSIS A/C                             
         NI    WOFFSTAT,X'FF'-RSTSEADD-RSTSGPEI                                 
         B     VALWAC4                                                          
*                                                                               
VALWAC2  MVI   WOFFINDS,WOFFIREV   SET REVERSE -DR TO +CR                       
         CLC   WOFFULSA,WOFF                                                    
         BE    VALWAC4                                                          
         CLC   WOFFULSB,WOFF                                                    
         BE    VALWAC4                                                          
         CLC   WOFFULSC,WOFF                                                    
         BE    VALWAC4                                                          
         CLC   WOFFULSE,WOFF                                                    
         BNE   *+12                                                             
         OI    WOFFINDS,WOFFISE    ALSO SET SE WRITE-OFF A/C                    
         B     VALWAC4                                                          
         CLC   WOFFULSF,WOFF                                                    
         BE    VALWAC4                                                          
         MVI   WOFFINDS,WOFFIPAY   SET WOFFUL IS PAYABLES (CR/-CR)              
*                                                                               
VALWAC4  TM    COMPSTA5,CPYSNCST   TEST NEW COSTING                             
         BZ    VALWAC8                                                          
         TM    WOFFINDS,WOFFISE    TEST WRITE-OFF TO EXPENSE                    
         BZ    VALWAC8                                                          
         LA    R1,CATBLK                                                        
         USING CATD,R1                                                          
         XC    CATD(CATLNQ),CATD                                                
         MVC   CATDMGR,VDATAMGR    BUILD CONTROL BLOCK                          
         MVC   CATSEAC+(ACTKCPY-ACTRECD)(L'ACTKCPY),COMPANY                     
         MVC   CATSEAC+(ACTKUNT-ACTRECD)(L'WOFF),WOFF                           
         MVC   CATOFF,NINES                                                     
         MVC   CATDPT,NINES                                                     
         GOTO1 VCATCALL,CATD                                                    
         CLI   CATERR,0                                                         
         BE    VALWAC6                                                          
         MVC   FVMSGNO,=AL2(EAACCINV)                                           
         MVC   FVXTRA,SPACES                                                    
         MVC   FVXTRA(L'CATACC3-L'ACTKCPY),CATACC3+L'ACTKCPY                    
         B     VALHEDE             SET CC=NOT EQUAL ON EXIT                     
*                                                                               
VALWAC6  CLI   CATPST,C'N'         NO COSTING POSTING                           
         BNE   *+12                                                             
         OI    WOFFINDS,WOFFINCP                                                
         B     *+8                                                              
         OI    WOFFINDS,WOFFIYCP                                                
*                                                                               
VALWAC8  TM    DISCINDS,DISCIYCP                                                
         BZ    *+12                                                             
         TM    WOFFINDS,WOFFIYCP                                                
         BNZ   VALWAC10                                                         
*                                                                               
         TM    DISCINDS,DISCINCP                                                
         BNZ   VALWACX                                                          
         TM    WOFFINDS,WOFFINCP                                                
         BNZ   VALWACX                                                          
*                                                                               
         TM    DISCSTAT,RSTSEADD+RSTSGPEI                                       
         BZ    *+12                                                             
         TM    WOFFSTAT,RSTSEADD+RSTSGPEI                                       
         BNZ   VALWAC10                                                         
*                                                                               
         TM    WOFFINDS,WOFFISE    TEST WRITE-OFF A/C IS EXPENSE                
         BZ    VALWACX                                                          
         TM    DISCINDS,DISCISE    TEST DISCOUNT A/C IS EXPENSE, TOO            
         BZ    VALWACX                                                          
         CLI   DISCCOST,C' '                                                    
         BNH   VALWACX                                                          
         CLI   WOFFCOST,C' '                                                    
         BNH   VALWACX                                                          
*                                                                               
VALWAC10 MVC   FVMSGNO,=AL2(EATWOANL)  CANNOT ANALYSE TWO EXPENSE A/CS          
         B     VALHEDE                                                          
*                                                                               
VALWACX  DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE WRITE-OFF CLIENT/PRODUCT                                   *         
***********************************************************************         
         SPACE 1                                                                
VALWCP   TM    RECWCPH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALWCPX                                                          
         CLI   WOFFCOST,C' '       TEST WRITE-OFF A/C ANALYSIS FLAG             
         BH    VALWCP2                                                          
         OC    WOFFANAL,WOFFANAL   TEST WRITE-OFF ANALYSIS A/C                  
         BNZ   VALWCP2                                                          
         TM    WOFFSTAT,RSTSGPEI   TEST STAFF ANALYSIS FLAG                     
         BZ    *+12                                                             
         TM    WOFFINDS,WOFFINCP   TEST WRITE-OFF NEW COSTING=NO                
         BZ    VALWCP2                                                          
         TM    WOFFINDS,WOFFISI    TEST WRITE-OFF TO SI                         
         BO    VALWCP2                                                          
         TM    WOFFINDS,WOFFIYCP   TEST WRITE-OFF NEW COSTING                   
         BO    VALWCP2                                                          
         CLI   DISCCOST,C' '       TEST DISCOUNT A/C ANALYSIS FLAG              
         BH    VALWCP2                                                          
         OC    DISCANAL,DISCANAL   TEST DISCOUNT ANALYSIS A/C                   
         BNZ   VALWCP2                                                          
         TM    DISCSTAT,RSTSGPEI   TEST STAFF ANALYSIS FLAG                     
         BZ    *+12                                                             
         TM    DISCINDS,DISCINCP   TEST DISCOUNT NEW COSTING=NO                 
         BZ    VALWCP2                                                          
         TM    DISCINDS,DISCISI    TEST DISCOUNT TO SI                          
         BO    VALWCP2                                                          
         TM    DISCINDS,DISCIYCP   TEST DISCOUNT NEW COSTING                    
         BZ    *+8                                                              
VALWCP2  MVI   FVMINL,1            WRITE-OFF CLI/PRO REQUIRED                   
         GOTO1 AVALWCP,RECWCPH                                                  
         BH    VALHEDXX            INVALID OR REQUIRED AND NOT INPUT            
VALWCPX  DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE WRITE-OFF DATE/REFERENCE/NARRATIVE                         *         
***********************************************************************         
         SPACE 1                                                                
VALWDR   TM    RECWRFH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALWDRX                                                          
         XC    WOFDATE,WOFDATE                                                  
         MVC   WOFREF,SPACES                                                    
         MVC   WOFNARR,SPACES                                                   
         OC    WOFF,WOFF           TEST WRITE-OFF ACCOUNT INPUT                 
         BNZ   VALWDR2                                                          
         LA    R1,RECWDTH          TEST NO WRITE-OFF DATE                       
         CLI   FVILEN-FVIHDR(R1),0                                              
         BNE   VALWDR1                                                          
         LA    R1,RECWRFH          TEST NO WRITE-OFF REFERENCE                  
         CLI   FVILEN-FVIHDR(R1),0                                              
         BNE   VALWDR1                                                          
         LA    R1,RECWNRH          TEST NO WRITE-OFF NARRATIVE                  
         CLI   FVILEN-FVIHDR(R1),0                                              
         BE    VALWDRX                                                          
VALWDR1  ST    R1,FVADDR                                                        
         MVC   FVMSGNO,=AL2(EGIFNOTV)                                           
         B     VALHEDE             EXIT WITH CC NEQ                             
VALWDR2  MVI   FVMINL,1                                                         
         GOTO1 AFVAL,RECWRFH                                                    
         BH    VALHEDXX                                                         
         MVC   WOFREF,FVIFLD                                                    
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,RECWDTH                                                    
         MVC   FVMSGNO,=AL2(EGDATINV)                                           
         BH    VALHEDXX                                                         
         MVC   WORK(1),AGYLANG     SET LANGUAGE                                 
         OI    WORK,X'60'          SINGLE DATE ONLY, RETURN AS SINGLE           
         GOTO1 VPERVAL,DMCB,(FVILEN,FVIFLD),(WORK,WORK)                         
         TM    4(R1),X'03'         CHECK VALIDITY                               
         BNZ   VALHEDE             EXIT SETS CC NEQ                             
         LA    RF,WORK             EDIT OUT DATE                                
         USING PERVALD,RF                                                       
         TM    PVALASSM,PVALASD+PVALASM+PVALASY  TEST ALL ASSUMED               
         BO    *+12                                                             
         TM    PVALASSM,PVALASD+PVALASM          TEST DAY/MON ASSUMED           
         BO    VALHEDE             ERROR IF ONLY YEAR INPUT                     
*                                                                               
* UK RULE:  ANY DAY INTO THE FUTURE AND 12 MONTHS INTO THE PAST. EG             
*           IF TODAY IS 8/31/99, THE EARLIEST DATE YOU CAN ENTER IS             
*           8/01/98                                                             
*&&UK                                                                           
         TM    COMPSTA4,CPYSOV12   TEST ALLOW OVER 12 MONTHS BACK DATE          
         BNZ   VALWDR4             & 12 MOS FORWARD - YES SKIP CHECK            
         MVC   FVMSGNO,=AL2(EADTPAST)   DATE TOO FAR IN THE PAST                
         SR    RE,RE                                                            
         ICM   RE,1,TODAYB                                                      
         BNZ   *+8                                                              
         LA    RE,100              YEAR 2000 IS 100 RELATIVE                    
         MH    RE,=H'12'                                                        
         SR    R1,R1                                                            
         IC    R1,TODAYB+1                                                      
         AR    RE,R1               RE=(CURRENT YEAR*12)+MONTH                   
         SH    RE,=H'12'                                                        
         STH   RE,DUB                                                           
         SR    RE,RE                                                            
         ICM   RE,1,PVALBSTA                                                    
         BNZ   *+8                                                              
         LA    RE,100                                                           
         MH    RE,=H'12'                                                        
         IC    R1,PVALBSTA+1                                                    
         AR    RE,R1               RE=CURRENT RELATIVE MONTH                    
         CH    RE,DUB              TEST MORE THAN NN MONTHS AGO                 
         BL    VALHEDE                                                          
*&&                                                                             
*                                                                               
* US RULE:  7 DAYS INTO THE FUTURE AND 1 YEAR IN THE PAST.  EG IF TODAY         
*           IS 8/31/99 THE EARLIEST DATE YOU CAN ENTER IS 8/31/98               
*           (CAN OVERRIDE THE 1 YEAR BACK DATE WITH THE CONTROL REC)            
*&&US                                                                           
         MVC   FVMSGNO,=AL2(EADTFUTR)   DATE TOO FAR IN THE FUTURE              
         XC    TEMP,TEMP                                                        
         GOTO1 VDATCON,DMCB,(5,0),(0,TEMP)        GET TODAY'S DATE              
         GOTO1 VADDAY,DMCB,(C'D',TEMP),TEMP+10,F'7' ADD 7 DAYS                  
         LA    RF,WORK             POINT BACK TO PERVAL BLOCK                   
         CLC   PVALESTA,TEMP+10                                                 
         BH    VALHEDE                                                          
*                                                                               
         TM    COMPSTA4,CPYSOV12   TEST ALLOW OVER 12 MONTHS BACK DATE          
         BNZ   VALWDR4             & 12 MOS FORWARD - YES SKIP CHECK            
         MVC   FVMSGNO,=AL2(EADTPAST) DATE TOO FAR IN THE PAST                  
         XC    TEMP,TEMP                                                        
         GOTO1 VDATCON,DMCB,(5,0),(0,TEMP)    GET TODAY'S DATE                  
         GOTO1 VADDAY,DMCB,(C'Y',TEMP),TEMP+10,F'-1' SUBTRACT 1 YEAR            
         LA    RF,WORK             POINT BACK TO PERVAL BLOCK                   
         CLC   PVALESTA,TEMP+10                                                 
         BL    VALHEDE                                                          
*&&                                                                             
*                                                                               
VALWDR4  MVC   WOFDATE,PVALPSTA    PWOS YYMMDD                                  
         XC    RECWDT,RECWDT                                                    
         OI    RECWDTH+(FVOIND-FVIHDR),FVOXMT                                   
         GOTO1 VDATCON,DMCB,(1,WOFDATE),(17,RECWDT)                             
         DROP  RF                                                               
         GOTO1 AFVAL,RECWNRH                                                    
         BH    VALHEDXX                                                         
         BL    VALWDRX                                                          
         MVC   WOFNARR,FVIFLD                                                   
VALWDRX  DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE OFFSET DATE                                                *         
***********************************************************************         
         SPACE 1                                                                
VALODT   MVC   OFSDATE,TODAYP                                                   
         GOTO1 AFVAL,RECODTH                                                    
         BL    VALODTX                                                          
         MVC   FVMSGNO,=AL2(EGDATINV)                                           
         BH    VALHEDXX                                                         
         MVC   WORK(1),AGYLANG     SET LANGUAGE                                 
         OI    WORK,X'60'          SINGLE DATE ONLY, RETURN AS SINGLE           
         GOTO1 VPERVAL,DMCB,(FVILEN,FVIFLD),(WORK,WORK)                         
         TM    4(R1),X'03'         CHECK VALIDITY                               
         BNZ   VALHEDE             EXIT SETS CC NEQ                             
         LA    RF,WORK             EDIT OUT DATE                                
         USING PERVALD,RF                                                       
         TM    PVALASSM,PVALASD+PVALASM+PVALASY  TEST ALL ASSUMED               
         BO    *+12                                                             
         TM    PVALASSM,PVALASD+PVALASM          TEST DAY/MON ASSUMED           
         BO    VALHEDE             ERROR IF ONLY YEAR INPUT                     
*                                                                               
* UK RULE:  ANY DAY INTO THE FUTURE AND 12 MONTHS IN THE PAST. EG IF            
*           TODAY IS 8/31/99, THE EARLIEST DTE YOU CAN ENTER IS 8/01/98         
*                                                                               
*&&UK                                                                           
         TM    COMPSTA4,CPYSOV12   TEST ALLOW OVER 12 MONTHS BACK DATE          
         BNZ   VALODT2             & 12 MOS INTO THE FUTURE NO - SKIP           
         MVC   FVMSGNO,=AL2(EADTPAST) DATE TOO FAR IN THE PAST                  
         SR    RE,RE                                                            
         ICM   RE,1,TODAYB                                                      
         BNZ   *+8                                                              
         LA    RE,100              YEAR 2000 IS 100 RELATIVE                    
         MH    RE,=H'12'                                                        
         SR    R1,R1                                                            
         IC    R1,TODAYB+1                                                      
         AR    RE,R1               RE=(CURRENT YEAR*12)+MONTH                   
         SH    RE,=H'12'                                                        
         STH   RE,DUB                                                           
         SR    RE,RE                                                            
         ICM   RE,1,PVALBSTA                                                    
         BNZ   *+8                                                              
         LA    RE,100                                                           
         MH    RE,=H'12'                                                        
         IC    R1,PVALBSTA+1                                                    
         AR    RE,R1               RE=CURRENT RELATIVE MONTH                    
         CH    RE,DUB              TEST MORE THAN NN MONTHS AGO                 
         BL    VALHEDE                                                          
*&&                                                                             
*                                                                               
* US RULE:  7 DAYS INTO THE FUTURE AND 1 YEAR IN THE PAST. EG IF TODAY          
*           IS 8/31/99, THE EARLIEST DTE YOU CAN ENTER IS 8/31/98               
*           (CAN OVERRIDE THE 1 YEAR BACK DATE WITH THE CONTROL REC)            
*                                                                               
*&&US                                                                           
         MVC   FVMSGNO,=AL2(EADTFUTR)   DATE TOO FAR IN THE FUTURE              
         XC    TEMP,TEMP                                                        
         GOTO1 VDATCON,DMCB,(5,0),(0,TEMP)    GET TODAY'S DATE                  
         GOTO1 VADDAY,DMCB,(C'D',TEMP),TEMP+10,F'7' ADD 7 DAYS                  
         LA    RF,WORK             POINT BACK TO PERVAL BLOCK                   
         CLC   PVALESTA,TEMP+10                                                 
         BH    VALHEDE                                                          
*                                                                               
         TM    COMPSTA4,CPYSOV12   TEST ALLOW OVER 12 MONTHS BACK DATE          
         BNZ   VALODT2             & 12 MOS INTO THE FUTURE NO - SKIP           
         MVC   FVMSGNO,=AL2(EADTPAST) DATE TOO FAR IN THE PAST                  
         XC    TEMP,TEMP                                                        
         GOTO1 VDATCON,DMCB,(5,0),(0,TEMP)    GET TODAY'S DATE                  
         GOTO1 VADDAY,DMCB,(C'Y',TEMP),TEMP+10,F'-1' SUBTRACT 1 YEAR            
         LA    RF,WORK             POINT BACK TO PERVAL BLOCK                   
         CLC   PVALESTA,TEMP+10                                                 
         BL    VALHEDE                                                          
*&&                                                                             
*                                                                               
VALODT2  MVC   OFSDATE,PVALPSTA    PWOS YYMMDD                                  
         XC    RECODT,RECODT                                                    
         OI    RECODTH+(FVOIND-FVIHDR),FVOXMT                                   
         GOTO1 VDATCON,DMCB,(1,OFSDATE),(17,RECODT)                             
         DROP  RF                                                               
VALODTX  DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE FINANCIAL OFFICE CODE                                      *         
***********************************************************************         
         SPACE 1                                                                
VALFOF   TM    RECFOFFH+(FVATRB-FVIHDR),FVAPROT                                 
         BO    VALFOFX                                                          
         MVC   BATOFF,SPACES       CLEAR OFFICE                                 
         TM    COMPSTAT,CPYSOROE   TEST OFFICE REQUIRED ON EXPENSES             
         BZ    VALFOF8             NO - ENSURE OFFICE NOT PRESENT               
         TM    WOFFINDS,WOFFISE    TEST WRITE-OFF TO SE                         
         BZ    *+12                NO - TRY DISCOUNT TO SE                      
         TM    WOFFSTAT,RSTSEADD+RSTSGPEI  TEST DEPT/PERSON ANALYSIS            
         BNZ   VALFOF4             YES - OFFICE REQUIRED                        
         TM    DISCINDS,DISCISE    TEST DISCOUNT TO SE                          
         BZ    *+12                NO - CHECK COSTING POSTINGS                  
         TM    DISCSTAT,RSTSEADD+RSTSGPEI  TEST DEPT/PERSON ANALYSIS            
         BNZ   VALFOF4                                                          
         OC    WOFFANAL,WOFFANAL   TEST WRITE-OFF ANALYSIS A/C                  
         BNZ   VALFOF4                                                          
         CLI   WOFFCOST,C' '       TEST WRITE-OFF COSTING                       
         BH    VALFOF4                                                          
         TM    WOFFINDS,WOFFIYCP   TEST WRITE-OFF NEW COSTING                   
         BNZ   VALFOF4                                                          
         OC    DISCANAL,DISCANAL   TEST DISCOUNT ANALYSIS A/C                   
         BNZ   VALFOF4                                                          
         CLI   DISCCOST,C' '       TEST DISCOUNT COSTING                        
         BH    VALFOF4                                                          
         TM    DISCINDS,DISCIYCP   TEST DISCOUNT NEW COSTING                    
         BZ    VALFOF8             NO - ENSURE OFFICE NOT PRESENT               
*                                                                               
VALFOF4  MVC   FVMAXL,COMPOFFL     SET MAXIMUM LENGTH (1 OR 2)                  
         MVC   FVMINL,COMPOFFL     AND MINIMUM LENGTH                           
         GOTO1 AFVAL,RECFOFFH                                                   
         BH    VALHEDXX                                                         
         BL    VALFOFX                                                          
         TM    BATCHSEC,CPYBSOFF   TEST OFFICE SECURITY OVERRIDE                
         BZ    VALFOF6                                                          
         L     R1,AOFFBLK          TEST OFFICE SECURITY                         
         USING OFFALD,R1                                                        
         MVI   OFFAACT,OFFAVAL                                                  
         MVC   OFFAOFFC,FVIFLD                                                  
         GOTO1 VOFFAL                                                           
         BNE   VALFOF10                                                         
         DROP  R1                                                               
VALFOF6  MVC   BATOFF,FVIFLD       SAVE OFFICE                                  
         B     VALFOFX                                                          
*                                                                               
VALFOF8  CLI   RECFOFFH+(FVILEN-FVIHDR),0                                       
         BE    VALFOFX                                                          
*                                                                               
VALFOF10 LA    R1,RECFOFFH                                                      
         ST    R1,FVADDR                                                        
         MVC   FVMSGNO,=AL2(EGIFNOTV)                                           
         B     VALHEDE                                                          
*                                                                               
VALFOFX  DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE DEPARTMENT ACCOUNT                                         *         
***********************************************************************         
         SPACE 1                                                                
VALDPT   TM    RECDPTH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALDPTX                                                          
         LA    R1,RECDPTH                                                       
         ST    R1,FVADDR                                                        
         XC    DEPTDEF,DEPTDEF     CLEAR DEFAULT DEPARTMENT                     
         MVI   DEPTDEFL,0          AND LENGTH                                   
         LA    R1,WOFF-1                                                        
         TM    WOFFINDS,WOFFISE    TEST SE WRITE-OFF                            
         BZ    *+12                                                             
         TM    WOFFSTAT,RSTSEADD+RSTSGPEI                                       
         BNZ   VALDPT0                                                          
         LA    R1,DISC-1                                                        
         TM    DISCINDS,DISCISE    TEST SE DISCOUNT                             
         BZ    VALDPT2                                                          
         TM    DISCSTAT,RSTSEADD+RSTSGPEI                                       
         BZ    VALDPT2                                                          
VALDPT0  GOTO1 EXTDEP              ATTEMPT TO SET IT                            
VALDPT2  XC    DEPTVALS(DEPTVALL),DEPTVALS                                      
         XC    RECDPTN,RECDPTN                                                  
         OI    RECDPTNH+(FVOIND-FVIHDR),FVOXMT                                  
         TM    WOFFINDS,WOFFISE    TEST SE WRITE-OFF                            
         BZ    *+12                                                             
         TM    WOFFSTAT,RSTSEADD+RSTSGPEI                                       
         BNZ   VALDPT4             DEMAND DEPARTMENT ACCOUNT                    
         TM    DISCINDS,DISCISE    TEST SE DISCOUNT                             
         BZ    *+12                                                             
         TM    DISCSTAT,RSTSEADD+RSTSGPEI                                       
         BNZ   VALDPT4             DEMAND DEPARTMENT ACCOUNT                    
         CLI   RECDPTH+(FVILEN-FVIHDR),0                                        
         BE    VALDPTX                                                          
         MVC   FVMSGNO,=AL2(EGIFNOTV)                                           
         B     VALHEDE             EXIT WITH CC NEQ                             
*                                                                               
VALDPT4  MVI   FVILEN,0            PRESET NO INPUT TO DEPARTMENT                
         OC    DEPTDEF,DEPTDEF                                                  
         BZ    *+12                                                             
         CLI   RECDPTH+(FVILEN-FVIHDR),0                                        
         BE    VALDPT5                                                          
         MVC   FVMINL,COMPDPTL     REQUIRED IF NO DEFAULT                       
         MVC   FVMAXL,COMPDPTL     SET MAXIMUM LENGTH TOO                       
         GOTO1 AFVAL,RECDPTH                                                    
         BNE   VALHEDXX                                                         
VALDPT5  MVC   KEY,SPACES                                                       
         LA    R2,KEY                                                           
         USING ACTRECD,R2                                                       
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(L'DEPTUL),DEPTUL                                         
         LA    RE,ACTKACT                                                       
         SR    RF,RF               CLEAR RF                                     
         TM    COMPSTAT,CPYSOROE   TEST OFFICE DEMANDED                         
         BZ    VALDPT6                                                          
         IC    RF,COMPOFFL         TAKE L'OFFICE                                
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   ACTKACT(0),BATOFF   SET OFFICE                                   
         OI    DEPTINDS,DEPTIOID   SET OFFICE IN DEPARTMENT A/C                 
         LA    RE,1(RF,RE)                                                      
VALDPT6  CLI   FVILEN,2            TEST TWO CHARACTER DEPARTMENT                
         BNE   *+8                                                              
         OI    DEPTINDS,DEPTITWO   SET TWO CHARACTER DEPARTMENT                 
         OC    DEPTDEF,DEPTDEF     TEST DEFAULT DEPARTMENT EXISTS               
         BZ    *+10                                                             
         MVC   0(L'DEPTDEF,RE),DEPTDEF                                          
         CLI   FVILEN,0            TEST ANY INPUT                               
         BE    VALDPT8                                                          
         MVC   0(L'DEPTDEF,RE),SPACES  CLEAR DEFAULT                            
         IC    RF,FVXLEN                                                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),FVIFLD      AND TAKE THE INPUT                           
VALDPT8  TM    WOFFINDS,WOFFISE    TEST SE WRITE-OFF                            
         BZ    *+12                                                             
         TM    WOFFSTAT,RSTSEADD                                                
         BNZ   VALDPT9                                                          
         TM    DISCINDS,DISCISE    TEST SE DISCOUNT                             
         BZ    *+12                                                             
         TM    DISCSTAT,RSTSEADD                                                
         BNZ   VALDPT9                                                          
         MVC   DEPT,ACTKUNT                                                     
         B     VALDPTX                                                          
VALDPT9  GOTO1 AGETACC,0           GET ACCOUNT, TEST SECURITY                   
         BNE   VALHEDXX                                                         
         MVC   DEPT,ACTKUNT        U/L/(O)/ACCOUNT                              
         MVC   DEPTNAME,RECNAME                                                 
         MVC   RECDPTN,RECNAME                                                  
         SR    RF,RF                                                            
         TM    DEPTINDS,DEPTIOID   SET OFFICE IN DEPARTMENT A/C                 
         BNO   *+8                                                              
         IC    RF,COMPOFFL         TAKE L'OFFICE                                
         LA    RF,ACTKACT(RF)                                                   
         MVC   RECDPT,0(RF)        RE-DISPLAY IN CASE IT'S DEFAULT              
         OI    RECDPTH+(FVOIND-FVIHDR),FVOXMT                                   
*                                                                               
         MVC   KEY,SPACES          GET 28 A/C (DISCOUNT OR WRITE-OFF)           
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(L'EXPAUL),EXPAUL                                         
         TM    DISCINDS,DISCISE                                                 
         BZ    VALDPT10                                                         
         TM    DISCSTAT,RSTSEADD+RSTSGPEI                                       
         BZ    VALDPT10                                                         
         MVC   ACTKACT,DISC+(ACTKACT-ACTKUNT)  DISCOUNT A/C IS FLAGGED          
         GOTO1 AGETACC,0                                                        
         BNE   VALHEDXX                                                         
         B     VALDPTX                                                          
VALDPT10 TM    WOFFINDS,WOFFISE                                                 
         BZ    VALDPTX                                                          
         TM    WOFFSTAT,RSTSEADD+RSTSGPEI                                       
         BZ    VALDPTX                                                          
         MVC   ACTKACT,SPACES                                                   
         MVC   ACTKACT,WOFF+(ACTKACT-ACTKUNT)  WRITE-OFF A/C IS FLAGGED         
         GOTO1 AGETACC,0                                                        
         BNE   VALHEDXX                                                         
*                                                                               
VALDPTX  DS    0H                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE STAFF ACCOUNT                                              *         
***********************************************************************         
         SPACE 1                                                                
VALSTF   TM    RECSTFH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALSTFX                                                          
         LA    R1,RECSTFH                                                       
         ST    R1,FVADDR                                                        
         XC    STAFVALS(STAFVALL),STAFVALS                                      
         XC    RECSTFN,RECSTFN                                                  
         OI    RECSTFNH+(FVOIND-FVIHDR),FVOXMT                                  
         TM    WOFFINDS,WOFFISE    TEST SE WRITE-OFF                            
         BZ    *+12                                                             
         TM    WOFFSTAT,RSTSGPEI                                                
         BNZ   VALSTF2             DEMAND STAFF ACCOUNT                         
         TM    DISCINDS,DISCISE    TEST SE DISCOUNT                             
         BZ    *+12                                                             
         TM    DISCSTAT,RSTSGPEI                                                
         BNZ   VALSTF2             DEMAND STAFF ACCOUNT                         
         CLI   RECSTFH+(FVILEN-FVIHDR),0                                        
         BE    VALSTFX                                                          
         MVC   FVMSGNO,=AL2(EGIFNOTV)                                           
         B     VALHEDE             EXIT WITH CC NEQ                             
*                                                                               
VALSTF2  MVI   FVMINL,1                                                         
         GOTO1 AFVAL,RECSTFH                                                    
         BNE   VALHEDXX                                                         
         MVC   KEY,SPACES                                                       
         LA    R2,KEY                                                           
         USING ACTRECD,R2                                                       
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(L'STAFUL),STAFUL                                         
         GOTO1 AGETLDG             GET LEDGER RECORD                            
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(EALDGINV)                                           
         B     VALHEDE                                                          
         USING LEDGTABD,RF                                                      
         ICM   RF,15,RECLEDG       RF=A(LEDGER TABLE ENTRY)                     
         LA    RE,ACTKACT                                                       
         CLI   LEDGTLVA,12         TEST 1 LEVEL 2P                              
         BE    VALSTF10                                                         
         OI    STAFINDS,STAFIDIS   SET DEPARTMENT IN STAFF A/C                  
         CLI   LEDGTLVB,12         TEST 2 LEVEL 2P                              
         BE    VALSTF4                                                          
         OI    STAFINDS,STAFIOIS   SET OFFICE IN STAFF A/C                      
         DROP  RF                                                               
         LA    R1,RECFOFFH         ELSE 3 LEVEL 2P                              
         ST    R1,FVADDR                                                        
         CLC   BATOFF,SPACES       REQUIRE OFFICE                               
         BH    *+14                                                             
         MVC   FVMSGNO,=AL2(EGIFMISS)                                           
         B     VALHEDE                                                          
         SR    RF,RF                                                            
         IC    RF,COMPOFFL                                                      
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   0(0,RE),BATOFF                                                   
         LA    RE,1(RF,RE)                                                      
VALSTF4  LA    R1,RECDPTH                                                       
         ST    R1,FVADDR                                                        
         SR    RF,RF                                                            
         ICM   RF,1,FVILEN-FVIHDR(R1)                                           
         BNZ   VALSTF6                                                          
         OC    DEPTDEF,DEPTDEF     TEST DEFAULT AVAILABLE                       
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(EGIFMISS)                                           
         B     VALHEDE                                                          
         LA    R1,DEPTDEF          R1=A(DEPTDEF)                                
         SR    RF,RF                                                            
         IC    RF,DEPTDEFL                                                      
         BCT   RF,VALSTF8          DROP FOR EXECUTE AND BRANCH                  
VALSTF6  BCTR  RF,0                                                             
         SR    R1,R1                                                            
         TM    DEPTINDS,DEPTIOID   TEST OFFICE IN DEPARTMENT A/C                
         BZ    *+8                                                              
         IC    R1,COMPOFFL         SET TO SKIP IT (ALREADY EXTRACTED)           
         LA    R1,DEPT+(L'DEPTUL)(R1)                                           
VALSTF8  EX    RF,*+4                                                           
         MVC   0(0,RE),0(R1)       EXTRACT DEPARTMENT                           
         LA    RE,1(RF,RE)                                                      
VALSTF10 SR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),FVIFLD      RE=U/L/(O)/(DPT)/ACCOUNT                     
         LA    R1,RECSTFH                                                       
         ST    R1,FVADDR                                                        
         GOTO1 AGETACC,0           GET ACCOUNT, TEST SECURITY                   
         BNE   VALHEDXX                                                         
         MVC   STAF,ACTKUNT        U/L/ACCOUNT                                  
         MVC   STAFNAME,RECNAME                                                 
         MVC   RECSTFN,RECNAME                                                  
*                                                                               
VALSTFX  DS    0H                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE DATE RANGE FILTERS                                         *         
***********************************************************************         
         SPACE 1                                                                
VALDAT   TM    RECDATH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALDATX                                                          
         GOTO1 AVALDAT,RECDATH                                                  
         BH    VALHEDXX                                                         
VALDATX  DS    0H                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE BILL RANGE FILTERS                                         *         
***********************************************************************         
         SPACE 1                                                                
VALBNR   TM    RECBNRH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALBNRX                                                          
         GOTO1 AVALBNR,RECBNRH                                                  
         BH    VALHEDXX                                                         
VALBNRX  DS    0H                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE BILLING SOURCE FILTER                                      *         
***********************************************************************         
         SPACE 1                                                                
VALBSO   TM    RECBSOH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALBSOX                                                          
         GOTO1 AVALBSO,RECBSOH                                                  
         BH    VALHEDXX                                                         
VALBSOX  DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE CLIENT ANALYSIS ACCOUNTS FOR EACH RECEIVABLE A/C           *         
***********************************************************************         
         SPACE 1                                                                
VALCLAN  TM    WOFFSTAT,RSTSGPEI   TEST STAFF ANALYSIS                          
         BNZ   *+12                                                             
         TM    DISCSTAT,RSTSGPEI                                                
         BZ    VALCLAX                                                          
         L     R3,ARECVTAB          R3=A(RECEIVABLE A/C TABLE)                  
         USING RECVTABD,R3                                                      
         LA    R2,KEY              R2=A(KEY)                                    
         USING ACTRECD,R2                                                       
         LA    R1,RECRCVH          SET A(RECEIVABLE A/C(S) FIELD)               
         ST    R1,FVADDR                                                        
*                                                                               
VALCLA2  CLI   RECVNDX,X'FF'       TEST E-O-T                                   
         BE    VALCLAX             FINISHED WITH RECEIVABLE A/C(S)              
         MVC   FVINDX,RECVNDX      SET MULTIPLE FIELD INDEX FOR ERROR           
         MVC   KEY,SPACES                                                       
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(L'CLIAUL),CLIAUL                                         
         MVC   ACTKACT,RECVCOST+(ACTKACT-ACTKUNT)                               
         CLC   ACTKACT,SPACES      TEST ACCOUNT GIVEN                           
         BH    VALCLA8                                                          
         TM    WOFFSTAT,RSTSGPEI   TEST WOFF/DISC STAFF ANALYSIS                
         BZ    VALCLA4                                                          
         TM    WOFFINDS,WOFFINCP   TEST WRITE-OFF NEW COSTING COST=N            
         BZ    VALCLA8                                                          
         B     VALCLA6                                                          
VALCLA4  TM    DISCINDS,DISCINCP   TEST DISCOUNT NEW COSTING COST=N             
         BZ    VALCLA8                                                          
VALCLA6  MVC   ACTKACT,NINES       ALLOW DEFAULT                                
VALCLA8  MVC   FVXTRA,SPACES                                                    
         MVC   FVXTRA(L'ACTKCULA-1),ACTKUNT                                     
         GOTO1 AGETACC,0           GET 29 ANALYSIS ACCOUNT                      
         BNE   VALHEDXX                                                         
         LA    R3,RECVTABL(R3)     NEXT TABLE ENTRY                             
         B     VALCLA2                                                          
*                                                                               
VALCLAX  DS    0H                                                               
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE ALL COSTING ACCOUNTS WHICH MIGHT BE NEEDED                 *         
***********************************************************************         
         SPACE 1                                                                
VALCOST  TM    WOFFINDS,WOFFIYCP   TEST NEW COSTING POSTINGS                    
         BZ    VALCOS1                                                          
         LA    R1,CATBLK                                                        
         USING CATD,R1                                                          
         XC    CATD(CATLNQ),CATD                                                
         MVC   CATDMGR,VDATAMGR    BUILD CONTROL BLOCK                          
         MVC   CATSEAC+(ACTKCPY-ACTRECD)(L'ACTKCPY),COMPANY                     
         MVC   CATSEAC+(ACTKUNT-ACTRECD)(L'WOFF),WOFF                           
         MVC   CATOFF,BATOFF                                                    
         SR    RF,RF                                                            
         TM    DEPTINDS,DEPTIOID   TEST OFFICE IN DEPARTMENT A/C                
         BZ    *+8                                                              
         IC    RF,COMPOFFL         SET TO SKIP IT                               
         LA    RF,DEPT+(L'DEPTUL)(RF)                                           
         MVC   CATDPT,0(RF)                                                     
         GOTO1 VCATCALL,CATD                                                    
         CLI   CATERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                IT WAS VALID EARLIER                         
         MVC   WOFFANAL,CATCDE                                                  
         MVC   WOFFNCST,CATACC3    SET COSTING (13) ACCOUNT                     
         CLI   CATPST,C'N'         NO COSTING POSTING                           
         BNE   VALCOS1                                                          
         DC    H'0'                THERE WERE COSTING POSTINGS EARLIER          
*                                                                               
VALCOS1  TM    WOFFINDS,WOFFIYCP   TEST NEW COSTING POSTINGS REQUIRED           
         BNZ   VALCOS4                                                          
         TM    WOFFINDS,WOFFINCP   TEST NO NEW COSTING POSTINGS                 
         BNZ   VALCOS20                                                         
*                                                                               
         TM    COMPSTA5,CPYSNCST   TEST NEW COSTING POSTINGS                    
         BNZ   VALCOS2                                                          
         TM    COMPSTAT,CPYSCOST   TEST COSTING POSTINGS                        
         BNZ   VALCOS2                                                          
*                                                                               
         MVI   DISCCOST,C' '       CLEAR ANY ANALYSIS FLAGS                     
         XC    DISCANAL,DISCANAL                                                
         MVI   WOFFCOST,C' '                                                    
         XC    WOFFANAL,WOFFANAL                                                
VALCOS2  CLI   WOFFCOST,C' '       TEST WRITE-OFF A/C ANALYSIS FLAG             
         BH    VALCOS4                                                          
         OC    WOFFANAL,WOFFANAL   TEST WRITE-OFF ANALYSIS A/C                  
         BNZ   VALCOS4                                                          
         CLI   DISCCOST,C' '       TEST DISCOUNT A/C ANALYSIS FLAG              
         BH    VALCOS4                                                          
         OC    DISCANAL,DISCANAL   TEST DISCOUNT ANALYSIS A/C                   
         BZ    VALCOSTX            NO COSTING ANALYSIS POSTINGS                 
*                                                                               
VALCOS4  L     R3,ARECVTAB          R3=A(RECEIVABLE A/C TABLE)                  
         USING RECVTABD,R3                                                      
         LA    R2,KEY              R2=A(KEY)                                    
         USING ACTRECD,R2                                                       
         LA    R1,RECRCVH          SET A(RECEIVABLE A/C(S) FIELD)               
         ST    R1,FVADDR                                                        
VALCOS6  CLI   RECVNDX,X'FF'       TEST E-O-T                                   
         BE    VALCOS8             FINISHED WITH RECEIVABLE A/C(S)              
         MVC   FVINDX,RECVNDX      SET MULTIPLE FIELD INDEX FOR ERROR           
         MVC   KEY,SPACES                                                       
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(L'RECVCOST),RECVCOST                                     
         MVC   FVXTRA,SPACES                                                    
         MVC   FVXTRA(L'RECVCOST),RECVCOST                                      
         GOTO1 AGETACC,0           GET 1C COSTING ACCOUNT                       
         BNE   VALHEDXX                                                         
         LA    R3,RECVTABL(R3)     NEXT TABLE ENTRY                             
         B     VALCOS6             TEST NEXT ACCOUNT                            
*                                                                               
VALCOS8  MVI   FVINDX,0            CLEAR MULTIPLE FIELD INDEX FOR ERROR         
         TM    COMPSTA5,CPYSNCST   TEST NEW COSTING                             
         BZ    *+12                                                             
         TM    WOFFINDS,WOFFISE                                                 
         BNZ   VALCOS11                                                         
         OC    WOFFANAL,WOFFANAL   TEST WRITE-OFF ANALYSIS A/C                  
         BNZ   *+12                                                             
         CLI   WOFFCOST,C' '       TEST WRITE-OFF A/C ANALYSIS FLAG             
         BNH   VALCOS22            NO - DEAL WITH DISCOUNT A/C FLAG             
         LA    R1,RECWACH          SET A(WRITE-OFF A/C FIELD)                   
         ST    R1,FVADDR                                                        
         TM    WOFFINDS,WOFFISE    TEST EXPENSE COSTING ANALYSIS                
         BO    VALCOS9                                                          
         TM    WOFFINDS,WOFFISI    TEST INCOME COSTING ANALYSIS                 
         BZ    VALCOS20                                                         
         MVC   KEY,SPACES          INCOME COSTING ANALYSIS                      
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(L'COST2),COST2                                           
         MVC   ACTKACT(L'WOFFCOST),WOFFCOST                                     
         OC    WOFFANAL,WOFFANAL   TEST WRITE-OFF ANALYSIS A/C                  
         BZ    *+10                                                             
         MVC   ACTKACT(L'WOFFANAL),WOFFANAL                                     
         GOTO1 AGETACC,0           GET 12 COSTING ACCOUNT                       
         BNE   VALHEDXX                                                         
         B     VALCOS20                                                         
*                                                                               
VALCOS9  MVC   KEY,SPACES          EXPENSE COSTING ANALYSIS                     
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(L'COST3),COST3                                           
         MVC   ACTKACT(L'WOFFCOST),WOFFCOST                                     
         GOTO1 AGETACC,0                                                        
         BNE   VALHEDXX                                                         
*                                                                               
VALCOS11 TM    WOFFINDS,WOFFINCP   TEST ANY COSTING POSTINGS                    
         BNZ   VALCOS20                                                         
         MVC   KEY,SPACES                                                       
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(L'COSTP),COSTP                                           
*                                                                               
         TM    COMPSTA5,CPYSNCST   NEW COSTING USES HARD CODED 1P A/C           
         BZ    *+14                                                             
         MVC   ACTKACT,NINES                                                    
         B     VALCOS19                                                         
*                                                                               
         LA    RE,ACTKACT                                                       
         TM    COMPSTAT,CPYSOROE                                                
         BZ    VALCOS14                                                         
         CLC   BATOFF,SPACES                                                    
         BH    VALCOS12                                                         
         MVC   FVMSGNO,=AL2(EGIFMISS)                                           
         LA    R1,RECFOFFH                                                      
         ST    R1,FVADDR                                                        
         B     VALHEDE                                                          
VALCOS12 SR    RF,RF                                                            
         IC    RF,COMPOFFL                                                      
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   0(0,RE),BATOFF                                                   
         LA    RE,1(RF,RE)                                                      
VALCOS14 SR    RF,RF                                                            
         IC    RF,COMPDPTL                                                      
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   0(0,RE),NINES                                                    
         OC    DEPTDEF,DEPTDEF     TEST DEFAULT DEPARTMENT SET                  
         BZ    VALCOS16                                                         
         IC    RF,DEPTDEFL                                                      
         MVC   0(L'ACTKACT,RE),SPACES  DOESN'T MATTER IF WE OVERSPACE           
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   0(0,RE),DEPTDEF     OVERRIDE DEPARTMENT DUMMY DEPT               
VALCOS16 CLI   RECDPTH+(FVILEN-FVIHDR),0  TEST ANY USER DEPARTMENT              
         BE    VALCOS18                                                         
         IC    RF,RECDPTH+(FVILEN-FVIHDR)                                       
         MVC   0(L'ACTKACT,RE),SPACES  DOESN'T MATTER IF WE OVERSPACE           
         SR    R1,R1                                                            
         TM    DEPTINDS,DEPTIOID   TEST OFFICE IN DEPARTMENT A/C                
         BZ    *+8                                                              
         IC    R1,COMPOFFL         SET TO SKIP IT (ALREADY EXTRACTED)           
         LA    R1,DEPT+(L'DEPTUL)(R1)                                           
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   0(0,RE),0(R1)       MOVE IN USER'S DEPARTMENT                    
VALCOS18 LA    RE,1(RF,RE)                                                      
         MVC   0(L'WOFFCOST,RE),WOFFCOST  U/L/(O)/DD/C                          
*                                                                               
VALCOS19 GOTO1 AGETACC,0           GET 1P COSTING ACCOUNT                       
         BNE   VALHEDXX                                                         
*                                                                               
VALCOS20 TM    DISCINDS,DISCIYCP   TEST NEW COSTING POSTINGS                    
         BZ    VALCOS21                                                         
         LA    R1,CATBLK                                                        
         USING CATD,R1                                                          
         XC    CATD(CATLNQ),CATD                                                
         MVC   CATDMGR,VDATAMGR    BUILD CONTROL BLOCK                          
         MVC   CATSEAC+(ACTKCPY-ACTRECD)(L'ACTKCPY),COMPANY                     
         MVC   CATSEAC+(ACTKUNT-ACTRECD)(L'DISC),DISC                           
         MVC   CATOFF,BATOFF                                                    
         SR    RF,RF                                                            
         TM    DEPTINDS,DEPTIOID   TEST OFFICE IN DEPARTMENT A/C                
         BZ    *+8                                                              
         IC    RF,COMPOFFL         SET TO SKIP IT                               
         LA    RF,DEPT+(L'DEPTUL)(RF)                                           
         MVC   CATDPT,0(RF)                                                     
         GOTO1 VCATCALL,CATD                                                    
         CLI   CATERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                IT WAS VALID EARLIER                         
         MVC   DISCANAL,CATCDE                                                  
         MVC   DISCNCST,CATACC3    SET COSTING (13) ACCOUNT                     
         CLI   CATPST,C'N'         NO COSTING POSTING                           
         BNE   VALCOS21                                                         
         DC    H'0'                THERE WERE COSTING POSTINGS EARLIER          
*                                                                               
VALCOS21 TM    DISCINDS,DISCIYCP   TEST NEW COSTING POSTINGS REQUIRED           
         BNZ   VALCOS25                                                         
         TM    DISCINDS,DISCINCP   TEST NO NEW COSTING POSTINGS                 
         BNZ   VALCOSTX                                                         
*                                                                               
         OC    DISCANAL,DISCANAL   NOW TEST DISCOUNT ANALYSIS A/C               
         BNZ   VALCOS22                                                         
         CLI   DISCCOST,C' '       NOW TEST DISCOUNT A/C ANALYSIS FLAG          
         BNH   VALCOS34                                                         
*                                                                               
VALCOS22 LA    R1,RECDSCH          ESTABLISH A(DISCOUNT A/C FIELD)              
         ST    R1,FVADDR           SET A(DISCOUNT A/C FIELD)                    
         TM    DISCINDS,DISCISE    TEST EXPENSE COSTING ANALYSIS                
         BO    VALCOS23                                                         
         TM    DISCINDS,DISCISI    TEST INCOME COSTING ANALYSIS                 
         BZ    VALCOS34                                                         
         MVC   KEY,SPACES          INCOME COSTING ANALYSIS                      
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(L'COST2),COST2                                           
         MVC   ACTKACT(L'DISCCOST),DISCCOST                                     
         OC    DISCANAL,DISCANAL                                                
         BZ    *+10                                                             
         MVC   ACTKACT(L'DISCANAL),DISCANAL                                     
         GOTO1 AGETACC,0           GET 12 COSTING ACCOUNT                       
         BNE   VALHEDXX                                                         
         B     VALCOS34                                                         
*                                                                               
VALCOS23 MVC   KEY,SPACES          EXPENSE COSTING ANALYSIS                     
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(L'COST3),COST3                                           
         MVC   ACTKACT(L'DISCCOST),DISCCOST                                     
         GOTO1 AGETACC,0                                                        
         BNE   VALHEDXX                                                         
*                                                                               
VALCOS25 TM    DISCINDS,DISCINCP   TEST ANY COSTING POSTINGS                    
         BNZ   VALCOS34                                                         
         MVC   KEY,SPACES                                                       
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(L'COSTP),COSTP                                           
*                                                                               
         TM    COMPSTA5,CPYSNCST   NEW COSTING USES HARD CODED 1P A/C           
         BZ    *+14                                                             
         MVC   ACTKACT,NINES                                                    
         B     VALCOS33                                                         
*                                                                               
         LA    RE,ACTKACT                                                       
         TM    COMPSTAT,CPYSOROE                                                
         BZ    VALCOS28                                                         
         CLC   BATOFF,SPACES                                                    
         BH    VALCOS26                                                         
         MVC   FVMSGNO,=AL2(EGIFMISS)                                           
         LA    R1,RECFOFFH                                                      
         ST    R1,FVADDR                                                        
         B     VALHEDE                                                          
VALCOS26 SR    RF,RF                                                            
         IC    RF,COMPOFFL                                                      
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   0(0,RE),BATOFF                                                   
         LA    RE,1(RF,RE)                                                      
VALCOS28 SR    RF,RF                                                            
         IC    RF,COMPDPTL                                                      
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   0(0,RE),NINES                                                    
         OC    DEPTDEF,DEPTDEF     TEST DEFAULT DEPARTMENT SET                  
         BZ    VALCOS30                                                         
         IC    RF,DEPTDEFL                                                      
         MVC   0(L'ACTKACT,RE),SPACES  DOESN'T MATTER IF WE OVERSPACE           
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   0(0,RE),DEPTDEF     OVERRIDE DEPARTMENT DUMMY DEPT               
VALCOS30 CLI   RECDPTH+(FVILEN-FVIHDR),0  TEST ANY USER DEPARTMENT              
         BE    VALCOS32                                                         
         IC    RF,RECDPTH+(FVILEN-FVIHDR)                                       
         MVC   0(L'ACTKACT,RE),SPACES  DOESN'T MATTER IF WE OVERSPACE           
         SR    R1,R1                                                            
         TM    DEPTINDS,DEPTIOID   TEST OFFICE IN DEPARTMENT A/C                
         BZ    *+8                                                              
         IC    R1,COMPOFFL         SET TO SKIP IT (ALREADY EXTRACTED)           
         LA    R1,DEPT+(L'DEPTUL)(R1)                                           
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   0(0,RE),0(R1)       MOVE IN USER'S DEPARTMENT                    
VALCOS32 LA    RE,1(RF,RE)                                                      
         MVC   0(L'DISCCOST,RE),DISCCOST  U/L/(O)/DD/C                          
*                                                                               
VALCOS33 GOTO1 AGETACC,0           GET 1P COSTING ACCOUNT                       
         BNE   VALHEDXX                                                         
*                                                                               
VALCOS34 DS    0H                                                               
*                                                                               
VALCOSTX DS    0H                                                               
         DROP  R2,R3                                                            
         SPACE 2                                                                
VALHEDX  CR    RB,RB               HEADER VALIDATION GOOD EXIT                  
         B     VALHEDXX                                                         
*                                                                               
VALHEDE  LTR   RB,RB               HEADER VALIDATION ERROR EXIT                 
*                                                                               
VALHEDXX XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* EXTRACT DEPARTMENT CODE FROM SE ACCOUNT                             *         
***********************************************************************         
         SPACE 1                                                                
         USING ACTRECD,R1                                                       
EXTDEP   LR    R0,RE               SAVE RETURN ADDRESS                          
         CLC   ACTKUNT(L'DISCULSE),DISCULSE                                     
         BNE   EXTDEPX                                                          
         ICM   RF,15,RECLEDG       RF=A(LEDGER TABLE ENTRY)                     
         BZ    EXTDEPX                                                          
         USING LEDGTABD,RF                                                      
         SR    RE,RE                                                            
         ICM   RE,1,LEDGTDDL                                                    
         BZ    EXTDEPX                                                          
         SRDL  RE,4                RE=DISPLACEMENT TO DEPT IN KEY               
         LTR   RE,RE               TEST DISPLACEMENT NON-ZERO                   
         BZ    EXTDEPX                                                          
         SRL   RF,32-4             RF=LENGTH OF DEPT IN KEY                     
         STC   RF,DEPTDEFL         SAVE THE LENGTH                              
         BCTR  RF,0                                                             
         LA    RE,ACTKACT-1(RE)    RE=A(DEPARTMENT CODE IN SE KEY)              
         MVC   DEPTDEF,SPACES                                                   
         EX    RF,*+4                                                           
         MVC   DEPTDEF(0),0(RE)    EXTRACT DEFAULT DEPARTMENT CODE              
EXTDEPX  LR    RE,R0                                                            
         BR    RE                  RETURN TO CALLER                             
         DROP  R1,RF                                                            
         EJECT                                                                  
         LTORG                                                                  
         SPACE 1                                                                
SIMD     DC    CL(L'ACTKCULA-1)'SIMD'                                           
SIMP     DC    CL(L'ACTKCULA-1)'SIMP'                                           
         EJECT                                                                  
***********************************************************************         
* UPDATE - MAKE POSTINGS VIA ACUPDATE                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING RECVTABD,R4         R4=A(RECEIVABLES TABLE ENTRY)                
         USING REPD,R5             R5=A(REPORT WORKING STORAGE)                 
UPDATE   NMOD1 0,**UPDT**,RA,R9,RR=RE                                           
         LR    RC,R1                                                            
         USING WORKD,RC                                                         
         SR    RF,RF                                                            
         ICM   RF,1,UPDMODE                                                     
         SLL   RF,2                                                             
         B     *+4(RF)                                                          
         B     UPDFRST                                                          
         B     UPDRCVF                                                          
         B     UPDPROC                                                          
         B     UPDRCVL                                                          
         B     UPDLAST                                                          
         EJECT                                                                  
UPDFRST  MVC   POSTNARR,SPACES                                                  
         TM    TWAMODE2,TWA2NALL   TEST NO ALLOCATION BATCH                     
         BO    UPDF2                                                            
         GOTO1 VDATCON,DMCB,(0,BANKDATE),(1,BANKDATP)                           
         GOTO1 ABLDNAR,POSTNARR                                                 
         LA    R1,REPH7                                                         
         USING JRNHEDD,R1                                                       
         MVC   JRNHTXT(L'JRNNARR),JRNNARR                                       
         MVC   JRNHACT(L'REPH7-(JRNHACT-JRNHEDD)),POSTNARR                      
UPDF2    MVC   OVRNARR,POSTNARR    SAVE STANDARD NARRATIVE                      
         LA    R0,JRNSPEC                                                       
         ST    R0,OVERSPEC         SAVE A(PRINT SPECS)                          
         LA    R0,ELEXTRA                                                       
         ST    R0,OVERXTRA         SAVE A(EXTRA ELEMENT AREA)                   
         LA    RF,JTACCUMS         CLEAR JOURNAL TOTALS                         
         LA    R0,JTACCUMN                                                      
         ZAP   0(L'JTACCUMS,RF),PZERO                                           
         LA    RF,L'JTACCUMS(RF)                                                
         BCT   R0,*-10                                                          
         ZAP   TOTITEM,PZERO       TOTAL ITEMS                                  
         TM    TWAMODE,TWAMDRFT    TEST DRAFT JOURNAL MODE                      
         BZ    UPDFRSTX                                                         
         MVI   REPSUBPG,1          SET SUB-PROGRAM NUMBER                       
         CLI   UPDACTN,ACTFILT     TEST FILTERED DRAFT JOURNAL                  
         BNE   UPDFRSTX                                                         
         MVI   REPSUBPG,2          SET SUB-PROGRAM NUMBER                       
UPDFRSTX B     UPDATEXX                                                         
         EJECT                                                                  
UPDRCVF  OI    REPHEADI,REPHFRCE   FORCE NEW PAGE FOR NEW ACCOUNT               
         LA    R1,REPH4                                                         
         MVC   JRNHTXT(L'JRNBTCH),JRNBTCH                                       
         MVC   JRNHACT(L'BATMON+L'BATREF),BATMON                                
         LA    R2,JRNHACTN-JRNHEDD(R1)                                          
         MVC   0(L'BATNAME,R2),BATNAME                                          
         LA    R2,L'BATNAME-1(R2)                                               
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         LA    R2,2(R2)                                                         
         MVC   TEMP(L'BATMONP),BATMONP                                          
         MVI   TEMP+L'BATMONP,X'01'                                             
         GOTO1 VDATCON,DMCB,(1,TEMP),(18,0(R2))                                 
         LA    R1,REPH5                                                         
         MVC   JRNHTXT(L'JRNBANK),JRNBANK                                       
         MVC   JRNHACT,BANK                                                     
         MVC   JRNHACTN,BANKNAME                                                
         LA    R1,REPH6                                                         
         MVC   JRNHTXT(L'JRNRECV),JRNRECV                                       
         MVC   JRNHACT,RECVACT+1                                                
         MVC   JRNHACTN,RECVACTN                                                
         LA    RF,JRACCUMS         CLEAR JOURNAL RECEIVABLE TOTALS              
         LA    R0,JRACCUMN                                                      
         ZAP   0(L'JRACCUMS,RF),PZERO                                           
         LA    RF,L'JRACCUMS(RF)                                                
         BCT   R0,*-10                                                          
         ZAP   RCVITEM,PZERO       RECEIVABLE TOTAL ITEMS                       
*                                                                               
         LA    R2,KEY              R2=A(KEY)                                    
         USING ACTRECD,R2                                                       
         MVI   OVRBYTE,0           CLEAR COSTING POSTINGS INDICATOR             
         TM    WOFFINDS,WOFFIYCP+WOFFINCP                                       
         BZ    *+16                                                             
         TM    WOFFINDS,WOFFIYCP                                                
         BNZ   UPDRCF01                                                         
         B     UPDRCF02                                                         
         OC    WOFFANAL,WOFFANAL   TEST WRITE-OFF ANALYSIS A/C                  
         BNZ   *+12                                                             
         CLI   WOFFCOST,C' '       TEST WRITE-OFF A/C ANALYSIS FLAG             
         BNH   *+8                                                              
UPDRCF01 OI    OVRBYTE,NOCWOF      PRESET NO WRITE-OFF COSTING POSTINGS         
UPDRCF02 TM    DISCINDS,DISCIYCP+DISCINCP                                       
         BZ    *+16                                                             
         TM    DISCINDS,DISCIYCP                                                
         BNZ   UPDRCF03                                                         
         B     UPDRCF04                                                         
         OC    DISCANAL,DISCANAL   TEST DISCOUNT ANALYSIS A/C                   
         BNZ   *+12                                                             
         CLI   DISCCOST,C' '       TEST DISCOUNT A/C ANALYSIS FLAG              
         BNH   *+8                                                              
UPDRCF03 OI    OVRBYTE,NOCDSC      PRESET NO DISCOUNT COSTING POSTINGS          
UPDRCF04 CLI   OVRBYTE,0           TEST ANY COSTING POSTINGS                    
         BE    UPDRCF40                                                         
*                                                                               
         MVC   KEY,SPACES                                                       
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(L'RECVCOST),RECVCOST                                     
         GOTO1 AGETACC,0           GET 1C COSTING ACCOUNT                       
         BNE   UPDRCF40                                                         
         MVC   COSTCACN,RECNAME    SAVE 1C COSTING ACCOUNT NAME                 
*                                                                               
         TM    WOFFINDS,WOFFIYCP   TEST NEW COSTING POSTINGS                    
         BNZ   UPDRCF10                                                         
         TM    WOFFINDS,WOFFINCP   TEST NO NEW COSTING POSTINGS                 
         BNZ   UPDRCF24                                                         
         OC    WOFFANAL,WOFFANAL   TEST WRITE-OFF ANALYSIS A/C                  
         BNZ   *+12                                                             
         CLI   WOFFCOST,C' '       TEST WRITE-OFF A/C ANALYSIS FLAG             
         BNH   UPDRCF24                                                         
*                                                                               
         TM    WOFFINDS,WOFFISE    TEST EXPENSE COSTING ANALYSIS                
         BO    UPDRCF12                                                         
         TM    WOFFINDS,WOFFISI    TEST INCOME COSTING ANALYSIS                 
         BZ    UPDRCF24                                                         
*                                                                               
UPDRCF08 MVC   KEY,SPACES          INCOME COSTING ANALYSIS                      
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(L'COST2),COST2                                           
         MVC   ACTKACT(L'WOFFCOST),WOFFCOST                                     
         OC    WOFFANAL,WOFFANAL   TEST WRITE-OFF ANALYSIS A/C                  
         BZ    *+10                                                             
         MVC   ACTKACT(L'WOFFANAL),WOFFANAL                                     
         GOTO1 AGETACC,0           GET 12 COSTING ACCOUNT                       
         BNE   UPDRCF24                                                         
         MVC   CWOF2ACT,ACTKUNT    SAVE 12 COSTING U/L/ACCOUNT                  
         MVC   CWOF2ACN,RECNAME    SAVE 12 COSTING ACCOUNT NAME                 
         NI    OVRBYTE,255-NOCWOF  SET WRITE-OFF COSTING OK                     
         B     UPDRCF24                                                         
*                                                                               
UPDRCF10 MVC   CWOF3ACT,WOFFNCST+L'CPYKCPY                                      
         MVC   CWOF3ACN,SPACES                                                  
         B     UPDRCF14                                                         
*                                                                               
UPDRCF12 MVC   KEY,SPACES          EXPENSE COSTING ANALYSIS                     
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(L'COST3),COST3                                           
         MVC   ACTKACT(L'WOFFCOST),WOFFCOST                                     
         GOTO1 AGETACC,0           GET 13 COSTING ACCOUNT                       
         BNE   UPDRCF24                                                         
         MVC   CWOF3ACT,ACTKUNT    SAVE 13 COSTING U/L/ACCOUNT                  
         MVC   CWOF3ACN,RECNAME    SAVE 13 COSTING ACCOUNT NAME                 
*                                                                               
UPDRCF14 MVC   KEY,SPACES                                                       
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(L'COSTP),COSTP                                           
*                                                                               
         TM    COMPSTA5,CPYSNCST   NEW COSTING USES HARD CODED 1P A/C           
         BZ    *+14                                                             
         MVC   ACTKACT,NINES                                                    
         B     UPDRCF22                                                         
*                                                                               
         LA    RE,ACTKACT                                                       
         TM    COMPSTAT,CPYSOROE                                                
         BZ    UPDRCF16                                                         
         CLC   BATOFF,SPACES                                                    
         BH    *+6                                                              
         DC    H'0'                                                             
         SR    RF,RF                                                            
         IC    RF,COMPOFFL                                                      
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   0(0,RE),BATOFF                                                   
         LA    RE,1(RF,RE)                                                      
*                                                                               
UPDRCF16 SR    RF,RF                                                            
         IC    RF,COMPDPTL                                                      
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   0(0,RE),NINES                                                    
         OC    DEPTDEF,DEPTDEF     TEST DEFAULT DEPARTMENT SET                  
         BZ    UPDRCF18                                                         
         IC    RF,DEPTDEFL                                                      
         MVC   0(L'ACTKACT,RE),SPACES  DOESN'T MATTER IF WE OVERSPACE           
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   0(0,RE),DEPTDEF     OVERRIDE DEPARTMENT DUMMY DEPT               
UPDRCF18 OC    DEPT,DEPT           TEST ANY USER DEPARTMENT                     
         BZ    UPDRCF20                                                         
         IC    RF,COMPDPTL                                                      
         MVC   0(L'ACTKACT,RE),SPACES  DOESN'T MATTER IF WE OVERSPACE           
         SR    R1,R1                                                            
         TM    DEPTINDS,DEPTIOID   TEST OFFICE IN DEPARTMENT A/C                
         BZ    *+8                                                              
         IC    R1,COMPOFFL         SET TO SKIP IT (ALREADY EXTRACTED)           
         LA    R1,DEPT+(L'DEPTUL)(R1)                                           
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   0(0,RE),0(R1)       MOVE IN USER'S DEPARTMENT                    
UPDRCF20 LA    RE,1(RF,RE)                                                      
         MVC   0(L'WOFFCOST,RE),WOFFCOST  U/L/(O)/DD/C                          
*                                                                               
UPDRCF22 GOTO1 AGETACC,0           GET 1P COSTING ACCOUNT                       
         BNE   UPDRCF24                                                         
         MVC   CWOFPACT,ACTKUNT    SAVE 1P COSTING ACCOUNT                      
         NI    OVRBYTE,255-NOCWOF  SET WRITE-OFF COSTING OK                     
*                                                                               
UPDRCF24 TM    DISCINDS,DISCIYCP   TEST NEW COSTING POSTINGS                    
         BNZ   UPDRCF26                                                         
         TM    DISCINDS,DISCINCP   TEST NO NEW COSTING POSTINGS                 
         BNZ   UPDRCF40                                                         
         OC    DISCANAL,DISCANAL   TEST DISCOUNT ANALYSIS A/C                   
         BNZ   *+12                                                             
         CLI   DISCCOST,C' '       TEST DISCOUNT A/C ANALYSIS FLAG              
         BNH   UPDRCF40                                                         
*                                                                               
         TM    DISCINDS,DISCISE    TEST EXPENSE COSTING ANALYSIS                
         BO    UPDRCF28                                                         
         TM    DISCINDS,DISCISI    TEST INCOME COSTING ANALYSIS                 
         BZ    UPDRCF40                                                         
*                                                                               
         MVC   KEY,SPACES          INCOME COSTING ANALYSIS                      
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(L'COST2),COST2                                           
         MVC   ACTKACT(L'DISCCOST),DISCCOST                                     
         OC    DISCANAL,DISCANAL   TEST DISCOUNT ANALYSIS A/C                   
         BZ    *+10                                                             
         MVC   ACTKACT(L'DISCANAL),DISCANAL                                     
         GOTO1 AGETACC,0           GET 12 COSTING ACCOUNT                       
         BNE   UPDRCF40                                                         
         MVC   CDSC2ACT,ACTKUNT    SAVE 12 COSTING U/L/ACCOUNT                  
         MVC   CDSC2ACN,RECNAME    SAVE 12 COSTING ACCOUNT NAME                 
         NI    OVRBYTE,255-NOCDSC  SET DISCOUNT COSTING OK                      
         B     UPDRCF40                                                         
*                                                                               
UPDRCF26 MVC   CDSC3ACT,DISCNCST+L'ACTKCPY                                      
         MVC   CDSC3ACN,SPACES                                                  
         B     UPDRCF30                                                         
*                                                                               
UPDRCF28 MVC   KEY,SPACES          EXPENSE COSTING ANALYSIS                     
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(L'COST3),COST3                                           
         MVC   ACTKACT(L'DISCCOST),DISCCOST                                     
         GOTO1 AGETACC,0           GET 13 COSTING ACCOUNT                       
         BNE   UPDRCF40                                                         
         MVC   CDSC3ACT,ACTKUNT    SAVE 13 COSTING U/L/ACCOUNT                  
         MVC   CDSC3ACN,RECNAME    SAVE 13 COSTING ACCOUNT NAME                 
*                                                                               
UPDRCF30 MVC   KEY,SPACES                                                       
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(L'COSTP),COSTP                                           
*                                                                               
         TM    COMPSTA5,CPYSNCST   NEW COSTING USES HARD CODED 1P A/C           
         BZ    *+14                                                             
         MVC   ACTKACT,NINES                                                    
         B     UPDRCF38                                                         
*                                                                               
         LA    RE,ACTKACT                                                       
         TM    COMPSTAT,CPYSOROE                                                
         BZ    UPDRCF32                                                         
         CLC   BATOFF,SPACES                                                    
         BH    *+6                                                              
         DC    H'0'                                                             
         SR    RF,RF                                                            
         IC    RF,COMPOFFL                                                      
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   0(0,RE),BATOFF                                                   
         LA    RE,1(RF,RE)                                                      
*                                                                               
UPDRCF32 SR    RF,RF                                                            
         IC    RF,COMPDPTL                                                      
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   0(0,RE),NINES                                                    
         OC    DEPTDEF,DEPTDEF     TEST DEFAULT DEPARTMENT SET                  
         BZ    UPDRCF34                                                         
         IC    RF,DEPTDEFL                                                      
         MVC   0(L'ACTKACT,RE),SPACES  DOESN'T MATTER IF WE OVERSPACE           
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   0(0,RE),DEPTDEF     OVERRIDE DEPARTMENT DUMMY DEPT               
UPDRCF34 OC    DEPT,DEPT           TEST ANY USER DEPARTMENT                     
         BZ    UPDRCF36                                                         
         IC    RF,COMPDPTL                                                      
         MVC   0(L'ACTKACT,RE),SPACES  DOESN'T MATTER IF WE OVERSPACE           
         SR    R1,R1                                                            
         TM    DEPTINDS,DEPTIOID   TEST OFFICE IN DEPARTMENT A/C                
         BZ    *+8                                                              
         IC    R1,COMPOFFL         SET TO SKIP IT (ALREADY EXTRACTED)           
         LA    R1,DEPT+(L'DEPTUL)(R1)                                           
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   0(0,RE),0(R1)       MOVE IN USER'S DEPARTMENT                    
UPDRCF36 LA    RE,1(RF,RE)                                                      
         MVC   0(L'DISCCOST,RE),DISCCOST  U/L/(O)/DD/C                          
*                                                                               
UPDRCF38 GOTO1 AGETACC,0           GET 1P COSTING ACCOUNT                       
         BNE   UPDRCF40                                                         
         MVC   CDSCPACT,ACTKUNT    SAVE 1P COSTING ACCOUNT                      
         NI    OVRBYTE,255-NOCDSC  SET DISCOUNT COSTING OK                      
*                                                                               
UPDRCF40 TM    WOFFINDS,WOFFISE    TEST WRITE-OFF/DISCOUNT TO SE                
         BNZ   *+12                                                             
         TM    DISCINDS,DISCISE                                                 
         BZ    UPDRCVFX                                                         
         TM    WOFFSTAT,RSTSEADD   TEST DEPARTMENT ANALYSIS                     
         BNZ   *+12                                                             
         TM    DISCSTAT,RSTSEADD                                                
         BZ    UPDRCF42                                                         
         MVC   KEY,SPACES                                                       
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(L'EXPAUL),EXPAUL                                         
         MVC   ACTKACT,WOFF+L'EXPAUL  USE WRITE-OFF A/C                         
         TM    DISCSTAT,RSTSEADD                                                
         BZ    *+10                                                             
         MVC   ACTKACT,DISC+L'EXPAUL  OR DISCOUNT A/C (CAN'T HAVE BOTH)         
         GOTO1 AGETACC,0           GET 28 ANALYSIS ACCOUNT                      
         BE    *+6                                                              
         DC    H'0'                DIE IF 28 ACCOUNT HAS VANISHED               
         MVC   ANAL8ACT,ACTKUNT    SAVE 28 ANALYSIS U/L/ACCOUNT                 
         MVC   ANAL8ACN,RECNAME    SAVE 28 ANALYSIS ACCOUNT NAME                
*                                                                               
UPDRCF42 TM    WOFFSTAT,RSTSGPEI   TEST STAFF ANALYSIS                          
         BNZ   *+12                                                             
         TM    DISCSTAT,RSTSGPEI                                                
         BZ    UPDRCVFX                                                         
         MVC   KEY,SPACES                                                       
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(L'CLIAUL),CLIAUL                                         
         MVC   ACTKACT,RECVCOST+(ACTKACT-ACTKUNT)                               
         CLC   ACTKACT,SPACES      TEST ACCOUNT GIVEN                           
         BH    UPDRCF48                                                         
         TM    WOFFSTAT,RSTSGPEI   TEST WOFF/DISC STAFF ANALYSIS                
         BZ    UPDRCF44                                                         
         TM    WOFFINDS,WOFFINCP   TEST WRITE-OFF NEW COSTING COST=N            
         BZ    UPDRCF48                                                         
         B     UPDRCF46                                                         
UPDRCF44 TM    DISCINDS,DISCINCP   TEST DISCOUNT NEW COSTING COST=N             
         BZ    UPDRCF48                                                         
UPDRCF46 MVC   ACTKACT,NINES       ALLOW DEFAULT                                
UPDRCF48 GOTO1 AGETACC,0           GET 29 ANALYSIS ACCOUNT                      
         BE    *+6                                                              
         DC    H'0'                DIE IF 29 A/C HAS VANISHED                   
         MVC   ANAL9ACT,ACTKUNT    SAVE 29 ANALYSIS U/L/ACCOUNT                 
         MVC   ANAL9ACN,RECNAME    SAVE 29 ANALYSIS ACCOUNT NAME                
         B     UPDRCVFX                                                         
*                                                                               
UPDRCVFX B     UPDATEXX                                                         
         DROP  R1,R2                                                            
         EJECT                                                                  
UPDPROC  LA    RF,JRCVWOF          ADDRESS WRITE-OFF TOTALS                     
         LA    RE,JTOTWOF                                                       
         TM    TSARINDS,TSARIWOF   TEST WRITE-OFF                               
         BO    UPDP04                                                           
         LA    RF,JRCVOFSP         ADDRESS POSITIVE OFFSET TOTALS               
         LA    RE,JTOTOFSP                                                      
         CP    TSARPOST,PZERO      TEST POSITIVE                                
         BH    *+12                                                             
         LA    RF,JRCVOFSN         ADDRESS NEGATIVE OFFSET TOTALS               
         LA    RE,JTOTOFSN                                                      
         TM    TSARINDS,TSARIOFS   TEST OFFSET                                  
         BO    UPDP04                                                           
         LA    RF,JRCVTRF          ADDRESS TRANSFER TOTALS                      
         LA    RE,JTOTTRF                                                       
         TM    TSARINDS,TSARITRF   TEST TRANSFER                                
         BO    UPDP04                                                           
         AP    RECVAMT,TSARPOST    ADD TO RECEIVABLE TOTAL                      
*                                                                               
         LA    R1,OFFTAB           POST AMOUNT TO OFFICE TABLE                  
         USING OFFTABD,R1                                                       
         LA    R0,OFFTMAX                                                       
UPDP01   CLI   OFFTABD,OFFTEOTQ    TEST FREE ENTRY                              
         BE    UPDP02                                                           
         CLC   OFFTOFFC,TSAROFFC   MATCH ON OFFICE CODE                         
         BNE   *+14                                                             
         AP    OFFTAMNT,TSARPOST   YES - ADD TO OFFICE POSTING                  
         B     UPDP03                                                           
         LA    R1,OFFTABL(R1)      BUMP TO NEXT TABLE ENTRY                     
         BCT   R0,UPDP01           DO FOR NUMBER OF TABLE ENTRIES               
         DC    H'0'                                                             
UPDP02   MVC   OFFTOFFC,TSAROFFC   CREATE NEW TABLE ENTRY                       
         ZAP   OFFTAMNT,TSARPOST                                                
         DROP  R1                                                               
*                                                                               
UPDP03   LA    RF,JRCVMRK          ADDRESS ALLOCATION TOTALS                    
         LA    RE,JTOTMRK                                                       
         CLI   TSARTYPE,TSARTOVR   TEST DIFFERENCE                              
         BNE   UPDP04                                                           
         LA    RF,JRCVDIF          ADDRESS DIFFERENCE TOTALS                    
         LA    RE,JTOTDIF                                                       
*                                                                               
UPDP04   AP    0(L'JRACCUMS,RF),TSARPOST                                        
         AP    0(L'JTACCUMS,RE),TSARPOST                                        
*                                                                               
         BRAS  RE,UPMYPARM                                                      
*                                                                               
         MVC   POSTACT,RECVACT+1   BUILD RECEIVABLE/SOURCE POSTING              
         MVC   POSTACTN,RECVACTN                                                
         MVC   POSTCAC,TSARCAC                                                  
         MVC   POSTCACN,SPACES     NO CONTRA NAME (IT'S THE SOURCE)             
         MVC   POSTDATE,TSARDAT                                                 
         MVC   POSTREF,TSARREF                                                  
         MVI   POSTSTAT,TRNSAUTH                                                
         ZAP   POSTAMNT,TSARPOST                                                
         MVC   POSTOFFC,TSAROFFC                                                
         OI    UPDINDS,UPDIPRTS    PRINT STATUS ON JOURNAL                      
         TM    TSARINDS,TSARITRF   TEST TRANSFER POSTING                        
         BZ    UPDP11                                                           
         MVI   POSTSTAT,TRNSAUTH   CREDIT AUTH'D                                
         TM    TSARINDS,TSARISDR   TEST DEBIT TRANSACTION                       
         BZ    *+8                                                              
         OI    POSTSTAT,TRNSDR     SET DEBIT                                    
         BAS   RE,REVAMNT          REVERSE POSTING AMOUNT                       
         BAS   RE,BLDTNAR          BUILD TRANSFER NARRATIVE                     
         L     R2,AIO6             RE-READ ORIGINAL SR POSTING INTO IO6         
         USING TRNRECD,R2                                                       
         MVC   TRNKEY,SPACES                                                    
         MVC   TRNKCULA,RECVACT    ACCOUNT                                      
         TM    COMPSTA4,CPYSOFF2   TEST NEW OFFICES IN USE                      
         BZ    *+18                                                             
         CLI   FILEFORM,VLISQ      TEST OLD FILE                                
         BE    *+10                                                             
         MVC   TRNKOFF,TSAROFFC    NEW FILE - SET OFFICE IN KEY                 
         MVC   TRNKCULC,TSARCAC    CONTRA (SOURCE)                              
         MVC   TRNKDATE,TSARDAT    DATE                                         
         MVC   TRNKREF,TSARREF     REFERENCE                                    
         MVC   TRNKSBR,TSARSUBR    SUB-REFERENCE                                
         GOTO1 AIORDNSI,TRNRECD    READ INTO IO6                                
         BE    *+6                                                              
         DC    H'0'                ORIGINAL SR POSTING NOT FOUND                
*                                                                               
         LA    RE,ELEXTRA          A(ELEMENT LIST FOR BLDTRN)                   
         AH    R2,DATADISP         R1=A(FIRST ELEMENT)                          
UPDP05   SR    RF,RF                                                            
         IC    RF,1(R2)                                                         
         BCTR  RF,0                                                             
         CLI   0(R2),OTHELQ                                                     
         BE    UPDP08                                                           
         CLI   0(R2),MDTELQ                                                     
         BE    UPDP09                                                           
*                                                                               
UPDP06   SR    RF,RF               BUMP TO NEXT ELEMENT                         
         IC    RF,1(R2)                                                         
         AR    R2,RF                                                            
         CLI   0(R2),0                                                          
         BNE   UPDP05                                                           
         B     UPDP12                                                           
*                                                                               
         USING OTHELD,R2                                                        
UPDP08   EX    RF,*+4                                                           
         MVC   0(0,RE),OTHELD      BLDTRN WILL ADD THE OTHEL                    
         LA    RE,1(RE,RF)                                                      
         MVI   0(RE),0                                                          
         B     UPDP06                                                           
*                                                                               
         USING MDTELD,R2                                                        
UPDP09   EX    RF,*+4                                                           
         MVC   0(0,RE),MDTELD      BLDTRN WILL ADD THE MDTEL                    
         LA    R1,MDTGRS-MDTELD(RE) REVERSE ALL AMOUNTS                         
         LA    R0,(MDTFDTE-MDTGRS)/L'MDTGRS                                     
UPDP10   ICM   RF,15,0(R1)                                                      
         BZ    *+12                                                             
         MH    RF,=H'-1'           REVERSE ALL NUMBERS                          
         STCM  RF,15,0(R1)                                                      
         LA    R1,L'MDTGRS(R1)     NEXT AMOUNT                                  
         BCT   R0,UPDP10                                                        
         ICM   RF,15,MDTVAT-MDTELD(RE)                                          
         BZ    *+12                                                             
         MH    RF,=H'-1'                                                        
         STCM  RF,15,MDTVAT-MDTELD(RE)                                          
         SR    RF,RF                                                            
         IC    RF,MDTLN                                                         
         AR    RE,RF                                                            
         MVI   0(RE),0                                                          
         B     UPDP06                                                           
         DROP  R2                                                               
*                                                                               
UPDP11   TM    TSARINDS,TSARIOFS   TEST OFFSET POSTING                          
         BZ    UPDP14                                                           
         MVC   POSTNARR,SPACES     SPECIAL NARRATIVE                            
         MVC   POSTNARR(L'TXTOFS),TXTOFS                                        
         LA    RF,POSTNARR+L'TXTOFS-1                                           
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         LA    RF,2(RF)                                                         
         MVC   0(L'TXTON,RF),TXTON                                              
         LA    RF,L'TXTON+1(RF)                                                 
         GOTO1 VDATCON,DMCB,(1,OFSDATE),(17,0(RF))                              
UPDP12   GOTO1 ABLDTRN,MYPARM                                                   
         AP    RCVITEM,=P'1'                                                    
         MVC   POSTNARR,OVRNARR    RESET STANDARD NARRATIVE                     
         TM    TSARINDS,TSARITRF   TEST TRANSFER POSTING                        
         BNZ   UPDP72              PRODUCE TRANSFER POSTING                     
         B     UPDPROCX            EXIT                                         
*                                                                               
UPDP14   TM    TSARINDS,TSARIWOF   TEST WRITE-OFF POSTING                       
         BZ    *+8                                                              
         BAS   RE,BLDWNAR          BUILD WRITE-OFF NARRATIVE                    
*                                                                               
         TM    TSARINDS,TSARILAT   TEST LATE (SEPARATE POSTING)                 
         BO    UPDP16                                                           
         OC    DISC,DISC           TEST DISCOUNT ACCOUNT PRESENT                
         BZ    UPDP16                                                           
         TM    TSARINDS,TSARIGRS   TEST GROSS POSTING - NO DISCOUNT             
         BO    UPDP16                                                           
         CP    TSARDISC,PZERO      TEST ANY POSTING TO MAKE                     
         BE    UPDP16                                                           
         SP    POSTAMNT,TSARDISC   TAKE OFF DISCOUNT                            
*                                                                               
UPDP16   GOTO1 ABLDTRN,MYPARM                                                   
         AP    RCVITEM,=P'1'                                                    
*                                                                               
         TM    TSARINDS,TSARIWOF   IS THIS A WRITE-OFF POSTING?                 
         BZ    UPDP46                                                           
         MVC   POSTACT,WOFF        BUILD WRITE-OFF/RECEIVABLE POSTING           
         MVC   POSTACTN,WOFFNAME                                                
         MVC   POSTCAC,RECVACT                                                  
         MVC   POSTCACN,RECVACTN                                                
         MVC   POSTDATE,WOFDATE    WRITE-OFF DATE                               
         MVC   POSTREF,WOFREF      WRITE-OFF REFERENCE                          
         BAS   RE,BLDINAR          BUILD WRITE-OFF INVOICE NARRATIVE            
         MVI   POSTSTAT,TRNSDR+TRNSAUTH  ASSUME RAW DEBIT                       
         ZAP   POSTAMNT,TSARPOST                                                
         TM    WOFFINDS,WOFFISI    TEST SI WRITE-OFF                            
         BO    UPDP26                                                           
         TM    WOFFINDS,WOFFISE    TEST SE WRITE-OFF                            
         BZ    UPDP28                                                           
         TM    WOFFINDS,WOFFIYCP                                                
         BNZ   *+12                                                             
         CLI   WOFFCOST,C' '       TEST WRITE-OFF A/C ANALYSIS FLAG             
         BNH   UPDP28              STANDARD POSTING TO SE                       
         TM    OVRBYTE,NOCWOF      TEST WRITE-OFF COSTING POSTINGS OK           
         BNZ   UPDP28              STANDARD POSTING TO SE                       
         CP    TSARPOST,PZERO      TEST -DR                                     
         BNL   *+12                                                             
         BAS   RE,REVAMNT          SWAP SIGN                                    
         MVI   POSTSTAT,TRNSAUTH   POST +CR                                     
*                                                                               
         GOTO1 ABLDTRN,MYPARM                                                   
         AP    RCVITEM,=P'1'                                                    
*                                                                               
         TM    COMPSTA5,CPYSNCST   COMPANY ON NEW COST?                         
         BZ    *+12                NO, SKIP NEXT TEST                           
         TM    WOFFINDS,WOFFIYCP   NEW COST?                                    
         BZ    UPDP36                                                           
         TM    POSTSTAT,TRNSDR     TEST DEBIT POSTED (NOT REVERSED)             
         BO    *+8                                                              
         BAS   RE,REVAMNT          RESTORE SIGN FOR ANALYSIS POSTINGS           
*                                                                               
         MVC   POSTACT,RECVCOST    SE - BUILD 1C/13 COSTING POSTING             
         MVC   POSTCAC(L'COMPANY),COMPANY                                       
         MVC   POSTCAC+L'COMPANY(L'CWOF3ACT),CWOF3ACT                           
         MVC   POSTCACN,CWOF3ACN                                                
         MVI   POSTSTAT,0          CREDIT                                       
         GOTO1 ABLDTRN,MYPARM                                                   
         AP    RCVITEM,=P'1'                                                    
*                                                                               
         MVC   POSTACT,CWOFPACT    BUILD 1P/1C COSTING POSTING                  
         MVC   POSTCAC(L'COMPANY),COMPANY                                       
         MVC   POSTCAC+L'COMPANY(L'RECVCOST),RECVCOST                           
         MVC   POSTCACN,COSTCACN                                                
         MVI   POSTSTAT,TRNSDR     DEBIT                                        
         GOTO1 ABLDTRN,MYPARM                                                   
         AP    RCVITEM,=P'1'                                                    
         B     UPDP36                                                           
*                                                                               
UPDP26   MVC   POSTCAC,RECVPCP     SI - SET CLIENT/PRODUCT CONTRA               
         MVC   POSTCACN,RECVPCPN                                                
         MVI   POSTSTAT,TRNSAUTH   CREDIT AUTH'D                                
         BAS   RE,REVAMNT          REVERSE POSTING SIGN                         
         LA    R1,ELEXTRA          R1=A(EXTRA ELEMENT AREA)                     
         USING SCIELD,R1                                                        
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN1Q                                                    
         MVI   SCITYPE,SCITGRSS    TYPE G GROSS BILLING                         
         ZAP   SCIAMNT,PZERO                                                    
         B     UPDP32                                                           
         DROP  R1                                                               
UPDP28   TM    WOFFINDS,WOFFIPAY   TEST CR/-CR (PAYABLES)                       
         BO    UPDP30              ALWAYS SWAP AMOUNT AND POST CR               
         TM    WOFFINDS,WOFFIREV   TEST REVERSE -DR TO +CR                      
         BO    *+6                                                              
         DC    H'0'                                                             
         CP    TSARPOST,PZERO      TEST -DR                                     
         BNL   UPDP32                                                           
UPDP30   BAS   RE,REVAMNT          SWAP SIGN                                    
         MVI   POSTSTAT,TRNSAUTH   POST CR                                      
UPDP32   GOTO1 ABLDTRN,MYPARM                                                   
         AP    RCVITEM,=P'1'                                                    
         TM    WOFFINDS,WOFFIREV   TEST REVERSE -DR TO +CR                      
         BZ    UPDP34                                                           
         TM    POSTSTAT,TRNSDR     TEST DEBIT POSTED (NOT REVERSED)             
         BO    UPDP34                                                           
         BAS   RE,REVAMNT          RESTORE SIGN FOR ANALYSIS POSTINGS           
*                                                                               
UPDP34   TM    WOFFINDS,WOFFISI    TEST SI WRITE-OFF                            
         BZ    UPDP36                                                           
         CLI   WOFFCOST,C' '       TEST WRITE-OFF A/C ANALYSIS FLAG             
         BH    *+14                                                             
         OC    WOFFANAL,WOFFANAL   TEST WRITE-OFF ANALYSIS A/C                  
         BZ    UPDP36                                                           
         TM    OVRBYTE,NOCWOF      TEST WRITE-OFF COSTING POSTINGS OK           
         BNZ   UPDP36                                                           
*                                                                               
         MVC   POSTACT,RECVCOST    SI - BUILD 1C/12 COSTING POSTING             
         MVC   POSTCAC(L'COMPANY),COMPANY                                       
         MVC   POSTCAC+L'COMPANY(L'CWOF2ACT),CWOF2ACT                           
         MVC   POSTCACN,CWOF2ACN                                                
         MVI   POSTSTAT,TRNSDR     DEBIT                                        
         GOTO1 ABLDTRN,MYPARM                                                   
         AP    RCVITEM,=P'1'                                                    
*                                                                               
         MVC   POSTACT,CWOF2ACT    BUILD 12/1C COSTING POSTING                  
         MVC   POSTCAC(L'COMPANY),COMPANY                                       
         MVC   POSTCAC+L'COMPANY(L'RECVCOST),RECVCOST                           
         MVC   POSTCACN,COSTCACN                                                
         MVI   POSTSTAT,0          CREDIT                                       
         GOTO1 ABLDTRN,MYPARM                                                   
         AP    RCVITEM,=P'1'                                                    
*                                                                               
UPDP36   TM    WOFFINDS,WOFFISE    TEST SE WRITE-OFF                            
         BZ    UPDP44                                                           
         TM    WOFFSTAT,RSTSEADD   TEST DEPARTMENT ANALYSIS                     
         BZ    UPDP38                                                           
         MVC   POSTACT,DEPT        BUILD 2D/28 ANALYSIS POSTING                 
         MVC   POSTCAC(L'COMPANY),COMPANY                                       
         MVC   POSTCAC+L'COMPANY(L'ANAL8ACT),ANAL8ACT                           
         MVC   POSTCACN,ANAL8ACN                                                
         MVI   POSTSTAT,TRNSDR     DEBIT                                        
         GOTO1 ABLDTRN,MYPARM                                                   
         AP    RCVITEM,=P'1'                                                    
*                                                                               
         MVC   POSTACT,ANAL8ACT    BUILD 28/2D ANALYSIS POSTING                 
         MVC   POSTCAC(L'COMPANY),COMPANY                                       
         MVC   POSTCAC+L'COMPANY(L'DEPT),DEPT                                   
         MVC   POSTCACN,DEPTNAME                                                
         MVI   POSTSTAT,0          CREDIT                                       
         GOTO1 ABLDTRN,MYPARM                                                   
         AP    RCVITEM,=P'1'                                                    
*                                                                               
UPDP38   TM    WOFFSTAT,RSTSGPEI   TEST STAFF ANALYSIS                          
         BZ    UPDP44                                                           
         MVC   POSTACT,STAF        BUILD 2P/29 ANALYSIS POSTING                 
         MVC   POSTCAC,SPACES                                                   
         MVI   POSTCAC,C'*'        BUILD *EXP-CLIENT STYLE CONTRA               
         MVC   POSTCAC+1(L'WOFF-L'WOFFULSE),WOFF+L'WOFFULSE                     
         CLI   TENO,C'0'                                                        
         BL    UPDP40                                                           
         MVC   WORK(1),TENO                                                     
         NI    WORK,X'0F'                                                       
         SR    R1,R1                                                            
         IC    R1,WORK                                                          
         BCTR  R1,0                                                             
         LA    RF,WOFF+L'WOFF-1                                                 
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         SR    RF,R1                                                            
         LA    R0,WOFF+L'WOFFULSE                                               
         CR    RF,R0                                                            
         BL    UPDP40                                                           
         MVC   POSTCAC+1(L'POSTCAC-1),SPACES                                    
         EX    R1,*+4                                                           
         MVC   POSTCAC+1(0),0(RF)                                               
UPDP40   LA    RF,POSTCAC+L'POSTCAC-1                                           
         SR    R1,R1                                                            
         CLI   0(RF),C' '                                                       
         BH    *+12                                                             
         LA    R1,1(R1)                                                         
         BCT   RF,*-12                                                          
         MVI   1(RF),C'-'                                                       
         BCTR  R1,0                                                             
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   2(0,RF),ANAL9ACT+L'CLIAUL                                        
         MVC   POSTCACN,ANAL9ACN                                                
         MVI   POSTSTAT,TRNSDR     DEBIT                                        
         GOTO1 ABLDTRN,MYPARM                                                   
         AP    RCVITEM,=P'1'                                                    
*                                                                               
         MVC   POSTACT,ANAL9ACT    BUILD 29/2P ANALYSIS POSTING                 
         MVC   POSTCAC,SPACES                                                   
         MVI   POSTCAC,C'*'        BUILD *EXP-STAFF STYLE CONTRA                
         MVC   POSTCAC+1(L'WOFF-L'WOFFULSE),WOFF+L'WOFFULSE                     
         CLI   TENO,C'0'                                                        
         BL    UPDP42                                                           
         MVC   WORK(1),TENO                                                     
         NI    WORK,X'0F'                                                       
         SR    R1,R1                                                            
         IC    R1,WORK                                                          
         BCTR  R1,0                                                             
         LA    RF,WOFF+L'WOFF-1                                                 
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         SR    RF,R1                                                            
         LA    R0,WOFF+L'WOFFULSE                                               
         CR    RF,R0                                                            
         BL    UPDP42                                                           
         MVC   POSTCAC+1(L'POSTCAC-1),SPACES                                    
         EX    R1,*+4                                                           
         MVC   POSTCAC+1(0),0(RF)                                               
UPDP42   LA    RF,POSTCAC+L'POSTCAC-1                                           
         SR    R1,R1                                                            
         CLI   0(RF),C' '                                                       
         BH    *+12                                                             
         LA    R1,1(R1)                                                         
         BCT   RF,*-12                                                          
         MVI   1(RF),C'-'                                                       
         BCTR  R1,0                                                             
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   2(0,RF),STAF+L'STAFUL                                            
         MVC   POSTCACN,STAFNAME                                                
         MVI   POSTSTAT,0          CREDIT                                       
         GOTO1 ABLDTRN,MYPARM                                                   
         AP    RCVITEM,=P'1'                                                    
*                                                                               
UPDP44   MVC   POSTDATE,TSARDAT    RESET TRANSACTION DATE                       
         MVC   POSTREF,TSARREF     RESET TRANSACTION REFERENCE                  
         MVC   POSTNARR,OVRNARR    RESET STANDARD NARRATIVE                     
*                                                                               
UPDP46   OC    DISC,DISC           TEST DISCOUNT ACCOUNT PRESENT                
         BZ    UPDPROCX                                                         
         TM    TSARINDS,TSARIGRS   TEST GROSS POSTING - NO DISCOUNT             
         BO    UPDPROCX                                                         
         CP    TSARDISC,PZERO      TEST ANY POSTING TO MAKE                     
         BE    UPDPROCX                                                         
         TM    TSARINDS,TSARIOFS   TEST OFFSET                                  
         BNZ   *+16                                                             
         AP    JRCVDSC,TSARDISC    ACCUMULATE DISCOUNT ALLOC/WOFF ONLY          
         AP    JTOTDSC,TSARDISC                                                 
         TM    TSARINDS,TSARILAT   TEST LATE (SEPARATE POSTING)                 
         BZ    UPDP48              1ST REC/SRC POSTING HANDLED DISCOUNT         
         MVC   POSTACT,RECVACT+1   BUILD 2ND RECEIVABLE/SOURCE POSTING          
         MVC   POSTACTN,RECVACTN                                                
         MVC   POSTCAC,TSARCAC                                                  
         MVC   POSTCACN,SPACES     NO CONTRA NAME (IT'S THE SOURCE)             
         MVC   POSTOFFC,TSAROFFC                                                
         MVI   POSTSTAT,TRNSDR+TRNSAUTH  DEBIT AUTH'D                           
         ZAP   POSTAMNT,TSARDISC                                                
         GOTO1 ABLDTRN,MYPARM                                                   
         AP    RCVITEM,=P'1'                                                    
*                                                                               
UPDP48   MVC   POSTACT,DISC        BUILD DISCOUNT/RECEIVABLE POSTING            
         MVC   POSTACTN,DISCNAME                                                
         MVC   POSTCAC,RECVACT                                                  
         MVC   POSTCACN,RECVACTN                                                
         MVC   POSTOFFC,TSAROFFC                                                
         MVI   POSTSTAT,TRNSDR+TRNSAUTH  DEBIT AUTH'D                           
         ZAP   POSTAMNT,TSARDISC                                                
         TM    DISCINDS,DISCISI    TEST SI DISCOUNT                             
         BO    UPDP58                                                           
         TM    DISCINDS,DISCISE    TEST SE DISCOUNT                             
         BZ    UPDP60                                                           
         BAS   RE,REVAMNT          SE - REVERSE SIGN                            
         TM    DISCINDS,DISCIYCP   TEST NEW COSTING POSTINGS                    
         BNZ   *+12                                                             
         CLI   DISCCOST,C' '       TEST DISCOUNT A/C ANALYSIS FLAG              
         BNH   UPDP60              STANDARD POSTING TO SE                       
         TM    OVRBYTE,NOCDSC      TEST DISCOUNT COSTING POSTINGS OK            
         BNZ   UPDP60              STANDARD POSTING TO SE                       
*                                                                               
UPDP54   GOTO1 ABLDTRN,MYPARM                                                   
         AP    RCVITEM,=P'1'                                                    
*                                                                               
         MVC   POSTACT,RECVCOST    SE - BUILD 1C/13 COSTING POSTING             
         MVC   POSTCAC(L'COMPANY),COMPANY                                       
         MVC   POSTCAC+L'COMPANY(L'CDSC3ACT),CDSC3ACT                           
         MVC   POSTCACN,CDSC3ACN                                                
         MVC   POSTDATE,BANKDATP   BANK DEPOSIT DATE                            
         MVI   POSTSTAT,0          CREDIT                                       
         GOTO1 ABLDTRN,MYPARM                                                   
         AP    RCVITEM,=P'1'                                                    
*                                                                               
         MVC   POSTACT,CDSCPACT    BUILD 1P/1C COSTING POSTING                  
         MVC   POSTCAC(L'COMPANY),COMPANY                                       
         MVC   POSTCAC+L'COMPANY(L'RECVCOST),RECVCOST                           
         MVC   POSTCACN,COSTCACN                                                
         MVI   POSTSTAT,TRNSDR     DEBIT                                        
         GOTO1 ABLDTRN,MYPARM                                                   
         AP    RCVITEM,=P'1'                                                    
         B     UPDP62                                                           
*                                                                               
UPDP58   MVC   POSTCAC,RECVPCP     SI - SET CLIENT/PRODUCT CONTRA               
         MVC   POSTCACN,RECVPCPN                                                
         MVI   POSTSTAT,TRNSAUTH   CREDIT AUTH'D                                
         LA    R1,ELEXTRA          R1=A(EXTRA ELEMENT AREA)                     
         USING SCIELD,R1                                                        
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN1Q                                                    
         MVI   SCITYPE,SCITGRSS    TYPE G GROSS BILLING                         
         ZAP   SCIAMNT,PZERO                                                    
         DROP  R1                                                               
UPDP60   GOTO1 ABLDTRN,MYPARM                                                   
         AP    RCVITEM,=P'1'                                                    
*                                                                               
UPDP62   TM    DISCINDS,DISCISE    TEST SE DISCOUNT                             
         BZ    UPDP70                                                           
         TM    DISCSTAT,RSTSEADD   TEST DEPARTMENT ANALYSIS                     
         BZ    UPDP64                                                           
         MVC   POSTACT,DEPT        BUILD 2D/28 ANALYSIS POSTING                 
         MVC   POSTCAC(L'COMPANY),COMPANY                                       
         MVC   POSTCAC+L'COMPANY(L'ANAL8ACT),ANAL8ACT                           
         MVC   POSTCACN,ANAL8ACN                                                
         MVI   POSTSTAT,TRNSDR     DEBIT                                        
         GOTO1 ABLDTRN,MYPARM                                                   
         AP    RCVITEM,=P'1'                                                    
*                                                                               
         MVC   POSTACT,ANAL8ACT    BUILD 28/2D ANALYSIS POSTING                 
         MVC   POSTCAC(L'COMPANY),COMPANY                                       
         MVC   POSTCAC+L'COMPANY(L'DEPT),DEPT                                   
         MVC   POSTCACN,DEPTNAME                                                
         MVI   POSTSTAT,0          CREDIT                                       
         GOTO1 ABLDTRN,MYPARM                                                   
         AP    RCVITEM,=P'1'                                                    
*                                                                               
UPDP64   TM    DISCSTAT,RSTSGPEI   TEST STAFF ANALYSIS                          
         BZ    UPDP70                                                           
         MVC   POSTACT,STAF        BUILD 2P/29 ANALYSIS POSTING                 
         MVI   POSTCAC,C'*'        BUILD *EXP-CLIENT STYLE CONTRA               
         MVC   POSTCAC+1(L'DISC-L'DISCULSE),DISC+L'DISCULSE                     
         CLI   TENO,C'0'                                                        
         BL    UPDP66                                                           
         MVC   WORK(1),TENO                                                     
         NI    WORK,X'0F'                                                       
         SR    R1,R1                                                            
         IC    R1,WORK                                                          
         BCTR  R1,0                                                             
         LA    RF,DISC+L'DISC-1                                                 
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         SR    RF,R1                                                            
         LA    R0,DISC+L'DISCULSE                                               
         CR    RF,R0                                                            
         BL    UPDP66                                                           
         MVC   POSTCAC+1(L'POSTCAC-1),SPACES                                    
         EX    R1,*+4                                                           
         MVC   POSTCAC+1(0),0(RF)                                               
UPDP66   LA    RF,POSTCAC+L'POSTCAC-1                                           
         SR    R1,R1                                                            
         CLI   0(RF),C' '                                                       
         BH    *+12                                                             
         LA    R1,1(R1)                                                         
         BCT   RF,*-12                                                          
         MVI   1(RF),C'-'                                                       
         BCTR  R1,0                                                             
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   2(0,RF),ANAL9ACT+L'CLIAUL                                        
         MVC   POSTCACN,ANAL9ACN                                                
         MVI   POSTSTAT,TRNSDR     DEBIT                                        
         GOTO1 ABLDTRN,MYPARM                                                   
         AP    RCVITEM,=P'1'                                                    
*                                                                               
         MVC   POSTACT,ANAL9ACT    BUILD 29/2P ANALYSIS POSTING                 
         MVI   POSTCAC,C'*'        BUILD *EXP-STAFF STYLE CONTRA                
         MVC   POSTCAC+1(L'DISC-L'DISCULSE),DISC+L'DISCULSE                     
         CLI   TENO,C'0'                                                        
         BL    UPDP68                                                           
         MVC   WORK(1),TENO                                                     
         NI    WORK,X'0F'                                                       
         SR    R1,R1                                                            
         IC    R1,WORK                                                          
         BCTR  R1,0                                                             
         LA    RF,DISC+L'DISC-1                                                 
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         SR    RF,R1                                                            
         LA    R0,DISC+L'DISCULSE                                               
         CR    RF,R0                                                            
         BL    UPDP68                                                           
         MVC   POSTCAC+1(L'POSTCAC-1),SPACES                                    
         EX    R1,*+4                                                           
         MVC   POSTCAC+1(0),0(RF)                                               
UPDP68   LA    RF,POSTCAC+L'POSTCAC-1                                           
         SR    R1,R1                                                            
         CLI   0(RF),C' '                                                       
         BH    *+12                                                             
         LA    R1,1(R1)                                                         
         BCT   RF,*-12                                                          
         MVI   1(RF),C'-'                                                       
         BCTR  R1,0                                                             
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   2(0,RF),STAF+L'STAFUL                                            
         MVC   POSTCACN,STAFNAME                                                
         MVI   POSTSTAT,0          CREDIT                                       
         GOTO1 ABLDTRN,MYPARM                                                   
         AP    RCVITEM,=P'1'                                                    
*                                                                               
UPDP70   TM    DISCINDS,DISCISI    TEST SI DISCOUNT                             
         BZ    UPDPROCX                                                         
         CLI   DISCCOST,C' '       TEST DISCOUNT A/C ANALYSIS FLAG              
         BH    *+14                                                             
         OC    DISCANAL,DISCANAL   TEST DISCOUNT ANALYSIS A/C                   
         BZ    UPDPROCX                                                         
         TM    OVRBYTE,NOCDSC      TEST DISCOUNT COSTING POSTINGS OK            
         BNZ   UPDPROCX                                                         
*                                                                               
         MVC   POSTACT,RECVCOST    SI - BUILD 1C/12 COSTING POSTING             
         MVC   POSTCAC(L'COMPANY),COMPANY                                       
         MVC   POSTCAC+L'COMPANY(L'CDSC2ACT),CDSC2ACT                           
         MVC   POSTCACN,CDSC2ACN                                                
         MVC   POSTDATE,BANKDATP   BANK DEPOSIT DATE                            
         MVI   POSTSTAT,TRNSDR     DEBIT                                        
         GOTO1 ABLDTRN,MYPARM                                                   
         AP    RCVITEM,=P'1'                                                    
*                                                                               
         MVC   POSTACT,CDSC2ACT    BUILD 12/1C COSTING POSTING                  
         MVC   POSTCAC(L'COMPANY),COMPANY                                       
         MVC   POSTCAC+L'COMPANY(L'RECVCOST),RECVCOST                           
         MVC   POSTCACN,COSTCACN                                                
         MVI   POSTSTAT,0          CREDIT                                       
         GOTO1 ABLDTRN,MYPARM                                                   
         AP    RCVITEM,=P'1'                                                    
         B     UPDPROCX                                                         
*                                                                               
UPDP72   BAS   RE,BLDXFR           BUILD TRANSFER POSTING                       
         LH    R1,=H'-1'           SET RECORD ALREADY BUILT IN IO               
         GOTO1 ABLDTRN             ADD TRANSACTION                              
         AP    RCVITEM,=P'1'                                                    
*                                                                               
UPDPROCX B     UPDATEXX                                                         
         EJECT                                                                  
UPDRCVL  CP    RECVAMT,PZERO       BUILD BANK/RECEIVABLE POSTING                
         BE    UPDRCL2             NOTHING TO POST                              
         MVC   POSTACT,BANK        ELSE BANK ACCOUNT                            
         MVC   POSTACTN,BANKNAME                                                
         MVC   POSTCAC,RECVACT                                                  
         MVC   POSTCACN,RECVACTN                                                
         MVC   POSTDATE,BANKDATP                                                
         MVC   POSTREF,BANKREF                                                  
         MVI   POSTSTAT,TRNSDR+TRNSAUTH  DEBIT AUTH'D                           
*                                                                               
         LA    R2,OFFTAB           BUILD OFFICE POSTINGS TO BANK                
         USING OFFTABD,R2          R2=A(OFFICE POSTING TABLE)                   
         LA    R0,OFFTMAX                                                       
UPDRCL1  CLI   OFFTABD,OFFTEOTQ    TEST E-O-T                                   
         BE    UPDRCL2                                                          
         ZAP   POSTAMNT,OFFTAMNT   SET AMOUNT & OFFICE CODE                     
         MVC   POSTOFFC,OFFTOFFC                                                
         GOTO1 ABLDTRN,0                                                        
         AP    RCVITEM,=P'1'                                                    
         XC    OFFTABD(OFFTABL),OFFTABD                                         
         LA    R2,OFFTABL(R2)      BUMP TO NEXT TABLE ENTRY                     
         BCT   R0,UPDRCL1          DO FOR NUMBER OF TABLE ENTRIES               
         DROP  R2                                                               
*                                                                               
UPDRCL2  OC    PRTSUB,PRTSUB                                                    
         BZ    UPDRCVLX                                                         
*                                                                               
         LA    R2,REPP2                                                         
         USING JRNLINED,R2                                                      
         MVC   JRNDATE(44),=44C'-'                                              
         MVC   JRNDATE+15(L'JRNRECV),JRNRECV                                    
*                                                                               
         LA    R2,L'REPP1(R2)      ALLOCATED                                    
         MVC   JRNDATE(L'JRNMRK),JRNMRK                                         
         AP    JRCVCRS,JRCVMRK                                                  
         CURED JRCVMRK,(L'JRNCR,JRNCR),2,MINUS=YES                              
*                                                                               
         LA    R2,L'REPP1(R2)      SPECIALS                                     
         AP    JRCVCRS,JRCVDIF                                                  
         MVC   JRNDATE(L'JRNDIF),JRNDIF                                         
         CURED JRCVDIF,(L'JRNCR,JRNCR),2,MINUS=YES                              
*                                                                               
         LA    R2,L'REPP1(R2)      PORTION OF CHEQUE                            
         MVC   JRNDATE(L'JRNCHQP),JRNCHQP                                       
         CURED RECVAMT,(L'JRNDR,JRNDR),2,MINUS=YES                              
*                                                                               
         LA    R2,L'REPP1(R2)      WRITTEN-OFF - SR                             
         AP    JRCVCRS,JRCVWOF                                                  
         MVC   JRNDATE(L'JRNWOFSR),JRNWOFSR                                     
         MVC   JRNDATE+L'JRNWOFSR+1(5),=C' - SR'                                
         CURED JRCVWOF,(L'JRNCR,JRNCR),2,MINUS=YES                              
*                                                                               
         LA    R2,L'REPP1(R2)      WRITE-OFF A/C                                
         MVC   JRNDATE(L'JRNWOF),JRNWOF                                         
         ZAP   DUB,JRCVWOF                                                      
         LA    RF,JRNDR                                                         
         LA    RE,JRCVDRS                                                       
         TM    WOFFINDS,WOFFISI    TEST SI WRITE-OFF                            
         BZ    UPDRCL3                                                          
         LA    RF,JRNCR            CREDIT                                       
         LA    RE,JRCVCRS                                                       
         MP    DUB,=P'-1'          MINUS                                        
UPDRCL3  AP    0(L'JRCVDRS,RE),DUB                                              
         CURED DUB,(L'JRNCR,(RF)),2,MINUS=YES                                   
*                                                                               
         LA    R2,L'REPP1(R2)                                                   
         MVC   JRNDATE(L'JRNOFS),JRNOFS                                         
         ZAP   DUB,JRCVOFSP        TAKE POSITIVE OFFSETS                        
         AP    DUB,JRCVOFSN        ADD NEGATIVE OFFSETS                         
         CP    DUB,PZERO           TEST OFFSETS IN BALANCE                      
         BE    UPDRCL4                                                          
         AP    JRCVCRS,DUB         ADD TO TOTAL                                 
         CURED DUB,(L'JRNCR,JRNCR),2,MINUS=YES                                  
         B     UPDRCL6                                                          
UPDRCL4  CURED JRCVOFSP,(L'JRNCR,JRNCR+1),2,MINUS=YES,BRACKET=Y                 
         LA    R1,JRNCR                                                         
UPDRCL5  LA    R1,1(R1)                                                         
         CLI   0(R1),C'('                                                       
         BNE   *+8                                                              
         MVI   0(R1),C'*'          REPLACE ( WITH *                             
         CLI   0(R1),C')'                                                       
         BNE   UPDRCL5                                                          
         MVI   0(R1),C'*'          REPLACE ) WITH *                             
*                                                                               
UPDRCL6  LA    R2,L'REPP1(R2)      DISCOUNT - SR                                
         MVC   JRNDATE(L'JRNDSCSR),JRNDSCSR                                     
         MVC   JRNDATE+L'JRNDSCSR+1(5),=C' - SR'                                
         AP    JRCVDRS,JRCVDSC                                                  
         CURED JRCVDSC,(L'JRNDR,JRNDR),2,MINUS=YES                              
*                                                                               
         LA    R2,L'REPP1(R2)      DISCOUNT A/C                                 
         MVC   JRNDATE(L'JRNDSC),JRNDSC                                         
         ZAP   DUB,JRCVDSC                                                      
         TM    DISCINDS,DISCISI    TEST SI DISCOUNT                             
         BZ    UPDRCL7                                                          
         LA    RF,JRNCR            SI - CREDIT                                  
         AP    JRCVCRS,DUB                                                      
         B     UPDRCL8                                                          
UPDRCL7  LA    RF,JRNDR            SE/SQ - DEBIT                                
         MP    DUB,=P'-1'          DISCOUNT TYPE - REVERSE SIGN                 
         AP    JRCVDRS,DUB                                                      
UPDRCL8  CURED DUB,(L'JRNCR,(RF)),2,MINUS=YES                                   
*                                                                               
         CP    JRCVTRF,PZERO       TRANSFERRED                                  
         BE    UPDRCL10                                                         
         LA    R2,L'REPP1(R2)                                                   
         MVC   JRNDATE(L'JRNTRF),JRNTRF                                         
         CURED JRCVTRF,(L'JRNDR,JRNDR+1),2,MINUS=YES,BRACKET=Y                  
         LA    R1,JRNDR                                                         
UPDRCL9  LA    R1,1(R1)                                                         
         CLI   0(R1),C'('                                                       
         BNE   *+8                                                              
         MVI   0(R1),C'*'          REPLACE ( WITH *                             
         CLI   0(R1),C')'                                                       
         BNE   UPDRCL9                                                          
         MVI   0(R1),C'*'          REPLACE ) WITH *                             
*                                                                               
UPDRCL10 LA    R2,L'REPP1(R2)      TOTAL DEBITS AND CREDITS                     
         MVC   JRNDATE(L'JRNTOT),JRNTOT                                         
         AP    JTOTCRS,JRCVCRS                                                  
         AP    JTOTDRS,JRCVDRS     DON'T ADD RECVAMT TO JTOTDRS                 
         AP    JRCVDRS,RECVAMT                                                  
         CURED JRCVCRS,(L'JRNCR,JRNCR),2,MINUS=YES                              
         CURED JRCVDRS,(L'JRNDR,JRNDR),2,MINUS=YES                              
*                                                                               
         LA    R2,L'REPP1(R2)      ITEMS                                        
         CURED RCVITEM,(8,JRNDATE),0,ALIGN=LEFT                                 
         LA    RF,JRNDATE                                                       
         AR    RF,R0                                                            
         MVC   1(L'JRNITEM,RF),JRNITEM                                          
*                                                                               
         LA    R2,L'REPP1(R2)      COSTING WARNING MESSAGE                      
         TM    OVRBYTE,NOCWOF+NOCDSC                                            
         BZ    UPDRCL12            BOTH NOCWOF+NOCDSC BITS OFF                  
         LA    R1,DMCB                                                          
         USING GETTXTD,R1                                                       
         XC    GTBLOCK,GTBLOCK                                                  
         MVI   GTMAXL,JRNMSGL      MAXIMUM LENGTH                               
         LA    R0,JRNDATE                                                       
         STCM  R0,7,GTAOUT         A(OUTPUT)                                    
         MVI   GTMTYP,GTMSCR                                                    
         MVI   GT1INDS,GT1NOREF+GT1OWRK                                         
         MVC   GTMSGNO,=AL2(AS$CSTAM)                                           
         GOTO1 VGETTXT,(R1)                                                     
         TM    OVRBYTE,NOCWOF+NOCDSC                                            
         BO    UPDRCL12            BOTH NOCWOF+NOCDSC BITS ON                   
*                                                                               
         MVC   JRNDATE,SPACES      ASSUME NOCWOF IS THE BIT ON                  
         MVC   GTMSGNO,=AL2(AS$WRTCM)                                           
         GOTO1 (RF),(R1)                                                        
         TM    OVRBYTE,NOCDSC      TEST NOCDSC BIT ON                           
         BZ    UPDRCL12                                                         
         MVC   JRNDATE,SPACES      DISCOUNT COSTING A/C(S) IN ERROR             
         MVC   GTMSGNO,=AL2(AS$DSFCM)                                           
         GOTO1 (RF),(R1)                                                        
*                                                                               
UPDRCL12 GOTO1 VREPORT,REPD                                                     
*                                                                               
UPDRCVLX AP    TOTITEM,RCVITEM     ADD RECEIVABLE ITEMS TO TOTAL                
         B     UPDATEXX                                                         
         DROP  R2                                                               
         EJECT                                                                  
UPDLAST  OI    REPHEADI,REPHFRCE   NEW PAGE FOR OVERALL POSTINGS                
         MVC   REPH6,SPACES                                                     
         OC    PRTSUB,PRTSUB       TEST IF PRINTING                             
         BZ    UPDLASTX                                                         
         LA    R2,REPP2                                                         
         USING JRNLINED,R2                                                      
         MVC   JRNDATE(44),=44C'-'                                              
         MVC   JRNDATE+16(L'JRNBTOT),JRNBTOT                                    
*                                                                               
         LA    R2,L'REPP1(R2)      ALLOCATED                                    
         MVC   JRNDATE(L'JRNMRK),JRNMRK                                         
         CURED JTOTMRK,(L'JRNCR,JRNCR),2,MINUS=YES                              
*                                                                               
         LA    R2,L'REPP1(R2)      SPECIALS                                     
         MVC   JRNDATE(L'JRNDIF),JRNDIF                                         
         CURED JTOTDIF,(L'JRNCR,JRNCR),2,MINUS=YES                              
*                                                                               
         LA    R2,L'REPP1(R2)      CHEQUE                                       
         MVC   JRNDATE(L'JRNCHQ),JRNCHQ                                         
         ZAP   JTOTCHQ,CHQAMT                                                   
         AP    JTOTDRS,CHQAMT      ADD CHEQUE TO JTOTDRS, NOW                   
         CURED JTOTCHQ,(L'JRNDR,JRNDR),2,MINUS=YES                              
*                                                                               
         LA    R2,L'REPP1(R2)      WRITTEN-OFF - SR                             
         MVC   JRNDATE(L'JRNWOFSR),JRNWOFSR                                     
         MVC   JRNDATE+L'JRNWOFSR+1(5),=C' - SR'                                
         CURED JTOTWOF,(L'JRNCR,JRNCR),2,MINUS=YES                              
*                                                                               
         LA    R2,L'REPP1(R2)      WRITE-OFF A/C                                
         MVC   JRNDATE(L'JRNWOF),JRNWOF                                         
         ZAP   DUB,JTOTWOF                                                      
         LA    RF,JRNDR                                                         
         TM    WOFFINDS,WOFFISI    TEST SI WRITE-OFF                            
         BZ    *+14                                                             
         LA    RF,JRNCR            CREDIT                                       
         MP    DUB,=P'-1'          MINUS                                        
         CURED DUB,(L'JRNCR,(RF)),2,MINUS=YES                                   
*                                                                               
         LA    R2,L'REPP1(R2)                                                   
         MVC   JRNDATE(L'JRNOFS),JRNOFS                                         
         ZAP   DUB,JTOTOFSP        TAKE POSITIVE OFFSETS                        
         AP    DUB,JTOTOFSN        ADD NEGATIVE OFFSETS                         
         CP    DUB,PZERO           TEST OFFSETS IN BALANCE                      
         BE    UPDLAS2                                                          
         CURED DUB,(L'JRNCR,JRNCR),2,MINUS=YES                                  
         B     UPDLAS6                                                          
UPDLAS2  CURED JTOTOFSP,(L'JRNCR,JRNCR+1),2,MINUS=YES,BRACKET=Y                 
         LA    R1,JRNCR                                                         
UPDLAS4  LA    R1,1(R1)                                                         
         CLI   0(R1),C'('                                                       
         BNE   *+8                                                              
         MVI   0(R1),C'*'          REPLACE ( WITH *                             
         CLI   0(R1),C')'                                                       
         BNE   UPDLAS4                                                          
         MVI   0(R1),C'*'          REPLACE ) WITH *                             
*                                                                               
UPDLAS6  LA    R2,L'REPP1(R2)      DISCOUNT - SR                                
         MVC   JRNDATE(L'JRNDSCSR),JRNDSCSR                                     
         MVC   JRNDATE+L'JRNDSCSR+1(5),=C' - SR'                                
         CURED JTOTDSC,(L'JRNDR,JRNDR),2,MINUS=YES                              
*                                                                               
         LA    R2,L'REPP1(R2)      DISCOUNT A/C                                 
         MVC   JRNDATE(L'JRNDSC),JRNDSC                                         
         ZAP   DUB,JTOTDSC                                                      
         TM    DISCINDS,DISCISI    TEST SI DISCOUNT                             
         BZ    *+12                                                             
         LA    RF,JRNCR            SI - CREDIT                                  
         B     *+14                                                             
         LA    RF,JRNDR            SE/SQ - DEBIT                                
         MP    DUB,=P'-1'          REVERSE SIGN                                 
         CURED DUB,(L'JRNCR,(RF)),2,MINUS=YES                                   
*                                                                               
         CP    JTOTTRF,PZERO       TRANSFERRED                                  
         BE    UPDLAS10                                                         
         LA    R2,L'REPP1(R2)                                                   
         MVC   JRNDATE(L'JRNTRF),JRNTRF                                         
         CURED JTOTTRF,(L'JRNDR,JRNDR+1),2,MINUS=YES,BRACKET=YES                
         LA    R1,JRNDR                                                         
UPDLAS8  LA    R1,1(R1)                                                         
         CLI   0(R1),C'('                                                       
         BNE   *+8                                                              
         MVI   0(R1),C'*'          REPLACE ( WITH *                             
         CLI   0(R1),C')'                                                       
         BNE   UPDLAS8                                                          
         MVI   0(R1),C'*'          REPLACE ) WITH *                             
*                                                                               
UPDLAS10 LA    R2,L'REPP1(R2)      TOTAL DEBITS AND CREDITS                     
         MVC   JRNDATE(L'JRNTOT),JRNTOT                                         
         CURED JTOTCRS,(L'JRNCR,JRNCR),2,MINUS=YES                              
         CURED JTOTDRS,(L'JRNDR,JRNDR),2,MINUS=YES                              
*                                                                               
         LA    R2,L'REPP1(R2)      BALANCE                                      
         MVC   JRNDATE(L'JRNBAL),JRNBAL                                         
         ZAP   JTOTBAL,JTOTCHQ                                                  
         SP    JTOTBAL,JTOTMRK                                                  
         SP    JTOTBAL,JTOTDIF                                                  
         TM    DISCINDS,DISCIADD   TEST ADDING DISCOUNT TO BALANCE              
         BNZ   *+10                                                             
         AP    JTOTBAL,JTOTDSC                                                  
         CURED JTOTBAL,(L'JRNCR,JRNDR),2,MINUS=YES                              
*                                                                               
         LA    R2,L'REPP1(R2)      ITEMS                                        
         CURED TOTITEM,(8,JRNDATE),0,ALIGN=LEFT                                 
         LA    RF,JRNDATE                                                       
         AR    RF,R0                                                            
         MVC   1(L'JRNITEM,RF),JRNITEM                                          
         ZAP   DUB,JTOTOFSP        TEST OFFSETS BALANCE                         
         AP    DUB,JTOTOFSN                                                     
         CP    DUB,PZERO                                                        
         BNE   *+14                                                             
         CP    JTOTBAL,PZERO       TEST BALANCE WARNING REQ'D                   
         BE    UPDLAS12                                                         
         LA    R1,DMCB                                                          
         USING GETTXTD,R1                                                       
         XC    GTBLOCK,GTBLOCK                                                  
         MVI   GTMAXL,JRNMSGL      MAXIMUM LENGTH                               
         LA    R0,L'JRNITEM+9(RF)                                               
         STCM  R0,7,GTAOUT         A(OUTPUT)                                    
         MVI   GTMTYP,GTMSCR                                                    
         MVI   GT1INDS,GT1NOREF+GT1OWRK                                         
         MVC   GTMSGNO,=AL2(AS$BATNB)                                           
         GOTO1 VGETTXT,(R1)                                                     
*                                                                               
UPDLAS12 GOTO1 VREPORT,REPD                                                     
*                                                                               
UPDLASTX B     UPDATEXX                                                         
         DROP  R2,R4,R5                                                         
         EJECT                                                                  
***********************************************************************         
* BUILD A TRANSFER DEBIT POSTING                                      *         
* NTRY - AIO6 HOLDS ORIGINAL POSTING TO TRANSFER                      *         
***********************************************************************         
         SPACE 1                                                                
         USING RECVTABD,R4         R4=A(RECEIVABLES TABLE ENTRY)                
BLDXFR   NTR1  ,                                                                
         L     R2,AIO1             R2=A(TRANSFER SR IOAREA)                     
         USING TRNRECD,R2                                                       
         L     R5,AIO6             R5=A(ORIGINAL SR IOAREA)                     
         MVC   TRNKEY(ACCORFST),TRNKEY-TRNRECD(R5)                              
         MVC   TRNKACT,TSARTRFA    SUBSTITUTE TRANSFER ACCOUNT                  
         LR    R3,R2                                                            
         AH    R3,DATADISP         ADDRESS FIRST ELEMENT                        
         USING TRNELD,R3                                                        
         AH    R5,DATADISP                                                      
         CLI   TRNEL-TRNELD(R5),TRNELQ                                          
         BE    *+6                                                              
         DC    H'0'                TRANSACTION ELEMENT NOT FIRST                
         IC    R1,TRNLN-TRNELD(R5)                                              
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   TRNEL(0),TRNEL-TRNELD(R5)                                        
         NI    TRNSTAT,255-TRNSREV ENSURE POSTING IS NOT A REVERSAL             
         MVC   TRNMOS,BATMON       SUBSTITUTE BATCH REFERENCE                   
         MVC   TRNBREF,BATREF                                                   
         MVI   TRNTYPE,30          SUBSTITUTE BATCH TYPE                        
         SR    R0,R0                                                            
         IC    R0,TRNLN            TAKE L'TRANSACTION                           
         AR    R5,R0               R5=A(NEXT ELEMENT)                           
         SR    RF,RF                                                            
         ICM   RF,3,TRNRLEN        RECORD LENGTH                                
         SR    RF,R0                                                            
         STCM  RF,3,TRNRLEN        REDUCE L'RECORD BY L'TRNEL                   
         SH    RF,DATADISP         SUBTRACT L'KEY                               
         STH   RF,HALF             SAVE L'RECORD BEYOND TRNEL                   
*                                                                               
BLDXFR2  MVC   TEMP,SPACES                                                      
         LA    RF,TEMP                                                          
         SR    RE,RE                                                            
         IC    RE,TRNLN                                                         
         SH    RE,=Y(TRNLN1Q+1)    MINUS L'FIXED PORTION -1 FOR EXECUTE         
         BM    BLDXFR4                                                          
         EX    RE,*+8                                                           
         BE    BLDXFR4                                                          
         CLC   TRNNARR(0),SPACES                                                
         EX    RE,*+4                                                           
         MVC   TEMP(0),TRNNARR                                                  
         LA    RF,0(RE,RF)         RF=LAST BYTE OF TRNNARR                      
         CLI   0(RF),C' '          SEEK LAST CHARACTER IN TRNNARR               
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         LA    RF,2(RF)                                                         
BLDXFR4  MVC   0(L'TXTTRFFR,RF),TXTTRFFR                                        
         LA    RF,L'TXTTRFFR+1(RF)                                              
         MVC   0(L'RECVACT-1,RF),RECVACT+1                                      
         LA    RF,L'RECVACT-2(RF)                                               
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         LA    RF,2(RF)                                                         
         MVC   0(L'TXTON,RF),TXTON                                              
         LA    RF,L'TXTON+1(RF)                                                 
         LR    R0,RF                                                            
         GOTO1 VDATCON,DMCB,(1,TODAYP),(17,0(RF))                               
         LR    RF,R0                                                            
         LA    RF,8(RF)            MAXIMUM L'DATE EXPRESSION                    
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         LA    RF,1(RF)                                                         
         LR    RE,RF                                                            
         LA    R0,TEMP                                                          
         SR    RF,R0               RF=NEW L'TRNNARR                             
         CLM   RF,1,=AL1(L'TRNNARR)                                             
         BNH   *+14                                                             
         MVC   TRNNARR,SPACES      ELSE CLEAR EXISTING NARRATIVE                
         B     BLDXFR2             AND RETURN TO BUILD NEW NARRATIVE            
*                                                                               
         BCTR  RF,0                L'TRNNARR -1 FOR EXECUTE                     
         EX    RF,*+4                                                           
         MVC   TRNNARR(0),TEMP                                                  
         LA    RF,TRNLN1Q+1(RF)    ADD L'FIXED PORTION +1                       
         STC   RF,TRNLN            SAVE NEW LENGTH                              
         ICM   R1,3,TRNRLEN        RECORD LENGTH (MINUS OLD TRNLN)              
         AR    R1,RF               ADD NEW TRNLN                                
         STCM  R1,3,TRNRLEN        UPDATE RECORD LENGTH                         
         LR    R0,RF               TAKE NEW TRNLN                               
         AR    R0,R3               R0=A(REMAINDER OF RECORD)                    
         LH    R1,HALF             R1=L'REMAINDER OF RECORD                     
         AR    R1,R0               LOCATE EOR                                   
         MVI   0(R1),0             SET EOR                                      
         SR    R1,R0               RESET R1=(LENGTH OF REMAINDER)               
         LR    RE,R5               RE=A(ORIGINAL RECORD BEYOND TRNEL)           
         LR    RF,R1                                                            
         MVCL  R0,RE               RESTORE REMAINDER OF RECORD                  
         SR    R0,R0                                                            
         USING TRSELD,R3                                                        
BLDXFR6  IC    R0,TRSLN            FIND TRANSACTION STATUS ELEMENT              
         AR    R3,R0                                                            
         CLI   0(R3),0             TEST E-O-R                                   
         BNE   *+6                                                              
         DC    H'0'                TRSEL MISSING                                
         CLI   0(R3),TRSELQ        TEST TRANSACTION STATUS ELEMENT              
         BNE   BLDXFR6             TRY AGAIN                                    
         SR    RF,RF                                                            
         IC    RF,TRSLN                                                         
         SH    RF,=H'3'                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         XC    TRSDATE(0),TRSDATE  CLEAR ELEMENT VALUES                         
BLDXFRX  GOTO1 VHELLO,DMCB,(C'D',ACCFIL),('APEELQ',(R2)),0,0                    
         B     UPDATEXX                                                         
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* BUILD 'TRANSFERRED TO' NARRATIVE FOR SR POSTING                     *         
***********************************************************************         
         SPACE 1                                                                
BLDTNAR  LR    R0,RE               SAVE RETURN ADDRESS                          
         MVC   POSTNARR,SPACES     TRANSFER NARRATIVE                           
         LA    R2,POSTNARR                                                      
         MVC   0(L'TXTTRFTO,R2),TXTTRFTO                                        
         LA    R2,L'TXTTRFTO+1(R2)                                              
         MVC   0(L'RECVUL,R2),RECVUL                                            
         MVC   L'RECVUL(L'TSARTRFA,R2),TSARTRFA                                 
         LA    R2,L'RECVUL+L'TSARTRFA-1(R2)                                     
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         LA    R2,2(R2)                                                         
         MVC   0(L'TXTON,R2),TXTON                                              
         LA    R2,L'TXTON+1(R2)                                                 
         GOTO1 VDATCON,DMCB,(1,TODAYP),(17,0(R2))                               
         LR    RE,R0               RESTORE RETURN ADDRESS                       
         BR    RE                  RETURN TO CALLER                             
         SPACE 2                                                                
***********************************************************************         
* BUILD WRITE-OFF NARRATIVE FOR SR POSTING                            *         
***********************************************************************         
         SPACE 1                                                                
BLDWNAR  LR    R0,RE               SAVE RETURN ADDRESS                          
         MVC   POSTNARR,SPACES                                                  
         LA    R2,POSTNARR                                                      
         MVC   0(L'TXTWOF,R2),TXTWOF                                            
         LA    R2,L'TXTWOF(R2)                                                  
         MVC   0(L'WOFREF,R2),WOFREF                                            
         LA    R2,L'WOFREF-1(R2)                                                
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         MVC   2(L'TXTDAT,R2),TXTDAT                                            
         LA    R2,L'TXTDAT+3(R2)                                                
         GOTO1 VDATCON,DMCB,(1,WOFDATE),(17,0(R2))                              
         LA    R2,9(R2)                                                         
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         MVC   2(L'TXTACC,R2),TXTACC                                            
         LA    R2,L'TXTACC+3(R2)                                                
         MVC   0(L'WOFF,R2),WOFF                                                
         LA    R2,L'WOFF+1(R2)                                                  
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         MVC   2(L'WOFFNAME,R2),WOFFNAME                                        
         LR    RE,R0               RESTORE RETURN ADDRESS                       
         BR    RE                  RETURN TO CALLER                             
         SPACE 2                                                                
***********************************************************************         
* BUILD WRITE-OFF INVOICE NARRATIVE FOR WRITE-OFF A/C POSTING         *         
***********************************************************************         
         SPACE 1                                                                
BLDINAR  LR    R0,RE               SAVE RETURN ADDRESS                          
         MVC   POSTNARR,SPACES                                                  
         LA    R2,POSTNARR                                                      
         MVC   0(L'TXTINV,R2),TXTINV                                            
         LA    R2,L'TXTINV(R2)                                                  
         MVC   0(L'TSARREF,R2),TSARREF                                          
         LA    R2,L'TSARREF-1(R2)                                               
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         MVC   2(L'TXTDAT,R2),TXTDAT                                            
         LA    R2,L'TXTDAT+3(R2)                                                
         GOTO1 VDATCON,DMCB,(1,TSARDAT),(17,0(R2))                              
         LA    R2,9(R2)                                                         
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         MVC   2(L'WOFNARR,R2),WOFNARR                                          
         LR    RE,R0               RESTORE RETURN ADDRESS                       
         BR    RE                  RETURN TO CALLER                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO REVERSE POSTING AMOUNT                                   *         
***********************************************************************         
         SPACE 1                                                                
REVAMNT  ZAP   DUB,POSTAMNT        ROUTINE TO REVERSE POSTING AMOUNT            
         MP    DUB,=P'-1'                                                       
         ZAP   POSTAMNT,DUB                                                     
         BR    RE                                                               
*                                                                               
UPDATEXX XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* BUILD STRING OF POSTINGS AT MYPARM                                  *         
***********************************************************************         
                                                                                
UPMYPARM NTR1  BASE=*,LABEL=*                                                   
         LA    R1,MYPARM                                                        
                                                                                
         LA    RE,RECVACT+1        RECVACT                                      
         ST    RE,0(R1)                                                         
         MVI   0(R1),0                                                          
         TM    TSARINDS,TSARISDR                                                
         BZ    *+8                                                              
         MVI   0(R1),TRNSDR                                                     
         LA    R1,4(R1)                                                         
         MVI   0(R1),X'FF'                                                      
*                                                                               
         TM    TSARINDS,TSARIWOF   IS THIS A WRITEOFF?                          
         BZ    UPMY44              NO                                           
*                                                                               
         LA    RE,WOFF             WOFF                                         
         ST    RE,0(R1)                                                         
         MVI   0(R1),TRNSDR                                                     
*                                                                               
         TM    WOFFINDS,WOFFISI    IF SI WRITE-OFF, LEAVE AS DR                 
         BO    UPMY32                                                           
         TM    WOFFINDS,WOFFISE    IS IT AN SE WRITEOFF                         
         BZ    UPMY28              NO                                           
         TM    WOFFINDS,WOFFIYCP                                                
         BNZ   *+12                                                             
         CLI   WOFFCOST,C' '                                                    
         BNH   UPMY28                                                           
         TM    OVRBYTE,NOCWOF                                                   
         BNZ   UPMY28                                                           
         CP    TSARPOST,PZERO                                                   
         BNL   *+8                                                              
         MVI   0(R1),0                                                          
         LA    R1,4(R1)                                                         
         MVI   0(R1),X'FF'                                                      
*                                                                               
         TM    COMPSTA5,CPYSNCST   COMPANY ON NEW COST?                         
         BZ    *+12                NO, SKIP NEXT TEST                           
         TM    WOFFINDS,WOFFIYCP   NEW COST?                                    
         BZ    UPMY36                                                           
*                                                                               
         LA    RE,RECVCOST         RECVCOST                                     
         ST    RE,0(R1)                                                         
         MVI   0(R1),0                                                          
         LA    R1,4(R1)                                                         
         MVI   0(R1),X'FF'                                                      
*                                                                               
         LA    RE,CWOF3ACT         CWOF3ACT                                     
         ST    RE,0(R1)                                                         
         MVI   0(R1),0                                                          
         LA    R1,4(R1)                                                         
         MVI   0(R1),X'FF'                                                      
*                                                                               
         LA    RE,CWOFPACT         CWOFPACT                                     
         ST    RE,0(R1)                                                         
         MVI   0(R1),TRNSDR                                                     
         LA    R1,4(R1)                                                         
         MVI   0(R1),X'FF'                                                      
         B     UPMY36                                                           
*                                                                               
UPMY28   TM    WOFFINDS,WOFFIPAY   TEST CR/-CR (PAYABLES)                       
         BO    UPMY30              ALWAYS SWAP AMOUNT AND POST CR               
         CP    TSARPOST,PZERO      TEST -DR                                     
         BNL   UPMY32                                                           
*                                                                               
UPMY30   MVI   0(R1),0                                                          
*                                                                               
UPMY32   LA    R1,4(R1)                                                         
         MVI   0(R1),X'FF'                                                      
*                                                                               
         TM    WOFFINDS,WOFFISI    TEST SI WRITE-OFF                            
         BZ    UPMY36                                                           
         CLI   WOFFCOST,C' '       TEST WRITE-OFF A/C ANALYSIS FLAG             
         BH    *+14                                                             
         OC    WOFFANAL,WOFFANAL   TEST WRITE-OFF ANALYSIS A/C                  
         BZ    UPMY36                                                           
         TM    OVRBYTE,NOCWOF      TEST WRITE-OFF COSTING POSTINGS OK           
         BNZ   UPMY36                                                           
*                                                                               
         LA    RE,RECVCOST         RECVCOST                                     
         ST    RE,0(R1)                                                         
         MVI   0(R1),TRNSDR                                                     
         LA    R1,4(R1)                                                         
         MVI   0(R1),X'FF'                                                      
*                                                                               
         LA    RE,CWOF2ACT         CWOF2ACT                                     
         ST    RE,0(R1)                                                         
         MVI   0(R1),0                                                          
         LA    R1,4(R1)                                                         
         MVI   0(R1),X'FF'                                                      
*                                                                               
UPMY36   TM    WOFFINDS,WOFFISE    TEST SE WRITE-OFF                            
         BZ    UPMY44                                                           
         TM    WOFFSTAT,RSTSEADD   TEST DEPARTMENT ANALYSIS                     
         BZ    UPMY38                                                           
*                                                                               
         LA    RE,DEPT             DEPT                                         
         ST    RE,0(R1)                                                         
         MVI   0(R1),TRNSDR                                                     
         LA    R1,4(R1)                                                         
         MVI   0(R1),X'FF'                                                      
*                                                                               
         LA    RE,ANAL8ACT         ANAL8ACT                                     
         ST    RE,0(R1)                                                         
         MVI   0(R1),0                                                          
         LA    R1,4(R1)                                                         
         MVI   0(R1),X'FF'                                                      
*                                                                               
UPMY38   TM    WOFFSTAT,RSTSGPEI   TEST STAFF ANALYSIS                          
         BZ    UPMY44                                                           
*                                                                               
         LA    RE,STAF             STAF                                         
         ST    RE,0(R1)                                                         
         MVI   0(R1),TRNSDR                                                     
         LA    R1,4(R1)                                                         
         MVI   0(R1),X'FF'                                                      
*                                                                               
         LA    RE,ANAL9ACT         ANAL9ACT                                     
         ST    RE,0(R1)                                                         
         MVI   0(R1),0                                                          
         LA    R1,4(R1)                                                         
         MVI   0(R1),X'FF'                                                      
*                                                                               
UPMY44   OC    DISC,DISC           TEST DISCOUNT ACCOUNT PRESENT                
         BZ    UPMY72                                                           
         TM    TSARINDS,TSARIGRS   TEST GROSS POSTING - NO DISCOUNT             
         BO    UPMY72                                                           
         CP    TSARDISC,PZERO      TEST ANY POSTING TO MAKE                     
         BE    UPMY72                                                           
         TM    TSARINDS,TSARILAT   TEST LATE (SEPARATE POSTING)                 
         BZ    UPMY48              1ST REC/SRC POSTING HANDLED DISCOUNT         
*                                                                               
         LA    RE,RECVACT+1        RECVACT                                      
         ST    RE,0(R1)                                                         
         MVI   0(R1),TRNSDR                                                     
         LA    R1,4(R1)                                                         
         MVI   0(R1),X'FF'                                                      
*                                                                               
UPMY48   LA    RE,DISC             DISC                                         
         ST    RE,0(R1)                                                         
         MVI   0(R1),TRNSDR                                                     
*                                                                               
         TM    DISCINDS,DISCISI    TEST SI DISCOUNT                             
         BO    UPMY58                                                           
         TM    DISCINDS,DISCISE    TEST SE DISCOUNT                             
         BZ    UPMY60                                                           
         TM    DISCINDS,DISCIYCP   TEST NEW COSTING POSTINGS                    
         BNZ   *+12                                                             
         CLI   DISCCOST,C' '       TEST DISCOUNT A/C ANALYSIS FLAG              
         BNH   UPMY60              STANDARD POSTING TO SE                       
         TM    OVRBYTE,NOCDSC      TEST DISCOUNT COSTING POSTINGS OK            
         BNZ   UPMY60              STANDARD POSTING TO SE                       
         LA    R1,4(R1)                                                         
         MVI   0(R1),X'FF'                                                      
*                                                                               
         LA    RE,RECVCOST         RECVCOST                                     
         ST    RE,0(R1)                                                         
         MVI   0(R1),0                                                          
         LA    R1,4(R1)                                                         
         MVI   0(R1),X'FF'                                                      
*                                                                               
         LA    RE,CDSCPACT         CDSCPACT                                     
         ST    RE,0(R1)                                                         
         MVI   0(R1),TRNSDR                                                     
         LA    R1,4(R1)                                                         
         MVI   0(R1),X'FF'                                                      
         B     UPMY62                                                           
*                                                                               
UPMY58   MVI   0(R1),0                                                          
UPMY60   LA    R1,4(R1)                                                         
         MVI   0(R1),X'FF'                                                      
*                                                                               
UPMY62   TM    DISCINDS,DISCISE    TEST SE DISCOUNT                             
         BZ    UPMY70                                                           
         TM    DISCSTAT,RSTSEADD   TEST DEPARTMENT ANALYSIS                     
         BZ    UPMY64                                                           
*                                                                               
         LA    RE,DEPT             DEPT                                         
         ST    RE,0(R1)                                                         
         MVI   0(R1),TRNSDR                                                     
         LA    R1,4(R1)                                                         
         MVI   0(R1),X'FF'                                                      
*                                                                               
         LA    RE,ANAL8ACT         ANAL8ACT                                     
         ST    RE,0(R1)                                                         
         MVI   0(R1),0                                                          
         LA    R1,4(R1)                                                         
         MVI   0(R1),X'FF'                                                      
*                                                                               
UPMY64   TM    DISCSTAT,RSTSGPEI   TEST STAFF ANALYSIS                          
         BZ    UPMY70                                                           
*                                                                               
         LA    RE,STAF             STAF                                         
         ST    RE,0(R1)                                                         
         MVI   0(R1),TRNSDR                                                     
         LA    R1,4(R1)                                                         
         MVI   0(R1),X'FF'                                                      
*                                                                               
         LA    RE,ANAL9ACT         ANAL9ACT                                     
         ST    RE,0(R1)                                                         
         MVI   0(R1),0                                                          
         LA    R1,4(R1)                                                         
         MVI   0(R1),X'FF'                                                      
*                                                                               
UPMY70   TM    DISCINDS,DISCISI    TEST SI DISCOUNT                             
         BZ    UPMY72                                                           
         CLI   DISCCOST,C' '       TEST DISCOUNT A/C ANALYSIS FLAG              
         BH    *+14                                                             
         OC    DISCANAL,DISCANAL   TEST DISCOUNT ANALYSIS A/C                   
         BZ    UPMY72                                                           
         TM    OVRBYTE,NOCDSC      TEST DISCOUNT COSTING POSTINGS OK            
         BNZ   UPMY72                                                           
*                                                                               
         LA    RE,RECVCOST         RECVCOST                                     
         ST    RE,0(R1)                                                         
         MVI   0(R1),TRNSDR                                                     
         LA    R1,4(R1)                                                         
         MVI   0(R1),X'FF'                                                      
*                                                                               
         LA    RE,CDSC2ACT         CDSC2ACT                                     
         ST    RE,0(R1)                                                         
         MVI   0(R1),0                                                          
         LA    R1,4(R1)                                                         
         MVI   0(R1),X'FF'                                                      
         B     UPMY72                                                           
*                                                                               
UPMY72   CP    RECVAMT,PZERO                                                    
         BE    UPMYX                                                            
         LA    RE,BANK                                                          
         ST    RE,0(R1)                                                         
         MVI   0(R1),TRNSDR                                                     
         LA    R1,4(R1)                                                         
         MVI   0(R1),X'FF'                                                      
*                                                                               
UPMYX    XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
JRNSPEC  DS    0X                  ** SPECS FOR JOURNAL **                      
         SPROG 0,1,2                                                            
         SPEC  H1,1,RUN                                                         
         SPEC  H1,71,PAGE                                                       
         SPEC  H8,1,AC#ACC,7                                                    
         SPEC  H9,1,AC#ACC,7,LU                                                 
         SPEC  H8,16,AC#CTRA,10                                                 
         SPEC  H9,16,AC#CTRA,10,LU                                              
         SPEC  H8,32,AC#OFF,3                                                   
         SPEC  H9,32,AC#OFF,3,LU                                                
         SPEC  H8,36,AC#DATE,4                                                  
         SPEC  H9,36,AC#DATE,4,LU                                               
         SPEC  H8,45,AC#REFN,6                                                  
         SPEC  H9,45,AC#REFN,6,LU                                               
         SPEC  H8,52,AC#STT,3                                                   
         SPEC  H9,52,AC#STT,3,LU                                                
         SPEC  H8,61,AC#DRS,6                                                   
         SPEC  H9,61,AC#DRS,6,LU                                                
         SPEC  H8,73,AC#CRS,7                                                   
         SPEC  H9,73,AC#CRS,7,LU                                                
         SPROG 0                                                                
         SPEC  H1,29,AC#APOS,24                                                 
         SPEC  H2,29,AC#APOS,24,LU                                              
         SPROG 1                                                                
         SPEC  H1,26,AC#DPOS,30                                                 
         SPEC  H2,26,AC#DPOS,30,LU                                              
         SPROG 2                                                                
         SPEC  H1,21,AC#FPOS,39                                                 
         SPEC  H2,21,AC#FPOS,39,LU                                              
JRNSPECX DC    AL1(EOT)            SPEC END MARKER                              
         EJECT                                                                  
OFFTABD  DSECT                     DSECT COVERS OFFICE AMOUNT TABLE             
OFFTOFFC DS    CL(L'TRNOFFC)       OFFICE CODE                                  
OFFTEOTQ EQU   0                   END OF TABLE INDICATOR                       
OFFTAMNT DS    PL(L'TRNAMNT)       AMOUNT FOR BANK POSTING                      
OFFTMAX  EQU   64                  MAXIMUM NUMBER OF ENTRIES IN TABLE           
OFFTABL  EQU   *-OFFTABD           LENGTH OF TABLE ENTRY                        
         SPACE 2                                                                
OVRWRKD  DSECT                     DSECT COVERS OVERLAY TEMPORARY W/S           
VCATCALL DS    V                                                                
AVALHED  DS    A                                                                
AUPDATE  DS    A                                                                
*                                                                               
SVFADDH  DS    A                   SAVED AREA FOR TEMPORARY ADDRESS             
SVFADD   DS    A                   SAVED AREA FOR TEMPORARY ADDRESS             
SVLEN    DS    XL1                 SAVED AREA FOR TEMPORARY LENGTH              
SVDFACT  DS    CL(L'ACTKACT)       SAVED AREA FOR DEFAULT CLI SR ACCT           
LASTCLI  DS    CL(L'ACTKACT)       SAVED AREA FOR CLIENT COMPARE                
PRDACT   DS    CL(L'ACTKACT)                                                    
*                                                                               
DEPTUL   DS    CL2                 DEPARTMENT UNIT/LEDGER                       
STAFUL   DS    CL2                 PERSON UNIT/LEDGER                           
EXPAUL   DS    CL2                 EXPENSE ANALYSIS UNIT/LEDGER                 
CLIAUL   DS    CL2                 CLIENT ANALYSIS UNIT/LEDGER                  
COST2    DS    CL2                 12 COSTING UNIT/LEDGER                       
COST3    DS    CL2                 13 COSTING UNIT/LEDGER                       
COSTP    DS    CL2                 1P COSTING UNIT/LEDGER                       
*                                                                               
WOFFLST  DS    0X                  VALID LEDGERS FOR WRITE-OFF                  
WOFFULSA DS    CL2                                                              
WOFFULSB DS    CL2                                                              
WOFFULSC DS    CL2                                                              
WOFFULSE DS    CL2                                                              
WOFFULSF DS    CL2                                                              
WOFFULSI DS    CL2                                                              
WOFFULSV DS    CL2                                                              
WOFFULSW DS    CL2                                                              
WOFFULSX DS    CL2                                                              
WOFFULSY DS    CL2                                                              
         DS    AL1                                                              
WOFFLSTL EQU   *-WOFFLST                                                        
*                                                                               
DISCLST  DS    0X                  VALID LEDGERS FOR DISCOUNT                   
DISCULSE DS    CL2                                                              
DISCULSI DS    CL2                                                              
         DS    AL1                                                              
DISCLSTL EQU   *-DISCLST                                                        
*                                                                               
NINES    DS    CL12                C'999999999999'                              
*                                                                               
OVRPKWK  DS    PL16                LARGE PACKED WORK AREA                       
OVRNARR  DS    CL(L'POSTNARR)      STANDARD NARRATIVE SAVED HERE                
OVRBYTE  DS    XL1                 FLAGGING BYTE                                
NOCWOF   EQU   X'80'               WRITE-OFF COSTING A/C(S) MISSING             
NOCDSC   EQU   X'40'               DISCOUNT COSTING A/C(S) MISSING              
OVRCLI   EQU   X'20'               USING CLI FILTERS INSTEAD OF SR              
OVRCLN   EQU   X'10'               NOTHING AT PROD LEV USE CLI LEV              
OVRRCVL  EQU   X'08'               WE HAVE A CURRENT LINE INFO                  
OVRCLV   EQU   X'04'               DOING CLIENT LEVEL                           
OVRCGD   EQU   X'02'               SHOW THAT CLIENT FILTER IS GOOD              
OVR2NDLN EQU   X'01'               DOING THE 2ND LINE OF RECFLD                 
*                                                                               
OVRBYT2  DS    XL1                 FLAGGING BYTE                                
OVRDNE   EQU   X'80'               READING IS DONE                              
*                                                                               
BANKDATP DS    PL3                 PWOS BANK DEPOSIT DATE                       
*                                                                               
COSTCACN DS    CL(L'RECNAME)       1C COSTING ACCOUNT NAME                      
*                                  WRITE-OFF COSTING ANALYSIS A/CS              
CWOF2ACT DS    CL(L'ACTKCULA-1)    12 COSTING UNIT/LEDGER/ACCOUNT               
CWOF2ACN DS    CL(L'RECNAME)       12 COSTING ACCOUNT NAME                      
CWOF3ACT DS    CL(L'ACTKCULA-1)    13 COSTING UNIT/LEDGER/ACCOUNT               
CWOF3ACN DS    CL(L'RECNAME)       13 COSTING ACCOUNT NAME                      
CWOFPACT DS    CL(L'ACTKCULA-1)    1P COSTING UNIT/LEDGER/ACCOUNT               
*                                  DISCOUNT COSTING ANALYSIS A/CS               
CDSC2ACT DS    CL(L'ACTKCULA-1)    12 COSTING UNIT/LEDGER/ACCOUNT               
CDSC2ACN DS    CL(L'RECNAME)       12 COSTING ACCOUNT NAME                      
CDSC3ACT DS    CL(L'ACTKCULA-1)    13 COSTING UNIT/LEDGER/ACCOUNT               
CDSC3ACN DS    CL(L'RECNAME)       13 COSTING ACCOUNT NAME                      
CDSCPACT DS    CL(L'ACTKCULA-1)    1P COSTING UNIT/LEDGER/ACCOUNT               
*                                                                               
ANAL8ACT DS    CL(L'ACTKCULA-1)    28 ANALYSIS UNIT/LEDGER/ACCOUNT              
ANAL8ACN DS    CL(L'RECNAME)       28 ANALYSIS ACCOUNT NAME                     
ANAL9ACT DS    CL(L'ACTKCULA-1)    29 ANALYSIS UNIT/LEDGER/ACCOUNT              
ANAL9ACN DS    CL(L'RECNAME)       29 ANALYSIS ACCOUNT NAME                     
*                                                                               
JRACCUMS DS    0PL6                ** JOURNAL RECEIVABLE TOTALS **              
JRCVMRK  DS    PL6                 ALLOCATION AMOUNT                            
JRCVWOF  DS    PL6                 WRITE-OFF AMOUNT                             
JRCVOFSP DS    PL6                 OFFSET AMOUNT - POSITIVE                     
JRCVOFSN DS    PL6                 OFFSET AMOUNT - NEGATIVE                     
JRCVTRF  DS    PL6                 TRANSFER AMOUNT                              
JRCVDIF  DS    PL6                 SPECIAL SCREEN AMOUNT                        
JRCVDSC  DS    PL6                 DISCOUNT AMOUNT                              
JRCVDRS  DS    PL6                 DEBITS                                       
JRCVCRS  DS    PL6                 CREDITS                                      
JRACCUMN EQU   (*-JRACCUMS)/L'JRACCUMS                                          
*                                                                               
JTACCUMS DS    0PL6                ** JOURNAL TOTALS **                         
JTOTMRK  DS    PL6                 ALLOCATION AMOUNT                            
JTOTWOF  DS    PL6                 WRITE-OFF AMOUNT                             
JTOTOFSP DS    PL6                 OFFSET AMOUNT - POSITIVE                     
JTOTOFSN DS    PL6                 OFFSET AMOUNT - NEGATIVE                     
JTOTTRF  DS    PL6                 TRANSFER AMOUNT                              
JTOTDIF  DS    PL6                 SPECIAL SCREEN AMOUNT                        
JTOTDSC  DS    PL6                 DISCOUNT AMOUNT                              
JTOTCHQ  DS    PL6                 CHEQUE AMOUNT                                
JTOTDRS  DS    PL6                 DEBITS                                       
JTOTCRS  DS    PL6                 CREDITS                                      
JTOTBAL  DS    PL6                 BALANCE                                      
JTACCUMN EQU   (*-JTACCUMS)/L'JTACCUMS                                          
*                                                                               
RCVITEM  DS    PL4                 RECEIVABLE ITEM COUNT                        
TOTITEM  DS    PL4                 TOTAL ITEM COUNT                             
*                                                                               
RCVFLDS  DS    0C                                                               
RCVFLDH  DS    CL8                                                              
RCVFLD   DS    CL234                                                            
RCVFLDX  DS    CL8                                                              
RCVFLDLN EQU   *-RCVFLDS                                                        
RCVFLD2H DS    CL8                                                              
RCVFLD2  DS    CL234                                                            
RCVFLD2X DS    CL8                                                              
RCVFLDLQ EQU   *-RCVFLDS                                                        
*                                                                               
ELEXTRA  DS    XL256               AREA FOR EXTRA ELEMENTS                      
*                                                                               
OFFTAB   DS    (OFFTMAX)XL(OFFTABL)                                             
*                                                                               
DOUTLSTU DS    0C                  ** UPPER CASE DICTIONARY **                  
TXTOFS   DS    CL6                                                              
*                                                                               
TXTACC   DS    CL3                                                              
TXTDAT   DS    CL4                                                              
TXTWOF   DS    CL11                                                             
TXTINV   DS    CL9                                                              
*                                                                               
TXTTRFFR DS    CL16                                                             
TXTTRFTO DS    CL14                                                             
TXTON    DS    CL2,CL1                                                          
*                                                                               
DOUTLSTL DS    0C                  ** LOWER CASE DICTIONARY **                  
JRNBTCH  DS    CL13                                                             
JRNRECV  DS    CL14                                                             
JRNBANK  DS    CL12                                                             
JRNNARR  DS    CL9                                                              
JRNBTOT  DS    CL12                                                             
JRNMRK   DS    CL9                                                              
JRNDIF   DS    CL8                                                              
JRNCHQP  DS    CL13                                                             
JRNCHQ   DS    CL12                                                             
JRNWOFSR DS    CL11                                                             
JRNWOF   DS    CL13                                                             
JRNOFS   DS    CL6                                                              
JRNTRF   DS    CL11                                                             
JRNDSCSR DS    CL8                                                              
JRNDSC   DS    CL12                                                             
JRNTOT   DS    CL11                                                             
JRNBAL   DS    CL7                                                              
JRNITEM  DS    CL5                                                              
*                                                                               
CATBLK   DS    XL(CATLNQ)          CATCALL BLOCK                                
*                                                                               
MYPARM   DS    10F                                                              
         SPACE 2                                                                
JRNHEDD  DSECT                     DSECT COVERS A HEADLINE                      
JRNHTXT  DS    CL(L'JRNRECV)                                                    
         DS    CL2                                                              
JRNHACT  DS    CL(L'RECVACT-1)                                                  
         DS    CL2                                                              
JRNHACTN DS    CL(L'RECVACTN)                                                   
         EJECT                                                                  
       ++INCLUDE ACRECWRK                                                       
TWAD     DSECT                                                                  
         ORG   RECOLAYH                                                         
       ++INCLUDE ACRECF2D                                                       
         SPACE 1                                                                
* ACCATCALLD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACCATCALLD                                                     
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'062ACREC02   07/18/19'                                      
         END                                                                    
