*          DATA SET PPBUY01    AT LEVEL 050 AS OF 02/26/20                      
*PHASE T41101B                                                                  
*INCLUDE SRCHCALL                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'PPBUY01 - BUY PROGRAM HEADLINE EDIT'                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* JSAY 08/30/19 SET ALLOEXTQ IF EXTEDED ALLOACTIONS ARE AVAILABLE               
*                                                                               
* KWAN 07/05/19 COS2 ADJUSTMENT FOR PRISMA BUYS (IAPP-224713)                   
*                                                                               
* KWAN 04/01/16 PUB ZONE LOCK FIX - BUMPING ZONE NUMBERS PREMATURELY            
*                                                                               
* KWAN 12/07/15 SUPPORT TRADING DESK BUYS (PUB ZONE LOCK)                       
*                                                                               
* KWAN 06/29/15 SUPPORT MEDIA B (MOBILE), V (NATL VIDEO), W (LOCAL VID)         
*                                                                               
* KWAN 02/25/14 SUPPORT MEDIA L (SOCIAL)                                        
*                                                                               
* KWAN 04/03/13 SAVE PURCHASE ORDER# LEVEL FLAG ON CLIENT RECORD                
*                                                                               
* KWAN 09/13/11 MIDAS CLIENT FLAG                                               
*                                                                               
* KWAN 10/26/10 PUB LOCK SWITCH                                                 
*                                                                               
* KWAN 08/05/10 BYPASS AB PROFILE FOR DDS TERMINAL (FOR DEBUGGING)              
*                                                                               
* KWAN 02/21/06 STEWARDSHIP INSERTION                                           
*                                                                               
* KWAN 08/19/05 COS2 FACTOR ROUNDING OPTION IN F0 PROFILE                       
*                                                                               
* KWAN 07/12/05 IGNORE AB PROFILE FOR PBU UPLOADS                               
*                                                                               
* REMOVE OF INCLUDE GETPROF                                                     
*                                                                               
* SMYE 01/28/04 ADD CUSTOM COLUMNS (T41120) TR CODES RU/CU                      
*                                                                               
* KWAN 05/29/03 FIX FOR AB PROFILE "ADBUYER ONLY" ACCESS                        
*                                                                               
* KWAN 01/21/03 BY PROFILE CK FOR "ADBUYER ONLY" ACCESS                         
*                                                                               
* KWAN 09/26/01 CHANGES TO CALLS FOR T41116 (ADDITIONAL CHARGES)                
*                                                                               
* KWAN 08/02/01 NO TRAFFIC, NEED TP SAVE PPRDSTAT BYTE IN SVPRDSTA              
*                                                                               
* KWAN 03/05/01 ACTIVATE NO OPED CODES FOR ADDITIONAL CHARGES                   
*                                                                               
* KWAN 03/05/01 NO OP ADDITIONAL CHARGES (FIND ON EC WORD AND MYOVRLY)          
*                                                                               
* KWAN 02/13/01 ALLOW TRANSACTION CODES FOR ADDITIONAL CHARGES (AR/AC)          
*                                                                               
* SMYE 11/08/00 WHEN VALIDATING CLIENT IF CLIENT IS "FROZEN WITH DATE"          
*               SAVE CONTROL BYTE AND DATE (YM) FROM CLIENT RECORD              
*               FREEZE STATUS ELEMENT IN SVCLPROF+27 (3 BYTES)                  
*                                                                               
* BPLA 11/99    SAVE PRODUCT EXCLUSION CLASS (PPRDEXC) IN SVPEXCL               
*                                                                               
* KWAN 02/99    WHEN VALIDATING CLIENT CHECK FOR                                
*               COST 2 FACTOR SYTLE AND SAVE THE FACTOR IN SVC2FAC              
*               ALSO SAVE POSSIBLE FACTOR OVERRIDE FROM ESTIMATE                
*               SAVED IN SVE2FAC                                                
*                                                                               
* BPLA 12/98    CHANGES FOR SHOWING PUB NAMES WHEN LIST BUYING                  
*                                                                               
* BPLA 12/98    CHANGES FOR INTERACTIVE LIST BUYING                             
*                                                                               
* BPLA 08/98    CHANGES FOR MAGAZINE LIST BUYING                                
*                                                                               
* BPLA 06/98    READ (INTO P72APROF) THE P72A PROFILE                           
*               SO I CAN CHECK FOR SUPPRESSION OF AUTO I/O'S                    
*                                                                               
* BPLA 04/98    CHANGE FOR FROZEN CLIENTS                                       
*                                                                               
* BPLA 02/04/98 CHANGE FOR NEW MEDIA "I" I INTERACTIVE                          
*                                                                               
* BPLA 12/05/95 IF NEW AOR SYSTEM ACTIVE AND I'M NOT THE AOR                    
*               AND I'M LOOKING UP RATES FROM AOR CONTRACTS                     
*               DON'T CHECK BFLAT FOR "S" - IT REQUIRES                         
*               CONTRACTS ONLY FOR SLIDING SCALE NEWSPAPERS.                    
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
T41101   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T41101*                                                       
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T411FFD,RA                                                       
         LA    R9,T41101+4095      NOTE USE OF R9 AS 2ND BASE REG               
         LA    R9,1(R9)                                                         
         USING T41101+4096,R9                                                   
*                                                                               
         RELOC RELO01                                                           
*                                                                               
         XC    SVINS,SVINS         CLEAR DISPLAYED INSERTION LIST               
         MVC   X(40),BUYTR1H       SAVE FIRST 3 FIELDS + HEADERS                
         MVI   HALF,C'N'           SET SCREEN NOT CLEAR SWITCH                  
         OC    BUYCL,BLANKS                                                     
         OC    BUYPR,BLANKS                                                     
         EJECT                                                                  
*                                                                               
* EDIT MEDIA                                                                    
*                                                                               
         LA    R2,BUYMDH                                                        
         LA    R3,INVERR                                                        
         TM    4(R2),X'20'                                                      
         BO    REQ                                                              
         BAS   RE,CLRMD                                                         
         BAS   RE,ANY                                                           
*                                                                               
         XC    KEY,KEY             READ AGENCY/MEDIA RECORD                     
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),BUYMD                                                   
         MVI   KEY+3,1                                                          
         BAS   RE,HIGH                                                          
         CLC   KEYSAVE(4),KEY                                                   
         BNE   ERROR                                                            
         BAS   RE,GETREC                                                        
         MVC   SVAGPROF,PAGYPROF   SAVE AGY PROFILE                             
         MVC   NATION,PAGYNAT      SAVE NATIONALITY                             
*                                                                               
*        CHECK FOR FXREP                                                        
*                                                                               
         MVI   ELCODE,X'10'        AGENCY RECORD FXREP ELEMENT                  
         LA    R5,PAGYREC+33                                                    
         CLC   ELCODE,0(R5)        FOUND IN FIRST ELEM?                         
         BE    *+12                                                             
         BRAS  RE,NXTELEM                                                       
         BNE   AGYFXX              NO FX REP ELEMENT                            
*                                                                               
         MVC   FXREP,PAGFXREP-PAGFXEL(R5) SAVE FX REP                           
*                                                                               
AGYFXX   DS    0H                                                               
*                                                                               
         FOUT  BUYMDNMH,PAGYMED                                                 
         OI    4(R2),X'20'         SET FIELD CORRECT                            
*                                                                               
* EDIT REQUESTOR                                                                
*                                                                               
REQ      LA    R2,BUYNMH                                                        
         BAS   RE,ANY                                                           
*                                                                               
         CLI   BUYNM,C'*'          MEANS NO ASR ON CHANGES                      
*        BE    CLT                                                              
         BE    PID                                                              
         CLI   5(R2),3             IF DOESN'T START WITH * MAX IS 3             
*        BNH   CLT                                                              
         BNH   PID                                                              
         LA    R3,2                FIELD INVALID                                
         B     ERROR                                                            
*                                                                               
******************************************************************              
**  THIS ROUTINE WILL GET TWO BYTES FROM FATBLOCK                               
**  WHICH ARE "PERSONAL ID"                                                     
*                                                                               
PID      XC    SVPID,SVPID         PASSWORD ID NUMBER CLEARED                   
         L     RF,ACOMFACS                                                      
         USING COMFACSD,RF                                                      
         GOTO1 CGETFACT,DMCB,(2,0),0,0                                          
         DROP  RF                                                               
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         MVC   SVSECAGY,FATAGYSC                                                
         TM    FATFLAG,X'08'       CHECK IF SECRET CODE IS THERE                
         BZ    *+10                                                             
         MVC   SVPID,FAPASSWD      SAVE PASSWORD ID NUMBER                      
         DROP  R1                                                               
         SPACE 1                                                                
*                                                                               
* EDIT CLIENT CODE                                                              
*                                                                               
CLT      LA    R2,BUYCLH                                                        
         LA    R3,CLERR                                                         
         TM    4(R2),X'20'                                                      
         BO    PRD                                                              
*                                                                               
         XC    SVAGYR,SVAGYR                                                    
*                                                                               
         BAS   RE,CLRCL                                                         
         BAS   RE,ANY                                                           
*                                                                               
         CLI   5(R2),2                                                          
         BL    ERROR                                                            
         CLI   5(R2),3                                                          
         BH    ERROR                                                            
         BE    CLT2                                                             
         OC    BUYCL,BLANKS                                                     
         FOUT  (R2)                                                             
*                                                                               
* READ CLIENT RECORD                                                            
*                                                                               
CLT2     XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),BUYMD                                                   
         MVI   KEY+3,2                                                          
         MVC   KEY+4(3),BUYCL                                                   
*                                                                               
         BAS   RE,READ                                                          
         BAS   RE,GETREC                                                        
         LA    R0,SECBLK           INIT SECBLK (TEMP USE)                       
         LHI   R1,1024             IT CAN BE REUSED AFTER SECURITY CK           
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               SAME AS XCEF, ONLY SHORTER                   
*                                                                               
         OC    4(2,RA),4(RA)       TEST AGENCY ON NEW SECURITY                  
         BNZ   *+14                                                             
         OC    6(2,RA),6(RA)       TEST ANY LIMIT ACCESS                        
         BZ    CLT2M                                                            
         L     RF,ACOMFACS                                                      
         L     RF,CSECRET-COMFACSD(RF)                                          
         LA    R3,SECBLK                                                        
         GOTO1 (RF),DMCB,('SECPINIT',(R3)),0                                    
         BE    *+6                                                              
         DC    H'0'                BLOCK NOT BIG ENOUGH                         
*                                                                               
CLT2M    MVC   BYTE2,PCLTOFF       SAVE ORIGINAL CLT OFF CODE                   
         MVC   SVCLTOFC,PCLTOFF                                                 
         LA    R3,ACCERR                                                        
         LA    R4,T411FFD+6                                                     
         OC    0(2,R4),0(R4)                                                    
         BZ    CLT4                                                             
*                                                                               
         BRAS  RE,CKTRAFID         TRAFFIC ID SIGN-ON?                          
         BNE   CLT3                NO                                           
         BRAS  RE,TRAFFACC         LOOK FOR CLIENT TRAFFIC OFFICE CODE          
         CLI   BYTE3,0             ANYTHING FOUND?                              
         BE    CLT3                NO                                           
         MVC   PCLTOFF,BYTE3       USE CLIENT TRAFFIC OFFICE CODE               
*                                                                               
CLT3     XC    WORK,WORK           WORK MUST BE AT LEAST 48 BYTES               
         LA    R1,WORK             (LENGTH OF OFFICED IS 48 BYTES)              
         USING OFFICED,R1                                                       
         MVI   OFCSYS,C'P'                                                      
         MVC   OFCAUTH,6(RA)                                                    
         MVC   OFCAGY,AGYALPHA                                                  
         MVC   OFCOFC,PCLTOFF                                                   
         MVC   OFCCLT,PCLTKCLT                                                  
         OC    OFCCLT,=3C' '                                                    
         MVC   OFCPMED,PCLTKMED                                                 
         MVC   OFCLMT(4),6(RA)                                                  
         LA    RE,SECBLK                                                        
         STCM  RE,15,OFCSECD       ADDRESS OF SECRET BLOCK                      
         DROP  R1                                                               
*                                                                               
         XC    DMCB(12),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000A38'                                           
         GOTO1 VCALLOV,DMCB                                                     
         CLI   4(R1),255           ADDRESS OF OFFICER FOUND?                    
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(C'N',WORK),ACOMFACS                                   
         CLI   0(R1),0                                                          
         BNE   ERROR                                                            
*                                                                               
CLT4     MVC   PCLTOFF,BYTE2       "RESTORE" CLT OFF CODE                       
         FOUT  BUYCLNMH,PCLTNAME                                                
         MVC   SVCLPROF,PCLTPROF                                                
*                                                                               
         CLI   PCLTPROF+5,C'1'     TEST MASTER CLIENT                           
         BNE   CLT4B                                                            
         CLI   BUYPR,C'*'          BUYS MUST BE OTHER AGENCY                    
         BE    CLT4D                                                            
         LA    R3,MCLTERR          NO BUYS ON MASTER                            
         B     ERROR                                                            
*                                                                               
CLT4B    DS    0H                                                               
         CLI   PCLTPROF+5,C'2'     TEST SLAVE                                   
         BNE   CLT4D                                                            
         CLI   BUYPR,C'*'          NO OA BUYS ON SLAVE                          
         BNE   CLT4D                                                            
         LA    R3,OAERR                                                         
         B     ERROR                                                            
*                                                                               
CLT4D    DS    0H                                                               
         MVC   SVESPROF+30(1),PCLTFIN                                           
         MVC   SVCLPROF+30(1),PCLTSTAT                                          
*                                                                               
         NI    GENBYSW1,X'FF'-MIDASCLQ                                          
         TM    SVCLPROF+30,X'40'   MIDAS CLIENT?                                
         JZ    *+8                                                              
         OI    GENBYSW1,MIDASCLQ   SET TO MIDAS CLIENT                          
*                                                                               
* CHECK FOR DRD OVERIDE CLIENT ELEMENT                                          
* IF FOUND SAVE CLT IN SVCLPROF+20(3)                                           
*                                                                               
* SET FOR NO DRD CLT OVERRIDE                                                   
*                                                                               
         MVC   SVCLPROF+20(3),=X'000000'                                        
         LA    R6,PCLTREC+33                                                    
CLT4D5   CLI   0(R6),0                                                          
         BE    CLT4DX                                                           
         CLI   0(R6),X'30'                                                      
         BE    CLT4D8                                                           
         ZIC   R0,1(R6)                                                         
         LTR   R0,R0               JUST IN CASE                                 
         BZ    CLT4DX                                                           
         AR    R6,R0                                                            
         B     CLT4D5                                                           
*                                                                               
CLT4D8   MVC   SVCLPROF+20(3),2(R6)                                             
         OC    SVCLPROF+20(3),=C'   '                                           
*                                                                               
CLT4DX   DS    0H                                                               
*                                                                               
CLT4E    DS    0H                                                               
*                                                                               
* CHECK FOR COST2 FACTOR STYLE CLIENT AND SAVE FACTOR                           
* IF FOUND FROM PCLTCFEL (X'45')                                                
*                                                                               
         ZAP   SVC2FAC,=P'0'       SET FACTOR TO ZERO (CLIENT)                  
         ZAP   SVE2FAC,=P'0'       SET FACTOR TO ZERO (ESTIMATE)                
         LA    R6,PCLTREC+33                                                    
         USING PCLTCFEL,R6                                                      
CLT4E5   CLI   0(R6),0                                                          
         BE    CLT4EX                                                           
         CLI   0(R6),X'45'                                                      
         BE    CLT4E8                                                           
         ZIC   R0,1(R6)                                                         
         LTR   R0,R0               JUST IN CASE                                 
         BZ    CLT4EX                                                           
         AR    R6,R0                                                            
         B     CLT4E5                                                           
*                                                                               
CLT4E8   MVC   SVC2FAC,PCLTCF      SAVE COST 2 FACTOR                           
         DROP  R6                                                               
*                                                                               
CLT4EX   DS    0H                                                               
*                                                                               
CLT4F2   MVI   SVCLTPLV,0          INIT CLIENT PURCHASE ORDER# LEVEL            
         LA    R6,PCLTREC+33                                                    
         USING PCLTPOEL,R6                                                      
CLT4F2F  CLI   0(R6),0                                                          
         JE    CLT4F2X                                                          
         CLI   0(R6),PCLTPOEQ      BILLED BY PURCHASE ORDER# ELEM?              
         JE    CLT4F2P                                                          
         LLC   R0,1(R6)                                                         
         LTR   R0,R0               JUST IN CASE                                 
         JZ    CLT4F2X                                                          
         AR    R6,R0                                                            
         J     CLT4F2F                                                          
CLT4F2P  MVC   SVCLTPLV,PCLTPOLV   SAVE CLIENT PURCHASE ORDER# LEVEL            
         DROP  R6                                                               
*                                                                               
CLT4F2X  DS    0H                                                               
*                                                                               
* SET FOR NOT FROZEN WITH DATE                                                  
*                                                                               
         MVC   SVCLPROF+27(3),=X'000000'                                        
*                                                                               
         TM    SVCLPROF+30,X'02'   FROZEN CLIENT ?                              
         BNO   CLT4FX              NO                                           
         TM    SVCLPROF+30,X'10'   FROZEN WITH DATE ?                           
         BNO   CLT4FX              NO                                           
*                                                                               
* GET FREEZE STATUS ELEMENT (MUST BE THERE IF FROZEN WITH DATE)                 
* SAVE STATUS INDICATOR AND YM FREEZE DATE IN SVCLPROF+27(3)                    
*                                                                               
         LA    R6,PCLTREC+33                                                    
CLT4F5   CLI   0(R6),0                                                          
         BNE   *+6                                                              
         DC    H'0'                SHOULD NOT HAPPEN                            
         CLI   0(R6),X'47'                                                      
         BE    CLT4F8                                                           
         ZIC   R0,1(R6)                                                         
         LTR   R0,R0               JUST IN CASE                                 
         BZ    CLT4FX                                                           
         AR    R6,R0                                                            
         B     CLT4F5                                                           
*                                                                               
CLT4F8   MVC   SVCLPROF+27(3),2(R6)                                             
*                                                                               
CLT4FX   DS    0H                                                               
*                                                                               
* CHECK FOR ADVERTISER CLIENT ELEMENT AND SAVE DATA IF FOUND                    
*                                                                               
         XC    SAVAORC,SAVAORC     USED TO SAVE CONTROLS                        
         XC    SADVDATA,SADVDATA                                                
         LA    R6,PCLTREC+33                                                    
CLT4H    CLI   0(R6),0                                                          
         BE    CLT4P                                                            
         CLI   0(R6),X'15'                                                      
         BE    CLT4L                                                            
         ZIC   R0,1(R6)                                                         
         LTR   R0,R0               JUST IN CASE                                 
         BZ    CLT4P                                                            
         AR    R6,R0                                                            
         B     CLT4H                                                            
*                                                                               
CLT4L    MVC   SADVDATA(18),2(R6)  SAVE ADVERTISER DATA                         
         MVC   SAVAORC,SVAORC      SAVE 'REAL' CONTROLS                         
*                                                                               
         USING PCLREPEL,R5                                                      
CLT4P    LA    R5,PCLTREC+33                                                    
         MVI   ELCODE,PCLREPEQ                                                  
         BRAS  RE,NXTELEM                                                       
         JNE   *+10                                                             
         MVC   SVESPROF+15(4),PCLREPCD                                          
         DROP  R5                                                               
*                                                                               
         MVI   CVNREQ,0                                                         
         MVI   ACOVRD,0                                                         
*                                                                               
         MVC   DUB+04(04),RELO01   SAME FOR ALL GETPROF PARAMETER               
*                                                                               
         XC    P72APROF,P72APROF   CLEAR P72A PROFILE                           
         MVC   DUB+00(04),=C'P72A'                                              
         LA    R3,P72APROF                                                      
         BRAS  RE,SETPROFL                                                      
*                                                                               
         XC    BYPROF,BYPROF       CLEAR BY PROFILE                             
         MVC   DUB+00(04),=C'P0BY'                                              
         LA    R3,BYPROF                                                        
         BRAS  RE,SETPROFL                                                      
*                                                                               
         XC    B1PROF,B1PROF       CLEAR B1 PROFILE                             
         MVC   DUB+00(04),=C'P0B1'                                              
         LA    R3,B1PROF                                                        
         BRAS  RE,SETPROFL                                                      
*                                                                               
         XC    B1XPROF,B1XPROF     CLEAR B1X PROFILE                            
         MVC   DUB+00(04),=C'PB1X'                                              
         LA    R3,B1XPROF                                                       
         BRAS  RE,SETPROFL                                                      
*                                                                               
         XC    ABPROF,ABPROF       CLEAR AB PROFILE                             
         MVC   DUB+00(04),=C'P0AB'                                              
         LA    R3,ABPROF                                                        
         BRAS  RE,SETPROFL                                                      
*                                                                               
         XC    WORK,WORK           CLEAR F0 PROFILE                             
         MVC   DUB+00(04),=C'P0F0'                                              
         LA    R3,WORK+30                                                       
         BRAS  RE,SETPROFL                                                      
*                                                                               
         NI    GENBYSW1,X'FF'-C2ROUNDQ                                          
         CLI   WORK+30+6,C'Y'      COS2 FACTOR ROUNDING?                        
         BNE   *+8                                                              
         OI    GENBYSW1,C2ROUNDQ   SET TO HAVE COS2 FACTOR ROUNDED              
*                                                                               
         NI    GENBYSW1,X'FF'-ADJCO2$Q                                          
         CLI   WORK+30+4,C'N'      NOT COS2?                                    
         JE    CLT4W                                                            
         CLI   WORK+30+4,C'F'      COS2 FACTOR?                                 
         JE    CLT4W                                                            
         CLI   WORK+30+8,C'Y'      EDITING RATE CHANGES COS2?                   
         JNE   CLT4W                                                            
         OI    GENBYSW1,ADJCO2$Q   SET TO HAVE COS2 RATE = BUY RATE             
*                                                                               
CLT4W    CLI   WORK+30+4,C'$'      COS2 RATE?                                   
         JNE   *+16                                                             
         TM    ABUPLDSW,IDSKUPLQ   IDESK INSERTION UPLOAD?                      
         JZ    *+8                                                              
         OI    GENBYSW1,ADJCO2$Q   SET TO HAVE COS2 RATE = BUY RATE             
*                                                                               
         MVI   CVNREQ,1            SET CLIENT VEN REQUIRED FOR CLIENT           
         CLI   BYPROF,C'C'                                                      
         BE    CLT5                                                             
         MVC   CVNREQ,PCLTOFF      REQ BY OFFICE                                
         CLI   BYPROF,C'O'                                                      
         BE    CLT5                                                             
         MVI   CVNREQ,0            NOT REQUIRED                                 
*                                                                               
CLT5     DS    0H                                                               
         MVC   ACOVRD,BYPROF+1     AGY COMM OVERRIDE                            
         CLI   ACOVRD,C'0'                                                      
         BNE   *+8                                                              
         MVI   ACOVRD,0                                                         
*                                                                               
         CLI   MADSW,C'Y'          PBU UPLOAD?                                  
         BE    CLT8                                                             
         CLI   ABPROF+00,C'Y'      BUY ACCESS IS ONLY ALLOWED VIA AB?           
         BNE   CLT8                                                             
         CLI   DDLINKSW,0          CALLING FROM AB?                             
         BNE   CLT8                NONE 0 MEANS ACCESSING FROM AB               
         CLI   T411FFD+1,C'*'      DDS TERMINAL?                                
         BE    CLT8                BYPASS AB PROFILE                            
         L     RF,ACOMFACS                                                      
         L     RF,(CGETTXT-COMFACSD)(RF)                                        
         LA    R3,289              ACCESS DENIED ERROR MSG                      
         MVI   ERRAREA,X'FF'                                                    
         GOTO1 (RF),DMCB+12,(R3),0,(C'E',DMCB),0,0,0                            
         J     SETCCNEQ                                                         
*                                                                               
CLT8     DS    0H                  FOR FUTURE USES                              
*                                                                               
         OI    4(R2),X'20'         SET FIELD CORRECT                            
*                                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
PRD      LA    R2,BUYPRH                                                        
         LA    R3,PRERR                                                         
         TM    4(R2),X'20'                                                      
         BO    EST                                                              
         BAS   RE,CLRPR                                                         
*                                                                               
         BAS   RE,ANY                                                           
         CLC   8(3,R2),=C'AAA'     NO BUYS ALLOWED FOR PRD AAA                  
         BE    ERROR                                                            
*                                                                               
         CLI   5(R2),2                                                          
         BL    ERROR                                                            
         CLI   5(R2),3                                                          
         BH    ERROR                                                            
         BE    PRD2                                                             
         OC    BUYPR,BLANKS                                                     
         FOUT  (R2)                                                             
*                                                                               
* READ PRODUCT RECORD                                                           
*                                                                               
PRD2     DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),BUYMD                                                   
         MVI   KEY+3,6                                                          
         MVC   KEY+4(3),BUYCL                                                   
         MVC   KEY+7(3),BUYPR                                                   
*                                                                               
         BAS   RE,READ                                                          
         BAS   RE,GETREC                                                        
*                                                                               
         FOUT  BUYPRNMH,PPRDNAME                                                
*                                                                               
         MVC   SVPRDIV,PPRDDIV                                                  
         MVC   SVPADJCS,PPRDEXCL   SAVE ADJANENCY CODES                         
         MVC   SVPEXCL,PPRDEXC     EXCLUSION CLASS                              
         MVC   SVPRDSTA,PPRDSTAT   PRODUCT STATUS BYTE                          
*                                                                               
         OI    4(R2),X'20'         SET FIELD CORRECT                            
         EJECT                                                                  
*                                                                               
EST      LA    R2,BUYESH                                                        
         LA    R3,ESERR                                                         
         TM    4(R2),X'20'                                                      
         BO    PUB                                                              
         BAS   RE,CLRES                                                         
         BAS   RE,PACK                                                          
         LTR   R0,R0                                                            
         BZ    ERROR                                                            
*                                                                               
* READ ESTHDR                                                                   
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),BUYMD                                                   
         MVI   KEY+3,7                                                          
         MVC   KEY+4(3),BUYCL                                                   
         MVC   KEY+7(3),BUYPR                                                   
         STH   R0,KEY+10                                                        
         STH   R0,BEST                                                          
*                                                                               
         BAS   RE,READ                                                          
         BAS   RE,GETREC                                                        
*                                                                               
         FOUT  BUYESNMH,PESTNAME                                                
*                                                                               
         XC    WORK(20),WORK                                                    
         GOTO1 VDATCON,DMCB,(0,PESTST),(5,WORK)                                 
         GOTO1 (RF),(R1),,(3,BESST)                                             
         GOTO1 (RF),(R1),(0,PESTEND),(5,WORK+9)                                 
         GOTO1 (RF),(R1),,(3,BESEND)                                            
*                                                                               
*                                                                               
         MVC   SVAORC,SAVAORC      MUST RESTORE 'REAL' CONTROL BYTES            
*                                                                               
         OC    SVADVST,SVADVST     SEE IF I HAVE ADV START DATE                 
         BZ    EST5                                                             
         CLC   BESST,SVADVST                                                    
         BNL   EST5                                                             
         XC    SVAORC,SVAORC       CLEAR CONTROL BYTES                          
         B     EST10                                                            
*                                                                               
EST5     DS    0H                                                               
         OC    SVADVED,SVADVED     SEE IF I HAVE ADV END DATE                   
         BZ    EST10                                                            
         CLC   BESST,SVADVED                                                    
         BL    EST10                                                            
*                                                                               
* EST STARTS AFTER ADV END                                                      
*                                                                               
         XC    SVAORC,SVAORC       CLEAR CONTROLS                               
*                                                                               
EST10    DS    0H                                                               
         MVI   WORK+8,C'-'                                                      
         TM    PESTTEST,X'80'      SEE IF TEST ESTIMATE                         
         BZ    EST30                                                            
         MVI   WORK+18,C'T'                                                     
         TM    PESTTEST,X'40'      STEWARDSHIP ESTIMATE?                        
         BZ    EST30                                                            
         MVI   WORK+18,C'S'                                                     
EST30    FOUT  BUYESDTH,WORK,20                                                 
*                                                                               
         MVC   SVESTJOB,PESTJOB                                                 
         MVC   SVESTALO,PESTZZZ                                                 
*                                                                               
* SAVE SPECIAL REP IN PROFILE USED IN PPBUY11,12,13                             
*                                                                               
         MVC   FULL,SVESPROF+15    SAVE CLIENT LEVEL SPECIAL REP                
         MVC   SVESPROF(29),PESTPROF                                            
         MVC   SVESPROF+15(4),FULL                                              
*                                                                               
* DON'T CLOBBER SVESPROF+30 - IT IS FINANCIAL INDICATOR FROM CLT REC            
*                                                                               
         MVC   SVESPROF+31(1),PESTPROF+31                                       
         MVC   SVESPROF(1),PESTSTAT                                             
*                                                                               
         OC    PESTREP,PESTREP     HAVE ESTIMATE SPECIAL REP?                   
         JZ    *+10                                                             
         MVC   SVESPROF+15(4),PESTREP                                           
*                                                                               
* SAVE TEST STAT IN SVESPROF+29, X'80' MEANS TEST ESTIMATE                      
*                                                                               
         MVC   SVESPROF+29(1),PESTTEST                                          
*                                                                               
         MVC   SVESPROF+28(1),PESTRTYP                                          
*                                                                               
* NOTE - PCLTSTAT WAS SAVED IN SVCLPROF+30                                      
*                                                                               
         TM    SVCLPROF+30,X'08'   COST 2 FACTOR CLT?                           
         BNO   *+10                                                             
         MVC   SVE2FAC,PESTCF      COST 2 FACTOR EST OVERRIDE                   
*                                                                               
         MVI   WORK,C' '                                                        
         MVC   WORK+1(49),WORK                                                  
         OC    SVESTJOB,WORK                                                    
         OC    SVESTALO,WORK                                                    
         OC    SVESPROF+15(4),WORK SPECIAL REP                                  
*                                                                               
         USING PESTALLX,R5                                                      
         NI    GENBYSW1,X'FF'-ALLOEXTQ                                          
         MVI   ELCODE,X'66'        ESTIMATE REC EXTENDED ALLOC ELEM             
         LA    R5,PESTREC+33                                                    
         CLC   ELCODE,0(R5)        FOUND IN FIRST ELEM?                         
         BE    *+12                                                             
         BRAS  RE,NXTELEM                                                       
         BNE   EST40               NO EXTENDED ALLOC ELEMENT                    
         CLC   PEALLZZZ,SPACES                                                  
         JNH   EST40                                                            
         OI    GENBYSW1,ALLOEXTQ   SET TO ALLOEXTQ - EXTENDED ALLOCS            
*                                  VALIDATED IN PPBUY04                         
         DROP  R5                                                               
*                                                                               
EST40    OI    4(R2),X'20'         SET FIELD CORRECT                            
*                                                                               
PUB      DS    0H                  VALIDATE THE PUBLICATION                     
         LA    R2,BUYPBH                                                        
         LA    R3,PBERR                                                         
         TM    4(R2),X'20'                                                      
         BO    GETSC                                                            
*                                                                               
* NAME SEARCH CALL                                                              
*                                                                               
         SR    R2,RA               GET DISPL INTO TWA OF KEY FIELD              
         LA    R3,WORK                                                          
         USING DSPARM,R3                                                        
         XC    DSPARM(DSPARML),DSPARM                                           
         MVC   DSMEDCOD,BUYMD      SET MEDIA CODE                               
         DROP  R3                                                               
*                                                                               
         LA    R3,PBERR            RESET R3 TO PUB ERROR                        
*                                                                               
         GOTO1 =V(SRCHCALL),DMCB,(3,(R2)),(X'80',(RA)),ACOMFACS,       X        
               ('DSPARML',WORK),(1,=CL8'PUB'),0,RR=RELO01                       
*                                                                               
         LA    R2,BUYPBH                                                        
         BAS   RE,CLRPB                                                         
         BAS   RE,ANY                                                           
         CLI   BUYMD,C'M'          MAGAZINE                                     
         BE    PUBL1                                                            
         CLI   BUYMD,C'S'          SUPPLEMENTS                                  
         BE    PUBL1                                                            
         CLI   BUYMD,C'T'          OR TRADE                                     
         BE    PUBL1                                                            
         CLI   BUYMD,C'I'          OR INTERACTIVE                               
         BE    PUBL1                                                            
         CLI   BUYMD,C'L'          OR SOCIAL                                    
         BE    PUBL1                                                            
         CLI   BUYMD,C'B'          OR MOBILE                                    
         BE    PUBL1                                                            
         CLI   BUYMD,C'D'          OR DIGITAL AUDIO                             
         BE    PUBL1                                                            
         CLI   BUYMD,C'V'          OR NVIDEO                                    
         BE    PUBL1                                                            
         CLI   BUYMD,C'W'          OR LVIDEO                                    
         BE    PUBL1                                                            
         CLI   BUYMD,C'N'                                                       
         BNE   PUBL1X                                                           
         CLC   =C'L=',8(R2)        TEST LIST REC                                
         BE    LIST                                                             
         CLC   =C'LW=',8(R2)       WSJ LIST                                     
         BE    WSJLIST                                                          
         B     PUBL1X                                                           
*                                                                               
PUBL1    DS    0H                                                               
         CLC   =C'L=',8(R2)  MAGAZINE LIST BUYING                               
         BE    LIST                                                             
*                                                                               
PUBL1X   CLI   8(R2),C'0'          SEE IF ALPHA                                 
         BL    PUBL2B                                                           
PUBL2    GOTO1 APUBVAL,DMCB,(5(R2),BUYPB),BPUB                                  
         CLI   0(R1),X'FF'                                                      
         BNE   PUBLX                                                            
         B     ERROR                                                            
*                                                                               
PUBL2B   CLC   AGYALPHA,=C'DM'     SPECIAL FOR DOREMUS                          
         BNE   ERROR                                                            
         MVI   WORK,C' '                                                        
         MVC   WORK+1(19),WORK                                                  
         LA    R1,BUYPB                                                         
         ZIC   R5,5(R2)                                                         
         LA    R6,WORK                                                          
PUBL3    CLI   0(R1),C','          SCAN FOR ZONE,EDT                            
         BE    PUBL5                                                            
         CLI   0(R1),0                                                          
         BE    PUBL5                                                            
         MVC   0(1,R6),0(R1)                                                    
         LA    R6,1(R6)                                                         
         LA    R1,1(R1)                                                         
         BCT   R5,PUBL3                                                         
*                                                                               
PUBL5    LA    R4,PUBTABLE                                                      
PUBL5C   CLI   0(R4),X'FF'         END OF TABLE                                 
         BE    ERROR                                                            
         CLC   0(8,R4),WORK                                                     
         BE    PUBL7                                                            
         LA    R4,16(R4)                                                        
         B     PUBL5C                                                           
*                                                                               
PUBL7    MVC   WORK+20(8),8(R4)                                                 
         LA    R7,8                                                             
         CLI   0(R1),C','          CK FOR ZONE,EDT                              
         BNE   PUBL10                                                           
         AR    R7,R5               ADD ZONE,EDT LENGHT                          
         MVC   WORK+28(7),0(R1)    MOVE ZONE + EDT                              
*                                                                               
PUBL10   DS    0H                                                               
         GOTO1 APUBVAL,DMCB,((R7),WORK+20),BPUB                                 
         CLI   0(R1),X'FF'                                                      
         BE    ERROR                                                            
*                                                                               
PUBLX    DS    0H                  READ PUBFILE RECORD                          
         XC    SVADVPUB,SVADVPUB                                                
         TM    SVAORC,X'01'        PUB LINK REQUIRED (NEW ADV)                  
         BZ    PUBLX9                                                           
         XC    KEY,KEY                                                          
         MVI   KEY,X'FE'                                                        
         MVC   KEY+1(1),BUYMD      MEDIA                                        
         MVC   KEY+2(2),AGYALPHA                                                
         MVC   KEY+4(3),SVADV                                                   
         MVC   KEY+7(2),SVAOR                                                   
         MVC   KEY+9(6),BPUB                                                    
         BAS   RE,HIGHPUB                                                       
         CLC   KEY(15),KEYSAVE                                                  
         BE    PUBLX5                                                           
         LA    R3,PUBCLTER                                                      
         B     ERROR                                                            
*                                                                               
PUBLX5   DS    0H                                                               
         MVC   SVADVPUB,KEY+15     SAVE ADV PUB NUMBER                          
*                                                                               
PUBLX9   DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(1),BUYMD                                                     
         MVC   KEY+1(6),BPUB                                                    
         MVC   KEY+7(2),AGYALPHA                                                
         MVI   KEY+9,X'81'                                                      
         BAS   RE,HIGHPUB                                                       
         CLC   KEY(25),KEYSAVE                                                  
         BE    PUB1                                                             
         CLI   SVAGPROF+16,C'0'    TEST DEFAULT TO SRDS                         
         BE    PUB0                NO                                           
         MVC   KEYSAVE+7(2),=C'ZZ' TEST DEFAULT FOUND                           
         CLC   KEYSAVE(25),KEY                                                  
         BE    PUB1                                                             
*                                                                               
PUB0     DS    0H                                                               
         MVC   KEY,KEYSAVE         SET FOR READ                                 
         BAS   RE,READPUB                                                       
PUB1     DS    0H                                                               
         MVC   SVPUBDA,KEY+27      SAVE DISK ADDRESS                            
         BAS   RE,GETPUB                                                        
*                                                                               
         L     R8,APUBIO                                                        
         USING PUBREC,R8                                                        
*                                                                               
         BRAS  RE,PRCZLOCK         PROCESS PUB ZONE LOCK                        
         JNE   PUBL2               TRY AGAIN                                    
*                                                                               
         LA    R5,PUBNAMEL         FIND '20' ELEM                               
         MVI   BFREQ,0             SET TO REQUIRE MONTH/DAY                     
         MVI   BFLAT,0                                                          
*                                                                               
         XC    BPUBPST,BPUBPST     CLEAR PST CODES                              
*                                                                               
         MVI   BPUBGST,C'S'        DEFAULT TO STANDARD                          
         CLI   PUBGST,0                                                         
         BE    *+10                                                             
         MVC   BPUBGST,PUBGST                                                   
*                                                                               
         SR    R0,R0                                                            
PUB2A    IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         CLI   0(R5),0                                                          
         BE    PUB2D                                                            
         CLI   0(R5),X'90'         LOOK FOR PUB PST ELEM                        
         BNE   PUB2A                                                            
         MVC   BPUBPST,2(R5)       SAVE PST CODES  (10 BYTES)                   
*                                                                               
PUB2D    LA    R5,PUBNAMEL                                                      
         SR    R0,R0                                                            
PUB2E    IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         CLI   0(R5),0                                                          
         BE    PUB2X                                                            
         CLI   0(R5),X'20'                                                      
         BNE   PUB2E                                                            
*                                                                               
         USING PUBGENEL,R5                                                      
         MVC   BFLAT,PUBFLAT                                                    
         CLI   BUYMD,C'N'                                                       
         BE    PUB2X                                                            
         OC    PUBMFREQ,PUBMFREQ                                                
         BE    PUB2X                                                            
         LA    R1,FRQLIST                                                       
         LA    R0,LFRQLIST/2                                                    
PUB2H    CLC   0(2,R1),PUBMFREQ                                                 
         BE    PUB2X                                                            
         LA    R1,2(R1)                                                         
         BCT   R0,PUB2H                                                         
         MVI   BFREQ,C'M'          REQUIRE MONTH ONLY IF NOT IN LIST            
         B     PUB2X                                                            
*                                                                               
FRQLIST  DC    C'W BWSWSMSQO D   '                                              
LFRQLIST EQU   *-FRQLIST                                                        
         DROP  R5                                                               
*                                                                               
PUB2X    DS    0H                                                               
         MVI   DUMEL,C' '                                                       
         MVC   DUMEL+1(70),DUMEL                                                
         CLI   BUYMD,C'N'                                                       
         BE    PUB3                                                             
*                                                                               
         MVC   DUMEL(20),PUBNAME   NON-NEWS   NAME ZONE                         
         LA    R7,DUMEL+20                                                      
         CLI   0(R7),C' '                                                       
         BH    *+8                                                              
         BCT   R7,*-8                                                           
         MVC   2(20,R7),PUBZNAME                                                
         B     PUB3B                                                            
*                                                                               
PUB3     DS    0H                  NEWS - ST, CITY PUBNAME ZONE                 
         MVC   DUMEL(2),PUBSTATE                                                
         MVI   DUMEL+2,C','                                                     
         MVC   DUMEL+4(16),PUBCITY                                              
         LA    R7,DUMEL+20                                                      
         CLI   0(R7),C' '                                                       
         BH    *+8                                                              
         BCT   R7,*-8                                                           
         MVC   2(20,R7),PUBNAME                                                 
         LA    R7,22(R7)                                                        
         CLI   0(R7),C' '                                                       
         BH    *+8                                                              
         BCT   R7,*-8                                                           
         MVC   2(20,R7),PUBZNAME                                                
*                                                                               
PUB3B    DS    0H                                                               
         CLI   CVNREQ,0            TEST CLIENT VENDOR NUM REQUIRED              
         BE    PUB3F               NO                                           
         LA    R6,PUBREC+33                                                     
*                                                                               
PUB3C    DS    0H                                                               
         CLI   0(R6),X'14'                                                      
         BE    PUB3E                                                            
         CLI   0(R6),0                                                          
         BNE   PUB3D                                                            
         LA    R3,CVNERR           NUMBER MISSING                               
         B     ERROR                                                            
*                                                                               
PUB3D    DS    0H                                                               
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     PUB3C                                                            
*                                                                               
PUB3E    DS    0H                                                               
         CLI   CVNREQ,1            TEST FOR CLIENT                              
         BNE   PUB3E2                                                           
         CLC   2(3,R6),BUYCL                                                    
         BNE   PUB3D                                                            
         B     PUB3F                                                            
*                                                                               
PUB3E2   DS    0H                  TEST FOR OFFICE                              
         CLI   2(R6),X'FF'                                                      
         BNE   PUB3D                                                            
         CLC   CVNREQ,3(R6)                                                     
         BNE   PUB3D                                                            
*                                                                               
PUB3F    DS    0H                                                               
*                                  KILL DATE                                    
PUB4     DS    0H                                                               
         MVI   BKILL,X'FF'                                                      
         OC    PUBKILL,PUBKILL                                                  
         BZ    *+10                                                             
         MVC   BKILL,PUBKILL                                                    
*                                                                               
         NI    BYPLCKSW,X'FF'-BPLOCKDQ                                          
         TM    PUBLOCSW,PUBLCKDQ   PUB IS LOCKED?                               
         BZ    *+8                                                              
         OI    BYPLCKSW,BPLOCKDQ                                                
*                                                                               
         NI    GENBYSW1,X'FF'-MIDASTPQ                                          
         CLC   AGYALPHA,=C'SJ'                                                  
         JE    PUB4A                                                            
         CLC   AGYALPHA,=C'*B'                                                  
         JE    PUB4A                                                            
         CLC   AGYALPHA,=C'H7'                                                  
         JE    PUB4A                                                            
         J     PUB4A_5                                                          
PUB4A    CLC   BPUB(4),=X'00666666'                                             
         JNE   *+8                                                              
         OI    GENBYSW1,MIDASTPQ   SET TO MIDAS TEST PUB                        
*                                                                               
* REG/DST                                                                       
*                                                                               
PUB4A_5  CLI   SVCLPROF,C'2'                                                    
         BNE   PUB4X                                                            
*                                                                               
         MVI   KEY+9,X'85'                                                      
*                                  USE PUB FROM PUBREC                          
         MVC   KEY+1(6),PUBKPUB    SINCE IT MAY NOT BE SAME AS KEY              
*                                                                               
         MVC   KEY+7(2),AGYALPHA                                                
         BAS   RE,HIGHPUB                                                       
         LA    R3,REGERR                                                        
         CLC   KEY(25),KEYSAVE                                                  
         BNE   ERROR                                                            
         BAS   RE,GETPUB                                                        
         LA    R6,PUBREC+33                                                     
*                                                                               
PUB4B    DS    0H                                                               
         CLI   0(R6),0                                                          
         BE    ERROR                                                            
         CLI   0(R6),X'71'                                                      
         BNE   PUB4F                                                            
         USING PUBDSTEL,R6                                                      
*                                                                               
* SEE IF USING SPECIAL DRD                                                      
*                                                                               
         CLC   SVCLPROF+20(3),=C'   '                                           
         BNH   PUB4B5              CLIENT SCHEME                                
         CLC   PUBDCLT,SVCLPROF+20                                              
         BNE   PUB4F                                                            
         CLC   PUBDDIV,=C'000'     MUST BE DIVISION 000                         
         BNE   PUB4F                                                            
         B     PUB4B7                                                           
*                                                                               
PUB4B5   CLC   PUBDCLT,BUYCL                                                    
         BNE   PUB4F                                                            
         CLC   PUBDDIV,SVPRDIV                                                  
         BNE   PUB4F                                                            
PUB4B7   OC    PUBDSHR(02),PUBDSHR BYPASS MULT REG/DST                          
         BNZ   PUB4X                                                            
         DROP  R6                  BASE REGISTER CONFLICTS                      
         LA    R7,DUMEL+63                                                      
         CLI   0(R7),C' '                                                       
         BH    *+8                                                              
         BCT   R7,*-8                                                           
         LA    R0,DUMEL+L'BUYPBNM-9                                             
         CR    R7,R0                                                            
         BH    PUB4X               NOT ENOUGH ROOM                              
         USING PUBDSTEL,R6                                                      
         LA    R7,1(R7)                                                         
         MVI   0(R7),C' '                                                       
         MVC   1(3,R7),PUBDREG                                                  
         MVI   4(R7),C'/'                                                       
         MVC   5(3,R7),PUBDDST                                                  
         B     PUB4X                                                            
         DROP  R6                                                               
*                                                                               
PUB4F    DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     PUB4B                                                            
*                                                                               
PUB4X    DS    0H                                                               
         MVC   PUBREC+1(6),BPUB                                                 
         TM    SVAGYR+2,X'80'      TEST PUB TRANSLATION NEEDED                  
         BNZ   PUB5                                                             
         TM    SVAGYR+2,X'20'      OR PUB/CLT VERIFICATION                      
         BZ    PUB9                                                             
*                                                                               
PUB5     DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+27(4),SVPUBDA   MUST REREAD PUB                              
         BAS   RE,GETPUB                                                        
         LA    R6,PUBREC+33                                                     
*                                                                               
PUB5B    DS    0H                                                               
         CLI   0(R6),X'14'                                                      
         BE    PUB6                                                             
         CLI   0(R6),0                                                          
         BE    PUB7                                                             
*                                                                               
PUB5D    DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     PUB5B                                                            
*                                                                               
PUB6     DS    0H                                                               
         LA    RF,BUYCL            USE CLIENT                                   
         CLI   SVCLPROF+5,C'2'     UNLESS SLAVE CLT                             
         BNE   *+8                                                              
         LA    RF,SVCLPROF+6       THEN USE MASTER                              
         CLC   2(3,R6),0(RF)                                                    
         BNE   PUB5D                                                            
*                                                                               
         TM    SVAGYR+2,X'80'                                                   
         BZ    PUB9                                                             
         MVC   PUBREC+1(6),17(R6)  MOVE TRANSLATION NUMBER                      
         B     PUB9                                                             
*                                                                               
PUB7     DS    0H                                                               
         TM    SVAGYR+2,X'20'                                                   
         BZ    PUB9                                                             
         LA    R3,PUBCLTER                                                      
         B     ERROR                                                            
PUB9     DS    0H                                                               
         LA    R7,DUMEL-1+L'BUYPBNM                                             
         CLI   0(R7),C' '                                                       
         BH    *+8                                                              
         BCT   R7,*-8                                                           
         LA    R0,DUMEL-1                                                       
         SR    R7,R0                                                            
         LA    R4,BUYPBNM+11                                                    
         LA    R0,L'BUYPBNM-11                                                  
         CR    R7,R0                                                            
         BNH   PUB9B                                                            
         LA    R4,BUYPBNM+L'BUYPBNM                                             
         SR    R4,R7                                                            
PUB9B    DS    0H                                                               
         BCTR  R7,R0                                                            
         EX    R7,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),DUMEL                                                    
         FOUT  BUYPBNMH                                                         
         OI    BUYPBH+4,X'20'      SET VALIDATED                                
         DROP  R8                                                               
         EJECT                                                                  
*                                                                               
* FIND NUMBERS AND DATES OF CONTRACTS APPLICABLE TO EST PERIOD (IF ANY)         
*                                                                               
FNDCON   LA    R2,BUYCONH                                                       
         LA    R8,SVCON                                                         
         XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),BUYMD                                                   
         MVI   KEY+3,X'10'                                                      
         MVC   KEY+4(3),BUYCL                                                   
*                                                                               
         CLI   SVCLPROF+5,C'2'     TEST SLAVE CLIENT                            
         BNE   *+10                NO                                           
         MVC   KEY+4(3),SVCLPROF+6 READ MASTER CLT CONTRACTS                    
*                                                                               
FNDCON5  CLI   SVAGYR,0                                                         
         BE    *+10                                                             
         MVC   KEY(2),SVAGYR       USE AGENCY OF RECORD CONTRACT                
*                                                                               
         L     RE,APUBIO                                                        
         MVC   KEY+7(6),1(RE)                                                   
*                                                                               
         TM    SVAORC,X'08'        CHK FOR NEW ADV SYSTEM CONTRACT              
         BZ    CON0                RATE LOOK-UP                                 
         CLC   SVAOR,AGYALPHA      OR I AM THE AOR                              
         BE    CON0                                                             
*                                                                               
* MUST SWITCH TO AOR FILE                                                       
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB(1),SVAORSE     AOR SE NUMBER                                
         GOTO1 (RF),DMCB                                                        
         CLI   4(R1),0                                                          
         BE    FNDCON8                                                          
         LA    R3,ADVFERR          ADVERTISER FILE NOT ACTIVE                   
         B     ERROR                                                            
*                                                                               
FNDCON8  TM    SVAORC,X'01'        TEST PUB LINK                                
         BZ    *+10                                                             
         MVC   KEY+7(6),SVADVPUB   USE ADV PUB NUMBER                           
         MVC   KEY(2),SVAOR        AOR                                          
         MVC   KEY+4(3),SVADV      ADV                                          
*                                                                               
CON0     DS    0H                                                               
         LA    R4,BUYCON+10        SET FIRST OUTPUT ADDRESS                     
         BAS   RE,HIGH                                                          
         B     *+8                                                              
CON2     BAS   RE,SEQ                                                           
         CLC   KEYSAVE(13),KEY     TEST SAME A/M/REC/CL/PUB                     
         BNE   CON4                                                             
         MVC   AREC,ACONIO         READ INTO CONIO                              
         BAS   RE,GETREC                                                        
         LA    RF,REC              RESTORE AREC                                 
         ST    RF,AREC                                                          
         L     R6,ACONIO                                                        
         USING PCONREC,R6                                                       
         CLC   PCONEND(3),BESST    CON END BEFORE EST START                     
         BL    CON2                                                             
         CLC   BESEND,PCONSTRT     ESTIMATE END BEFORE CONTRACT START           
         BL    CON2                                                             
         CLI   PCONPRD,C'A'        SAVE ONLY CONS FOR ALL CLTS                  
         BL    CON2B                                                            
         CLC   PCONPRD,BUYPR       OR MATCHING PRDS                             
         BNE   CON2                                                             
*                                                                               
CON2B    DS    0H                                                               
         LA    R0,SVCON+L'SVCON-1                                               
         CR    R8,R0                                                            
         BL    *+6                                                              
         DC    H'0'                EXPAND CONTRACT SAVE AREA                    
*                                                                               
         GOTO1 VDATCON,DMCB,(3,PCONSTRT),(2,0(R8))                              
         GOTO1 (RF),(R1),(3,PCONEND),(2,2(R8))                                  
*                                                                               
         TM    PCONLIND,X'80'      SEE IF LOCKED                                
         BZ    *+8                                                              
         OI    0(R8),X'F0'         SET ON HIGH ORDER BIT OF DATE                
*                                  SO LOCKED CAN'T BE BOUGHT AGAINST            
         MVC   4(4,R8),KEY+27      SAVE DISK ADDRESS                            
         LA    R8,8(R8)                                                         
*                                                                               
* SEE IF MORE DISPLAY ROOM AVAILABLE                                            
*                                                                               
         LA    R0,BUYCON+L'BUYCON-32                                            
         CR    R4,R0                                                            
         BH    CON2                NO-KEEP READING                              
         MVC   HALF,PCONREC+13     GET CONTRACT NUMBER                          
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(2,R4),DUB                                                      
         MVI   2(R4),C'='                                                       
         GOTO1 VDATCON,DMCB,(3,PCONSTRT),(5,3(R4))                              
         MVI   11(R4),C'-'                                                      
         GOTO1 (RF),(R1),(3,PCONEND),(5,12(R4))                                 
         TM    PCONLIND,X'80'                                                   
         BZ    *+10                                                             
         MVC   21(6,R4),=C'LOCKED'                                              
         LA    R4,32(R4)                                                        
         B     CON2                                                             
*                                                                               
CON4     DS    0H                                                               
         TM    SVAORC,X'08'                                                     
         BZ    CON4C                                                            
         CLC   SVAOR,AGYALPHA      SEE IF I AM THE AOR                          
         BE    CON4C                                                            
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,=C'PRINT',0                                            
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                MUST BE ABLE TO SWITCH BACK                  
*                                                                               
CON4C    DS    0H                                                               
         LA    R0,BUYCON+10        TEST ANY CONTRACTS FOUND                     
         CR    R4,R0                                                            
         BH    CON6                                                             
*                                                                               
* NO CONTRACTS - CHECK MEDIA                                                    
*                                                                               
         MVC   BUYCON+10(22),=C'** NO CONTRACT OPEN **'                         
*                                                                               
         LA    R2,BUYCLH           PUT CURSOR BACK TO CLIENT IF ERROR           
         LA    R3,CONERR                                                        
         CLI   SVCLPROF+12,C'N'    CONTRACT NOT REQUIEED                        
         BE    CON6                                                             
*                                                                               
         CLI   SVCLPROF+12,C'T'    CONTRACT NOT REQUIRED FOR TEST BUY           
         BE    CON6                CAN'T CHECK HERE BECAUSE TEST BUY            
*                                  OR NOT CAN'T BE DETERMINED                   
         CLI   BUYMD,C'N'                                                       
         BNE   CON5                                                             
*                                                                               
         OC    SADVDATA,SADVDATA   SEE IF NEW AOR SYSTEM                        
         BZ    CON4H                                                            
         CLC   SVAOR,AGYALPHA      SEE IF I AM THE AOR                          
         BE    CON4H                                                            
         TM    SVAORC,X'08'        CHK FOR NEW ADV SYSTEM CONTRACT              
         BZ    CON4H               RATE LOOK-UP                                 
         B     CON5                IF AOR SYSTEM AND I'M NOT THE AOR            
*                                  THEN ALWAYS REQUIRE CONTRACT                 
CON4H    CLI   BFLAT,C'S'          EVEN IF NOT SLIDING SCALE PUB                
         BNE   CON6                                                             
*                                                                               
CON5     DS    0H                                                               
         NI    BUYPBH+4,X'DF'      UNSET PUB VALID TO FORCE RE-EDIT             
         B     ERROR                                                            
*                                                                               
CON6     DS    0H                                                               
         FOUT  (R2)                                                             
         B     GETSC                                                            
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
WSJLIST  SR    R5,R5               EDIT WSJ LIST CODE                           
         IC    R5,5(R2)                                                         
         AHI   R5,-3                                                            
         LTR   R5,R5                                                            
         BNP   ERROR                                                            
         CHI   R5,3                                                             
         BH    ERROR                                                            
         LA    R4,11(R2)           GET PAST LW=                                 
         BCTR  R5,0                                                             
         EX    R5,MVLIST                                                        
         OC    SVLSTID(3),BLANKS                                                
         B     LIST4               REST SAME AS REGULAR LIST                    
*                                                                               
LIST     L     RF,VUSCAN           EDIT LIST DESCRIPTION                        
         LA    R1,PARS             FORMAT IS L=XXX(/NNN)                        
         LA    RE,10(R2)           WHERE XXX IS LIST ID                         
         ST    RE,UADDR-1          NNN IS NUMBER OF PUBS TO BE SKIPPED          
         MVI   UFRST,X'FF'                                                      
         SR    R0,R0                                                            
         IC    R0,5(R2)                                                         
         AHI   R0,-2                                                            
         STC   R0,USTRNG+1                                                      
         MVI   USCN1,C'/'                                                       
         BASR  RE,RF                                                            
         L     R4,UADDR-1                                                       
         LH    R5,ULNGTH                                                        
         LTR   R5,R5                                                            
         BZ    ERROR                                                            
         CHI   R5,3                MAX IS 3 CHARS                               
         BH    ERROR                                                            
         BCTR  R5,0                                                             
         EX    R5,MVLIST                                                        
         OC    SVLSTID(3),BLANKS                                                
*                                                                               
LIST2    LA    R3,LFRSTERR                                                      
         MVI   UVAL,X'80'          EDIT DISPLACEMENT                            
         MVI   USCN1,0                                                          
         BASR  RE,RF                                                            
         L     R4,UADDR-1                                                       
         LH    R5,ULNGTH                                                        
         LTR   R5,R5                                                            
         BZ    LIST4                                                            
         TM    UVAL,X'01'                                                       
         BNZ   ERROR                                                            
         BCTR  R5,0                                                             
         EX    R5,PKLIST                                                        
         CVB   R0,DUB                                                           
         STH   R0,SVNFRST                                                       
*                                                                               
LIST4    XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),BUYMD                                                   
         MVI   KEY+3,X'17'                                                      
         MVC   KEY+4(3),BUYCL                                                   
         MVC   KEY+7(3),SVLSTID                                                 
         MVI   KEY+10,1                                                         
         MVC   SVLSTKEY,KEY                                                     
         BAS   RE,HIGH                                                          
         CLC   KEY(10),KEYSAVE                                                  
         BE    LIST6                                                            
         MVC   KEY,KEYSAVE         TRY FOR DEFAULT                              
         MVC   KEY+4(3),=C'ZZZ'                                                 
         BAS   RE,HIGH                                                          
         CLC   KEY(10),KEYSAVE                                                  
         BE    LIST6                                                            
         SR    R3,R3               CLEAR ERROR NUMBER                           
         MVI   DMCB+8,X'10'        SET REC NOT FOUND                            
         B     ERROR                                                            
LIST6    DS    0H                                                               
         MVC   SVLSTDA,KEY+27      SAVE DISK ADDRESS - DO NOT READ              
         OI    4(R2),X'20'         SET FIELD CORRECT                            
         MVI   BFREQ,0             MMMDD                                        
         MVI   BKILL,X'FF'                                                      
         B     GETSC                                                            
MVLIST   MVC   SVLSTID(0),0(R4)                                                 
PKLIST   PACK  DUB,0(0,R4)                                                      
         EJECT                                                                  
*                                                                               
* GET PROPER SCREEN                                                             
*                                                                               
GETSC    DS    0H                                                               
         XC    DMCB(24),DMCB                                                    
         MVI   DMCB+7,MAG                                                       
         LA    RE,14                                                            
         CLI   BUYMD,C'M'          MAGS                                         
         BE    GETSC1L                                                          
         CLI   BUYMD,C'S'          SUPP                                         
         BE    GETSC1L                                                          
         CLI   BUYMD,C'T'          TRADE                                        
         BE    GETSC1L                                                          
         MVI   DMCB+7,ODR                                                       
         LA    RE,15                                                            
         CLI   BUYMD,C'O'          OUTDOOR                                      
         BE    GETSC2                                                           
         MVI   DMCB+7,INT          INTERACTIVE/SOCIAL/MOBILE/VIDEO              
         LA    RE,13                                                            
         CLI   BUYMD,C'I'          INTERACTIVE                                  
         BE    GETSC1LI                                                         
         CLI   BUYMD,C'L'          SOCIAL                                       
         BE    GETSC1LI                                                         
         CLI   BUYMD,C'B'          MOBILE                                       
         BE    GETSC1LI                                                         
         CLI   BUYMD,C'D'          DIGITAL AUDIO                                
         BE    GETSC1LI                                                         
         CLI   BUYMD,C'V'          NVIDEO                                       
         BE    GETSC1LI                                                         
         CLI   BUYMD,C'W'          LVIDEO                                       
         BE    GETSC1LI                                                         
         MVI   DMCB+7,NEWS                                                      
         LA    RE,13                                                            
         CLC   BUYPB(2),=C'L='                                                  
         BNE   GETSC1                                                           
         MVI   DMCB+7,NEWSLST                                                   
         LA    RE,9                WAS 8 BEFORE DISPLAYING PUB NAMES            
         B     GETSC2                                                           
*                                                                               
GETSC1   CLC   BUYPB(3),=C'LW='    LW= FOR WSJ SCREEN                           
         BNE   GETSC2                                                           
         MVI   DMCB+7,WSJ                                                       
         LA    RE,4                                                             
         B     GETSC2                                                           
*                                                                               
GETSC1L  CLC   BUYPB(2),=C'L='     MAGAZINE LIST                                
         BNE   GETSC2                                                           
         MVI   DMCB+7,MAGLST                                                    
         LA    RE,12                                                            
         B     GETSC2                                                           
*                                                                               
GETSC1LI CLC   BUYPB(2),=C'L='     INTERACTIVE LIST BUYING                      
         BNE   GETSC2                                                           
         MVI   DMCB+7,INTLST                                                    
         LA    RE,10               FIELDS PER TRANSACTION                       
*                                                                               
GETSC2   CLI   SVSCRN,0                                                         
         BNE   *+8                                                              
         MVI   SVSCRN,MAG          VIRGIN IS A MAG SCRN                         
*                                                                               
         CLI   DMCB+7,WSJ          FOR WSJ -  ZZZ USES SAME SCREEN              
         BE    GETSC2D                                                          
         CLC   =C'ZZZ',BUYPR                                                    
         BNE   *+12                                                             
         OI    DMCB+7,X'01'        POL SCRNS ARE ODD-NUMBERED                   
         LA    RE,2(RE)            ADD 2 TO NUMBER OF FLDS/TRN                  
*                                                                               
GETSC2D  CLC   BUYTR1(2),=C'RT'    RECALL TEARSHEET INFO?                       
         BE    *+14                                                             
         CLC   BUYTR1(2),=C'CT'    CHANGE TEARSHEET INFO?                       
         BNE   GETSC2D2                                                         
         MVI   DMCB+7,TSH                                                       
         LA    RE,35               NUMB OF FLDS (FROM DATE TO LAST)             
         B     GETSC2X                                                          
*                                                                               
GETSC2D2 CLC   BUYTR1(2),=C'RU'    RECALL CUSTOM COLUMNS INFO?                  
         BE    *+14                                                             
         CLC   BUYTR1(2),=C'CU'    CHANGE CUSTOM COLUMNS INFO?                  
         BNE   GETSC2D5                                                         
         CLC   BUYPB(3),=C'LW='    LW= FOR WSJ SCREEN (NOT ALLOWED)             
         BE    *+14                                                             
         CLC   BUYPB(2),=C'L='     MAGAZINE LIST      (NOT ALLOWED)             
         BNE   *+16                                                             
         LA    R2,BUYPBH                                                        
         LA    R3,ACHGRERR                                                      
         B     ERROR                                                            
         MVI   BYTE,CCH            CUSTOM COLUMNS SCREEN EQU                    
         MVI   DMCB+7,CCH          CUSTOM COLUMNS SCREEN EQU                    
         LA    RE,25               NUMB OF FLDS (FROM DATE TO LAST)             
         B     GETSC2X                                                          
*                                                                               
GETSC2D5 DS    0H                                                               
         CLC   BUYTR1(2),=C'RC'    RECALL ADDITIONAL CHARGES?                   
         BE    *+14                                                             
         CLC   BUYTR1(2),=C'CC'    CHANGE ADDITIONAL CHARGES?                   
         BNE   GETSC2D7                                                         
         CLC   BUYPB(3),=C'LW='    LW= FOR WSJ SCREEN (NOT ALLOWED)             
         BE    *+14                                                             
         CLC   BUYPB(2),=C'L='     MAGAZINE LIST      (NOT ALLOWED)             
         BNE   *+16                                                             
         LA    R2,BUYPBH                                                        
         LA    R3,ACHGRERR                                                      
         B     ERROR                                                            
         MVI   BYTE,ACH            ADDITIONAL CHARGES SCREEN EQU                
         MVI   DMCB+7,ACH          ADDITIONAL CHARGES SCREEN EQU                
         LA    RE,77               NUMB OF FLDS (FROM DATE TO LAST)             
         B     GETSC2X                                                          
*                                                                               
GETSC2D7 DS    0H                  FUTURE USES                                  
*                                                                               
GETSC2X  STH   RE,SVNTRNS          SAVE FLDS/TRN                                
         CLC   SVSCRN,DMCB+7                                                    
         BE    GETSC4C                                                          
*                                                                               
         LA    R2,BUYTR1H          ON CHANGE OF SCREEN                          
         CLC   8(2,R2),=C'CC'      ADDTNL CHRGS CHANGE?                         
         BNE   GETSC3              YES, ALLOW IT                                
*                                                                               
         CLC   BUYPB(3),=C'LW='    LW= FOR WSJ SCREEN (NOT ALLOWED)             
         BE    *+14                                                             
         CLC   BUYPB(2),=C'L='     MAGAZINE LIST      (NOT ALLOWED)             
         BNE   *+16                                                             
         LA    R2,BUYPBH                                                        
         LA    R3,ACHGRERR                                                      
         B     ERROR                                                            
         BRAS  RE,MYOVRLY                                                       
         B     GETSCX                                                           
*                                                                               
GETSC3   CLI   8(R2),C' '                                                       
         BNH   GETSC3B                                                          
         LA    R3,TRERR                                                         
         CLI   8(R2),C'R'          ONLY R IS VALID TRANSACTION                  
         BNE   ERROR                                                            
*                                                                               
GETSC3B  DS    0H                                                               
*                                                                               
* BUILD REST OF OVERLAY NAME                                                    
*                                                                               
         MVC   DMCB+4(3),=X'D90411'                                             
         GOTO1 VCALLOV,DMCB,BUYHDH                                              
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R1,25               26-1 WSJ SCREEN RESTORE                      
         CLI   SVSCRN,X'F8'        SEE IF LAST WAS NEW LIST                     
         BE    GETSC3C                                                          
         CLI   SVSCRN,X'F9'        SEE IF LAST WAS NEW LIST (ZZZ)               
         BE    GETSC3C                                                          
         LA    R1,39               40-1 CAN RESTORE DATE + AD CODE              
*                                                                               
GETSC3C  MVC   SVSCRN,DMCB+7                                                    
GETSC4   MVC   BUYTR1H(26),X       RESTORE FIRST 2 FIELDS (8+2+8+8)             
*                                                                               
         CLI   SVSCRN,X'EC'        ADDITIONAL CHARGES RECALL?                   
         BE    GETSCX              YES, NO SCREEN ADJUSTMENT NEEDED             
*                                                                               
         CLI   SVSCRN,X'EF'        CUSTOM COLUMNS RECALL?                       
         BE    GETSCX              YES, NO SCREEN ADJUSTMENT NEEDED             
*                                                                               
         CLI   SVSCRN,X'FE'                                                     
         BNE   GETSC4C                                                          
         EX    R1,GETSCMV                                                       
         B     GETSC4C                                                          
*                                                                               
GETSCMV  MVC   BUYTR1H(0),X        FOR WSJ RESTORE FIRST 2 OR 3 FLDS            
*                                  BECAUSE I NEED JOB CODE                      
GETSC4C  CLC   BUYPB(2),=C'L='                                                  
         BE    GETSC4E                                                          
         CLC   BUYPB(3),=C'LW='                                                 
         BNE   *+14                                                             
GETSC4E  MVC   BUYTR1,=C'R '       FORCE RECALL FOR LIST BUYING                 
         MVI   BUYTR1H+5,1         FORCE LENGTH                                 
*                                                                               
GETSCX   B     EXXMOD                                                           
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CLRMD    NI    BUYMDH+4,X'DF'                                                   
         XC    BUYMDNM,BUYMDNM                                                  
         FOUT  BUYMDNMH                                                         
CLRCL    NI    BUYCLH+4,X'DF'                                                   
         XC    BUYCLNM,BUYCLNM                                                  
         FOUT  BUYCLNMH                                                         
CLRPR    NI    BUYPRH+4,X'DF'                                                   
         XC    BUYPRNM,BUYPRNM                                                  
         FOUT  BUYPRNMH                                                         
         XC    SVJOB(10),SVJOB     CLEAR SVJOB + SVJOBDA                        
CLRES    NI    BUYESH+4,X'DF'                                                   
         XC    BUYESNM,BUYESNM                                                  
         FOUT  BUYESNMH                                                         
         XC    BUYESDT,BUYESDT                                                  
         XC    SVPRDS,SVPRDS       CLEAR VALIDATED POL PRDS                     
         FOUT  BUYESDTH                                                         
CLRPB    NI    BUYPBH+4,X'DF'                                                   
         XC    BUYPBNM,BUYPBNM                                                  
         FOUT  BUYPBNMH                                                         
         XC    BUYCON+9(69),BUYCON+9                                            
         FOUT  BUYCONH                                                          
         XC    SVLSTKEY(SVLSTX-SVLSTKEY),SVLSTKEY                               
         MVI   SVLSTX-1,X'FF'      SET EOL                                      
         XC    SVCON,SVCON                                                      
*                                                                               
* CLEAR REST OF SCREEN                                                          
*                                                                               
         CLI   HALF,C'C'           TEST SCREEN CLEAR YET                        
         BCR   8,RE                                                             
*                                                                               
         CLI   SVSCRN,X'F4'        LOWER SCREEN IS TEARSHEET?                   
         BER   RE                  IF SO JUST EXIT                              
*                                                                               
         CLI   SVSCRN,X'EC'        LOWER SCREEN IS ADDTNL CHRGS?                
         BER   RE                  IF SO JUST EXIT                              
*                                                                               
         CLI   SVSCRN,X'EF'        LOWER SCREEN IS CUSTOM COLUMNS?              
         BER   RE                  IF SO JUST EXIT                              
*                                                                               
         MVI   HALF,C'C'           SET CLEAR SWITCH                             
         LA    R4,BUYTR1H                                                       
         SR    R5,R5                                                            
CLRSC2   IC    R5,0(R4)            GET LEN+8                                    
         AHI   R5,-9               SET FOR EX                                   
         TM    1(R4),X'20'         TEST PROTECTED                               
         BZ    CLRSC4              DON'T CLEAR UNPROTECTED FIELDS               
         CLC   =C'ALLO',8(R4)                                                   
         BE    CLRSC4                                                           
         CLC   =C'OPTI',8(R4)                                                   
         BE    CLRSC4                                                           
         CLC   =C'-EDT-',8(R4)                                                  
         BE    CLRSC4                                                           
         CLC   =C'-LINE',8(R4)                                                  
         BE    CLRSC4                                                           
         CLC   =C'-PREM',8(R4)                                                  
         BE    CLRSC4                                                           
         CLC   =C'SPACE CLOSING',8(R4)                                          
         BE    CLRSC4                                                           
         CLC   =C'ON-SALE',8(R4)                                                
         BE    CLRSC4                                                           
CLRSC3   EX    R5,CLROC                                                         
         BZ    CLRSC4              NO                                           
         EX    R5,CLRXC            CLEAR                                        
         FOUT  (R4)                                                             
CLRSC4   LA    R4,9(R4,R5)         POINT TO NEXT FIELD                          
         CLI   0(R4),0                                                          
         BNE   CLRSC2                                                           
         BR    RE                                                               
CLRXC    XC    8(0,R4),8(R4)                                                    
CLROC    OC    8(0,R4),8(R4)                                                    
*                                                                               
BLANKS   DC    10C' '                                                           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         EJECT                                                                  
*                                                                               
* FARMABLE CODE                                                                 
*                                                                               
ANY      CLI   5(R2),0                                                          
         BNE   ANY2                                                             
         LA    R3,1                                                             
         B     ERROR                                                            
*                                                                               
ANY2     TM    4(R2),X'10'         IS IT VALID NUMERIC                          
         BCR   8,RE                IF APPLICABLE                                
         LA    R3,3                                                             
         B     ERROR                                                            
*                                                                               
PACK     SR    R1,R1                                                            
         SR    R0,R0                                                            
         ZAP   DUB,=P'0'                                                        
         IC    R1,5(R2)                                                         
         LTR   R1,R1                                                            
         BCR   8,RE                EXIT ON ZERO LENGTH                          
         TM    4(R2),X'08'                                                      
         BCR   8,RE                OR NON NUMERIC                               
         BCTR  R1,R0                                                            
         EX    R1,VARPACK                                                       
         CVB   R0,DUB                                                           
         BR    RE                                                               
*                                                                               
VARPACK  PACK  DUB,8(0,R2)                                                      
         EJECT                                                                  
*                                                                               
* COMMUNICATION WITH DATA MANAGER (DIRECTORY)                                   
*                                                                               
READ     MVC   COMMAND,=C'DMREAD'                                               
         MVC   KEYSAVE,KEY                                                      
         B     DIRCTRY                                                          
*                                                                               
SEQ      MVC   COMMAND,=C'DMRSEQ'                                               
         B     DIRCTRY                                                          
*                                                                               
HIGH     MVC   COMMAND,=C'DMRDHI'                                               
         MVC   KEYSAVE,KEY                                                      
         B     DIRCTRY                                                          
*                                                                               
DIRCTRY  NTR                                                                    
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PRTDIR',             X        
               KEY,KEY,(TERMNAL,0)                                              
         B     DMCHECK                                                          
         EJECT                                                                  
*                                                                               
* COMMUNICATION WITH DATA MANAGER (FILE)                                        
*                                                                               
GETREC   MVC   COMMAND,=C'GETREC'                                               
         B     FILE                                                             
*                                                                               
FILE     NTR                                                                    
         LA    R2,KEY+27                                                        
         CLC   COMMAND(5),=C'DMDEL'                                             
         BE    *+12                                                             
         CLI   COMMAND,C'A'                                                     
         BNE   *+8                                                              
         LA    R2,KEY                                                           
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PRTFILE',            X        
               (R2),AREC,(TERMNAL,DMWORK)                                       
         B     DMCHECK                                                          
         EJECT                                                                  
*                                                                               
* COMMUNICATION WITH DATA MANAGER (PUBDIR)                                      
*                                                                               
READPUB  MVC   COMMAND,=C'DMREAD'                                               
         MVC   KEYSAVE,KEY                                                      
         B     PUBDIRY                                                          
*                                                                               
SEQPUB   MVC   COMMAND,=C'DMRSEQ'                                               
         B     PUBDIRY                                                          
*                                                                               
HIGHPUB  MVC   COMMAND,=C'DMRDHI'                                               
         MVC   KEYSAVE,KEY                                                      
         B     PUBDIRY                                                          
*                                                                               
PUBDIRY  NTR                                                                    
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PUBDIR',             X        
               KEY,KEY,(TERMNAL,0)                                              
         B     DMCHECK                                                          
         EJECT                                                                  
*                                                                               
* COMMUNICATION WITH DATA MANAGER (PUBFILE)                                     
*                                                                               
GETPUB   MVC   COMMAND,=C'GETREC'                                               
         B     PUBFILE                                                          
*                                                                               
PUBFILE  NTR                                                                    
         LA    R2,KEY+27                                                        
         CLI   COMMAND,C'A'                                                     
         BNE   *+8                                                              
         LA    R2,KEY                                                           
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PUBFILE',            X        
               (R2),APUBIO,(TERMNAL,DMWORK)                                     
         B     DMCHECK                                                          
         EJECT                                                                  
*                                                                               
* DATA MANAGER ERRORS AND EXIT                                                  
*                                                                               
DMCHECK  MVC   BYTE,DMCB+8                                                      
         NC    BYTE,DMOUTBTS                                                    
         BNZ   DMERRS                                                           
         XIT                                                                    
*                                                                               
DMERRS   L     RD,4(RD) .          UNWIND WITHOUT XIT                           
         LM    RE,RC,12(RD)                                                     
         SR    R3,R3 .             LET GETMSG SORT IT OUT                       
         B     ERROR                                                            
         EJECT                                                                  
*                                                                               
* EXITS FROM PROGRAM                                                            
*                                                                               
LOCK     OI    6(R2),X'02'         LOCK SCREEN                                  
*                                                                               
ERROR    L     R4,ERRAREA                                                       
         MVI   ERRAREA,X'FF'                                                    
         MVC   DMCB+20(4),VDATAMGR                                              
         MVC   DMCB+20(1),TERMNAL                                               
         GOTO1 VGETMSG,DMCB+12,((R3),8(R4)),(4,DMCB)                            
*                                                                               
EXIT     OI    6(R2),OI1C          INSERT CURSOR                                
         L     R4,ERRAREA                                                       
         FOUT  (R4)                                                             
*                                                                               
         ST    R2,ADBERRFD         IN CASE CALLER NEEDS IT                      
*                                                                               
EXXMOD   XMOD1 1                                                                
         LTORG                                                                  
*                                                                               
SPACES   DC    CL50' '                                                          
*                                                                               
NXTELEM  ZIC   R0,1(R5)                                                         
         AR    R5,R0                                                            
         CLC   ELCODE,0(R5)                                                     
         JE    NXTELX              CC IS EQUAL                                  
         CLI   0(R5),0                                                          
         JNE   NXTELEM                                                          
         LTR   R5,R5               CC IS NOT EQUAL                              
NXTELX   BR    RE                                                               
*                                                                               
ACH      EQU    X'EC'              ADDITIONAL CHARGES SCREEN NUMBER             
CCH      EQU    X'EF'              CUSTOM COLUMNS SCREEN NUMBER                 
TSH      EQU    X'F4'              TEARSHEET SCREEN NUMBER                      
INT      EQU    X'F2'              INTERACTIVE/SOCIAL/MOBILE/VIDEO              
*                                                                               
ACHGRERR EQU   29                  ADDTNL CHRGS NOT ALLOWED ERROR               
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* TABLE OF MNEMONIC CODES FOR DOREMUS                                           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* NEWSPAPERS                                                                    
*                                                                               
PUBTABLE DC     CL8'NYT     ',CL8'11336000'                                     
         DC     CL8'WSJ     ',CL8'11336120'                                     
         DC     CL8'CHITRIB ',CL8'11143335'                                     
         DC     CL8'BOSGLO  ',CL8'11221125'                                     
         DC     CL8'CATIMES ',CL8'11054125'                                     
         DC     CL8'SFC+E   ',CL8'11056900'                                     
         DC     CL8'SFC&&E  ',CL8'11056900'                                     
         DC     CL8'WASPOST ',CL8'11096580'                                     
         DC     CL8'MIAH+N  ',CL8'11054050'                                     
         DC     CL8'MIAH&&N ',CL8'11054050'                                     
         DC     CL8'PHIINQNE',CL8'11396210'                                     
         DC     CL8'ATCONJRN',CL8'11112130'                                     
         DC     CL8'DETNE   ',CL8'11232660'                                     
         DC     CL8'DETFRPR ',CL8'11232470'                                     
         DC     CL8'DALNEW  ',CL8'11452500'                                     
         DC     CL8'HOUPOST ',CL8'11454700'                                     
         DC     CL8'HOUCHR  ',CL8'11454600'                                     
         DC     CL8'DALTIMHE',CL8'11452600'                                     
         DC     CL8'BALTSUN ',CL8'11213735'                                     
         DC     CL8'SEAPI+T ',CL8'11497125'                                     
         DC     CL8'SEAPI&&T',CL8'11497125'                                     
         DC     CL8'CLEPD   ',CL8'11362200'                                     
         DC     CL8'POROREG ',CL8'11388240'                                     
         DC     CL8'MILSJ   ',CL8'11516090'                                     
         DC     CL8'MINNST  ',CL8'11245250'                                     
         DC     CL8'STLOUPD ',CL8'11267900'                                     
         DC     CL8'BAR     ',CL8'72751200'                                     
         DC     CL8'INVDLY  ',CL8'71103625'                                     
         DC     CL8'FTL     ',CL8'91200005'                                     
         DC     CL8'WSJI    ',CL8'90100010'                                     
*                                                                               
* MAGAZINES                                                                     
*                                                                               
         DC     CL8'IDD     ',CL8'72754000'                                     
         DC     CL8'NTN     ',CL8'70553375'                                     
         DC     CL8'FORB    ',CL8'71102800'                                     
         DC     CL8'FORT    ',CL8'71102940'                                     
         DC     CL8'BW      ',CL8'71101120'                                     
         DC     CL8'II      ',CL8'71103605'                                     
         DC     CL8'BB      ',CL8'72751600'                                     
         DC     CL8'AB      ',CL8'70550130'                                     
         DC     CL8'PIA     ',CL8'72754650'                                     
         DC     CL8'CASHFL  ',CL8'72751800'                                     
         DC     CL8'PUF     ',CL8'72355200'                                     
         DC     CL8'OGJRNL  ',CL8'76353543'                                     
         DC     CL8'ECO     ',CL8'71102275'                                     
         DC     CL8'ECOWW   ',CL8'90500008'                                     
         DC     CL8'EURO    ',CL8'90500009'                                     
         DC     CL8'III     ',CL8'90900003'                                     
         DC     X'FFFF'                                                         
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
SETPROFL NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK+00(4),DUB      PROFILE TO BE RETRIEVED                      
         CLC   DUB(4),=C'P72A'                                                  
         BE    SETPRF30                                                         
         CLC   DUB(4),=C'PB1X'                                                  
         BNE   SETPRF35                                                         
*                                                                               
SETPRF30 NI    WORK,X'BF'          LOWER CASE                                   
*                                                                               
SETPRF35 MVC   WORK+04(2),AGYALPHA                                              
         MVC   WORK+06(1),BUYMD                                                 
         MVC   WORK+07(3),BUYCL                                                 
         CLI   PCLTOFF,C' '                                                     
         BNH   *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),PCLTOFF                                               
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CGETPROF-COMFACSD(RF)                                         
*                                                                               
         GOTO1 (RF),DMCB,(X'C0',WORK),(R3),VDATAMGR                             
*                                                                               
JUMPXIT1 XIT1                                                                   
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKTRAFID NTR1  BASE=*,LABEL=*      CHECKING FOR TRAFFIC ID SIGN-ON              
*                                                                               
         L     R0,AWRKREC                                                       
         LHI   R1,400*4                                                         
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               SAME AS XCEF, ONLY SHORTER                   
*                                                                               
         L     R4,AWRKREC                                                       
         USING CTIREC,R4                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKEY,C'I'                                                      
         MVC   CTIKNUM,10(RA)      ID NUMBER                                    
*                                                                               
         GOTO1 VDATAMGR,DMCB,(0,=C'DMREAD'),=C'CTFILE',(R4),(R4)                
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    RE,CTIDATA                                                       
CKTRA10  CLI   0(RE),0                                                          
         BNE   *+6                                                              
         DC    H'0'                ELEM MUST BE PRESENT                         
         CLI   0(RE),CTAGYELQ      AGENCY ALPHA ID ELEMENT (X'06')              
         BE    CKTRA20                                                          
         SR    R1,R1                                                            
         IC    R1,1(RE)                                                         
         AR    RE,R1                                                            
         B     CKTRA10                                                          
*                                                                               
         USING CTAGYD,RE                                                        
CKTRA20  CLI   CTAGYIDT,CTAGYTTQ   TRAFFIC ID (C'T')?                           
         BNE   SETCCNEQ                                                         
         DROP  R4,RE                                                            
*                                                                               
SETCCEQ  CR    RB,RB               EQUAL                                        
         J     *+6                                                              
SETCCNEQ LTR   RB,RB               NOT EQUAL (SIGN-ON IS NOT TRAFFIC)           
         J     JUMPXIT1                                                         
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
TRAFFACC NTR1  BASE=*,LABEL=*      CHECKING FOR TRAFFIC LIMIT ACCESS            
*                                                                               
         MVI   BYTE3,0             WILL RETURN CODE                             
*                                                                               
         MVI   ELCODE,X'50'        CLIENT TRAFFIC OFFICE ELEM CODE              
         LA    R5,PCLTREC+33                                                    
         CLC   ELCODE,0(R5)        FOUND IN FIRST ELEM?                         
         BE    *+12                                                             
         BRAS  RE,NXTELEM                                                       
         BNE   TRACCX              NO CLIENT TRAFFIC OFFICE CODE ELEM           
         MVC   BYTE3,2(R5)         SAVE CLIENT TRAFFIC OFFICE CODE              
*                                                                               
TRACCX   J     JUMPXIT1                                                         
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*   CALL ADDITIONAL CHARGES OR CUSTOM COLUMNS SCREENS AND PROGRAMS    *         
*NTRY    BYTE=X'EC'   ADDITIONAL CHARGES SCREEN                       *         
*             X'EF'   CUSTOM COLUMNS SCREEN                           *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
MYOVRLY  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   SVTRCODE,BUYTR1     SAVE TRANSACTION CODE                        
         MVC   BYTE4,BUYDT1H+5     SAVE DATE INPUT LENGTH (IF ANY)              
         MVC   DOUBLE,BUYDT1       SAVE DATE (IF ANY)                           
*                                                                               
         CLI   BYTE,X'EC'          ADDTNL CHRGS BEING CALLED?                   
         BNE   MYOVR05             MUST BE CUSTOM COLUMN CALL                   
         CLI   SVSCRN,X'EC'        ADDTNL CHRGS LOWER SCREEN?                   
         BE    MYOVR30                                                          
MYOVR05  DS    0H                                                               
         CLI   BYTE,X'EF'          CUSTOM COLUMNS BEING CALLED?                 
         BNE   MYOVR10             MUST BE ADDTNL CHRGS CALL                    
         CLI   SVSCRN,X'EF'        CUSTOM COLUMNS LOWER SCREEN?                 
         BE    MYOVR30                                                          
MYOVR10  XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(3),=X'D90411'                                             
         MVC   DMCB+7(L'BYTE),BYTE                                              
         GOTO1 VCALLOV,DMCB,BUYHDH    GET LOWER SCREEN                          
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   SVSCRN,BYTE         LOWER SCREEN NOW ADD CHG OR CUST COL         
*                                                                               
MYOVR30  DS    0H             CALL ADDTNL CHRGS OR CUSTOM COL'S PROGRAM         
         XC    DMCB(24),DMCB                                                    
         MVI   DMCB,X'16'                                                       
         CLI   SVSCRN,X'EC'        LOWER SCR ADDTNL CHRGS ?                     
         BE    *+8                 YES                                          
         MVI   DMCB,X'20'          MUST BE CUSTOM COLUMNS                       
         GOTO1 VCALLOV,DMCB,,(RA)                                               
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   FULL,0(R1)                                                       
         GOTO1 FULL,DMCB,(RC),(RA)                                              
*                                                                               
MYOVRX   DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* PROCESS PUB ZONE LOCK (TRADING DESK PUBS) - WILL AUTOMATICALLY ADD            
* THE NEXT ZONE RECORD UNDER THE BASE PUB RECORD IF 255 BUYS EXIST              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PRCZLOCK NTR1  BASE=*,LABEL=*,WORK=(R4,LOCALWSL)                                
*                                                                               
         USING LOCALWSD,R4                                                      
         L     R8,APUBIO                                                        
         USING PUBREC,R8                                                        
*                                                                               
         TM    PLINKSW1,PUBZLOCQ   PUB ZONE LOCK IS ALREADY PROCESSED?          
         JNZ   PRCZL_X                                                          
*                                                                               
         TM    ABUPLDSW,IDSKUPLQ   IDESK INSERTION UPLOAD?                      
         JZ    PRCZL_X                                                          
         CLI   DDLINKSW,C'N'       NEW INSERTION?                               
         JNE   PRCZL_X                                                          
         TM    PUBLOCSW,PUBZLCKQ   PUB ZONE IS LOCKED?                          
         JZ    PRCZL_X                                                          
*                                                                               
         L     R3,VTIA             FIRST 4096 BYTES HAVE WORKER REC             
         LA    R3,4(R3)            POINT TO WORKER ELEM                         
         USING LQ_EL,R3                                                         
PRCZL22  CLI   LQ_EL,LQ_RDATQ      RETURNED DATA HEADER ELEM?                   
         JE    PRCZL_X                                                          
         CLI   LQ_EL,LQ_RQSTQ      REQUEST DATA ELEM?                           
         JE    *+12                                                             
PRCZL24  BRAS  RE,NXTWFELM                                                      
         J     PRCZL22                                                          
         CLC   =AL2(D#STRDAT),3(R3)                                             
         JNE   PRCZL24                                                          
         XC    LOCALEL1,LOCALEL1                                                
         GOTO1 VPERVAL,DMCB,(10,6(R3)),(X'40',LOCALEL1)                         
         LA    RE,LOCALEL1                                                      
         USING PERVALD,RE                                                       
         MVC   LOCALIDT,PVALBSTA   SAVE BINARY INSERTION DATE                   
         MVC   LOCALBPU,BPUB       SAVE BINARY PUB                              
         DROP  R3,RE                                                            
*                                                                               
         XC    KEY,KEY                                                          
         LA    RE,KEY                                                           
         USING PBUYKEY,RE                                                       
         MVC   PBUYKAGY,AGYALPHA                                                
         MVC   PBUYKMED,BUYMD                                                   
         MVI   PBUYKRCD,X'20'                                                   
         MVC   PBUYKCLT,BUYCL                                                   
         MVC   PBUYKPRD,BUYPR                                                   
         MVC   PBUYKPUB(6),LOCALBPU                                             
         MVC   PBUYKDAT,LOCALIDT                                                
         MVC   PBUYKEST,BEST                                                    
         MVI   PBUYKLIN,X'FF'                                                   
         MVC   KEYSAVE,KEY                                                      
         MVC   LOCALSKE,KEY                                                     
         DROP  RE                                                               
*                                                                               
         MVI   LOCALPZS,0          NO NEED TO ADD NEXT ZONE YET                 
*                                                                               
         GOTO1 VDATAMGR,DMCB,(DMINBTS,=C'DMRDHI'),=C'PRTDIR',          +        
               KEY,KEY,(TERMNAL,0)                                              
         CLC   KEY(24),KEYSAVE     MATCH UP TO EVERYTHING EXCEPT LINE#?         
         JNE   PRCZL28                                                          
*                                                                               
PRCZL26  LLC   RE,LOCALBPU+4       GET CURRENT ZONE                             
         AHI   RE,1                SET NEXT ZONE                                
         CHI   RE,99               MAX ZONE?                                    
         JNH   *+6                                                              
         DC    H'0'                ALL ZONES ARE USED!                          
         STC   RE,LOCALBPU+4       TRY NEXT ZONE                                
         XC    KEY,KEY                                                          
         MVC   KEY(25),LOCALSKE                                                 
         STC   RE,KEY+14           CHECK BUYS FROM NEXT ZONE                    
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,(DMINBTS,=C'DMRDHI'),=C'PRTDIR',          +        
               KEY,KEY,(TERMNAL,0)                                              
         CLC   KEY(24),KEYSAVE     MATCH UP TO EVERYTHING EXCEPT LINE#?         
         JE    PRCZL26                                                          
         XC    KEY,KEY                                                          
         MVC   KEY+00(10),LOCALSKE                                              
         MVC   KEY+10(06),LOCALBPU                                              
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,(DMINBTS,=C'DMRDHI'),=C'PRTDIR',          +        
               KEY,KEY,(TERMNAL,0)                                              
         CLC   KEY(14),KEYSAVE     HAVE ZONE OR STILL UNDER 255 LINES?          
         JE    *+12                                                             
         MVI   LOCALPZS,C'Y'       NEED TO ADD NEXT AVAILABLE ZONE              
         J     PRCZL28                                                          
         CLI   KEY+14,0            HAVE ZONE?                                   
         JNE   PRCZL30                                                          
*                                                                               
PRCZL28  CLC   LOCALBPU(4),BPUB    STILL SAME BASE PUB?                         
         JE    *+6                                                              
         DC    H'0'                BAD PUB NUMBER!                              
         CLI   LOCALPZS,C'Y'       NEED TO ADD ZONE FOR TRADING DESK?           
         JNE   PRCZL_X                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(25),PUBREC                                                   
         LLC   RE,LOCALBPU+4       NEXT ZONE TO ADD                             
         STC   RE,KEY+1+4          NEXT ZONE IN PUB KEY                         
         STC   RE,PUBKZON          NEXT ZONE IN PUB RECORD                      
         MVC   LOCALBPU,PUBKPUB                                                 
*                                                                               
         GOTO1 VDATAMGR,DMCB,(DMINBTS,=C'ADDREC'),=C'PUBFILE',         +        
               KEY,(R8),(TERMNAL,DMWORK)                                        
*                                                                               
PRCZL30  XC    BUYPB,BUYPB                                                      
         GOTO1 APUBEDIT,DMCB,(X'08',LOCALBPU),(C'S',BUYPB)                      
         SR    RE,RE                                                            
         LA    RF,BUYPB                                                         
*                                                                               
PRCZL32  CLI   0(RF),0             END OF PUB FLD?                              
         JE    PRCZL34                                                          
         AHI   RE,1                                                             
         CHI   RE,15               MAX FLD LENGTH ALREADY?                      
         JE    PRCZL34                                                          
         LA    RF,1(RF)            POINT TO NEXT CHARACTER IN PUB FLD           
         J     PRCZL32                                                          
*                                                                               
PRCZL34  STC   RE,BUYPBH+5                                                      
         OI    BUYPBH+6,X'80'      PUBLICATION IS DISPLAYED                     
*                                                                               
         OI    PLINKSW1,PUBZLOCQ   PUB ZONE LOCK IS IN PLACE                    
         J     SETCCNEQ                                                         
*                                                                               
PRCZL_X  J     SETCCEQ                                                          
*                                                                               
NXTWFELM SR    R0,R0                                                            
         ICM   R0,3,(LQ_LN-LQ_D)(R3)                                            
         AR    R3,R0                                                            
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
         DROP  RB,R4,R8                                                         
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         PRINT  OFF                                                             
       ++INCLUDE PPBUYWRK1                                                      
         EJECT                                                                  
*                                                                               
         ORG   NEWREC              MAP BUY RECORD TO NEWREC                     
*                                                                               
       ++INCLUDE PPBUYWRK2                                                      
         EJECT                                                                  
         PRINT  ON                                                              
*                                                                               
LOCALWSD DSECT                                                                  
*                                                                               
         DS    0D                                                               
LOCALAIO DS    4000C                                                            
LOCALWSX EQU   *                                                                
LOCALWSL EQU   LOCALWSX-LOCALWSD                                                
*                                                                               
         ORG   LOCALAIO                                                         
LOCALHA1 DS    H                                                                
LOCALHA2 DS    H                                                                
LOCALFU1 DS    F                                                                
LOCALFU2 DS    F                                                                
LOCALDU1 DS    D                                                                
LOCALDU2 DS    D                                                                
LOCALBY1 DS    X                                                                
LOCALBY2 DS    X                                                                
LOCALBY3 DS    X                                                                
LOCALBY4 DS    X                                                                
LOCALEL1 DS    XL256                                                            
LOCALEL2 DS    XL256                                                            
LOCALSKE DS    XL32                                                             
LOCALBPU DS    XL6                                                              
LOCALIDT DS    XL3                                                              
LOCALPZS DS    C                                                                
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'050PPBUY01   02/26/20'                                      
         END                                                                    
