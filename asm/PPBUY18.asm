*          DATA SET PPBUY18    AT LEVEL 160 AS OF 01/25/21                      
*PHASE T41118A                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'PPBUY18 - EDITING OPTIONAL DATA FROM WORKER FILE'               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* KWAN 04/21/20 NO FINANCIAL CHANGES TO FRZ CLIENTS (SPEC-37363)                
*                                                                               
* KWAN 03/19/20 SREP ADJUSTMENT FOR PRISMA BUYS (IAPP-249829)                   
*                                                                               
* KWAN 07/05/19 COS2 ADJUSTMENT FOR PRISMA BUYS (IAPP-224713)                   
*                                                                               
* KWAN 05/09/19 SRC FIX FOR PBT (SPEC-35166)                                    
*                                                                               
* KWAN 03/27/17 SUPPRESS PO# OVERLAPPING DATE ERROR FOR PRISMA/RADIA            
*                                                                               
* KWAN 05/20/15 FIX DATA CHECK FOR FIS (CAN'T COMPARE LOOK-UP FLAG)             
*                                                                               
* KWAN 02/06/15 ENABLE REGULAR, PI AND IC COMMENTS FOR PRISMA                   
*                                                                               
* KWAN 10/03/14 UNPRINTABLE CHARS FROM PRINT BUY TOOLKIT COMMENTS               
*                                                                               
* KWAN 08/19/14 IGNORE PLANNED COST FOR MEDIA T, M, N AND O IN PRISMA           
*                                                                               
* KWAN 07/11/14 NEW IDK PROFILE TO LIMIT COS2 CHANGES                           
*                                                                               
* KWAN 05/09/14 DUAL COST FOR PRISMA (COS2 RATE)                                
*                                                                               
* KWAN 03/10/14 SUPPORT MEDIA L (SOCIAL)                                        
*                                                                               
* KWAN 01/30/14 CHECK "REAL" CHANGE IN SREP FOR PRISMA                          
*                                                                               
* KWAN 01/15/14 MAX DAILY EFFECTIVE CIRCULATION IS NOW 999,999,999              
*                                                                               
* KWAN 01/08/14 IGNORE SREP=0000 ON NEW BUYS (PRISMA IS SENDING IT)             
*                                                                               
* KWAN 12/04/13 SREP=0000 ADJUSTMENT (ALLLOW IT EVEN IF NO SREP FOUND)          
*                                                                               
* KWAN 11/19/13 NO DATA CHECK FOR PRISMA CHANGE UPLOADS                         
*                                                                               
* KWAN 04/03/13 ASSIGN PURCHASE ORDER# BY LEVEL                                 
*                                                                               
* KWAN 02/28/12 COS2 FACTOR FOR BUY (WORKING IN ADBUYER, BROKEN IN BUY)         
*                                                                               
* KWAN 01/04/12 DRAFT INSERTION FIX FOR 100% AGY COMMISSION                     
*                                                                               
* KWAN 09/27/11 TC CHARGE CAN ONLY BE ATTACHED TO MIDAS PUB 666666              
*                                                                               
* KWAN 09/13/11 SET COS2 $ TO 0 FOR MIDAS CLIENT & NON-MIDAS TEST PUB           
*                                                                               
* KWAN 08/23/11 COS$ FIX FOR NEW BUYS USING PRINTPAK BUY PROGRAM                
*                                                                               
* KWAN 07/20/11 INFINITE LOOP FIX IN COS2 $ FOR FREE BUYS                       
*                                                                               
* KWAN 10/05/10 NEW ROUTINE TO HANDLE COS2 $ IN BUY & ADBUYER                   
*                                                                               
* KWAN 06/04/10 NEED TO REPLY TOKEN FOR DRAFT UPLOAD                            
*                                                                               
* KWAN 06/16/09 BYPASS PLANNED COST FOR IDESK'S MEDIA S UPLOADS                 
*                                                                               
* KWAN 03/14/07 RETOOL ADDITIONAL CHARGES UPLOAD LOGIC                          
*                                                                               
* KWAN 09/26/06 RECONFIGURE MODULE FOR BOTH ADBUYER & BUY PROGRAM               
*                                                                               
* KWAN 09/19/06 SUPPORT PURCHASE ORDER # UPLOAD                                 
*                                                                               
* KWAN 06/08/06 ALLOW ZERO VALUE PAGE VIEW AND CLICK THRU ELEMENTS              
*                                                                               
* KWAN 01/24/06 FIX LOWER CASE LETTERS IN COMMENT (FROM PBU)                    
*                                                                               
* KWAN 11/07/05 CORRECT T/S X'00' VS. X'40' FIX                                 
*                                                                               
* KWAN 08/19/05 COS2 FACTOR ROUNDING OPTION IN F0 PROFILE                       
*                                                                               
* SMYE 09/04    FOR PBDCDATE CHANGE NON-WORK DAY DATES                          
*                 TO FIRST PRIOR WORK DAY IN LITTLE BRAS ROUTINE                
*                 "CGWRKDT" IF PROFILE SO INDICATES                             
*                                                                               
* KWAN 04/28/04 COPY= & CAP= FOR REGULAR COMMENTS MAX CHARS VALIDATION          
*                                                                               
* KWAN 03/19/04 CLE REFRESH AND RESTRUCT WORKING STORAGE                        
*                                                                               
* KWAN 12/09/03 IF MATCHED (PBMTSMTQ) TURN ON X'40' IN PBDSTAT                  
*                                                                               
* KWAN 09/10/03 NEW BUY ELEM (PPGENPBMAT) FOR INVOICES                          
*                                                                               
* KWAN 06/23/03 CU=0 OR NONE IS STORED AS X'000001' (SAME AS 0.0001)            
*                                                                               
* KWAN 06/19/03 FIX CU CHANGE UPLOAD BUG                                        
*                                                                               
* KWAN 06/10/03 FIX TYPO IN DLC VALIDATING ROUTINE                              
*                                                                               
* KWAN 05/02/03 DISALLOW INS WITH TEST EST AND LIVE INS STATUS                  
*                                                                               
* KWAN 03/21/03 FIXES FOR ADBUYER 1.1                                           
*                                                                               
* KWAN 02/13/03 ALLOW 2ND INSERTION DATE FORMAT MMMDD FOR MONTHLY               
*                                                                               
* KWAN 02/10/03 FIX CLE CHANGE UPLOAD BUG (WORK IS IMPROPERLY USED)             
*                                                                               
* KWAN 12/19/02 GIVE CLEAR ERROR MSG FOR CLE='BLANK' INPUT                      
*                                                                               
* KWAN 11/25/02 BUG FIX FOR DATE VALIDATION (ERRORNUM NOT SET)                  
*                                                                               
* YKAP 09/10/02 ISS= ISSUE NAME                                                 
*                                                                               
* KWAN 08/12/02 BUG FIX FOR COMMENT ELEM UPLOADS                                
*                                                                               
* KWAN 07/01/02 VALIDATE STD COMMENT CODES FOR X67 AND X68 COMMENTS             
*                                                                               
* KWAN 11/01/01 CODES FOR EDITING OPTIONAL DATA FROM WORKER FILE                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
T41118   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORK18X-WORK18D,T41118,RR=R2,CLEAR=YES                           
*                                                                               
         LR    R9,RC                                                            
         USING WORK18D,R9          R9 = A(GLOBAL STORAGE)                       
*                                                                               
         BASR  R8,0                                                             
         AHI   R8,GLOBALS-*                                                     
         USING GLOBALS,R8          R8 = A(GLOBAL LITERALS)                      
*                                                                               
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
*                                                                               
         L     RA,4(R1)                                                         
         USING T411FFD,RA                                                       
*                                                                               
         BRAS  RE,INITWKST         INITIALIZE WORKING STORAGE AREAS             
*                                                                               
         BRAS  RE,P_REQBUY         PROCESS REQUESTS FROM BUY PROGRAM            
         JE    EXXMOD                                                           
*                                                                               
         BRAS  RE,P_REQADB         PROCESS REQUESTS FROM ADBUYER                
*                                                                               
EXXMOD   XMOD1 1                                                                
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
REGCOM   LR    R0,RE               SAVE RETURN REGISTER                         
         MVI   ELCODE,X'66'                                                     
         OI    ADBCHGSW,X66CHGD    X66 ELEM CHANGE ENCOUNTERED                  
         J     EDTCOMM                                                          
*                                                                               
IOCOM    LR    R0,RE               SAVE RETURN REGISTER                         
         MVI   ELCODE,X'67'                                                     
         OI    ADBCHGSW,X67CHGD    X67 ELEM CHANGE ENCOUNTERED                  
         J     EDTCOMM                                                          
*                                                                               
PICOM    LR    R0,RE               SAVE RETURN REGISTER                         
         MVI   ELCODE,X'68'                                                     
         OI    ADBCHGSW,X68CHGD    X68 ELEM CHANGE ENCOUNTERED                  
         J     EDTCOMM                                                          
*                                                                               
SRCOM    LR    R0,RE               SAVE RETURN REGISTER                         
         MVI   ELCODE,X'6A'                                                     
         OI    ADBCHGSW,X6ACHGD    X6A ELEM CHANGE ENCOUNTERED                  
*                                                                               
EDTCOMM  BRAS  RE,CKCOMCHG         CHECK COMMENT CHANGES                        
         J     EXIT_VRE                                                         
*                                                                               
EDTCAC   LR    R0,RE               SAVE RETURN REGISTER                         
         BRAS  RE,CKAGYCOM         CHECK AGENCY COMMISSION                      
         J     EXIT_VRE                                                         
*                                                                               
EDTPLCOS LR    R0,RE               SAVE RETURN REGISTER                         
         OI    OTHELMSW,PLANCOSQ   PC MAP CODES PRESENT IN UPLOAD REQ           
         BRAS  RE,CKPLCOST         CHECK PLANNED COST                           
         J     EXIT_VRE                                                         
*                                                                               
EDTCOS2$ LR    R0,RE               SAVE RETURN REGISTER                         
         OI    OTHELMSW,COS2$$$Q   PC MAP CODES PRESENT IN UPLOAD REQ           
         BRAS  RE,CKCOS2$F         CHECK COS2 $ OR FACTOR                       
         J     EXIT_VRE                                                         
*                                                                               
EDTCCD   LR    R0,RE               SAVE RETURN REGISTER                         
         BRAS  RE,CKCASDIS         CHECK CASH DISCOUNT                          
         J     EXIT_VRE                                                         
*                                                                               
EDTCBLDT LR    R0,RE               SAVE RETURN REGISTER                         
         BRAS  RE,CKBILDAT         CHECK BILLABLE DATE                          
         J     EXIT_VRE                                                         
*                                                                               
EDTCPDDT LR    R0,RE               SAVE RETURN REGISTER                         
         BRAS  RE,CKPAYDAT         CHECK PAYABLE DATE                           
         J     EXIT_VRE                                                         
*                                                                               
EDTCIODT LR    R0,RE               SAVE RETURN REGISTER                         
         BRAS  RE,CKIORDAT         CHECK INSERTION ORDER DATE                   
         J     EXIT_VRE                                                         
*                                                                               
EDTCD2   LR    R0,RE               SAVE RETURN REGISTER                         
         BRAS  RE,CK2NDDAT         CHECK 2ND INSERTION DATE                     
         J     EXIT_VRE                                                         
*                                                                               
EDTCSHDT LR    R0,RE               SAVE RETURN REGISTER                         
         BRAS  RE,CKSHPDAT         CHECK SHIPPING DATE                          
         J     EXIT_VRE                                                         
*                                                                               
EDTSREP  LR    R0,RE                                                            
         BRAS  RE,CKSPEREP         CHECK SPECIAL REP                            
         J     EXIT_VRE                                                         
*                                                                               
EDTTAX   LR    R0,RE               STORE RETURN REGISTER                        
         BRAS  RE,CKBUYTAX         CHECK TAX                                    
         J     EXIT_VRE                                                         
*                                                                               
EDTFSI   LR    R0,RE               SAVE RETURN REGISTER                         
         BRAS  RE,CKBUYFSI         CHECK FREE STANDING INSERTS                  
         J     EXIT_VRE                                                         
*                                                                               
EDTCU    LR    R0,RE               SAVE RETURN REGISTER                         
         BRAS  RE,CKCONUNT         CHECK CONTRACT UNIT                          
         J     EXIT_VRE                                                         
*                                                                               
EDTREF   LR    R0,RE               SAVE RETURN REGISTER                         
         BRAS  RE,CKREFNUM         CHECK REFERENCE NUMBER                       
         J     EXIT_VRE                                                         
*                                                                               
EDTPVWS  LR    R0,RE               SAVE RETURN REGISTER                         
         MVI   WORK,X'87'          PAGE VIEW                                    
         J     EDTIM50                                                          
*                                                                               
EDTCTUS  LR    R0,RE               SAVE RETURN REGISTER                         
         MVI   WORK,X'88'          CLICK THRU                                   
         J     EDTIM50                                                          
*                                                                               
EDTIMP   LR    R0,RE               SAVE RETURN REGISTER                         
         MVI   WORK,X'92'          IMPRESSION                                   
         J     EDTIM50                                                          
*                                                                               
EDTAIMP  LR    R0,RE               SAVE RETURN REGISTER                         
         MVI   WORK,X'93'          ACTUAL IMPRESSION                            
         J     EDTIM50                                                          
*                                                                               
EDTEXD   LR    R0,RE               SAVE RETURN REGISTER                         
         MVI   WORK,X'89'          MATERIAL EXTENSION DAYS ELEM CODE            
*                                                                               
EDTIM50  BRAS  RE,EDTPACK                                                       
         J     EXIT_VRE                                                         
*                                                                               
EDTTSHT  LR    R0,RE               SAVE RETURN REGISTER                         
         BRAS  RE,TSHEET           EDIT TEARSHEET ELEM                          
         J     EXIT_VRE                                                         
*                                                                               
EDTECPM  LR    R0,RE               SAVE RETURN REGISTER                         
         MVI   BYTE3,X'A0'         ESTIMATED CPM                                
         J     EDTCPMS                                                          
*                                                                               
EDTACPM  LR    R0,RE               SAVE RETURN REGISTER                         
         MVI   BYTE3,X'A1'         ACTUAL CPM                                   
*                                                                               
EDTCPMS  BRAS  RE,CKCPMS                                                        
         J     EXIT_VRE                                                         
*                                                                               
EDTINSTA LR    R0,RE               SAVE RETURN REGISTER                         
         BRAS  RE,CKINSTA          EDIT INSERTION STATUS                        
         J     EXIT_VRE                                                         
*                                                                               
EDTINTC  LR    R0,RE               SAVE RETURN REGISTER                         
         BRAS  RE,INTRCON          EDIT INTERNET CONTRACT ELEM                  
         J     EXIT_VRE                                                         
*                                                                               
EDTINVM  LR    R0,RE               SAVE RETURN REGISTER                         
         BRAS  RE,INVMSTAS         EDIT INVOICE MATCHING STATUS ELEM            
         J     EXIT_VRE                                                         
*                                                                               
EDTITSR  LR    R0,RE               SAVE RETURN REGISTER                         
         BRAS  RE,INVTRSTA         EDIT TEARSHEET RECEIVED STATUS               
         J     EXIT_VRE                                                         
*                                                                               
EDTCOS2F LR    R0,RE               SAVE RETURN REGISTER                         
         BRAS  RE,C2FACTOR         EDIT COS2 FACTOR                             
         J     EXIT_VRE                                                         
*                                                                               
EDT_PO#  LR    R0,RE               SAVE RETURN REGISTER                         
         OI    OTHELMSW,PURORD#Q   PO# MAP CODES PRESENT IN UPLOAD REQ          
         BRAS  RE,PURORDER         EDIT PURCHASE ORDER #                        
         J     EXIT_VRE                                                         
*                                                                               
EDTGST   LR    R0,RE               SAVE RETURN REGISTER                         
         BRAS  RE,CKGSTOVR         CHECK GST OVERRIDE                           
         J     EXIT_VRE                                                         
*                                                                               
EDTPST   LR    R0,RE               SAVE RETURN REGISTER                         
         BRAS  RE,CKCANPST         CHECK CANADIAN PST                           
         J     EXIT_VRE                                                         
*                                                                               
EDTACHRG LR    R0,RE               SAVE RETURN REGISTER                         
         BRAS  RE,BLDACHRG         EXTRACT CHARGE MAP CODES                     
         J     EXIT_VRE                                                         
*                                                                               
EDTSFH   LR    R0,RE               SAVE RETURN REGISTER                         
         BRAS  RE,CKBUYSFH         CHECK SPECIAL FINANCIAL HANDLING             
         J     EXIT_VRE                                                         
*                                                                               
EDTOSDT  LR    R0,RE               SAVE RETURN REGISTER                         
         BRAS  RE,CKONSDAT         CHECK NEWSPAPER ON-SALE DATE                 
         J     EXIT_VRE                                                         
*                                                                               
EDTDLC   LR    R0,RE               SAVE RETURN REGISTER                         
         BRAS  RE,CKBUYDEC         CHECK DAILY EFFECTIVE CIRCULATION            
         J     EXIT_VRE                                                         
*                                                                               
EDTRPT   LR    R0,RE               SAVE RETURN REGISTER                         
         BRAS  RE,CKNUMRPT         CHECK NUMBER OF REPAINT                      
         J     EXIT_VRE                                                         
*                                                                               
EDTEXDT  LR    R0,RE               SAVE RETURN REGISTER                         
         BRAS  RE,CKMEXDAT         CHECK MATERIAL EXTENSION DATE                
         J     EXIT_VRE                                                         
*                                                                               
EDTTRAFF LR    R0,RE               SAVE RETURN REGISTER                         
         BRAS  RE,CHKNTRA          NO TRAFFIC STATUS                            
         J     EXIT_VRE                                                         
*                                                                               
EDTSITEL LR    R0,RE               SAVE RETURN REGISTER                         
         BRAS  RE,CHKSITEL         SITE LOCATION                                
         J     EXIT_VRE                                                         
*                                                                               
EDTISSNM LR    R0,RE               SAVE RETURN REGISTER                         
         BRAS  RE,EDTIS00          ISSUE NAME                                   
         J     EXIT_VRE                                                         
*                                                                               
EDTCLE   LR    R0,RE               SAVE RETURN REGISTER                         
         BRAS  RE,CKBUYCLE         CHECK CONTRACT LINE EQUIVALENCY              
         J     EXIT_VRE                                                         
*                                                                               
EDTCLDT  LR    R0,RE               SAVE RETURN REGISTER                         
         BRAS  RE,CKCLODAT         CHECK NEWSPAPER CLOSING DATE                 
         J     EXIT_VRE                                                         
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
INITWKST NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         ST    R2,RELOBY18                                                      
         MVC   SVPARM_3,DMCB+08                                                 
         MVC   SVPARM_4,DMCB+12                                                 
*                                                                               
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
*                                                                               
         CLI   SVPARM_3,0          VALIDATING PO# REQUEST FROM BUY?             
         BNE   INIT20                                                           
         MVI   CHGIND1,0           INIT CHANGE INDICATORS                       
         MVI   CHGIND2,0                                                        
         MVI   CHGIND3,0                                                        
         MVI   CHGIND4,0                                                        
         MVI   CHGIND5,0                                                        
*                                                                               
INIT20   MVI   OTHELMSW,0          INIT SWITCH FOR "OTHER" ELEMS                
         MVI   ADBCHGSW,0          ADBUYER ELEM CHANGE SWITCH                   
         MVI   ABCBITSW,0          VARIOUS BIT INDICATORS FOR CHANGE UL         
         MVI   WKDATESW,0          DATE VALIDATION SWITCH                       
         MVI   WKAIOSW1,0          AIO SWITCH                                   
*                                                                               
         XC    WKPO#PRD,WKPO#PRD                                                
         XC    WKPO#PNO,WKPO#PNO                                                
         XC    WKPO#SQ#,WKPO#SQ#                                                
         MVI   WKPO#SW1,0                                                       
*                                                                               
         LR    RE,R9                                                            
         A     RE,=A(WKAIO1-WORK18D)                                            
         ST    RE,AWKAIO1                                                       
*                                                                               
         LH    RE,=Y(PO#ULTAB-GENOLD)                                           
         AR    RE,RC                                                            
         XC    0(L'PO#ULTAB*PO#_MAXQ,RE),0(RE)                                  
         ST    RE,APO#TAB          ADDRESS OF PO# TABLE                         
*                                                                               
         XC    TSHTELEM,TSHTELEM   INIT TEARSHEET ELEM                          
         LA    RE,TSHTELEM                                                      
         USING PTSHTEL,RE                                                       
         MVI   PTSHSTAT,C' '                                                    
         MVC   PTSHIND1(09),SPACES                                              
         MVC   PTSHPAGE,SPACES                                                  
         DROP  RE                                                               
*                                                                               
         LA    R0,TSHTC1EL                                                      
         LHI   R1,4*68             4 COMMENT ELEMS TO BE CLEARED                
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         XC    INTCONEL,INTCONEL   INIT INTERNET CONTRACT                       
         LA    RE,INTCONEL                                                      
         USING PICONEL,RE                                                       
         MVC   PICCONN,SPACES      CONTRACT CODE (INIT TO 8 SPACES)             
         ZAP   PICRATE,=P'0'       INIT PACKED FLDS                             
         ZAP   PICIMPS,=P'0'                                                    
         ZAP   PICCPM,=P'0'                                                     
         ZAP   PICINS,=P'0'                                                     
         DROP  RE                                                               
*                                                                               
         LA    RE,ACHRGEL1         INIT ADDITIONAL CHARGES                      
         LA    RF,10                                                            
INIT30   XC    0(32,RE),0(RE)                                                   
         USING PACELEM,RE                                                       
         MVI   PACGN,C'G'          GROSS BY DEFAULT                             
         ZAP   PACAMT,=P'0'        INIT PACKED FLDS                             
         LA    RE,32(RE)                                                        
         BCT   RF,INIT30                                                        
*                                                                               
         XC    ACHRGCOD(2*10),ACHRGCOD                                          
         DROP  RE                                                               
*                                                                               
         CLI   DDLINKSW,C'N'       NEW INSERTION UPLOAD?                        
         BNE   INIT40                                                           
         LA    RE,NEWREC+33                                                     
         CLI   0(RE),X'20'         BUY DESCRIPTION ELEM PRESENT?                
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    ABUPLDSW,IDSKUPLQ   IDESK INSERTION UPLOAD?                      
         BZ    *+12                                                             
         OI    PBDSTAT2-PBDELEM(RE),X'20'                                       
         B     *+8                                                              
         OI    PBDSTAT2-PBDELEM(RE),X'80'                                       
*                                                                               
INIT40   XC    INVMSTEL,INVMSTEL   INVOICE MATCHING STATUSES ELEM               
*                                                                               
INIT50   DS    0H                  FOR FUTURE UESE                              
*                                                                               
         J     EXIT                                                             
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PRT_READ LR    R0,RE                                                            
         MVC   COMMAND,=C'DMREAD'                                               
         J     PRT_DDIR                                                         
*                                                                               
PRT_WRIT LR    R0,RE                                                            
         MVC   COMMAND,=C'DMWRT '                                               
         J     PRT_DDIR                                                         
*                                                                               
PRT_RSEQ LR    R0,RE                                                            
         MVC   COMMAND,=C'DMRSEQ'                                               
         J     PRT_DDIR                                                         
*                                                                               
PRT_ADD_ LR    R0,RE                                                            
         MVC   COMMAND,=C'DMADD '                                               
         J     PRT_DDIR                                                         
*                                                                               
PRT_RDHI LR    R0,RE                                                            
         MVC   COMMAND,=C'DMRDHI'                                               
         MVC   KEYSAVE,KEY                                                      
*                                                                               
PRT_DDIR LR    R0,RE                                                            
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PRTDIR  ',KEY,KEY,   +        
               (TERMNAL,0)                                                      
         J     EXIT_VRE                                                         
*                                                                               
PRT_GETR LR    R0,RE                                                            
         MVC   COMMAND,=C'GETREC'                                               
         J     PRT_DFIL                                                         
*                                                                               
PRT_ADDR LR    R0,RE                                                            
         MVC   COMMAND,=C'ADDREC'                                               
         J     PRT_DFIL                                                         
*                                                                               
PRT_PUTR LR    R0,RE                                                            
         MVC   COMMAND,=C'PUTREC'                                               
*                                                                               
PRT_DFIL LA    RF,KEY+27                                                        
         CLI   COMMAND,C'A'                                                     
         JNE   *+8                                                              
         LA    RF,KEY                                                           
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PRTFILE ',           +        
               (RF),AREC,(TERMNAL,DMWORK)                                       
         J     EXIT_VRE                                                         
*                                                                               
PUB_READ LR    R0,RE                                                            
         MVC   COMMAND,=C'DMREAD'                                               
         J     PUB_DDIR                                                         
*                                                                               
PUB_RDHI LR    R0,RE                                                            
         MVC   COMMAND,=C'DMRDHI'                                               
         MVC   KEYSAVE,KEY                                                      
*                                                                               
PUB_DDIR LR    R0,RE                                                            
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PUBDIR  ',KEY,KEY,   +        
               (TERMNAL,0)                                                      
         J     EXIT_VRE                                                         
*                                                                               
PUB_GETR LR    R0,RE                                                            
         MVC   COMMAND,=C'GETREC'                                               
*                                                                               
         LA    RF,KEY+27                                                        
         CLI   COMMAND,C'A'                                                     
         JNE   *+8                                                              
         LA    RF,KEY                                                           
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PUBFILE ',           +        
               (RF),APUBIO,(TERMNAL,DMWORK)                                     
*                                                                               
EXIT_VRE LR    RE,R0               EXIT VIA SAVED RE                            
         BR    RE                                                               
*                                                                               
GET_ETXT LR    R0,RE               SAVE RETURN ADDRESS                          
         XC    BUYMSG,BUYMSG                                                    
         L     RF,ACOMFACS                                                      
         L     RF,(CGETTXT-COMFACSD)(RF)                                        
         GOTO1 (RF),DMCB+12,(R6),0,(C'E',DMCB),0,0,0                            
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
CGWRKDT  LR    R0,RE               ADJUST DATE TO PRIOR WORKDAY                 
         CLI   BYPROF+10,C'Y'      NEED TO ADJUST DATE?                         
         JNE   CGWRKDTX                                                         
         GOTOR VPPWKDAY,DMCB,(NATION,DUB),DUB,ACOMFACS                          
CGWRKDTX LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
NXTWFELM SR    R0,R0                                                            
         ICM   R0,3,(LQ_LN-LQ_D)(R3)                                            
         AR    R3,R0                                                            
         BR    RE                                                               
*                                                                               
NXTELEM  ZIC   R0,1(R5)            R5 POINTS TO FIRST BUY RECORD ELEM           
         AR    R5,R0               FIRST ELEM IS ALWAYS X'20'                   
         CLC   ELCODE,0(R5)                                                     
         JE    NXTELX              CC IS EQUAL                                  
         CLI   0(R5),0                                                          
         JNE   NXTELEM                                                          
         LTR   R5,R5               CC IS NOT EQUAL                              
NXTELX   BR    RE                                                               
*                                                                               
CKCHGMOD LR    R0,RE                                                            
         CLI   DDLINKSW,C'C'       CHANGE MODE?                                 
         JE    CKCHGM10                                                         
         CLI   DDLINKSW,0          ADBUYER?                                     
         JNE   CKCHGMX                                                          
         CLI   SVTRCODE,C'C'       BUY CHANGE TRANSACTION?                      
         JNE   CKCHGMX                                                          
CKCHGM10 LA    R5,REC+33                                                        
         CLI   0(R5),X'20'         FIRST BUY RECORD ELEMFOUND?                  
         JE    CKCHGMX                                                          
         DC    H'0'                NOT A BUY RECORD!                            
CKCHGMX  LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
X_XCERR# XC    ERRORNUM,ERRORNUM   CLEAR ERROR NUMBER AND EXIT                  
         J     EXIT                                                             
*                                                                               
INVFLDER MVI   ERRORNUM+1,INVERR   SET INVALID ERROR, EXIT VIA XIT1             
         J     EXIT                                                             
*                                                                               
DIFFUERR MVI   ERRORNUM+1,DCHGDUER DATA CHANGED BY DIFFERENT USERS              
         J     EXIT                                                             
*                                                                               
ACSSERR  MVI   ERRORNUM+1,FACCERR  ACCESS TO DATA NOT AUTHORIZED                
         J     EXIT                                                             
*                                                                               
RCHGDERR MVI   ERRORNUM+1,DCHGDUER DATA CHANGED BY DIFFERENT USERS              
         J     EXIT                                                             
*                                                                               
MLYDTERR MVI   ERRORNUM+1,MLYDTMSG INVALID DATE FORMAT FOR MONTHLY PUB          
         J     EXIT                                                             
*                                                                               
XCERRNUM XC    ERRORNUM,ERRORNUM   CLEAR ERROR NUMBER, EIXT VIA XIT1            
*                                                                               
SETCCEQ  CR    RB,RB               EQUAL                                        
         J     *+6                                                              
SETCCNEQ LTR   RB,RB               NOT EQUAL                                    
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
P_REQBUY NTR1  BASE=*,LABEL=*      PROCESS REQUESTS FROM BUY PROGRAM            
*                                                                               
         CLI   SVPARM_3,0          BUY PROGRAM REQUEST CALL?                    
         JE    SETCCNEQ                                                         
*                                                                               
         LA    RE,ROUT_TAB         POINT TO ROUTINE TABLE                       
RQBUY20  CLI   0(RE),X'FF'         END OF TABLE?                                
         BNE   *+6                                                              
         DC    H'0'                UNDEFINED ROUTINE                            
         CLC   0(1,RE),SVPARM_3                                                 
         BNE   *+12                                                             
         ICM   RF,15,1(RE)         ADDRESS OF ROUTINE                           
         B     RQBUY40                                                          
         LA    RE,L'ROUT_TAB(RE)   POINT TO NEXT TABLE ENTRY                    
         B     RQBUY20                                                          
*                                                                               
RQBUY40  A     RF,RELOBY18                                                      
         BASR  RE,RF               BRANCH TO REQUESTED ROUTINE                  
*                                                                               
         J     SETCCEQ                                                          
*                                                                               
ROUT_TAB DS   0XL5                                                              
         DC   AL1(VAL_PO#Q),AL4(PURORDER)     VALIDATE PURCHASE ORDER #         
         DC   AL1(VAL_PLCQ),AL4(CKPLCOST)     VALIDATE PLANNED COST             
         DC   AL1(VAL_CO2Q),AL4(CKCOS2$F)     VALIDATE COS2 $ OR FACTOR         
         DC   X'FF'                                                             
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
P_REQADB NTR1  BASE=*,LABEL=*      PROCESS REQUESTS FROM ADBUYER                
*                                                                               
         MVI   WKERRSW,0           SET ERROR SWITCH                             
         L     R3,VTIA             FIRST 4096 BYTES HAVE WORKER REC             
         LA    R3,4(R3)            POINT TO WORKER ELEM                         
         USING LQ_EL,R3                                                         
*                                                                               
RQADB30  CLI   LQ_EL,0             ELEM CODES EXIST?                            
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   LQ_EL,LQ_RDATQ      RETURNED DATA HEADER ELEM?                   
         BE    RQADB90             DONE                                         
         CLI   LQ_EL,LQ_RQSTQ      REQUEST DATA ELEM?                           
         BNE   RQADB40                                                          
*                                                                               
         BRAS  RE,CKKEYWRD                                                      
         BNE   RQADB40             KEYWORD IS AN OPTIONAL DATA FLD              
         BRAS  RE,CKMIDSFD                                                      
         BNE   RQADB40             MIDDLE SCR FLD, NOT OPT DATA FLD             
*                                                                               
         CLI   DDLINKSW,C'C'       CHANGE INSERTION UPLOAD?                     
         BNE   RQADB35                                                          
         TM    ABUPLDSW,IDSKUPLQ   IDESK INSERTION UPLOAD?                      
         BNZ   RQADB35                                                          
         CLC   =AL2(D#PO#PRD),3(R3)                                             
         BE    RQADB35                                                          
         CLC   =AL2(D#ACHCOD),3(R3)                                             
         BE    RQADB35                                                          
         ZICM  R2,3(R3),(3)        SHOULD BE ODD NUMBERED MAP CODE              
         AHI   R2,1                                                             
         BRAS  RE,NXTWFELM         BUMP TO NEW DATA (EVEN NUMBERED)             
         MVC   HALF,3(R3)                                                       
         CH    R2,HALF             ODD-EVEN PAIR IS IN RIGHT ORDER?             
         BE    *+6                                                              
         DC    H'0'                OUT OF SYNC!                                 
*                                                                               
RQADB35  L     RF,FULL                                                          
         A     RF,RELOBY18                                                      
         LA    R2,6(R3)            POINT R2 TO DATA                             
         MVC   HALF,1(R3)          HALF HAS LENGTH OF DATA                      
         LH    R0,HALF                                                          
         AHI   R0,-6               ADJUST FOR MAPCODE AND LENGTH                
         STH   R0,HALF                                                          
*                                                                               
         XC    ERRORNUM,ERRORNUM   CLEAR ERROR NUMBER                           
         ST    R3,SVWRKELM                                                      
         BASR  RE,RF               EDIT OPTIONAL DATA                           
         L     R3,SVWRKELM                                                      
*                                                                               
         OC    ERRORNUM,ERRORNUM   SEE IF ANY ERROR FOUND                       
         BZ    RQADB40                                                          
         TM    WKERRSW,ERRORFND    ERROR FOUND?                                 
         BO    *+12                                                             
         BRAS  RE,BLDDDEL          BUILD DOWNLOAD DATA ELEM & INS. KEY          
         OI    WKERRSW,ERRORFND    ERROR ENCOUNTERED                            
         BRAS  RE,BLDERREL         BUILD ERROR ELEM                             
         XC    ERRORNUM,ERRORNUM                                                
         MVI   ERRAREA,0           FOR INTERNAL ERROR MSGS                      
*                                                                               
RQADB40  TM    OTHELMSW,ACHARGEQ   ADDITIONAL CHARGES PROCESSED?                
         BZ    *+12                                                             
         NI    OTHELMSW,X'FF'-ACHARGEQ                                          
         B     RQADB30                                                          
         BRAS  RE,NXTWFELM                                                      
         B     RQADB30             PROCESS NEXT MAP CODE                        
*                                                                               
RQADB90  BRAS  RE,FNLZTSHT         FINALIZE TEARSHEET ELEMS                     
         BRAS  RE,FNLZICON         FINALIZE INTERNET CONTRACT ELEM              
         BRAS  RE,FNLZINVM         FINALIZE INV MATCHING STATUSES ELEM          
*                                                                               
         CLI   DDLINKSW,C'C'       CHANGE INSERTION UPLOAD?                     
         BE    *+20                                                             
         TM    OTHELMSW,PURORD#Q   PO# MAP CODES FOUND IN UPLOAD REQ?           
         BNZ   *+12                                                             
         BRAS  RE,PURORDER         LOOK UP PURCHASE ORDER #                     
         BRAS  RE,RPLY_ERR         REPLY ERROR IF ANY                           
*                                                                               
         CLI   DDLINKSW,C'C'       CHANGE INSERTION UPLOAD?                     
         BE    *+20                                                             
         TM    OTHELMSW,PLANCOSQ   PC MAP CODES FOUND IN UPLOAD REQ?            
         BNZ   *+12                                                             
         BRAS  RE,CKPLCOST         ASSIGN PLANNED COST IF NECESSARY             
         BRAS  RE,RPLY_ERR         REPLY ERROR IF ANY                           
*                                                                               
         CLI   DDLINKSW,C'C'       CHANGE INSERTION UPLOAD?                     
         BE    *+20                                                             
         TM    OTHELMSW,COS2$$$Q   PC MAP CODES FOUND IN UPLOAD REQ?            
         BNZ   *+12                                                             
         BRAS  RE,CKCOS2$F         ASSIGN COS2 $ IF NECESSARY                   
         BRAS  RE,RPLY_ERR         REPLY ERROR IF ANY                           
*                                                                               
         BRAS  RE,FNLZERRS         FINALIZE ALL OTHER ERRORS                    
*                                                                               
         MVI   ERRAREA,0                                                        
         TM    WKERRSW,ERRORFND    ERROR FOUND?                                 
         BZ    RQADB_X                                                          
         MVI   ERRAREA,X'FF'       NO CHANGE(S) ALLOWED ON ERRORS               
         XC    BUYMSG,BUYMSG       ERRORS RECORDED, NO DISPLAYBLE ERRS          
*                                                                               
RQADB_X  J     EXIT                                                             
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKCLTFRZ NTR1  BASE=*,LABEL=*      CHECK CLIENT FROZEN OPTION                   
*                                                                               
         CLI   BYPROF+14,C'Y'      NO CHANGE TO FRZ CLIENT?                     
         JNE   CKCFRZ_X                                                         
         TM    SVCLPROF+30,X'02'   FROZEN CLIENT?                               
         JZ    CKCFRZ_X                                                         
*                                                                               
         TM    SVCLPROF+27,X'08'   LOCK THIS MONTH AND ALL FORWARD?             
         JNZ   CKCFRZ20                                                         
         TM    SVCLPROF+27,X'04'   LOCK THIS MONTH AND ALL PRIOR?               
         JNZ   CKCFRZ40                                                         
         TM    SVCLPROF+27,X'02'   LOCK THIS MONTH ONLY?                        
         JNZ   CKCFRZ60                                                         
*                                                                               
         J     CKCFRZER            NO FINANCIAL CHANGES TO FRZ CLIENT           
*                                                                               
CKCFRZ20 CLC   PBUYKDAT(2),SVCLPROF+28                                          
         JL    CKCFRZ_X                                                         
         J     CKCFRZER            NO FINANCIAL CHANGES TO FRZ CLIENT           
*                                                                               
CKCFRZ40 CLC   PBUYKDAT(2),SVCLPROF+28                                          
         JH    CKCFRZ_X                                                         
         J     CKCFRZER            NO FINANCIAL CHANGES TO FRZ CLIENT           
*                                                                               
CKCFRZ60 CLC   PBUYKDAT(2),SVCLPROF+28                                          
         JNE   CKCFRZ_X                                                         
         J     CKCFRZER            NO FINANCIAL CHANGES TO FRZ CLIENT           
*                                                                               
CKCFRZ_X J     SETCCEQ                                                          
CKCFRZER J     SETCCNEQ                                                         
         DROP  RB                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKAGYCOM NTR1  BASE=*,LABEL=*      CHECK AGENCY COMMISSION                      
*                                                                               
         BRAS  RE,CKCHGMOD         CHANGE MODE?                                 
         BNE   EDTCAC3                                                          
*                                                                               
         MVC   ERRORNUM,=AL2(NOFRZCER)                                          
         BRAS  RE,CKCLTFRZ         CLIENT FROZEN OPTION FOUND?                  
         JNE   EXIT                                                             
         XC    ERRORNUM,ERRORNUM                                                
*                                                                               
         TM    ABUPLDSW,IDSKUPLQ   PRISMA UPLOAD?                               
         JNZ   EDTCAC3                                                          
         CLI   OLDCDATA,0          ANY ORIGINAL DATA TO BE COMPARED?            
         BH    *+18                                                             
         CP    PBDACP-PBDELEM(L'PBDACP,R5),=P'0'                                
         JNE   RCHGDERR                                                         
         B     EDTCAC3                                                          
         SR    RF,RF                                                            
         IC    RF,OLDCDATA                                                      
         GOTOR VCASHVAL,DMCB,(3,OLDCDATA+1),(RF)                                
         CLI   0(R1),X'FF'                                                      
         JE    INVFLDER            EXIT WITH INVALID ERROR                      
         L     R0,4(R1)                                                         
         CVD   R0,DUB                                                           
         CP    DUB,=P'100000'      100%?                                        
         JNE   *+10                                                             
         ZAP   DUB,=P'-1'          SET TO INTERNAL EQUIVALENT OF 100%           
         CP    PBDACP-PBDELEM(L'PBDACP,R5),DUB+5(3)                             
         JNE   RCHGDERR                                                         
*                                                                               
EDTCAC3  CLI   PBDCOSIN,C'S'                                                    
         BNE   *+12                                                             
         MVI   ERRORNUM+1,ACERR                                                 
         J     EXIT                                                             
*                                                                               
         CLC   0(3,R2),=C'100'     TEST INPUT OF 100                            
         JE    EDTCAC3K                                                         
         CP    PBDACP,=P'0'        TEST ALREADY HAVE DATA?                      
         JNE   INVFLDER                                                         
         J     EDTCAC4                                                          
EDTCAC3K ZAP   PBDACP,=P'-1'       SET 100 PCT TO -1 PCT                        
         J     EDTCAC_X                                                         
*                                                                               
EDTCAC4  LA    R4,100                                                           
         LA    R5,PBDACP                                                        
         LA    R6,(L'PBDACP-1)*16  HIGH NIBBLE                                  
*                                                                               
         BRAS  RE,EDTPCT           EDIT PERCENTAGE                              
*                                                                               
EDTCAC_X J     EXIT                                                             
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDTPCT   NTR1  BASE=*,LABEL=*      EDIT PERCENTAGE                              
*                                                                               
         MVI   ERRORNUM+1,INVERR   DEFAULT ERROR                                
         LH    R0,HALF                                                          
*                                                                               
         GOTO1 VCASHVAL,DMCB,(5,0(R2)),(R0)                                     
         CLI   0(R1),X'FF'                                                      
         JE    EXIT                                                             
         L     R1,4(R1)                                                         
         LTR   R1,R1                                                            
         JM    EXIT                                                             
         SR    R0,R0                                                            
         DR    R0,R4                                                            
         CVD   R1,DUB                                                           
         CP    DUB,=P'0'                                                        
         BNE   *+10                                                             
         ZAP   DUB,=P'1'                                                        
         EX    R6,*+8                                                           
         B     *+10                                                             
         ZAP   0(0,R5),DUB                                                      
         EX    R6,*+8                                                           
         B     *+10                                                             
         CP    0(0,R5),DUB         TEST INPUT NUMBER TOO LARGE                  
         JNE   EXIT                                                             
*                                                                               
         J     X_XCERR#            CLEAR ERROR NUMBER AND EXIT                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKPLCOST NTR1  BASE=*,LABEL=*      CHECK PLANNED COST                           
*                                                                               
         XC    ERRORNUM,ERRORNUM                                                
         TM    ABUPLDSW,IDSKUPLQ   IDESK INSERTION UPLOAD?                      
         BNZ   PLCOS10                                                          
         CLI   DDLINKSW,0          ADBUYER UPLOAD?                              
         BE    PLCOS10                                                          
         CLC   PCVERSN#,=AL1(03,04,00,31)                                       
         BL    PLCOS12                                                          
*                                                                               
PLCOS10  CLI   BUYMD,C'I'          INTERACTIVE?                                 
         BE    PLCOS30                                                          
         CLI   BUYMD,C'L'          SOCIAL?                                      
         JE    PLCOS30                                                          
         CLI   BUYMD,C'S'          SEARCH?                                      
         JE    PLCOS30                                                          
         CLI   BUYMD,C'B'          MOBILE?                                      
         JE    PLCOS30                                                          
         CLI   BUYMD,C'D'          DIGITAL AUDIO?                               
         JE    PLCOS30                                                          
         CLI   BUYMD,C'V'          NATIONAL VIDEO (NVIDEO)?                     
         JE    PLCOS30                                                          
         CLI   BUYMD,C'W'          LOCAL VIDEO (LVIDEO)?                        
         JE    PLCOS30                                                          
*                                                                               
         TM    ABUPLDSW,IDSKUPLQ   PRISMA UPDATE?                               
         JNZ   PLCOS_X             IGNORE FOR ALL OTHER MEDIA FOR NOW           
*                                                                               
PLCOS12  TM    OTHELMSW,PLANCOSQ   PC MAP CODES PRESENT IN UPLOAD REQ?          
         BZ    PLCOS_X                                                          
         BRAS  RE,CKCHGMOD         CHANGE MODE?                                 
         BNE   PLCOS16                                                          
*                                                                               
         CLI   PBDPLCOS-PBDELEM(R5),X'FF'                                       
         BE    PLCOS16                                                          
*                                                                               
         CLI   OLDCDATA,0          ANY ORIGINAL DATA TO BE COMPARED?            
         BH    *+18                                                             
         OC    PBDPLCOS-PBDELEM(L'PBDPLCOS,R5),PBDPLCOS-PBDELEM(R5)             
         JNZ   RCHGDERR                                                         
         B     PLCOS16                                                          
         SR    RF,RF                                                            
         IC    RF,OLDCDATA                                                      
         GOTOR VCASHVAL,DMCB,(2,OLDCDATA+1),(RF)                                
         CLI   0(R1),X'FF'                                                      
         JE    INVFLDER                                                         
         L     RF,4(R1)                                                         
         ICM   RE,15,PBDPLCOS-PBDELEM(R5)                                       
         CR    RE,RF                                                            
         JNE   RCHGDERR            CHANGED SINCE LAST DOWNLOAD                  
*                                                                               
PLCOS16  XC    PBDPLCOS,PBDPLCOS   SET PC=NONE                                  
         OC    HALF,HALF                                                        
         BZ    PLCOS_X                                                          
         CLC   =C'NONE',0(R2)      NONE?                                        
         BE    PLCOS_X                                                          
*                                                                               
         LH    R0,HALF                                                          
         GOTO1 VCASHVAL,DMCB,(2,0(R2)),(R0)                                     
         CLI   0(R1),X'FF'                                                      
         JE    INVFLDER                                                         
         ICM   RF,15,4(R1)                                                      
         JZ    INVFLDER                                                         
         STCM  RF,15,PBDPLCOS                                                   
         B     PLCOS_X                                                          
*                                                                               
PLCOS30  CLI   SVTRCODE,C'B'       NEW INSERTION?                               
         BNE   PLCOS60                                                          
         TM    OTHELMSW,PLANCOSQ   PC MAP CODES PRESENT IN UPLOAD REQ?          
         BNZ   PLCOS60                                                          
         OC    SVPARM_3+1(3),SVPARM_3+1                                         
         BNZ   PLCOS60                                                          
*                                                                               
         CLI   BYPROF+12,C'Y'      ADD PLANNED COST ELEM?                       
         BNE   PLCOS_X                                                          
         LA    R5,NEWREC+33                                                     
         MVI   ELCODE,BYPCIDQ                                                   
         BRAS  RE,NXTELEM                                                       
         JE    INVFLDER            SHOULD NOT FOUND ONE                         
*                                                                               
PLCOS36  XC    WKBUYELM,WKBUYELM   COPY PLANNED COST FROM RATE                  
         LA    RE,WKBUYELM                                                      
         USING BYPCELD,RE                                                       
         MVI   BYPCELM,BYPCIDQ                                                  
         MVI   BYPCLEN,BYPCELQ                                                  
         MVC   BYPCIND,PBDCOSIN                                                 
         MVC   BYPCNIND,PBDCTYP                                                 
         MVC   BYPCTYP,PBDCOSTY                                                 
         ZAP   BYPCCST,PBDCOS                                                   
         CP    BYPCCST,=P'1'       FREE?                                        
         BNE   *+10                                                             
         ZAP   BYPCCST,=P'0'                                                    
         DROP  RE                                                               
PLCOS46  LA    R5,NEWREC+33                                                     
         MVI   ELCODE,X'FF'                                                     
         BRAS  RE,NXTELEM                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         GOTO1 VRECUP,DMCB,(1,NEWREC),WKBUYELM,(R5)                             
         B     PLCOS_X                                                          
*                                                                               
PLCOS60  BRAS  RE,VALPLCOS         VALIDATE PLANNED COST                        
         BNE   PLCOS_X                                                          
         CLI   WKBUYELM,X'FF'      C'COPYRATE' FEATURE IS USED?                 
         BE    PLCOS36                                                          
         OC    WKBUYELM(2),WKBUYELM                                             
         BNZ   PLCOS46                                                          
*                                                                               
PLCOS_X  CLI   SVPARM_3,VAL_PLCQ   VALIDATING PC REQUEST FROM BUY?              
         BNE   *+12                                                             
         LH    RE,ERRORNUM                                                      
         ST    RE,DMCB+8           PASS BY ERROR NUMBER TO CALLER               
         OI    GLBVALSW,BUYPLCVQ   SET PLANNED COST VALIDATED BIT               
         J     EXIT                                                             
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKCOS2$F NTR1  BASE=*,LABEL=*      CHECK COS2 $ OR FACTOR                       
*                                                                               
         XC    ERRORNUM,ERRORNUM                                                
*                                                                               
         NI    WARN,X'FF'-WX08C2RQ INIT WARNING BIT FOR COS2 CLIENT             
         TM    SVCLPROF+30,X'04'   COS2 $?                                      
         JNZ   *+8                                                              
         OI    WARN,WX08C2RQ       WARNING - CLIENT CODE IS NOT COS2            
*                                                                               
         CLI   SVTRCODE,C'C'       BUY CHANGE TRANSACTION?                      
         JNE   C2$_10                                                           
*                                                                               
         BRAS  RE,CKCLTFRZ         CLIENT FROZEN OPTION FOUND?                  
         JNE   C2$_E02                                                          
*                                                                               
         BRAS  RE,CKIDKC           CK FOR IDESK CONTROL                         
         JNE   C2$_E_X                                                          
         TM    ABUPLDSW,IDSKUPLQ   IDESK INSERTION UPLOAD?                      
         JNZ   C2$_06                                                           
         CLI   SVIDKPRF+02,C'Y'                                                 
         JNE   C2$_06                                                           
         MVC   ERRORNUM,=AL2(CHGCOS2Q)                                          
         J     C2$_E_X             CANNOT CHANGE COS2 RATE                      
*                                                                               
C2$_06   CLI   SVPARM_3,VAL_CO2Q   VALIDATING C2$ REQUEST FROM BUY?             
         JNE   *+12                                                             
         TM    SVCLPROF+30,X'08'   COS2 FACTOR?                                 
         JNZ   C2$_12                                                           
         TM    SVCLPROF+30,X'04'   COS2 $?                                      
         JZ    C2$_E01                                                          
         CLI   SVESPROF+30,C'Y'    FINANCIAL CLIENT?                            
         JE    C2$_E01                                                          
*                                                                               
C2$_10   CLI   SVPARM_3,VAL_CO2Q   VALIDATING C2$ REQUEST FROM BUY?             
         JNE   C2$_30                                                           
         OC    SVPARM_3+1(3),SVPARM_3+1                                         
         JZ    C2$_50              COPY COS2 $ FOR NEW INSERTIONS               
*                                                                               
C2$_12   L     R3,SVPARM_3         ADDRESS OF COS2 $ INPUT                      
         XC    WKTELEM,WKTELEM                                                  
         MVC   WKTELEM(15),0(R3)                                                
         LA    RF,WKTELEM+15-1                                                  
         BRAS  RE,LAST_CHR                                                      
         LA    RE,WKTELEM                                                       
         SR    RF,RE                                                            
         AHI   RF,1                                                             
         STH   RF,HALF             INPUT LENGTH                                 
*                                                                               
         TM    SVCLPROF+30,X'08'   COS2 FACTOR?                                 
         JZ    C2$_14                                                           
         SR    RF,RF                                                            
         ICM   RF,3,HALF                                                        
         GOTOR VCASHVAL,DMCB,(6,(R3)),(RF)                                      
         CLI   0(R1),X'FF'                                                      
         JE    C2$_E01             INVALID FACTOR                               
         L     RF,4(R1)                                                         
         CVD   RF,DUB                                                           
         ZAP   SVE2FAC,DUB+3(5)                                                 
         OI    GENBYSW1,C2FOVRDQ   FACTOR IS OVERRIDDEN IN BUY                  
         OI    GENBYSW1,C2FC2$MQ   FACTOR IS VALIDATED IN C2$ MODULE            
         J     C2$_X               DONE WITH COS2 FACTOR                        
*                                                                               
C2$_14   TM    SVCLPROF+30,X'04'   COS2 $?                                      
         JZ    C2$_E01                                                          
         CLI   SVESPROF+30,C'Y'    FINANCIAL CLIENT?                            
         JE    C2$_E01                                                          
*                                                                               
         J     C2$_70                                                           
*                                                                               
C2$_30   TM    OTHELMSW,COS2$$$Q   PC MAP CODES PRESENT IN UPLOAD REQ?          
         JZ    C2$_32                                                           
         OC    HALF,HALF           HAVE ANY COS2 $ INPUT?                       
         JZ    C2$_32                                                           
         TM    SVCLPROF+30,X'04'   COS2 $?                                      
         JZ    C2$_X                                                            
         LA    R3,6(R3)            POINT TO COS2 $ MAP CODE DATA                
         J     C2$_70                                                           
*                                                                               
C2$_32   CLI   SVTRCODE,C'B'       NEW INSERTION?                               
         JNE   C2$_X                                                            
*                                                                               
C2$_50   TM    SVCLPROF+30,X'04'   COS2 $?                                      
         JZ    C2$_X                                                            
         CLI   SVESPROF+30,C'Y'    FINANCIAL CLIENT?                            
         JE    C2$_X                                                            
*                                                                               
         XC    WKBUYELM,WKBUYELM   BUILD COS2 $ ELEMENT                         
         LA    RE,WKBUYELM                                                      
         USING PORELEM,RE                                                       
         MVI   PORELMCD,PORELMEQ   ELEMENT CODE                                 
         MVI   PORELMLN,PORELLNQ   ELEMENT LENGTH                               
         MVC   PORCOSTY,PBDCOSTY   COPY COST TYPE FROM INSERTION                
         ZAP   PORCOS,PBDCOS       COPY RATE FROM INSERTION                     
         CP    PORCOS,=P'1'        FREE?                                        
         JE    C2$_52                                                           
         CLI   PBDCOSTY,C'U'       UNIT RATE?                                   
         JNE   C2$_54                                                           
         XC    PORCOSTY,PORCOSTY   UNIT RATE NOT SUPPORTED                      
C2$_52   ZAP   PORCOS,=P'0'                                                     
C2$_54   MVC   PORC$TYP,PBDCTYP    COPY NET TYPE FROM INSERTION                 
         OI    PORCOSS1,PORCOS$Q                                                
*                                                                               
* FOR MIDAS CLIENT AND NOT TEST PUB, SET COS2 $ TO ZEOR IF NOT ENTERED          
*                                                                               
         TM    GENBYSW1,MIDASCLQ   MIDAS CLIENT?                                
         JZ    C2$_56                                                           
         TM    GENBYSW1,MIDASTPQ   MIDAS TEST PUB?                              
         JNZ   C2$_56                                                           
         ZAP   PORCOS,=P'0'        SET COS2 $ TO ZERO                           
         DROP  RE                                                               
*                                                                               
C2$_56   TM    GENBYSW1,ADJCO2$Q   ADJUST COS2 RATE TO BUY RATE?                
         JNZ   C2$_56K                                                          
         TM    ABUPLDSW,IDSKUPLQ   IDESK INSERTION UPLOAD?                      
         JZ    C2$_60                                                           
         TM    OTHELMSW,COS2$$$Q   COS2 MAP CODE PRESENT UPLOAD REQ?            
         JNZ   C2$_60                                                           
C2$_56K  CLI   SVTRCODE,C'C'       BUY CHANGE TRANSACTION?                      
         JNE   C2$_60                                                           
         LA    R5,REC+33                                                        
         MVI   ELCODE,X'30'                                                     
         BRAS  RE,NXTELEM                                                       
         JE    C2$_56M                                                          
         GOTO1 VRECUP,DMCB,(1,REC),WKBUYELM,(R5)                                
         J     C2$_X                                                            
C2$_56M  LLC   RE,1(R5)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   0(0,R5),WKBUYELM    COPY COS3 RATE FROM BUY RATE                 
         J     C2$_X                                                            
*                                                                               
C2$_60   LA    R5,NEWREC+33                                                     
         MVI   ELCODE,X'30'                                                     
         BRAS  RE,NXTELEM                                                       
         JE    C2$_E01             SHOULD NOT FOUND ONE                         
*                                                                               
         GOTO1 VRECUP,DMCB,(1,NEWREC),WKBUYELM,(R5)                             
         J     C2$_X                                                            
*                                                                               
C2$_70   XC    WKBUYELM,WKBUYELM   BUILD COS2 $ ELEMENT                         
         LA    R5,WKBUYELM                                                      
         USING PORELEM,R5                                                       
         MVI   PORELMCD,PORELMEQ   ELEMENT CODE                                 
         MVI   PORELMLN,PORELLNQ   ELEMENT LENGTH                               
         MVI   PORC$TYP,PORC$GRQ   DEFAULT TO GROSS                             
         OI    PORCOSS1,PORCOS$Q                                                
*                                                                               
         LH    RF,HALF             INPUT LENGTH                                 
         MVI   PORC$TYP,PORC$GRQ   DEFAULT TO GROSS                             
         CLI   0(R3),C'G'          GROSS?                                       
         JE    C2$_72                                                           
         CLI   0(R3),C'N'          NET?                                         
         JNE   C2$_74                                                           
         MVI   PORC$TYP,PORC$NEQ   SET TO NET                                   
C2$_72   LA    R3,1(R3)            POINT TO $ AMOUNT                            
         BCTR  RF,0                                                             
*                                                                               
C2$_74   GOTOR VCASHVAL,DMCB,(2,(R3)),(RF)                                      
         CLI   0(R1),X'FF'                                                      
         JE    C2$_E01                                                          
*                                                                               
         L     R0,4(R1)                                                         
         CVD   R0,DUB                                                           
         ZAP   PORCOS,DUB                                                       
         CLI   PORC$TYP,PORC$NEQ   ENTERED AS NET?                              
         JNE   C2$_60                                                           
         CLI   SVTRCODE,C'C'       BUY CHANGE TRANSACTION?                      
         JNE   C2$_60                                                           
         ZAP   DUB,PORCOS                                                       
         BRAS  RE,CALC_GRS                                                      
         JNE   *+10                                                             
         ZAP   PORCOS,DUB                                                       
         J     C2$_60                                                           
         DROP  R5                                                               
*                                                                               
C2$_X    CLI   SVPARM_3,VAL_CO2Q   VALIDATING PC REQUEST FROM BUY?              
         BNE   *+12                                                             
         LH    RE,ERRORNUM                                                      
         ST    RE,DMCB+8           PASS BY ERROR NUMBER TO CALLER               
         OI    GLBVALSW,BUYCOS2Q   SET COS2 VALIDATED BIT                       
         J     EXIT                                                             
*                                                                               
C2$_E01  MVI   ERRORNUM+1,INVERR                                                
         J     C2$_EXX                                                          
*                                                                               
C2$_E02  MVC   ERRORNUM,=AL2(NOFRZCER)                                          
         J     C2$_EXX                                                          
*                                                                               
C2$_EXX  TM    ABUPLDSW,IDSKUPLQ   IDESK INSERTION UPLOAD?                      
         JZ    C2$_E_X                                                          
         TM    WARN,WX08C2RQ       WARNING - CLIENT CODE IS NOT COS2?           
         JZ    C2$_E_X                                                          
         XC    ERRORNUM,ERRORNUM   FOR PRISMA SEND WARNING MESSAGE              
         J     C2$_E_X                                                          
*                                                                               
C2$_E_X  TM    SVCLPROF+30,X'04'   COS2 $?                                      
         JZ    *+10                                                             
         MVC   SVMAPCOD,=AL2(D#C2$NEW)                                          
         TM    SVCLPROF+30,X'08'   COS2 FACTOR?                                 
         JZ    *+10                                                             
         MVC   SVMAPCOD,=AL2(D#C2FACT)                                          
         J     C2$_X                                                            
*                                                                               
CALC_GRS LR    R3,RE                                                            
         CP    REC+33+(PBDACP-PBDELEM)(L'PBDACP),=P'0'                          
         JE    CALCGNEQ                                                         
         CP    REC+33+(PBDACP-PBDELEM)(L'PBDACP),=P'1'                          
         JE    CALCGNEQ                                                         
         CVB   R1,DUB                                                           
         M     R0,=F'100000'                                                    
         ZAP   DUB,REC+33+(PBDACP-PBDELEM)(L'PBDACP)                            
         CVB   RF,DUB                                                           
         S     RF,=F'100000'       =NET PCT                                     
         LCR   RF,RF                                                            
         JNP   CALCGNEQ                                                         
         SLDA  R0,1                                                             
         DR    R0,RF                                                            
         LTR   R1,R1                                                            
         JNP   *+8                                                              
         AHI   R1,1                                                             
         SRA   R1,1                                                             
         CVD   R1,DUB              RETURN GROSS                                 
CALCGEQ  SR    RE,RE                                                            
         CR    RE,RE               SET CC EQUAL                                 
         J     CALCG_X                                                          
CALCGNEQ LA    RE,1                                                             
         LTR   RE,RE               SET CC NOT EQUAL                             
CALCG_X  LR    RE,R3                                                            
         BR    RE                                                               
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKIDKC   NTR1  BASE=*,LABEL=*      CHECK IDESK CONTROL                          
*                                                                               
         XC    SVIDKPRF,SVIDKPRF   IDESK CONTROL PROFILE VALUES                 
         TM    ABUPLDSW,IDSKUPLQ   IDESK INSERTION UPLOAD?                      
         JNZ   CKIDKC10                                                         
         TM    SVESPROF+29,X'20'   IDESK ESTIMATE?                              
         JZ    CKIDKC10                                                         
         MVC   ERRORNUM,=AL2(IDKESERQ)                                          
         J     SETCCNEQ                                                         
*                                                                               
CKIDKC10 TM    REC+(PBDSTAT2-PBUYREC),X'20'                                     
         JZ    CKIDKC_X                                                         
*                                                                               
         TM    ABUPLDSW,IDSKUPLQ   IDESK INSERTION UPLOAD?                      
         JNZ   CKIDKC_X                                                         
*                                                                               
         MVC   WORK+00(04),=C'PIDK'                                             
         NI    WORK,X'BF'          MAKE SYSTEM LOWER CASE                       
         MVC   WORK+04(2),AGYALPHA                                              
         MVC   WORK+06(1),BUYMD                                                 
         MVC   WORK+07(3),BUYCL                                                 
         CLI   SVCLTOFC,C' '                                                    
         JNH   *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVCLTOFC                                              
         L     RF,ACOMFACS                                                      
         L     RF,CGETPROF-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,(X'C0',WORK),SVIDKPRF,VDATAMGR                         
*                                                                               
CKIDKC_X J     SETCCEQ             SET CC EQUAL                                 
*                                                                               
CKIDKC_E MVC   ERRORNUM,=AL2(IDSKCERR)                                          
         J     SETCCNEQ                                                         
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKCASDIS NTR1  BASE=*,LABEL=*      CHECK CASH DISCOUNT                          
*                                                                               
         BRAS  RE,CKCHGMOD         CHANGE MODE?                                 
         BNE   EDTCCD3                                                          
         TM    ABUPLDSW,IDSKUPLQ   PRISMA UPLOAD?                               
         JNZ   EDTCCD3                                                          
*                                                                               
         MVC   ERRORNUM,=AL2(NOFRZCER)                                          
         BRAS  RE,CKCLTFRZ         CLIENT FROZEN OPTION FOUND?                  
         JNE   EXIT                                                             
         XC    ERRORNUM,ERRORNUM                                                
*                                                                               
         CLI   OLDCDATA,0          ANY ORIGINAL DATA TO BE COMPARED?            
         BH    *+18                                                             
         CP    PBDCD-PBDELEM(L'PBDCD,R5),=P'0'                                  
         JNE   RCHGDERR                                                         
         B     EDTCCD3                                                          
         SR    RF,RF                                                            
         IC    RF,OLDCDATA                                                      
         GOTOR VCASHVAL,DMCB,(1,OLDCDATA+1),(RF)                                
         CLI   0(R1),X'FF'                                                      
         JE    INVFLDER            EXIT WITH INVALID ERROR                      
         L     R0,4(R1)                                                         
         CVD   R0,DUB                                                           
         CP    PBDCD-PBDELEM(L'PBDCD,R5),DUB+6(2)                               
         JNE   RCHGDERR                                                         
*                                                                               
EDTCCD3  CP    PBDCD,=P'0'                                                      
         JNE   INVFLDER                                                         
         LHI   R4,10000                                                         
         LA    R5,PBDCD                                                         
         LA    R6,(L'PBDCD-1)*16   HIGH NIBBLE                                  
*                                                                               
         BRAS  RE,EDTPCT           EDIT PERCENTAGE                              
*                                                                               
         J     EXIT                                                             
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKBILDAT NTR1  BASE=*,LABEL=*      CHECK BILLABLE DATE                          
*                                                                               
         BRAS  RE,CKCHGMOD         CHANGE MODE?                                 
         BNE   EDTCBLD3                                                         
         TM    ABUPLDSW,IDSKUPLQ   PRISMA UPLOAD?                               
         JNZ   EDTCBLD3                                                         
         CLI   OLDCDATA,0          ANY ORIGINAL DATA TO BE COMPARED?            
         BH    *+18                                                             
         OC    PBDBDATE-PBDELEM(L'PBDBDATE,R5),PBDBDATE-PBDELEM(R5)             
         JNZ   RCHGDERR                                                         
         B     EDTCBLD3                                                         
         GOTO1 VDATCON,DMCB,(3,PBDBDATE-PBDELEM(R5)),(8,DUB)                    
         CLC   DUB,OLDCDATA+1                                                   
         JNE   RCHGDERR            CHANGED SINCE LAST DOWNLOAD                  
*                                                                               
EDTCBLD3 LA    R4,PBDBDATE         BILLABLE DATE FORMAT IS MMMDD/YY             
         OC    PBDBDATE,PBDBDATE                                                
         JNZ   INVFLDER                                                         
         TM    T411FFD+12,X'80'    NO BD= CHANGES                               
         JO    ACSSERR                                                          
         LA    R5,0(2)                                                          
         MVI   BYTE,0                                                           
         BRAS  RE,EDTCDT           GO TO STANDARD DATE EDIT                     
*                                                                               
         J     EXIT                                                             
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKPAYDAT NTR1  BASE=*,LABEL=*      CHECK PAYABLE DATE                           
*                                                                               
         BRAS  RE,CKCHGMOD         CHANGE MODE?                                 
         BNE   EDTCPDD3                                                         
         TM    ABUPLDSW,IDSKUPLQ   PRISMA UPLOAD?                               
         JNZ   EDTCPDD3                                                         
         CLI   OLDCDATA,0          ANY ORIGINAL DATA TO BE COMPARED?            
         BH    *+18                                                             
         OC    PBDPDATE-PBDELEM(L'PBDPDATE,R5),PBDPDATE-PBDELEM(R5)             
         JNZ   RCHGDERR                                                         
         B     EDTCPDD3                                                         
         GOTO1 VDATCON,DMCB,(3,PBDPDATE-PBDELEM(R5)),(8,DUB)                    
         CLC   DUB,OLDCDATA+1                                                   
         JNE   RCHGDERR            CHANGED SINCE LAST DOWNLOAD                  
*                                                                               
EDTCPDD3 LA    R4,PBDPDATE         PAYABLE DATE FORMAT IS MMMDD/YY              
         OC    PBDPDATE,PBDPDATE                                                
         JNZ   INVFLDER                                                         
         LA    R5,0(R2)                                                         
         MVI   BYTE,0                                                           
         BRAS  RE,EDTCDT           GO TO STANDARD DATE EDIT                     
*                                                                               
         J     EXIT                                                             
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKIORDAT NTR1  BASE=*,LABEL=*      CHECK INSERTION ORDER DATE                   
*                                                                               
         BRAS  RE,CKCHGMOD         CHANGE MODE?                                 
         BNE   EDTCIOD3                                                         
         TM    ABUPLDSW,IDSKUPLQ   PRISMA UPLOAD?                               
         JNZ   EDTCIOD3                                                         
         CLI   OLDCDATA,0          ANY ORIGINAL DATA TO BE COMPARED?            
         BH    *+18                                                             
         OC    PBDIODAT-PBDELEM(L'PBDIODAT,R5),PBDIODAT-PBDELEM(R5)             
         JNZ   RCHGDERR                                                         
         B     EDTCIOD3                                                         
         GOTO1 VDATCON,DMCB,(3,PBDIODAT-PBDELEM(R5)),(8,DUB)                    
         CLC   DUB,OLDCDATA+1                                                   
         JNE   RCHGDERR            CHANGED SINCE LAST DOWNLOAD                  
*                                                                               
EDTCIOD3 LA    R4,PBDIODAT                                                      
         OC    PBDIODAT,PBDIODAT                                                
         JNZ   INVFLDER                                                         
         MVC   PBDIODAT,=3X'FF'    IOD = 'NONE'                                 
         OC    HALF,HALF                                                        
         JZ    EDTCDTX                                                          
         CLC   0(4,R2),=C'NONE'    NONE?                                        
         JE    EDTCDTX                                                          
         MVI   BYTE,0                                                           
         BRAS  RE,EDTCDT           GO TO STANDARD DATE EDIT                     
*                                                                               
         J     EXIT                                                             
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CK2NDDAT NTR1  BASE=*,LABEL=*      CHECK 2ND INSERTION DATE                     
*                                                                               
         MVI   ERRORNUM+1,INVDTERR DEFAULT ERROR MSG, INVALID DATE              
         BRAS  RE,CKCHGMOD         CHANGE MODE?                                 
         BNE   EDTCD2C                                                          
         TM    ABUPLDSW,IDSKUPLQ   PRISMA UPLOAD?                               
         JNZ   EDTCD2C                                                          
         CLI   OLDCDATA,0          ANY ORIGINAL DATA TO BE COMPARED?            
         BH    *+18                                                             
         OC    PBDIDAT2-PBDELEM(L'PBDIDAT2,R5),PBDIDAT2-PBDELEM(R5)             
         JNZ   RCHGDERR                                                         
         B     EDTCD2C                                                          
         GOTO1 VDATCON,DMCB,(3,PBDIDAT2-PBDELEM(R5)),(8,DUB)                    
         CLC   DUB(5),OLDCDATA+1   SAME MMMDD?                                  
         JNE   RCHGDERR            CHANGED SINCE LAST DOWNLOAD                  
*                                                                               
EDTCD2C  MVI   PBDEMIND,0          M/E IND                                      
         OC    PBDIDAT2,PBDIDAT2                                                
         JNZ   INVFLDER            CAN'T HAVE ANYTHING IN THERE YET             
*                                                                               
         MVI   PBDIDAT2,X'FF'      SET FOR 'NONE'                               
         OC    HALF,HALF                                                        
         BZ    *+14                NOTHING IN INPUT                             
         CLC   0(4,R2),=C'NONE'    NONE?                                        
         BNE   EDTCD2L                                                          
*                                                                               
         CLI   SVTRCODE,C'B'       NEW INSERTION?                               
         BNE   *+8                                                              
         MVI   PBDIDAT2,0          SET FOR 'NONE' ON A NEW BUY                  
         B     EDTCD2X                                                          
*                                                                               
EDTCD2L  MVC   WORK(6),SPACES      INIT TO SPACES                               
         LH    RE,HALF                                                          
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(R2)                                                    
*                                                                               
EDTCD2P  LA    RF,WORK+5                                                        
         CLI   0(RF),C' '                                                       
         BE    EDTCD2T                                                          
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         CLI   0(RF),C'0'                                                       
         BNL   EDTCD2T                                                          
         MVC   PBDEMIND,0(RF)                                                   
         CLI   PBDEMIND,C'E'                                                    
         BE    EDTCD2T                                                          
         CLI   PBDEMIND,C'M'                                                    
         JNE   EXIT                                                             
         B     EDTCD2T                                                          
*                                                                               
EDTCD2T  LA    R4,PBDIDAT2                                                      
         LA    R5,WORK                                                          
         MVI   BYTE,1                                                           
         BRAS  RE,EDTCDT           GO TO STANDARD DATE EDIT                     
         J     EXIT                                                             
*                                                                               
EDTCD2X  J     X_XCERR#            CLEAR ERROR NUMBER AND EXIT                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKSHPDAT NTR1  BASE=*,LABEL=*      CHECK SHIPPING DATE                          
*                                                                               
         MVI   ELCODE,X'86'        SHIPPING DATE ELEM CODE                      
         BRAS  RE,CKCHGMOD         CHANGE MODE?                                 
         BNE   EDTSHP2                                                          
         TM    ABUPLDSW,IDSKUPLQ   PRISMA UPLOAD?                               
         JNZ   EDTSHP2                                                          
         USING PBSHPDEL,R5                                                      
         CLI   OLDCDATA,0          ANY ORIGINAL DATA TO BE COMPARED?            
         BH    EDTSHP1                                                          
         BRAS  RE,NXTELEM                                                       
         BNE   EDTSHP2                                                          
         OC    PBSHDATE,PBSHDATE   ANYTHING IN SHIP DATE?                       
         JNZ   RCHGDERR            CHANGED SINCE LAST DOWNLOAD                  
         B     EDTSHP2                                                          
*                                                                               
EDTSHP1  BRAS  RE,NXTELEM                                                       
         JNE   RCHGDERR            CHANGED SINCE LAST DOWNLOAD                  
         GOTO1 VDATCON,DMCB,(3,PBSHDATE),(8,DUB)                                
         CLC   DUB,OLDCDATA+1      NOT CHANGED SINCE LAST DOWNLOAD?             
         JNE   RCHGDERR            CHANGED SINCE LAST DOWNLOAD                  
         DROP  R5                                                               
*                                                                               
EDTSHP2  LA    R4,0(R2)            POINT TO INPUT                               
         LA    R3,INVERR                                                        
         XC    WORK(20),WORK                                                    
         LH    R5,HALF             DATA LENGTH                                  
         LTR   R5,R5                                                            
         BP    EDTSHP4                                                          
*                                                                               
         CLI   SVTRCODE,C'B'       BUYING?                                      
         BE    EDTSHP30            JUST EXIT                                    
*                                                                               
EDTSHP3H MVC   WORK(3),=3X'FF'     SPECIAL DELETE CODE                          
         B     EDTSHP10                                                         
*                                                                               
EDTSHP4  MVI   ERRORNUM+1,INVERR   RESET DEFAULT ERR MSG, JUST IN CASE          
         CHI   R5,10               MAX IS MMMDD/YY 0R MM/DD/YYYY                
         JH    EXIT                                                             
*                                                                               
         LR    RE,R5                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(R4)                                                    
         GOTO1 VDATVAL,DMCB,(0,WORK),WORK+10                                    
*                                                                               
         OC    0(4,R1),0(R1)                                                    
         JZ    EXIT                                                             
*                                                                               
         GOTO1 VDATCON,(R1),(0,WORK+10),(3,WORK)                                
*                                                                               
         XC    WORK+3(2),WORK+3                                                 
         CLC   WORK+10(2),=C'00'   TEST HAVE YEAR                               
         BNE   EDTSHP10                                                         
         MVC   WORK+0(1),PBUYKDAT  SET YEAR                                     
         CLC   WORK(3),PBUYKDAT                                                 
         BH    EDTSHP10                                                         
         ZIC   RF,PBUYKDAT                                                      
         LA    RF,1(RF)            DATE IN NEXT YEAR                            
         STC   RF,WORK                                                          
*                                                                               
EDTSHP10 LA    R5,NEWREC+33        NOW STORE IN ELEM                            
         BRAS  RE,NXTELEM                                                       
         BNE   *+8                 MUST ADD ONE                                 
         J     INVFLDER            CAN'T ALREADY HAVE DATE ELEM                 
         XC    WKBUYELM,WKBUYELM                                                
         MVC   WKBUYELM+00(02),=X'8607'                                         
         MVC   WKBUYELM+02(05),WORK                                             
         BRAS  RE,ADDBUYEL                                                      
*                                                                               
EDTSHP30 J     X_XCERR#            CLEAR ERROR NUMBER AND EXIT                  
*                                                                               
ADDBUYEL LR    R0,RE                                                            
         GOTO1 VRECUP,DMCB,(1,NEWREC),WKBUYELM,(R5)                             
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKMEXDAT NTR1  BASE=*,LABEL=*      CHECK MATERIAL EXTENSION DATE                
*                                                                               
         MVI   ELCODE,X'96'        MATERIAL EXTENSION DATE ELEM CODE            
         BRAS  RE,CKCHGMOD         CHANGE MODE?                                 
         BNE   MEXDAT30                                                         
         TM    ABUPLDSW,IDSKUPLQ   PRISMA UPLOAD?                               
         JNZ   MEXDAT30                                                         
         USING PEXDATEL,R5                                                      
         CLI   OLDCDATA,0          ANY ORIGINAL DATA TO BE COMPARED?            
         BH    MEXDAT20                                                         
         BRAS  RE,NXTELEM                                                       
         BNE   MEXDAT30                                                         
         OC    PEXDATE,PEXDATE     ANYTHING IN EXTENSION DATE?                  
         JNZ   RCHGDERR            CHANGED SINCE LAST DOWNLOAD                  
         B     MEXDAT30                                                         
*                                                                               
MEXDAT20 BRAS  RE,NXTELEM                                                       
         JNE   RCHGDERR            CHANGED SINCE LAST DOWNLOAD                  
         GOTO1 VDATCON,DMCB,(3,PEXDATE),(8,DUB)                                 
         CLC   DUB,OLDCDATA+1      NOT CHANGED SINCE LAST DOWNLOAD?             
         JNE   RCHGDERR            CHANGED SINCE LAST DOWNLOAD                  
         DROP  R5                                                               
*                                                                               
MEXDAT30 LA    R4,0(R2)            POINT TO INPUT                               
         LH    R5,HALF             INPUT LENGTH                                 
         LTR   R5,R5                                                            
         BP    MEXDAT40                                                         
*                                                                               
         CLI   SVTRCODE,C'B'       NEW INSERTION?                               
         BE    MEXDAT_X                                                         
*                                                                               
MEXDAT34 MVC   WORK(3),=3X'FF'     SPECIAL DELETE CODE                          
         B     MEXDAT60                                                         
*                                                                               
MEXDAT40 MVI   ERRORNUM+1,INVERR   RESET DEFAULT ERR MSG, JUST IN CASE          
         CHI   R5,10               MAX IS MMMDD/YY 0R MM/DD/YYYY                
         JH    EXIT                                                             
*                                                                               
         OC    HALF,HALF                                                        
         BZ    MEXDAT34            DELETE EXTENSION DATE ELEM                   
         CLC   =C'NONE',0(R4)                                                   
         BE    MEXDAT34            DELETE EXTENSION DATE ELEM                   
*                                                                               
         LR    RE,R5                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(R4)                                                    
         GOTO1 VDATVAL,DMCB,(0,WORK),WORK+10                                    
*                                                                               
         OC    0(4,R1),0(R1)                                                    
         JZ    EXIT                                                             
*                                                                               
         GOTO1 VDATCON,(R1),(0,WORK+10),(3,WORK)                                
*                                                                               
         CLC   WORK(3),PBUYKDAT                                                 
         BNH   *+12                                                             
         MVI   ERRORNUM+1,EXDERR1  EXDATE MUST BE ON/BEFORE INS DATE            
         J     EXIT                                                             
*                                                                               
         XC    WORK+3(2),WORK+3                                                 
         CLC   WORK+10(2),=C'00'   TEST HAVE YEAR                               
         BNE   MEXDAT60                                                         
         MVC   WORK+0(1),PBUYKDAT  SET YEAR                                     
         CLC   WORK(3),PBUYKDAT                                                 
         BH    MEXDAT60                                                         
         ZIC   RF,PBUYKDAT                                                      
         LA    RF,1(RF)            DATE IN NEXT YEAR                            
         STC   RF,WORK                                                          
*                                                                               
MEXDAT60 LA    R5,NEWREC+33        NOW STORE IN ELEM                            
         BRAS  RE,NXTELEM                                                       
         BNE   *+8                 MUST ADD ONE                                 
         J     INVFLDER            CAN'T ALREADY HAVE DATE ELEM                 
         XC    WKBUYELM,WKBUYELM                                                
         MVC   WKBUYELM+00(02),=X'9605'                                         
         MVC   WKBUYELM+02(05),WORK                                             
         BRAS  RE,ADDBUYEL                                                      
*                                                                               
MEXDAT_X J     X_XCERR#            CLEAR ERROR NUMBER AND EXIT                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKONSDAT NTR1  BASE=*,LABEL=*      CHECK NEWSPAPER ON-SALE DATE                 
*                                                                               
         CLI   PBUYKMED,C'N'                                                    
         BNE   EDTOSDTX            IGNORE UNLESS NEWSPAPERS                     
*                                                                               
         BRAS  RE,CKCHGMOD         CHANGE MODE?                                 
         BNE   EDTOSDT3                                                         
         TM    ABUPLDSW,IDSKUPLQ   PRISMA UPLOAD?                               
         JNZ   EDTOSDT3                                                         
         CLI   OLDCDATA,0          ANY ORIGINAL DATA TO BE COMPARED?            
         BH    *+18                                                             
         OC    PBDSDATE-PBDELEM(L'PBDSDATE,R5),PBDSDATE-PBDELEM(R5)             
         JNZ   RCHGDERR                                                         
         B     EDTOSDT3                                                         
         GOTO1 VDATCON,DMCB,(3,PBDSDATE-PBDELEM(R5)),(8,DUB)                    
         CLC   DUB,OLDCDATA+1                                                   
         JNE   RCHGDERR            CHANGED SINCE LAST DOWNLOAD                  
*                                                                               
EDTOSDT3 LA    R4,PBDSDATE         ON SALE DATE FORMAT IS MMMDD/YY              
         OC    PBDSDATE,PBDSDATE                                                
         JNZ   INVFLDER                                                         
         MVI   PBDSDATE,X'FF'      SET FOR 'NONE'                               
         OC    HALF,HALF                                                        
         BZ    EDTOSDTX                                                         
         CLC   0(4,R2),=C'NONE'    NONE?                                        
         BE    EDTOSDTX                                                         
         LA    R5,0(R2)                                                         
         MVI   BYTE,0                                                           
         BRAS  RE,EDTCDT           GO TO STANDARD DATE EDIT                     
         J     EXIT                                                             
*                                                                               
EDTOSDTX J     X_XCERR#            CLEAR ERROR NUMBER AND EXIT                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKCLODAT NTR1  BASE=*,LABEL=*      CHECK NEWSPAPER CLOSING DATE                 
*                                                                               
         CLI   PBUYKMED,C'N'                                                    
         BNE   EDTCLDTX            IGNORE UNLESS NEWSPAPERS                     
*                                                                               
         BRAS  RE,CKCHGMOD         CHANGE MODE?                                 
         BNE   EDTCLDT3                                                         
         TM    ABUPLDSW,IDSKUPLQ   PRISMA UPLOAD?                               
         JNZ   EDTCLDT3                                                         
         CLI   OLDCDATA,0          ANY ORIGINAL DATA TO BE COMPARED?            
         BH    *+18                                                             
         OC    PBDCDATE-PBDELEM(L'PBDCDATE,R5),PBDCDATE-PBDELEM(R5)             
         JNZ   RCHGDERR                                                         
         B     EDTCLDT3                                                         
         GOTO1 VDATCON,DMCB,(3,PBDCDATE-PBDELEM(R5)),(8,DUB)                    
         CLC   DUB,OLDCDATA+1                                                   
         JNE   RCHGDERR            CHANGED SINCE LAST DOWNLOAD                  
*                                                                               
EDTCLDT3 LA    R4,PBDCDATE         CLOSING DATE FORMAT IS MMMDD/YY              
         OC    PBDCDATE,PBDCDATE                                                
         JNZ   INVFLDER                                                         
         MVI   PBDCDATE,X'FF'      SET FOR 'NONE'                               
         OC    HALF,HALF                                                        
         BZ    EDTCLDTX                                                         
         CLC   0(4,R2),=C'NONE'    NONE?                                        
         BE    EDTCLDTX                                                         
         LA    R5,0(R2)                                                         
         MVI   BYTE,0                                                           
         MVI   WKDATESW,C'A'       INDICATES DATE "CAN" BE ADJUSTED             
         BRAS  RE,EDTCDT           GO TO STANDARD DATE EDIT                     
         J     EXIT                                                             
*                                                                               
EDTCLDTX J     X_XCERR#            CLEAR ERROR NUMBER AND EXIT                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDTCDT   NTR1  BASE=*,LABEL=*      STANDARD DATE EDIT                           
*                                                                               
         MVC   ERRORNUM,=AL2(INVDTERR)                                          
*                                                                               
         GOTO1 VDATVAL,DMCB,(BYTE,0(R2)),WORK                                   
         OC    0(4,R1),0(R1)                                                    
         JZ    EXIT                                                             
*                                                                               
* R4 IS POINTING TO DATE IN RECORD                                              
*                                                                               
         GOTO1 VDATCON,(R1),(0,WORK),(3,(R4))                                   
*                                                                               
         CLC   WORK(2),=C'00'      HAVE YEAR?                                   
         BNE   EDTCDT5                                                          
*                                                                               
         MVC   0(1,R4),PBUYKDAT    SET YEAR                                     
         CLC   0(3,R4),PBUYKDAT                                                 
         BH    EDTCDT2                                                          
         IC    RF,PBUYKDAT                                                      
         LA    RF,1(RF)            DATE IN NEXT YEAR                            
         STC   RF,0(R4)                                                         
*                                                                               
EDTCDT2  XC    WORK+10(20),WORK+10                                              
         GOTO1 VDATCON,(R1),(3,0(R4)),(5,WORK+10)                               
         GOTO1 VDATVAL,(R1),(0,WORK+10),WORK+20                                 
         OC    0(4,R1),0(R1)                                                    
         JZ    EXIT                CATCH ERR ESCAPED FROM PREVIOUS DVAL         
*                                                                               
EDTCDT5  CLI   WKDATESW,C'A'       "ADJUSTABLE" DATE?                           
         BNE   EDTCDTX                                                          
*                                                                               
         MVC   DUB(3),0(R4)        DATE TO DUB                                  
         BRAS  RE,CGWRKDT          NON-WORK DAY TEST AND ADJUST                 
         MVC   0(3,R4),DUB         RESTORE POSSIBLY CHANGED DATE                
*                                                                               
EDTCDTX  MVI   WKDATESW,0          RESET DATE SWITCH                            
         J     X_XCERR#            CLEAR ERROR NUMBER AND EXIT                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKSPEREP NTR1  BASE=*,LABEL=*      CHECK SPECIAL REP                            
*                                                                               
         TM    ABUPLDSW,IDSKUPLQ   PRISMA INSERTION UPLOAD?                     
         JZ    EDTSREP2                                                         
         BRAS  RE,CKCHGMOD         CHANGE MODE?                                 
         JNE   EDTSREP5                                                         
         USING PBSREPEL,R5                                                      
         MVI   ELCODE,X'80'                                                     
         BRAS  RE,NXTELEM                                                       
         JNE   EDTSREP5                                                         
         CLC   PBSREP,0(R2)        REAL CHANGE?                                 
         JNE   EDTSREP5                                                         
         J     EDTSREPX            SREP IS NOT CHANGED, DONE                    
         DROP  R5                                                               
*                                                                               
EDTSREP2 BRAS  RE,CKCHGMOD         CHANGE MODE?                                 
         BNE   EDTSREP5                                                         
         USING PBSREPEL,R5                                                      
         MVI   ELCODE,X'80'                                                     
         CLI   OLDCDATA,0          ANY ORIGINAL DATA TO BE COMPARED?            
         BH    EDTSREP3                                                         
         BRAS  RE,NXTELEM                                                       
         BNE   EDTSREP5                                                         
         OC    PBSREP,PBSREP       ANYTHING IN SPECIAL REP?                     
         JNZ   RCHGDERR            CHANGED SINCE LAST DOWNLOAD                  
         B     EDTSREP5                                                         
*                                                                               
EDTSREP3 BRAS  RE,NXTELEM                                                       
         JNE   RCHGDERR            CHANGED SINCE LAST DOWNLOAD                  
         CLC   PBSREP,OLDCDATA+1   NOT CHANGED SINCE LAST DOWNLOAD?             
         JNE   RCHGDERR            CHANGED SINCE LAST DOWNLOAD                  
         DROP  R5                                                               
*                                                                               
EDTSREP5 BRAS  RE,EDTSR            EDIT SPECIAL REP                             
*                                                                               
EDTSREPX J     EXIT                                                             
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKBUYTAX NTR1  BASE=*,LABEL=*      CHECK TAX                                    
*                                                                               
         BRAS  RE,CKCHGMOD         CHANGE MODE?                                 
         BNE   EDTTAX3                                                          
*                                                                               
         MVC   ERRORNUM,=AL2(NOFRZCER)                                          
         BRAS  RE,CKCLTFRZ         CLIENT FROZEN OPTION FOUND?                  
         JNE   EXIT                                                             
         XC    ERRORNUM,ERRORNUM                                                
*                                                                               
         TM    ABUPLDSW,IDSKUPLQ   PRISMA UPLOAD?                               
         JNZ   EDTTAX3                                                          
         CLI   OLDCDATA,0          ANY ORIGINAL DATA TO BE COMPARED?            
         BH    *+18                                                             
         OC    PBDTAX-PBDELEM(L'PBDTAX,R5),PBDTAX-PBDELEM(R5)                   
         JNZ   RCHGDERR                                                         
         B     EDTTAX3                                                          
         SR    RF,RF                                                            
         IC    RF,OLDCDATA                                                      
         GOTOR VCASHVAL,DMCB,(4,OLDCDATA+1),(RF)                                
         CLI   0(R1),X'FF'                                                      
         JE    INVFLDER            EXIT WITH INVALID ERROR                      
         L     RF,4(R1)                                                         
         ZICM  RE,PBDTAX-PBDELEM(R5),(7)                                        
         CR    RE,RF                                                            
         JNE   RCHGDERR            CHANGED SINCE LAST DOWNLOAD                  
*                                                                               
EDTTAX3  OC    PBDTAX,PBDTAX                                                    
         JNZ   EXIT                                                             
         MVI   PBDTAX+2,X'01'      SET TAX=NONE                                 
         OC    HALF,HALF                                                        
         BZ    EDTTAX4                                                          
         CLC   =C'NONE',0(R2)      NONE?                                        
         BE    EDTTAX4                                                          
*                                                                               
         MVI   ERRORNUM+1,INVERR                                                
         LH    R0,HALF             LENGTH OF DATA                               
         CHI   R0,0                                                             
         JL    EXIT                                                             
         GOTO1 VCASHVAL,DMCB,(4,0(R2)),(R0)                                     
         CLI   0(R1),X'FF'                                                      
         JE    EXIT                                                             
         MVI   PBDTAX+2,X'01'                                                   
         L     RF,4(R1)                                                         
         LTR   RF,RF                                                            
         BZ    EDTTAX4                                                          
         JM    EXIT                                                             
         C     RF,=F'1000000'      SHOULD NOT EXCEED 100 PCT                    
         JH    EXIT                                                             
         MVC   PBDTAX,5(R1)                                                     
*                                                                               
EDTTAX4  CLI   SVESPROF+28,C'C'    SEE IF 'C' RATE EST                          
         BNE   EDTTAX6                                                          
         CLC   PBDTAX,=X'000001'   TAX MUST BE ZERO                             
         JNE   EXIT                                                             
*                                                                               
EDTTAX6  J     X_XCERR#            CLEAR ERROR NUMBER AND EXIT                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKBUYFSI NTR1  BASE=*,LABEL=*      CHECK FREE STANDING INSERTS                  
*                                                                               
         BRAS  RE,CKCHGMOD         CHANGE MODE?                                 
         BNE   EDTFSI3                                                          
         TM    ABUPLDSW,IDSKUPLQ   PRISMA UPLOAD?                               
         JNZ   EDTFSI3                                                          
*                                                                               
         J     EDTFSI3             IGNORE DATA CHECK (Y/N FLAG)                 
*                                                                               
         USING PBFSIEL,R5                                                       
         MVI   ELCODE,X'82'                                                     
         CLI   OLDCDATA,0          ANY ORIGINAL DATA TO BE COMPARED?            
         BH    EDTFSI2                                                          
         BRAS  RE,NXTELEM                                                       
         BNE   EDTFSI3                                                          
         CP    PBFSI,=P'0'         ANYTHING IN FREE STANDING INSERT?            
         JNE   RCHGDERR            CHANGED SINCE LAST DOWNLOAD                  
         B     EDTFSI3                                                          
*                                                                               
EDTFSI2  BRAS  RE,NXTELEM                                                       
         JNE   RCHGDERR            CHANGED SINCE LAST DOWNLOAD                  
         LLC   RF,OLDCDATA                                                      
         CHI   RF,1                                                             
         JH    *+12                                                             
         CLI   OLDCDATA+1,C'Y'     FSI LOOK-UP FLAG?                            
         JE    EDTFSI3                                                          
         GOTOR VCASHVAL,DMCB,(0,OLDCDATA+1),(X'40',(RF))                        
         CLI   0(R1),X'FF'                                                      
         JE    INVFLDER            EXIT WITH INVALID ERROR                      
         L     R0,4(R1)                                                         
         CVD   R0,DUB                                                           
         CP    PBFSI,DUB+3(5)      DATA IS STILL SAME AS ORIGINAL?              
         JNE   RCHGDERR            CHANGED SINCE LAST DOWNLOAD                  
         DROP  R5                                                               
*                                                                               
EDTFSI3  LA    R4,0(R2)            POINT TO INPUT                               
         MVI   ERRORNUM+1,INVERR                                                
         LH    R5,HALF             DATA LENGHT                                  
         LTR   R5,R5                                                            
         BP    EDTFSI4                                                          
*                                                                               
         MVI   DUB+1,C'X'          SPECIAL DELETE CODE                          
         B     EDTFSI10                                                         
*                                                                               
EDTFSI4  CHI   R5,8                MAX IS 9,999,999                             
         JH    EXIT                                                             
*                                                                               
         ZAP   DUB(6),=P'0'                                                     
*                                                                               
         CHI   R5,1                                                             
         BNE   EDTFSI6                                                          
         CLI   0(R2),C'N'                                                       
         BE    EDTFSI10                                                         
         CLI   0(R2),C'Y'                                                       
         BNE   EDTFSI6                                                          
EDTFSI5  MVI   DUB+1,C'Y'          SET FOR LOOK-UP                              
         B     EDTFSI10                                                         
*                                                                               
EDTFSI6  CLC   0(2,R2),=C'NO'                                                   
         BE    EDTFSI10                                                         
         CLC   0(3,R2),=C'YES'                                                  
         BE    EDTFSI5             SET FOR LOOK-UP                              
*                                                                               
         MVI   ERRORNUM+1,INVERR                                                
         LH    R0,HALF             DATA LENGTH                                  
*                                                                               
         GOTO1 VCASHVAL,DMCB,(2,0(R2)),(R0)                                     
         CLI   0(R1),X'FF'                                                      
         JE    EXIT                                                             
         L     R0,4(R1)                                                         
         CVD   R0,DUB                                                           
         CP    DUB,=P'0'                                                        
         JL    EXIT                                                             
         DP    DUB,=P'100'                                                      
         CP    DUB+6(2),=P'0'                                                   
         JNE   EXIT                MUST GET REMAINDER 0                         
*                                                                               
EDTFSI10 LA    R5,NEWREC+33        NOW STORE IN PBDECEL                         
         MVI   ELCODE,X'82'                                                     
         BRAS  RE,NXTELEM                                                       
         BNE   EDTFSI20            MUST ADD ONE                                 
*                                                                               
         J     INVFLDER            CAN'T ALREADY HAVE FSI ELEM                  
*                                                                               
EDTFSI20 LA    R4,WKBUYELM                                                      
         XC    WKBUYELM,WKBUYELM                                                
         USING PBFSIELD,R4                                                      
         MVC   PBFSIEL(2),=X'820D' CODE AND LENGHT                              
         MVI   PBFSIIND,X'02'      OVERRIDDEN                                   
         CLI   DUB+1,C'Y'                                                       
         BNE   *+8                                                              
         MVI   PBFSIIND,X'01'      LOOKED-UP                                    
*                                                                               
         MVC   PBFSILDT(3),BTODAY                                               
         MVC   PBFSI(5),DUB+1      FSI DATA OR 'Y' (OR 'X' TO DELETE)           
         MVC   PBFSIPID,SVPID      PERSONAL ID                                  
*                                                                               
         BRAS  RE,ADDBUYEL                                                      
*                                                                               
EDTFSI30 J     X_XCERR#            CLEAR ERROR NUMBER AND EXIT                  
         DROP  RB,R4                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKCONUNT NTR1  BASE=*,LABEL=*      CHECK CONTRACT UNIT                          
*                                                                               
         BRAS  RE,CKCHGMOD         CHANGE MODE?                                 
         BNE   EDTCU3                                                           
         TM    ABUPLDSW,IDSKUPLQ   PRISMA UPLOAD?                               
         JNZ   EDTCU3                                                           
         CLI   OLDCDATA,0          ANY ORIGINAL DATA TO BE COMPARED?            
         BH    *+18                                                             
         OC    PBDCU-PBDELEM(L'PBDCU,R5),PBDCU-PBDELEM(R5)                      
         JNZ   RCHGDERR                                                         
         B     EDTCU3                                                           
         SR    RF,RF                                                            
         IC    RF,OLDCDATA                                                      
         GOTOR VCASHVAL,DMCB,(4,OLDCDATA+1),(RF)                                
         CLI   0(R1),X'FF'                                                      
         JE    INVFLDER            EXIT WITH INVALID ERROR                      
         L     RF,4(R1)                                                         
         CHI   RF,0                CU=0?                                        
         BNE   *+8                                                              
         AHI   RF,1                CU=0 IS REPRESENTED AS X'000001'             
         SR    RE,RE                                                            
         ICM   RE,7,PBDCU-PBDELEM(R5)                                           
         CR    RE,RF                                                            
         JNE   RCHGDERR            CHANGED SINCE LAST DOWNLOAD                  
*                                                                               
EDTCU3   OC    PBDCU,PBDCU                                                      
         JNZ   EXIT                                                             
         MVI   PBDCU+2,X'01'       SET CU=NONE                                  
         OC    HALF,HALF                                                        
         BZ    EDTCU4                                                           
         CLC   =C'NONE',0(R2)      NONE?                                        
         BE    EDTCU4                                                           
*                                                                               
         MVI   ERRORNUM+1,INVERR                                                
         LH    R0,HALF                                                          
         LTR   R0,R0                                                            
         JNP   EXIT                                                             
         GOTO1 VCASHVAL,DMCB,(4,0(R2)),(R0)                                     
         CLI   0(R1),X'FF'                                                      
         JE    EXIT                                                             
         MVI   PBDCU+2,X'01'       .0001 = ZERO UNITS                           
         L     RF,4(R1)                                                         
         LTR   RF,RF                                                            
         BZ    EDTCU4                                                           
         JM    EXIT                                                             
         CLI   PBDSPACE,C'*'       * BUYS CAN NOT HAVE CU=                      
         JE    EXIT                                                             
         CHI   RF,1                CANNOT INPUT .0001 SINCE 0                   
         JE    EXIT                IS CARRIED AS X'000001'                      
         C     RF,=F'16777215'     CANNOT EXCEED 1,677.7215                     
         JH    EXIT                MAX IS 3 BYTES                               
         MVC   PBDCU,5(R1)                                                      
*                                                                               
EDTCU4   J     X_XCERR#            CLEAR ERROR NUMBER AND EXIT                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKREFNUM NTR1  BASE=*,LABEL=*      CHECK REFERENCE NUMBER                       
*                                                                               
         BRAS  RE,CKCHGMOD         CHANGE MODE?                                 
         BNE   EDTREF3                                                          
         TM    ABUPLDSW,IDSKUPLQ   PRISMA UPLOAD?                               
         JNZ   EDTREF3                                                          
         USING PBREFEL,R5                                                       
         MVI   ELCODE,X'83'                                                     
         CLI   OLDCDATA,0          ANY ORIGINAL DATA TO BE COMPARED?            
         BH    EDTREF2                                                          
         BRAS  RE,NXTELEM                                                       
         BNE   EDTREF3                                                          
         OC    PBREFNO,PBREFNO     ANYTHING IN REFERENCE NUMBER?                
         JNZ   RCHGDERR            CHANGED SINCE LAST DOWNLOAD                  
         B     EDTREF3                                                          
*                                                                               
EDTREF2  BRAS  RE,NXTELEM                                                       
         JNE   RCHGDERR            CHANGED SINCE LAST DOWNLOAD                  
         XC    WORK(10),WORK                                                    
         SR    RF,RF                                                            
         IC    RF,OLDCDATA                                                      
         CHI   RF,10                                                            
         JH    EXIT                REFERENCE NUMBER IS 10 CHAR OR LESS          
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),OLDCDATA+1                                               
         CLC   PBREFNO,WORK        NOT CHANGED SINCE LAST DOWNLOAD?             
         JNE   RCHGDERR            CHANGED SINCE LAST DOWNLOAD                  
         DROP  R5                                                               
*                                                                               
EDTREF3  LA    R4,0(R2)            POINT TO INPUT                               
         MVI   ERRORNUM+1,INVERR                                                
         XC    WORK(10),WORK                                                    
         LH    R5,HALF             INPUT LENGTH                                 
         LTR   R5,R5                                                            
         BP    EDTREF4                                                          
*                                                                               
         MVI   WORK,X'FF'          SPECIAL DELETE CODE                          
         CLI   SVTRCODE,C'B'       ADDING EMPTY ELEM ON NEW BUY?                
         JE    EXIT                                                             
         B     EDTREF10                                                         
*                                                                               
EDTREF4  CLC   AGYALPHA,=C'BS'                                                  
         BNE   EDTREF6                                                          
         CHI   R5,6                MAX IS CHARACTERS FOR BACKER                 
         JH    EXIT                                                             
*                                                                               
EDTREF6  CHI   R5,10               SJR + OTHERS - MAX IS 10 CHARS               
         JH    EXIT                                                             
*                                                                               
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(R4)                                                    
*                                                                               
EDTREF10 LA    R5,NEWREC+33        NOW STORE IN PBREFEL                         
         MVI   ELCODE,X'83'                                                     
         BRAS  RE,NXTELEM                                                       
         BNE   EDTREF20            MUST ADD ONE                                 
*                                                                               
         J     INVFLDER            CAN'T ALREADY HAVE REF ELEM                  
*                                                                               
EDTREF20 XC    WKBUYELM,WKBUYELM                                                
         MVC   WKBUYELM+00(02),=X'830F'                                         
         MVC   WKBUYELM+02(10),WORK                                             
         BRAS  RE,ADDBUYEL                                                      
*                                                                               
EDTREF30 J     X_XCERR#            CLEAR ERROR NUMBER AND EXIT                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKGSTOVR NTR1  BASE=*,LABEL=*      EDIT GST OVERRIDE                            
*                                                                               
         MVI   ERRORNUM+1,INVERR                                                
         CLI   NATION,C'C'         CANADIAN AGENCY?                             
         JNE   EXIT                                                             
*                                                                               
         MVC   ERRORNUM,=AL2(NOFRZCER)                                          
         BRAS  RE,CKCLTFRZ         CLIENT FROZEN OPTION FOUND?                  
         JNE   EXIT                                                             
         MVI   ERRORNUM+1,INVERR                                                
*                                                                               
         BRAS  RE,CKCHGMOD         CHANGE MODE?                                 
         BNE   EDTG3                                                            
         TM    ABUPLDSW,IDSKUPLQ   PRISMA UPLOAD?                               
         JNZ   EDTG3                                                            
         CLI   OLDCDATA,0          ANY ORIGINAL DATA TO BE COMPARED?            
         BH    *+18                                                             
         OC    PBDGST-PBDELEM(L'PBDGST,R5),PBDGST-PBDELEM(R5)                   
         JNE   RCHGDERR                                                         
         B     EDTG3                                                            
         ZIC   RE,OLDCDATA                                                      
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   PBDGST-PBDELEM(0,R5),OLDCDATA+1                                  
         JNE   RCHGDERR                                                         
*                                                                               
EDTG3    LA    R4,0(R2)            POINT TO INPUT                               
         LH    R5,HALF             INPUT LENGTH                                 
         CHI   R5,1                                                             
         JNE   EXIT                                                             
         LA    R5,GSTTAB                                                        
EDTG2    CLC   0(1,R4),0(R5)                                                    
         BE    EDTG5                                                            
         LA    R5,1(R5)                                                         
         CLI   0(R5),X'FF'                                                      
         JE    EXIT                                                             
         B     EDTG2                                                            
*                                                                               
EDTG5    MVC   PBDGST,0(R4)                                                     
*                                                                               
EDTGSTX  J     X_XCERR#            CLEAR ERROR NUMBER AND EXIT                  
*                                                                               
GSTTAB   DC    C'SXZT',X'FF'                                                    
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKCANPST NTR1  BASE=*,LABEL=*      CHECK CANADIAN PST                           
*                                                                               
         MVI   ERRORNUM+1,INVERR                                                
         CLI   NATION,C'C'         CANADIAN AGENCY?                             
         JNE   EXIT                                                             
*                                                                               
         BRAS  RE,CKCHGMOD         CHANGE MODE?                                 
         BNE   EDTPST3                                                          
*                                                                               
         MVC   ERRORNUM,=AL2(NOFRZCER)                                          
         BRAS  RE,CKCLTFRZ         CLIENT FROZEN OPTION FOUND?                  
         JNE   EXIT                                                             
         MVI   ERRORNUM+1,INVERR                                                
*                                                                               
         TM    ABUPLDSW,IDSKUPLQ   PRISMA UPLOAD?                               
         JNZ   EDTPST3                                                          
         USING PBYPSTEL,R5                                                      
         MVI   ELCODE,X'84'                                                     
         CLI   OLDCDATA,0          ANY ORIGINAL DATA TO BE COMPARED?            
         BH    EDTPST2                                                          
         BRAS  RE,NXTELEM                                                       
         BNE   EDTPST3                                                          
         OC    PBYPSTC,PBYPSTC     ANYTHING IN PST CODES?                       
         JNZ   RCHGDERR            CHANGED SINCE LAST DOWNLOAD                  
         B     EDTPST3                                                          
*                                                                               
EDTPST2  BRAS  RE,NXTELEM                                                       
         JNE   RCHGDERR            CHANGED SINCE LAST DOWNLOAD                  
         SR    RF,RF                                                            
         IC    RF,OLDCDATA                                                      
         CHI   RF,4                LENGTH AFTER PST/                            
         JH    EXIT                MAX IS ONE PROVINCE                          
         XC    WORK,WORK           BUILD FIELD HEADER FOR PSTVAL                
         MVI   WORK,48                                                          
         STC   RF,WORK+5                                                        
         AHI   RF,-1                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK+8(0),OLDCDATA+1                                             
*                                                                               
         XC    X(PSTLNQ),X                                                      
         LA    R4,X                                                             
         USING PSTBLKD,R4                                                       
         MVI   PSTACT,PSTVALQ      ACTION = VALIDATE                            
         LA    R0,WORK                                                          
         ST    R0,PSTADIN          INPUT ADDRESS                                
         XC    X+50(20),X+50                                                    
         LA    R0,X+50                                                          
         ST    R0,PSTADOUT         OUTPUT ADDRESS                               
         MVC   PSTACOM,ACOMFACS    A(COMFACS)                                   
         XC    DMCB(12),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000A6B'                                           
         GOTO1 VCALLOV,DMCB                                                     
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(R4)                                                   
         CLI   PSTERR,0                                                         
         JNE   EXIT                                                             
         DROP  R4                                                               
*                                                                               
         CLC   PBYPSTC,X+50        NOT CHANGED SINCE LAST DOWNLOAD?             
         JNE   RCHGDERR            CHANGED SINCE LAST DOWNLOAD                  
         DROP  R5                                                               
*                                                                               
EDTPST3  LA    R4,0(R2)            POINT TO INPUT                               
         LH    R5,HALF             INPUT LENGTH                                 
         LTR   R5,R5                                                            
         JNP   EXIT                                                             
*                                                                               
         CHI   R5,4                LENGTH AFTER PST/                            
         JH    EXIT                MAX IS ONE PROVINCE                          
*                                                                               
         XC    WORK,WORK           BUILD FIELD HEADER FOR PSTVAL                
         MVI   WORK,48                                                          
         STC   R5,WORK+5                                                        
         AHI   R5,-1                                                            
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   WORK+8(0),0(R4)     EXECUTED                                     
*                                                                               
         XC    X(PSTLNQ),X                                                      
         LA    R5,X                                                             
         USING PSTBLKD,R5                                                       
         MVI   PSTACT,PSTVALQ      ACTION = VALIDATE                            
         LA    R0,WORK                                                          
         ST    R0,PSTADIN          INPUT ADDRESS                                
         XC    X+50(20),X+50                                                    
         LA    R0,X+50                                                          
         ST    R0,PSTADOUT         OUTPUT ADDRESS                               
         MVC   PSTACOM,ACOMFACS    A(COMFACS)                                   
*                                                                               
         XC    DMCB(12),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000A6B'                                           
         GOTO1 VCALLOV,DMCB                                                     
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(R5)                                                   
         CLI   PSTERR,0                                                         
         JNE   EXIT                                                             
*                                                                               
         DROP  R5                                                               
*                                                                               
         LA    R5,NEWREC+33                                                     
         MVI   ELCODE,X'84'                                                     
         BRAS  RE,NXTELEM                                                       
         BNE   EDTPST20                                                         
*                                                                               
         MVC   2(10,R5),X+50       ELEM EXISTS - JUST MOVE IN NEW DATA          
         B     EDTPST30                                                         
*                                                                               
EDTPST20 XC    X(25),X                                                          
         MVC   X(2),=X'840C'                                                    
         MVC   X+2(10),X+50                                                     
         GOTO1 VRECUP,DMCB,(1,NEWREC),X,0(R5)                                   
*                                                                               
EDTPST30 J     X_XCERR#            CLEAR ERROR NUMBER AND EXIT                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKBUYSFH NTR1  BASE=*,LABEL=*      CHECK SPECIAL FINANCIAL HANDLING             
*                                                                               
         BRAS  RE,CKCHGMOD         CHANGE MODE?                                 
         BNE   EDTSFH5                                                          
         TM    ABUPLDSW,IDSKUPLQ   PRISMA UPLOAD?                               
         JNZ   EDTSFH5                                                          
         CLI   OLDCDATA,0          ANY ORIGINAL DATA TO BE COMPARED?            
         BH    *+16                                                             
         TM    PBDSTAT-PBDELEM(R5),X'04'                                        
         JO    RCHGDERR                                                         
         B     EDTSFH5                                                          
         CLI   OLDCDATA+1,C'H'     HELD, HOLD OR SIMPLY H?                      
         BNE   *+16                                                             
         TM    PBDSTAT-PBDELEM(R5),X'0C'                                        
         JZ    RCHGDERR                                                         
         B     EDTSFH5                                                          
         CLI   OLDCDATA+1,C'R'     REL OR SIMPLY R?                             
         JNE   INVFLDER                                                         
         TM    PBDSTAT-PBDELEM(R5),X'04'                                        
         JZ    RCHGDERR                                                         
*                                                                               
EDTSFH5  MVI   ERRORNUM+1,INVERR                                                
         LA    R4,0(R2)            POINT TO INPUT                               
         LH    R5,HALF             LENGTH                                       
         CHI   R5,1                                                             
         JL    EXIT                                                             
         CLC   0(4,R4),=C'HOLD'                                                 
         BE    EDTSFHH                                                          
         CLC   0(4,R4),=C'HELD'                                                 
         BE    EDTSFHH                                                          
         CLI   0(R4),C'H'                                                       
         BE    EDTSFHH                                                          
         CLC   0(3,R4),=C'REL'                                                  
         BE    EDTSFHR                                                          
         CLI   0(R4),C'R'                                                       
         BE    EDTSFHR                                                          
         J     EXIT                                                             
*                                                                               
EDTSFHH  OI    PBDSTAT,X'0C'       SET ON SFH AND HOLD BITS                     
         CLI   SVTRCODE,C'B'       BUYING?                                      
         BE    EDTSFHX                                                          
*                                                                               
* ON CHANGES LOOK FOR I/O ELEMEMT WITH A DATE                                   
*                                                                               
         LA    R5,REC+33                                                        
         MVI   ELCODE,X'70'                                                     
*                                                                               
EDTSFHH1 BRAS  RE,NXTELEM                                                       
         BNE   EDTSFHX                                                          
         OC    2(3,R5),2(R5)       CHECK FOR DATE                               
         BZ    EDTSFHH1                                                         
         XC    BUYMSG,BUYMSG                                                    
         MVC   BUYMSG(34),=C'CAN''T HOLD- BUY HAS BEEN ON AN I/O'               
         MVI   ERRAREA,X'FF'       SPECIAL ERROR                                
         MVI   ERRORNUM+1,X'FF'                                                 
         J     EXIT                                                             
*                                                                               
EDTSFHR  OI    PBDSTAT,X'04'       SFH BIT                                      
         NI    PBDSTAT,X'F7'       SET OFF X'08' HOLD BIT                       
*                                  IF NEW BUY COULD ALREADY BE ON               
EDTSFHX  J     X_XCERR#            CLEAR ERROR NUMBER AND EXIT                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKBUYDEC NTR1  BASE=*,LABEL=*      CHECK DAILY EFFECTIVE CIRCULATION            
*                                                                               
         CLI   PBUYKMED,C'O'                                                    
         BNE   EDTDLCX             IGNORE UNLESS OUTDOOR                        
*                                                                               
         BRAS  RE,CKCHGMOD         CHANGE MODE?                                 
         BNE   EDTDLC3                                                          
         TM    ABUPLDSW,IDSKUPLQ   PRISMA UPLOAD?                               
         JNZ   EDTDLC3                                                          
         USING PBDECEL,R5                                                       
         MVI   ELCODE,X'81'                                                     
         CLI   OLDCDATA,0          ANY ORIGINAL DATA TO BE COMPARED?            
         BH    EDTDLC2                                                          
         BRAS  RE,NXTELEM                                                       
         BNE   EDTDLC3                                                          
         CP    PBDEC,=P'0'         ANYTHING IN DEC?                             
         JNZ   RCHGDERR            CHANGED SINCE LAST DOWNLOAD                  
         B     EDTDLC3                                                          
*                                                                               
EDTDLC2  BRAS  RE,NXTELEM                                                       
         JNE   RCHGDERR            CHANGED SINCE LAST DOWNLOAD                  
         SR    RF,RF                                                            
         IC    RF,OLDCDATA                                                      
         GOTOR VCASHVAL,DMCB,(C'0',OLDCDATA+1),(RF)                             
         CLI   0(R1),X'FF'                                                      
         JE    INVFLDER            EXIT WITH INVALID ERROR                      
         ZAP   DUB,4(8,R1)                                                      
         CP    PBDEC,DUB           DATA IS STILL SAME AS ORIGINAL?              
         JNE   RCHGDERR            CHANGED SINCE LAST DOWNLOAD                  
         DROP  R5                                                               
*                                                                               
EDTDLC3  MVI   ERRORNUM+1,INVERR                                                
         LH    R5,HALF             INPUT LENGTH                                 
         LTR   R5,R5                                                            
         BP    EDTDLC4                                                          
*                                                                               
         MVI   DUB,C'X'            SPECIAL DELETE CODE                          
         CLI   SVTRCODE,C'B'       BUYING?                                      
         JE    EXIT                ERROR WHEN BUYING                            
         B     EDTDLC10                                                         
*                                                                               
EDTDLC4  GOTO1 VCASHVAL,DMCB,(C'0',0(R2)),(R5)                                  
         CLI   0(R1),X'FF'                                                      
         JE    EXIT                                                             
         CP    4(8,R1),=P'999999999'                                            
         JH    EXIT                                                             
         ZAP   DUB(6),4(8,R1)                                                   
*                                                                               
EDTDLC10 LA    R5,NEWREC+33        NOW STORE IN PBDECEL                         
         MVI   ELCODE,X'81'                                                     
         BRAS  RE,NXTELEM                                                       
         BNE   *+8                 MUST ADD ONE                                 
         J     INVFLDER            CAN'T ALREADY HAVE DLC ELEM                  
*                                                                               
         XC    WKBUYELM,WKBUYELM                                                
         MVC   WKBUYELM+00(02),=X'810A'                                         
         MVC   WKBUYELM+02(06),DUB                                              
         BRAS  RE,ADDBUYEL                                                      
*                                                                               
EDTDLCX  J     X_XCERR#            CLEAR ERROR NUMBER AND EXIT                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKNUMRPT NTR1  BASE=*,LABEL=*      CHECK NUMBER OF REPAINT                      
*                                                                               
         CLI   PBUYKMED,C'O'                                                    
         BNE   EDTRPTX             IGNORE UNLESS OUTDOOR                        
*                                                                               
         BRAS  RE,CKCHGMOD         CHANGE MODE?                                 
         BNE   EDTRPT3                                                          
         TM    ABUPLDSW,IDSKUPLQ   PRISMA UPLOAD?                               
         JNZ   EDTRPT3                                                          
         USING PBRPTEL,R5                                                       
         MVI   ELCODE,X'85'                                                     
         CLI   OLDCDATA,0          ANY ORIGINAL DATA TO BE COMPARED?            
         BH    EDTRPT2                                                          
         BRAS  RE,NXTELEM                                                       
         BNE   EDTRPT3                                                          
         CP    PBRPTNO,=P'0'       ANYTHING IN RPT?                             
         JNZ   RCHGDERR            CHANGED SINCE LAST DOWNLOAD                  
         B     EDTRPT3                                                          
*                                                                               
EDTRPT2  BRAS  RE,NXTELEM                                                       
         JNE   RCHGDERR            CHANGED SINCE LAST DOWNLOAD                  
         SR    RF,RF                                                            
         IC    RF,OLDCDATA                                                      
         GOTOR VCASHVAL,DMCB,(0,OLDCDATA+1),(X'40',(RF))                        
         CLI   0(R1),X'FF'                                                      
         JE    INVFLDER            EXIT WITH INVALID ERROR                      
         L     R0,4(R1)                                                         
         CVD   R0,DUB                                                           
         CP    PBRPTNO,DUB+5(3)    DATA IS STILL SAME AS ORIGINAL?              
         JNE   RCHGDERR            CHANGED SINCE LAST DOWNLOAD                  
         DROP  R5                                                               
*                                                                               
EDTRPT3  MVI   ERRORNUM+1,INVERR                                                
         LH    R5,HALF             INPUT LENGTH                                 
         LTR   R5,R5                                                            
         BP    EDTRPT4                                                          
*                                                                               
         MVI   DUB+3,C'X'          SPECIAL DELETE CODE                          
         CLI   SVTRCODE,C'B'       BUYING?                                      
         JE    EXIT                CAN'T DELETE IF BUYING                       
*                                                                               
         B     EDTRPT10                                                         
*                                                                               
EDTRPT4  GOTO1 VCASHVAL,DMCB,(2,0(R2)),(R5)                                     
         CLI   0(R1),X'FF'                                                      
         JE    EXIT                                                             
         L     R0,4(R1)                                                         
         CVD   R0,DUB                                                           
         CP    DUB,=P'0'                                                        
         JL    EXIT                                                             
         CP    DUB,=P'99900'       MAX IS 999.00                                
         JH    EXIT                                                             
         DP    DUB,=P'100'                                                      
         CP    DUB+6(2),=P'0'                                                   
         JNE   EXIT                MUST GET REMAINDER 0 (NO DECIMALS)           
*                                                                               
EDTRPT10 LA    R5,NEWREC+33        NOW STORE IN PBRPTEL                         
         MVI   ELCODE,X'85'                                                     
         BRAS  RE,NXTELEM                                                       
         BNE   *+8                 MUST ADD ONE                                 
         J     INVFLDER            CAN'T ALREADY HAVE RPT ELEM                  
*                                                                               
         XC    WKBUYELM,WKBUYELM                                                
         MVC   WKBUYELM+00(02),=X'8505'                                         
         MVC   WKBUYELM+02(03),DUB+3                                            
         BRAS  RE,ADDBUYEL                                                      
*                                                                               
EDTRPTX  J     X_XCERR#            CLEAR ERROR NUMBER AND EXIT                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKSTDCOM NTR1  BASE=*,LABEL=*      CHECK STANDARD COMMENT CODE                  
*                                                                               
         LH    RE,HALF                                                          
*                                                                               
         CLC   =C'CAP=',0(R2)                                                   
         BNE   *+16                                                             
         CHI   RE,25+4                                                          
         JH    J_DMXERR            DATA EXCEEDS MAX ERROR                       
         J     SETCCEQ                                                          
*                                                                               
         CLC   =C'COPY=',0(R2)                                                  
         BNE   *+16                                                             
         CHI   RE,17+5                                                          
         JH    J_DMXERR            DATA EXCEEDS MAX ERROR                       
         J     SETCCEQ                                                          
*                                                                               
         CHI   RE,4                INPUT IS GREATER THAN 4, COM=?               
         JL    SETCCEQ                                                          
         CLC   =C'COM=',0(R2)      DOING STD COMMENT CODE?                      
         JNE   SETCCEQ                                                          
         CLI   ELCODE,X'66'        REGULAR COMMENT?                             
         JE    CKSCMERR            CANNOT DO STD COMMENT CODE FOR X66           
*                                                                               
         CHI   RE,10               6 CHARS MAX FOR STD COMMENT CODE             
         JH    CKSCMERR                                                         
         CHI   RE,5                AT LEAST 1 CHAR FOR STD COMMENT CODE         
         JL    CKSCMERR                                                         
*                                                                               
         AHI   RE,-4               MINUS OVERHEAD, COM=                         
         MVC   WORK(6),SPACES                                                   
         LA    R5,WORK+6                                                        
         SR    R5,RE                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),4(R2)       RIGHT ALIGN IN WORK                          
         XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),BUYMD      MEDIA                                        
         MVI   KEY+3,X'40'                                                      
         MVC   KEY+4(6),WORK       STD COMMENT CODE                             
         MVC   KEYSAVE(L'KEY),KEY                                               
*                                                                               
         BRAS  RE,PRT_RDHI                                                      
         CLC   KEY(10),KEYSAVE                                                  
         JE    SETCCEQ             FOUND - OK                                   
*                                                                               
         MVI   ERRORNUM+1,STDCNFND                                              
         J     SETCCNEQ                                                         
*                                                                               
CKSCMERR MVI   ERRORNUM+1,INVERR                                                
         J     SETCCNEQ                                                         
*                                                                               
J_DMXERR MVC   ERRORNUM,=AL2(DEMAXERR)                                          
         J     SETCCNEQ                                                         
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKCOMCHG NTR1  BASE=*,LABEL=*      ELCODE HAS 66, 67 OR 68                      
*                                                                               
         BRAS  RE,CKCHGMOD         CHANGE MODE?                                 
         BNE   COMC50                                                           
*                                                                               
         TM    ABUPLDSW,IDSKUPLQ   PRISMA UPLOAD?                               
         JZ    *+12                                                             
         MVI   WKTBYTE,0           SET NO ERROR FOR PRISMA                      
         J     COMC80K                                                          
*                                                                               
         CLI   OLDCDATA,0          ANYTHING IN OLD DATA?                        
         BH    COMC40                                                           
         BRAS  RE,NXTELEM                                                       
         JNE   COMC50              OK TO ADD COMMENT                            
         LLC   R1,1(R5)            ELEMENT LENGTH                               
         AHI   R1,-2               COMMENT LENGTH                               
         LA    RF,2(R5)            POINT TO COMMENT                             
         BRAS  RE,FIXUCHAR         FIX UNPRINTABLE CHARACTERS                   
         LLC   RE,1(R5)                                                         
         SHI   RE,2+1              2 FOR ELEM OVERHEAD AND 1 FOR EX             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   2(0,R5),SPACES      HAVE COMMENT?                                
         JE    COMC50                                                           
         J     DIFFUERR            ELEM STATE IS DIFFERENT                      
*                                                                               
COMC40   BRAS  RE,NXTELEM          FOUND COMMENT?                               
         JE    COMC80              YES, COMPARE ORIGINAL WITH REC               
         CLI   ELCODE,X'6A'        SPECIAL REMITTANCE COMMENT?                  
         JE    COMC80                                                           
         J     DIFFUERR            COM NOT FOUND, MIGHT BE REMOVED!             
*                                                                               
COMC50   LH    RE,HALF             OKAY TO BUILD A COMMENT ELEM                 
         CHI   RE,0                                                             
         BNH   COMC50X             NO COMMENT                                   
         LA    R5,NEWREC+33                                                     
COMC50B  BRAS  RE,NXTELEM          SHOULD GET TO LAST OR END OF REC             
         BE    COMC50B                                                          
*                                                                               
         XC    WKTELEM,WKTELEM                                                  
         MVC   WKTELEM(1),ELCODE                                                
         LH    R0,HALF                                                          
         AHI   R0,2                ADJUST FOR CODE AND LENGTH                   
         STC   R0,WKTELEM+1                                                     
         LH    R1,HALF                                                          
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WKTELEM+2(0),0(R2)                                               
*                                                                               
         LH    R1,HALF             LENGTH OF COMMENT                            
         LA    RF,WKTELEM+2        POINT TO COMMENT                             
         BRAS  RE,FIXUCHAR         FIX UNPRINTABLE CHARACTERS                   
*                                                                               
         GOTO1 VRECUP,DMCB,(1,NEWREC),WKTELEM,(R5)                              
*                                                                               
         BRAS  RE,CKSTDCOM         VALIDATE STD COMMENT CODE IF ENTERED         
         JNE   EXIT                ERRORNUM IS SET IN CKSTDCOM                  
*                                                                               
COMC50X  J     XCERRNUM                                                         
*                                                                               
COMC80   LLC   R1,1(R5)            ELEMENT LENGTH                               
         AHI   R1,-2               COMMENT LENGTH                               
         LA    RF,2(R5)            POINT TO COMMENT                             
         BRAS  RE,FIXUCHAR         FIX UNPRINTABLE CHARACTERS                   
         LLC   R1,OLDCDATA                                                      
         LA    RF,OLDCDATA+1                                                    
         BRAS  RE,FIXUCHAR         FIX UNPRINTABLE CHARACTERS                   
*                                                                               
         MVI   WKTBYTE,0           INIT ERROR FLAG                              
*                                                                               
         LLC   RE,1(R5)                                                         
         SHI   RE,2+1              2 FOR ELEM OVERHEAD AND 1 FOR EX             
         EX    RE,*+8                                                           
         B     *+10                                                             
         OC    2(0,R5),SPACES      FORCE UPPER CASE                             
*                                                                               
         LLC   RE,OLDCDATA                                                      
         BCTR  RE,0                FOR EX INSTRUCTION                           
         EX    RE,*+8                                                           
         J     *+10                                                             
         OC    OLDCDATA+1(0),SPACES                                             
*                                                                               
         CLI   0(R5),X'6A'         SPECIAL REMITTANCE COMMENT?                  
         JE    COMC80K             SKIP LENGTH AND COMMENT COMPARE              
*                                                                               
         LLC   RE,OLDCDATA         PREPARE TO COMPARE ORIGINAL WITH REC         
         CHI   RE,0                                                             
         BH    *+6                                                              
         DC    H'0'                BAD LENGTH                                   
         BCTR  RE,0                FOR EX                                       
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   2(0,R5),OLDCDATA+1  OLD DATA IS SAME AS THAT ON REC?             
         BNE   COMC80H                                                          
*                                                                               
         AHI   RE,1                RESTORE FROM EX INSTRUCTION                  
         SR    RF,RF                                                            
         IC    RF,1(R5)                                                         
         LA    R1,0(R5)            BEGINNING OF ELEM                            
         AR    R1,RF               POINT TO END                                 
         AHI   RF,-2               MINUS 2 FROM ELEM OVERHEAD                   
*                                                                               
         CR    RE,RF               LENGTH OF ORIGINAL AND REC ARE SAME?         
         BE    COMC80K             NO ERROR                                     
         BH    COMC80H             SET ERROR SWITCH                             
*                                                                               
         SR    RF,RE               DIFFERENCE OF CHARACTER(S)                   
         SR    R1,RF               POINT TO POSSIBLE TRAILING SPACE(S)          
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R1),SPACES      TRAILING CHARS ARE SPACES?                   
         BNE   COMC80H                                                          
         B     COMC80K             NO ERROR                                     
*                                                                               
COMC80H  CLI   ELCODE,X'6A'        SPECIAL REMITTANCE COMMENT?                  
         JE    *+8                                                              
         MVI   WKTBYTE,C'Y'        ERROR OCCURED                                
*                                                                               
COMC80K  LA    R5,REC+33                                                        
         BRAS  RE,NXTELEM          REMOVE PROCESSED COMMENT ELEM                
         BNE   COMC80U                                                          
         GOTO1 VRECUP,DMCB,(1,REC),(R5),0                                       
*                                                                               
COMC80U  CLI   WKTBYTE,C'Y'        ERROR OCCURED?                               
         JE    DIFFUERR                                                         
         B     COMC50              OKAY TO ADD A COMMENT                        
*                                                                               
FIXUCHAR CLI   0(RF),X'4A'         UNPRINTABLE ARACTERS?                        
         JNL   *+12                                                             
FXUCH10  MVI   0(RF),C' '          GET RID OF UNPRINTABLE CHARACTERS            
         J     FXUCH30                                                          
         CLI   0(RF),X'51'         BYPASS ASCII CHARACTERS THAT                 
         JL    FXUCH30             FAILED EBCDIC TRANSLATION                    
         CLI   0(RF),X'59'                                                      
         JNH   FXUCH10                                                          
         CLI   0(RF),X'62'                                                      
         JL    FXUCH30                                                          
         CLI   0(RF),X'69'                                                      
         JNH   FXUCH10                                                          
         CLI   0(RF),X'70'                                                      
         JL    FXUCH30                                                          
         CLI   0(RF),X'78'                                                      
         JNH   FXUCH10                                                          
         CLI   0(RF),X'80'                                                      
         JL    FXUCH30                                                          
         CLI   0(RF),X'80'                                                      
         JE    FXUCH10                                                          
         CLI   0(RF),X'8A'                                                      
         JL    FXUCH30                                                          
         CLI   0(RF),X'90'                                                      
         JNH   FXUCH10                                                          
         CLI   0(RF),X'9A'                                                      
         JL    FXUCH30                                                          
         CLI   0(RF),X'A1'                                                      
         JNH   FXUCH10                                                          
         CLI   0(RF),X'AA'                                                      
         JL    FXUCH30                                                          
         CLI   0(RF),X'BF'                                                      
         JNH   FXUCH10                                                          
*                                                                               
FXUCH30  AHI   RF,1                BUMP TO NEXT CHAR IN COMMENT                 
         JCT   R1,FIXUCHAR                                                      
         BR    RE                                                               
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* BUILD ELEM WITH ONE PACKED DATA FIELD, WORK+0 HAS ELEMENT CODE                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDTPACK  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   ELCODE,WORK         ELEM CODE TO BE PROCESSED                    
         MVI   ERRORNUM+1,INVERR                                                
         LA    R4,0(R2)            POINT TO INPUT                               
*                                                                               
         CLI   DDLINKSW,C'C'       CHANGE INSERTION UPLOAD?                     
         BNE   EDTPAC50                                                         
         LA    R5,REC+33                                                        
         CLI   0(R5),X'20'                                                      
         BE    *+6                                                              
         DC    H'0'                BUY DESCRIPTION ELEM MUST BE THERE!          
*                                                                               
         CLI   ELCODE,X'87'        PAGE VIEW?     (PL5, NO DECIMAL)             
         BE    EDTPAC10                                                         
         CLI   ELCODE,X'88'        CLICK THROUGH? (PL5, NO DECIMAL)             
         BE    EDTPAC10                                                         
         CLI   ELCODE,X'89'        EXTENSION DAY? (PL2, NO DECIMAL)             
         BE    EDTPAC10                                                         
         CLI   ELCODE,X'92'        IMPRESSION?    (PL5, NO DECIMAL)             
         BE    EDTPAC10                                                         
         CLI   ELCODE,X'93'        ACTUAL IMPS?   (PL5, NO DECIMAL)             
         BNE   EDTPAC45                                                         
*                                                                               
EDTPAC10 CLI   OLDCDATA,0          ANYTHING IN OLD DATA?                        
         BNH   EDTPAC40                                                         
         BRAS  RE,NXTELEM                                                       
         JNE   DIFFUERR            NOT FOUND, MIGHT BE REMOVED!                 
         ZIC   R6,OLDCDATA                                                      
         GOTOR VCASHVAL,DMCB,(0,OLDCDATA+1),(X'40',(R6))                        
         CLI   0(R1),X'FF'                                                      
         BE    EDTPACX             EXIT WITH DEFAULT ERROR (INVALID)            
         L     R0,4(R1)                                                         
         CVD   R0,DUB                                                           
         CLI   ELCODE,X'89'        EXTENSION DAY? (PL2, NO DECIMAL)             
         BNE   EDTPAC20                                                         
         CP    2(2,R5),DUB+6(2)    DATA IS STILL SAME AS ORIGINAL?              
         JNE   DIFFUERR            CHANGED SINCE LAST DOWNLOAD                  
         B     EDTPAC50                                                         
*                                                                               
EDTPAC20 CP    2(5,R5),DUB+3(5)    DATA IS STILL SAME AS ORIGINAL?              
         JNE   DIFFUERR            CHANGED SINCE LAST DOWNLOAD                  
         B     EDTPAC50                                                         
*                                                                               
EDTPAC40 BRAS  RE,NXTELEM                                                       
         JE    DIFFUERR            ELEM STATE IS DIFFERENT                      
         B     EDTPAC50            OK TO BUILD AN ADDITIONAL ELEM               
*                                                                               
EDTPAC45 DC    H'0'                NO OTHER ELEM(S) PROCESSED HERE              
*                                                                               
EDTPAC50 LH    R5,HALF                                                          
         LTR   R5,R5                                                            
         BP    EDTPAC65                                                         
*                                                                               
EDTPAC60 MVI   DUB+1,C'X'          SPECIAL DELETE CODE                          
         CLI   SVTRCODE,C'B'       ADDING EMPTY ELEM ON NEW BUY?                
         BE    EDTPACX                                                          
         B     EDTPAC75                                                         
*                                                                               
EDTPAC65 CHI   R5,11               MAX INPUT FORMAT: 123,456,789                
         BH    EDTPACX                                                          
*                                                                               
         LH    R0,HALF             LENGTH OF DATA                               
         LA    R5,0(R2)            POINT TO INPUT (NO OVERHEAD)                 
*                                                                               
         CLC   =C'DEL',0(R5)       SEE IF REMOVING ELEM                         
         BE    EDTPAC60            NO NEED TO VALIDATE FURTHER                  
*                                                                               
         GOTOR VCASHVAL,DMCB,(0,(R5)),(X'40',(R0))                              
         CLI   0(R1),X'FF'                                                      
         BE    EDTPACX                                                          
         L     R0,4(R1)                                                         
         CVD   R0,DUB                                                           
         CP    DUB,=P'0'                                                        
         BL    EDTPACX                                                          
         BNE   EDTPAC70                                                         
         CLI   WORK,X'87'          PAGE VIEW ALLOWS ZERO                        
         BE    EDTPAC75                                                         
         CLI   WORK,X'88'          CLICK THRU ALLOWS ZERO                       
         BE    EDTPAC75                                                         
         CLI   WORK,X'92'          IMPS OR EIMPS ALLOWS ZERO                    
         BE    EDTPAC75                                                         
         CLI   WORK,X'93'          AIMPS ALLOWS ZERO ALSO                       
         BE    EDTPAC75                                                         
         B     EDTPAC60            ZERO ENTERED - SET SPECIAL DEL CODE          
*                                                                               
EDTPAC70 CP    DUB,=P'999999999'   MAX IS 999,999,999 FOR PL5                   
         BH    EDTPACX                                                          
*                                                                               
         CLI   WORK,X'89'          SEE IF EXTENSION DAYS                        
         BNE   EDTPAC75                                                         
         CP    DUB,=P'999'         MAX IS 999                                   
         BH    EDTPACX                                                          
*                                                                               
EDTPAC75 LA    R5,NEWREC+33                                                     
         MVC   ELCODE,WORK                                                      
         BRAS  RE,NXTELEM                                                       
         BNE   *+8                                                              
         J     INVFLDER            ALREADY HAVE ERROR                           
*                                                                               
         XC    WKBUYELM,WKBUYELM                                                
         MVC   WKBUYELM+00(01),WORK                                             
         MVI   WKBUYELM+01,X'07'                                                
*                                                                               
         CLI   WORK,X'89'          EXTENSION DAYS ELEM?                         
         BNE   EDTPAC80                                                         
         MVI   WKBUYELM+01,X'04'   LENGTH IS 4                                  
         CLI   DUB+1,C'X'          SPECIAL CODE PRESENT?                        
         BNE   *+14                                                             
         MVC   WKBUYELM+02(01),DUB+1                                            
         B     EDTPAC85                                                         
         ZAP   WKBUYELM+02(02),DUB+6(2)                                         
         B     EDTPAC85                                                         
*                                                                               
EDTPAC80 CLI   DUB+1,C'X'          SPECIAL CODE PRESENT?                        
         BE    *+14                                                             
         ZAP   WKBUYELM+02(05),DUB+3(5)                                         
         B     EDTPAC85                                                         
         MVC   WKBUYELM+02(01),DUB+1                                            
*                                                                               
EDTPAC85 BRAS  RE,ADDBUYEL                                                      
*                                                                               
         J     XCERRNUM                                                         
*                                                                               
EDTPACX  J     EXIT                                                             
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CHKNTRA  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         OC    PBDJOB,PBDJOB       ADCODE PRESENT?                              
         BZ    *+12                                                             
         MVI   ERRORNUM+1,NTRAFERR CANNOT CHANGE "NO TRAFFIC" STATUS            
         B     CHKNTERR                                                         
*                                                                               
         MVI   ERRORNUM+1,INVERR   INIT TO INVALID INPUT ERROR                  
         LA    R4,0(R2)            POINT TO INPUT                               
         LH    R5,HALF             DATA LENGTH                                  
*                                                                               
         CHI   R5,0                ANY INPUTS?                                  
         BNE   *+8                                                              
         B     CHKNTERR                                                         
*                                                                               
         CHI   R5,3                                                             
         BNE   *+18                                                             
         CLC   =C'YES',0(R4)       TRAFF=YES?                                   
         BE    CHKNT50                                                          
         B     CHKNTERR                                                         
*                                                                               
         CHI   R5,2                                                             
         BNE   *+18                                                             
         CLC   =C'NO',0(R4)        TRAFF=NO?                                    
         BE    CHKNT50                                                          
         B     CHKNTERR                                                         
*                                                                               
         CHI   R5,1                                                             
         BNE   CHKNTERR            NO OTHER VALID LENGTH                        
         CLI   0(R4),C'Y'          TRAFF=Y?                                     
         BE    CHKNT50                                                          
         CLI   0(R4),C'N'          TRAFF=N?                                     
         BE    CHKNT50                                                          
         B     CHKNTERR                                                         
*                                                                               
CHKNT50  CLI   0(R4),C'Y'          TRAFFIC STATUS IS YES?                       
         BNE   *+12                                                             
         NI    PBDSTAT,X'FF'-X'20'                                              
         B     CHKNTRAX                                                         
         OI    PBDSTAT,X'20'       STATUS IS NO TRAFFIC                         
*                                                                               
CHKNTRAX J     XCERRNUM                                                         
*                                                                               
CHKNTERR J     EXIT                                                             
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CHKSITEL NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   ERRORNUM+1,INVERR   INIT TO INVALID INPUT ERROR                  
*                                                                               
         CLI   BUYMD,C'I'          INTERACTIVE?                                 
         JE    CKSIL12                                                          
         CLI   BUYMD,C'L'          SOCIAL?                                      
         JE    CKSIL12                                                          
         CLI   BUYMD,C'B'          MOBILE?                                      
         JE    CKSIL12                                                          
         CLI   BUYMD,C'D'          DIGITAL AUDIO?                               
         JE    CKSIL12                                                          
         CLI   BUYMD,C'V'          NATIONAL VIDEO (NVIDEO)?                     
         JE    CKSIL12                                                          
         CLI   BUYMD,C'W'          LOCAL VIDEO (LVIDEO)?                        
         JE    CKSIL12                                                          
*                                                                               
         J     CKSILERR            ONLY ALLOWED FOR I/L/B/V/W                   
*                                                                               
CKSIL12  MVI   ELCODE,X'98'        SITE LOCATION ELEM CODE                      
*                                                                               
         CLI   DDLINKSW,C'C'       CHANGE MODE?                                 
         BNE   CKSIL50                                                          
         LA    R5,REC+33                                                        
         CLI   0(R5),X'20'         FIRST BUY RECORD ELEMFOUND?                  
         BE    *+6                                                              
         DC    H'0'                NOT A BUY RECORD!                            
*                                                                               
         CLI   OLDCDATA,0          ANYTHING IN OLD DATA?                        
         BH    *+16                                                             
         BRAS  RE,NXTELEM                                                       
         JE    DIFFUERR            ELEM STATE IS DIFFERENT                      
         B     CKSIL50             OK TO ADD SITE LOCATION                      
*                                                                               
         BRAS  RE,NXTELEM                                                       
         JNE   DIFFUERR            SITE LOCATION ELEM MIGHT BE REMOVED!         
*                                                                               
         ZIC   RE,OLDCDATA         PREPARE TO COMPARE ORIGINAL WITH REC         
         CHI   RE,0                                                             
         BH    *+6                                                              
         DC    H'0'                BAD LENGTH                                   
         CHI   RE,20                                                            
         BH    CKSILERR            MAX INPUT LENGTH IS 20!                      
         MVC   WKTELEM,SPACES                                                   
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WKTELEM(0),OLDCDATA+1                                            
         CLC   2(20,R5),WKTELEM    OLD DATA IS SAME AS THAT ON REC?             
         JNE   DIFFUERR                                                         
*                                                                               
CKSIL50  LH    RE,HALF             OKAY TO BUILD SITE LOCATION ELEM             
         CHI   RE,20               GREATER THAN MAX OF 20 CHARACTERS?           
         BH    CKSILERR                                                         
*                                                                               
         LA    R5,NEWREC+33                                                     
         MVI   ELCODE,X'98'                                                     
         BRAS  RE,NXTELEM                                                       
         BE    CKSILERR            CANNOT HAVE SITE LOCATION ELEM YET!          
         LA    R5,NEWREC+33                                                     
         MVI   ELCODE,X'FF'                                                     
         BRAS  RE,NXTELEM          POINT TO END OF RECORD                       
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    WKBUYELM,WKBUYELM                                                
         MVI   WKBUYELM+00,X'98'                                                
         MVI   WKBUYELM+01,22                                                   
         MVC   WKBUYELM+02(L'PISITE),SPACES                                     
         LH    R1,HALF                                                          
         CHI   R1,0                ANY INPUTS?                                  
         JH    *+12                                                             
         MVI   WKBUYELM+02,X'FF'   SPECIAL DELETION CODE                        
         J     CKSIL80                                                          
*                                                                               
         BCTR  R1,0                PREPARE FOR EX INSTRUCTION                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WKBUYELM+02(0),0(R2)                                             
*                                                                               
CKSIL80  BRAS  RE,ADDBUYEL                                                      
*                                                                               
CKSILX   J     XCERRNUM                                                         
*                                                                               
CKSILERR J     EXIT                                                             
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDTIS00  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   ERRORNUM+1,INVERR   INIT TO INVALID INPUT ERROR                  
         MVI   ELCODE,X'A6'        ISSUE NAME ELEM CODE                         
         XC    WKBUYELM,WKBUYELM   INIT ISSUE NAME ELEM                         
         MVI   WKBUYELM+00,X'A6'                                                
         MVI   WKBUYELM+01,13                                                   
         MVC   WKBUYELM+02(11),SPACES                                           
*                                                                               
         BRAS  RE,CKCHGMOD                                                      
         BNE   EDTIS50                                                          
*                                                                               
         CLI   OLDCDATA,0          ANYTHING IN OLD DATA?                        
         BH    *+16                                                             
         BRAS  RE,NXTELEM                                                       
         JE    DIFFUERR            ELEM STATE IS DIFFERENT                      
         B     EDTIS50             OK TO ADD ISSUE NAME                         
*                                                                               
         BRAS  RE,NXTELEM                                                       
         JNE   DIFFUERR            ISSUE NAME ELEM IS REMOVED                   
*                                                                               
         ZIC   RE,OLDCDATA         PREPARE TO COMPARE ORIGINAL WITH REC         
         CHI   RE,0                                                             
         BH    *+6                                                              
         DC    H'0'                BAD LENGTH                                   
         CHI   RE,11                                                            
         BH    EDTISERR            MAX INPUT LENGTH IS 11!                      
         MVC   WKTELEM,SPACES                                                   
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WKTELEM(0),OLDCDATA+1                                            
         CLC   2(11,R5),WKTELEM    OLD DATA IS SAME AS THAT ON REC?             
         JNE   DIFFUERR                                                         
*                                                                               
EDTIS50  LA    R5,NEWREC+33                                                     
         MVI   ELCODE,X'A6'                                                     
         BRAS  RE,NXTELEM                                                       
         BE    EDTISERR            CANNOT HAVE ISSUE NAME ELEM YET!             
         LA    R5,NEWREC+33                                                     
         MVI   ELCODE,X'FF'                                                     
         BRAS  RE,NXTELEM          POINT TO END OF RECORD                       
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LH    RE,HALF             OKAY TO BUILD ISSUE NAME ELEM                
         LTR   RE,RE               ANY INPUTS?                                  
         BNP   EDTIS55                                                          
         CHI   RE,11               GREATER THAN MAX OF 11 CHARACTERS?           
         BH    EDTISERR                                                         
         CHI   RE,04               REMOVING ISSUE NAME?                         
         BNE   *+14                                                             
         CLC   =C'NONE',0(R2)      NONE?                                        
         BE    EDTIS55                                                          
*                                                                               
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WKBUYELM+02(0),0(R2)                                             
         B     EDTIS60             ELEM BUILT, ADD IT                           
*                                                                               
EDTIS55  CLI   SVTRCODE,C'B'       NEW INSERTION?                               
         BE    EDTISERR            CANNOT ADD AN EMPTY ELEM ON NEW INS          
         MVI   WKBUYELM+2,X'FF'    MOVE IN DELETION CODE                        
*                                                                               
EDTIS60  BRAS  RE,ADDBUYEL                                                      
*                                                                               
EDTISX   J     XCERRNUM                                                         
*                                                                               
EDTISERR J     EXIT                                                             
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
TSHEET   NTR1  BASE=*,LABEL=*      EDITING TEARSHEET                            
*                                                                               
         LA    R6,TSHTELEM                                                      
         USING PTSHTEL,R6                                                       
*                                                                               
         MVC   WKTHALF,3(R3)       SAVE MAP CODE                                
*                                                                               
         BRAS  RE,CKCHGMOD         WILL POINT R5 TO REC+33 IF CHG MODE          
         BNE   TSHT05                                                           
*                                                                               
         LH    RE,WKTHALF                                                       
         AHI   RE,-1               MAKE EVEN MAP CODE ODD                       
         STH   RE,WKTHALF                                                       
         OC    TSHTELEM(2),TSHTELEM                                             
         BNZ   TSHT60                                                           
         MVI   ELCODE,X'95'        LOOK UP TEARSHEET ELEM                       
         BRAS  RE,NXTELEM                                                       
         BNE   TSHT10                                                           
         MVC   TSHTELEM,0(R5)      SAVE TEARSHEET ELEM                          
         B     TSHT60                                                           
*                                                                               
TSHT05   CLI   DDLINKSW,C'N'       NEW INS?                                     
         BNE   TSHTX                                                            
*                                                                               
TSHT10   OC    TSHTELEM(2),TSHTELEM                                             
         BNZ   TSHT15                                                           
         MVI   PTSHTEL,PTSHTELQ    TEARSHEET ELEM CODE                          
         MVI   PTSHTLEN,39         ELEM LENGTH                                  
*                                                                               
TSHT15   CLC   WKTHALF,=AL2(D#TSHAPR)                                           
         BNE   TSHT20                                                           
         LH    RE,HALF             DATA LENGTH                                  
         CHI   RE,0                                                             
         BNE   *+12                                                             
         MVI   PTSHSTAT,C' '       NO EVALUATION HAS BEEN MADE                  
         B     TSHT15K                                                          
         CHI   RE,1                                                             
         JNE   INVFLDER                                                         
         CLI   0(R2),C' '          NO STATUS?                                   
         BE    TSHT15H                                                          
         CLI   0(R2),C'A'          APPROVED?                                    
         BE    TSHT15H                                                          
         CLI   0(R2),C'N'          NOT APPROVED?                                
         JNE   INVFLDER                                                         
TSHT15H  MVC   PTSHSTAT,0(R2)                                                   
TSHT15K  OI    CHGIND5,PCHGTSAQ    T/S APPROVED STATUS CHANGED                  
         B     TSHT_OIX                                                         
*                                                                               
TSHT20   CLC   WKTHALF,=AL2(D#TSHSTA)                                           
         BNE   TSHT25                                                           
         MVC   WKTELEM(5),SPACES                                                
         LH    RE,HALF             DATA LENGTH                                  
         CHI   RE,5                                                             
         JH    INVFLDER                                                         
         CHI   RE,0                                                             
         JL    INVFLDER                                                         
         BNH   TSHT20F                                                          
         BCTR  RE,0                FOR EX                                       
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WKTELEM(0),0(R2)    GET INPUT                                    
TSHT20F  LA    RE,5                5 TS STATUSES TO PROCESS                     
         LA    RF,WKTELEM                                                       
TSHT20H  CLI   0(RF),C' '          NOT EVALUATED?                               
         BE    TSHT20L                                                          
         CLI   0(RF),C'N'          TEAR INFO DOES NOT MATCH BUY?                
         BE    TSHT20L                                                          
         CLI   0(RF),C'Y'          TEAR INFO MATCHES BUY?                       
         JNE   INVFLDER                                                         
TSHT20L  LA    RF,1(RF)            CHECK NEXT STATUS                            
         BCT   RE,TSHT20H                                                       
         MVC   PTSHIND1(5),WKTELEM                                              
         B     TSHT_OIX                                                         
*                                                                               
TSHT25   CLC   WKTHALF,=AL2(D#REPROQ)                                           
         BNE   TSHT30                                                           
         LH    RF,HALF             DATA LENGTH                                  
         CHI   RF,0                                                             
         BE    TSHT25H                                                          
         GOTOR VCASHVAL,DMCB,(0,0(R2)),(X'40',(RF))                             
         CLI   0(R1),X'FF'                                                      
         JE    INVFLDER                                                         
         L     RF,4(R1)            1-10 OR NULL                                 
         CHI   RF,0                                                             
         JL    INVFLDER                                                         
         CHI   RF,10                                                            
         JH    INVFLDER                                                         
TSHT25H  STC   RF,PTSHREPO         REP QUALITY (BINARY)                         
         B     TSHT_OIX                                                         
*                                                                               
TSHT30   CLC   WKTHALF,=AL2(D#TSHNOT)                                           
         BNE   TSHT40                                                           
         MVC   PTSHPAGE,SPACES                                                  
         LH    RF,HALF             DATA LENGTH                                  
         CHI   RF,0                                                             
         BE    TSHT30H             NO INPUT, USE SPACES                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   PTSHPAGE(0),0(R2)                                                
TSHT30H  OC    PTSHPAGE(10),SPACES                                              
         B     TSHT_OIX                                                         
         DROP  R6                                                               
*                                                                               
TSHT40   CLC   WKTHALF,=AL2(D#TSHCO1)                                           
         BNE   *+8                                                              
         LA    RF,TSHTC1EL                                                      
         CLC   WKTHALF,=AL2(D#TSHCO2)                                           
         BNE   *+8                                                              
         LA    RF,TSHTC2EL                                                      
         CLC   WKTHALF,=AL2(D#TSHCO3)                                           
         BNE   *+8                                                              
         LA    RF,TSHTC3EL                                                      
         CLC   WKTHALF,=AL2(D#TSHCO4)                                           
         BNE   *+8                                                              
         LA    RF,TSHTC4EL                                                      
*                                                                               
TSHT45   XC    0(68,RF),0(RF)                                                   
         LH    RE,HALF             DATA LENGTH                                  
         CHI   RE,0                                                             
         BNH   TSHT45M                                                          
         MVI   0(RF),X'69'         TEARSHEET COMMENT ELEM CODE                  
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   2(0,RF),0(R2)                                                    
         AHI   RE,2+1              2 FOR OVERHEAD AND 1 FROM EX                 
         STC   RE,1(RF)                                                         
TSHT45M  OI    OTHELMSW,TEARSCOM                                                
         OI    ADBCHGSW,X69CHGD    TS COMMENT IS CHG'D, SW USED LATER           
         B     TSHTX                                                            
*                                                                               
TSHT50   DS    0H                                                               
*                                                                               
TSHT60   LA    R5,REC+33                                                        
         MVI   ELCODE,X'95'        LOOK UP TEARSHEET ELEM                       
         BRAS  RE,NXTELEM                                                       
         BNE   TSHT10              NOT FOUND, ADD ONE                           
*                                                                               
* NOTE THAT X'69' CANNOT EXIST WITHOUT A PRECEDING X'95' ELEM                   
*                                                                               
         USING PTSHTEL,R5                                                       
*                                                                               
         CLC   WKTHALF,=AL2(D#TSHAPR)                                           
         BNE   TSHT65                                                           
         CLI   OLDCDATA+1,0                                                     
         BNE   *+8                                                              
         OI    OLDCDATA+1,C' '                                                  
         CLC   PTSHSTAT,OLDCDATA+1                                              
         JNE   DIFFUERR            CHANGED SINCE LAST DOWNLOAD                  
         B     TSHT10                                                           
*                                                                               
TSHT65   CLC   WKTHALF,=AL2(D#TSHSTA)                                           
         BNE   TSHT70                                                           
         OC    OLDCDATA+1(5),SPACES                                             
         CLC   PTSHIND1(5),OLDCDATA+1                                           
         JNE   DIFFUERR            CHANGED SINCE LAST DOWNLOAD                  
         B     TSHT10                                                           
*                                                                               
TSHT70   CLC   WKTHALF,=AL2(D#REPROQ)                                           
         BNE   TSHT75                                                           
         SR    RF,RF                                                            
         IC    RF,OLDCDATA                                                      
         CHI   RF,0                ANYTHING IN OLD DATA?                        
         BH    TSHT70H                                                          
         OC    PTSHREPO,PTSHREPO                                                
         JNZ   DIFFUERR            CHANGED SINCE LAST DOWNLOAD                  
         B     TSHT10                                                           
TSHT70H  GOTOR VCASHVAL,DMCB,(0,OLDCDATA+1),(X'40',(RF))                        
         CLI   0(R1),X'FF'                                                      
         JE    INVFLDER                                                         
         L     RF,4(R1)                                                         
         STC   RF,WKTELEM          REP QUALITY (BINARY)                         
         CLC   PTSHREPO,WKTELEM                                                 
         JNE   DIFFUERR            CHANGED SINCE LAST DOWNLOAD                  
         B     TSHT10                                                           
*                                                                               
TSHT75   CLC   WKTHALF,=AL2(D#TSHNOT)                                           
         BNE   TSHT80                                                           
         OC    OLDCDATA+1(10),SPACES                                            
         CLC   PTSHPAGE,OLDCDATA+1                                              
         JNE   DIFFUERR            CHANGED SINCE LAST DOWNLOAD                  
         B     TSHT10                                                           
*                                                                               
         DROP  R5                                                               
*                                                                               
TSHT80   LA    R5,REC+33                                                        
         MVI   ELCODE,X'69'        LOOK UP TEARSHEET COMMENT ELEM               
*                                                                               
         CLC   WKTHALF,=AL2(D#TSHCO1)                                           
         BE    TSHT80D                                                          
         CLC   WKTHALF,=AL2(D#TSHCO2)                                           
         BE    TSHT80D                                                          
         CLC   WKTHALF,=AL2(D#TSHCO3)                                           
         BE    TSHT80D                                                          
         CLC   WKTHALF,=AL2(D#TSHCO4)                                           
         BE    TSHT80D                                                          
*                                                                               
         DC    H'0'                NO OTHER MAP CODE AT THIS TIME               
*                                                                               
TSHT80D  CLI   OLDCDATA,0          ANYTHING IN OLD DATA?                        
         BH    *+16                                                             
         BRAS  RE,NXTELEM                                                       
         JE    DIFFUERR            ELEM STATE IS DIFFERENT                      
         B     TSHT10              OKAY TO ADD TEARSHEET COMMENT                
*                                                                               
         BRAS  RE,NXTELEM                                                       
         JNE   DIFFUERR            TCOM NOT FOUND, MIGHT BE REMOVED!            
*                                                                               
         MVI   WKTBYTE,0                                                        
         SR    RE,RE               PREPARE TO COMPARE ORIGINAL WITH REC         
         IC    RE,OLDCDATA                                                      
         CHI   RE,0                                                             
         BH    *+6                                                              
         DC    H'0'                BAD LENGTH                                   
         ZIC   RF,1(R5)                                                         
         AHI   RF,-2               MINUS 2 FROM ELEM OVERHEAD                   
         CR    RE,RF               LENGTH OF ORIGINAL AND REC ARE SAME?         
         BNE   TSHT80H             SET ERROR SWITCH                             
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   2(0,R5),OLDCDATA+1  OLD DATA IS SAME AS THAT ON REC?             
         BE    *+8                                                              
*                                                                               
TSHT80H  MVI   WKTBYTE,C'Y'        ERROR OCCURED                                
*                                                                               
         LH    RE,HALF             NEW DATA LENGTH                              
         SR    RF,RF                                                            
         IC    RF,1(R5)                                                         
         CR    RE,RF               COMMENT HAS BEEN CHANGED?                    
         BNE   TSHT80P                                                          
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R2),2(R5)       COMMENT HAS BEEN CHANGED?                    
         BE    *+8                                                              
TSHT80P  OI    TSHTELEM+(PTSHCIN2-PTSHTEL),X'02'                                
*                                                                               
         GOTO1 VRECUP,DMCB,(1,REC),(R5),0                                       
*                                                                               
TSHT80U  CLI   WKTBYTE,C'Y'        ERROR OCCURED?                               
         JE    DIFFUERR                                                         
         B     TSHT10              OKAY TO ADD TEARSHEET COMMENT                
*                                                                               
TSHT_OIX OI    OTHELMSW,TEARSHET                                                
TSHTX    J     EXIT                                                             
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
INTRCON  NTR1  BASE=*,LABEL=*      CONSTRUCT INTERNET CONTRACT ELEM             
*                                                                               
         MVC   WKTHALF,3(R3)       SAVE MAP CODE                                
         LA    R5,NEWREC+33                                                     
         MVI   ELCODE,X'97'        INTERNET CONTRACT ELEM CODE                  
         BRAS  RE,NXTELEM                                                       
         JE    INVFLDER            CANNOT HAVE ONE YET                          
*                                                                               
         CLI   DDLINKSW,C'C'       CHANGE INS?                                  
         JNE   INTC50                                                           
         LH    RE,WKTHALF                                                       
         AHI   RE,-1                                                            
         STH   RE,WKTHALF                                                       
         LA    R5,REC+33                                                        
         CLI   0(R5),X'20'                                                      
         BE    *+6                                                              
         DC    H'0'                BUY DESCRIPTION ELEM MUST BE THERE!          
         ZIC   R6,OLDCDATA                                                      
         CHI   R6,0                ANYTHING IN OLD DATA?                        
         BH    INTC10                                                           
         MVI   ELCODE,X'97'                                                     
         BRAS  RE,NXTELEM                                                       
         JE    DIFFUERR            ELEM STATE IS DIFFERENT                      
         B     INTC50              OK TO ADD AN INTERNET CONTRACT ELEM          
*                                                                               
INTC10   BRAS  RE,NXTELEM                                                       
         JNE   DIFFUERR            ELEM STATE IS DIFFERENT                      
*                                                                               
         OC    INTCONEL(2),INTCONEL                                             
         BNZ   *+10                                                             
         MVC   INTCONEL,0(R5)      SAVE ELEM ON CHANGE MODE                     
*                                                                               
         USING PICONEL,R5                                                       
*                                                                               
         CLC   WKTHALF,=AL2(D#ICNUM)                                            
         BNE   INTC20                                                           
         MVC   WKTELEM(8),SPACES   INIT TO SPACES                               
         CHI   R6,8                                                             
         JH    INVFLDER            INVALID LENGTH (SHOULD NOT HAPPEN)           
         LR    RE,R6                                                            
         AHI   RE,-1               FOR EX                                       
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WKTELEM(0),OLDCDATA+1                                            
         CLC   PICCONN,WKTELEM     DATA IS STILL SAME AS ORIGINAL?              
         JNE   DIFFUERR                                                         
         B     INTC50              OK TO ADD AN INTERNET CONTRACT ELEM          
*                                                                               
INTC20   CLC   WKTHALF,=AL2(D#TOTRAT)                                           
         BNE   INTC25                                                           
         GOTOR VCASHVAL,DMCB,(2,OLDCDATA+1),(R6)                                
         CLI   0(R1),X'FF'                                                      
         JE    INVFLDER                                                         
         L     R0,4(R1)                                                         
         CVD   R0,DUB                                                           
         CP    PICRATE,DUB+3(5)    DATA IS STILL SAME AS ORIGINAL?              
         JNE   DIFFUERR                                                         
         B     INTC50              OK TO ADD AN INTERNET CONTRACT ELEM          
*                                                                               
INTC25   CLC   WKTHALF,=AL2(D#TOTIMP)                                           
         BNE   INTC30                                                           
         GOTOR VCASHVAL,DMCB,(0,OLDCDATA+1),(X'40',(R6))                        
         CLI   0(R1),X'FF'                                                      
         JE    INVFLDER                                                         
         L     R0,4(R1)                                                         
         CVD   R0,DUB                                                           
         CP    PICIMPS,DUB+3(5)    DATA IS STILL SAME AS ORIGINAL?              
         JNE   DIFFUERR                                                         
         B     INTC50              OK TO ADD AN INTERNET CONTRACT ELEM          
*                                                                               
INTC30   CLC   WKTHALF,=AL2(D#TOTCPM)                                           
         BNE   INTC35                                                           
         GOTOR VCASHVAL,DMCB,(2,OLDCDATA+1),(R6)                                
         CLI   0(R1),X'FF'                                                      
         JE    INVFLDER                                                         
         L     R0,4(R1)                                                         
         CVD   R0,DUB                                                           
         CP    PICCPM,DUB+3(5)     DATA IS STILL SAME AS ORIGINAL?              
         JNE   DIFFUERR                                                         
         B     INTC50              OK TO ADD AN INTERNET CONTRACT ELEM          
*                                                                               
INTC35   CLC   WKTHALF,=AL2(D#ICINUM)                                           
         JNE   INVFLDER                                                         
         GOTOR VCASHVAL,DMCB,(0,OLDCDATA+1),(X'40',(R6))                        
         CLI   0(R1),X'FF'                                                      
         JE    INVFLDER                                                         
         L     R0,4(R1)                                                         
         CVD   R0,DUB                                                           
         CP    PICINS,DUB+5(3)     DATA IS STILL SAME AS ORIGINAL?              
         JNE   DIFFUERR                                                         
*                                                                               
         DROP  R5                                                               
*                                                                               
INTC50   LA    R4,INTCONEL                                                      
         USING PICONEL,R4                                                       
*                                                                               
         OC    INTCONEL(2),INTCONEL                                             
         BNZ   *+12                                                             
         MVI   INTCONEL+0,X'97'    INTERNET CONTRACT ELEM CODE                  
         MVI   INTCONEL+1,36       ELEM LENGTH                                  
*                                                                               
         CLC   WKTHALF,=AL2(D#ICNUM)                                            
         BNE   INTC55                                                           
         LH    RE,HALF                                                          
         CHI   RE,10                                                            
         JH    INVFLDER            MAX INPUT LENGTH IS 10                       
         CHI   RE,0                                                             
         JL    INVFLDER                                                         
         BE    INTCX                                                            
         BCTR  RE,0                FOR EX                                       
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   PICCONN(0),0(R2)    LEFT ALIGNED                                 
         B     INTC_OIX                                                         
*                                                                               
INTC55   CLC   WKTHALF,=AL2(D#TOTRAT)                                           
         BNE   INTC60                                                           
         LH    R6,HALF                                                          
         CLI   0(R2),C'N'          NET AMOUNT IS ENTERED?                       
         BNE   *+16                                                             
         MVI   PICRIND,C'N'        RATE INDICATOR IS NET                        
         LA    R2,1(R2)            BUMP OVER CHAR "N"                           
         AHI   R6,-1               ADJUST DATA LENGTH                           
         GOTOR VCASHVAL,DMCB,(2,0(R2)),(R6)                                     
         CLI   0(R1),X'FF'                                                      
         JE    INVFLDER                                                         
         L     R0,4(R1)                                                         
         CVD   R0,DUB                                                           
         ZAP   PICRATE,DUB+3(5)                                                 
         B     INTC_OIX                                                         
*                                                                               
INTC60   CLC   WKTHALF,=AL2(D#TOTIMP)                                           
         BNE   INTC65                                                           
         LH    R6,HALF                                                          
         GOTOR VCASHVAL,DMCB,(0,0(R2)),(X'40',(R6))                             
         CLI   0(R1),X'FF'                                                      
         JE    INVFLDER                                                         
         L     R0,4(R1)                                                         
         CVD   R0,DUB                                                           
         ZAP   PICIMPS,DUB+3(5)                                                 
         B     INTC_OIX                                                         
*                                                                               
INTC65   CLC   WKTHALF,=AL2(D#TOTCPM)                                           
         BNE   INTC70                                                           
         LH    R6,HALF                                                          
         GOTOR VCASHVAL,DMCB,(2,0(R2)),(R6)                                     
         CLI   0(R1),X'FF'                                                      
         JE    INVFLDER                                                         
         L     R0,4(R1)                                                         
         CVD   R0,DUB                                                           
         ZAP   PICCPM,DUB+3(5)                                                  
         B     INTC_OIX                                                         
*                                                                               
INTC70   CLC   WKTHALF,=AL2(D#ICINUM)                                           
         JNE   INVFLDER                                                         
         LH    R6,HALF                                                          
         GOTOR VCASHVAL,DMCB,(0,0(R2)),(X'40',(R6))                             
         CLI   0(R1),X'FF'                                                      
         JE    INVFLDER                                                         
         L     R0,4(R1)                                                         
         CVD   R0,DUB                                                           
         ZAP   PICINS,DUB+5(3)                                                  
         B     INTC_OIX                                                         
*                                                                               
INTC_OIX OI    OTHELMSW,INTRNCON   INTERNET CONTRACT DATA EDITED                
INTCX    J     XCERRNUM                                                         
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
INVMSTAS NTR1  BASE=*,LABEL=*      INVOICE MATCHING STATUSES ELEM               
*                                                                               
         MVC   WKTHALF,3(R3)       SAVE MAP CODE                                
         LA    R5,NEWREC+33                                                     
         MVI   ELCODE,PBMATELQ     INVOICE MATCHING STATUSES ELEM CODE          
         BRAS  RE,NXTELEM                                                       
         JE    INVFLDER            CANNOT HAVE ONE YET                          
*                                                                               
         CLI   DDLINKSW,C'C'       CHANGE INS?                                  
         BNE   INVM50                                                           
         LH    RE,WKTHALF                                                       
         AHI   RE,-1                                                            
         STH   RE,WKTHALF                                                       
         LA    R5,REC+33                                                        
         CLI   0(R5),X'20'                                                      
         BE    *+6                                                              
         DC    H'0'                BUY DESCRIPTION ELEM MUST BE THERE!          
         ZIC   R6,OLDCDATA                                                      
         MVI   ELCODE,PBMATELQ                                                  
         BRAS  RE,NXTELEM          NEED TO COMPARE WITH CURRENT RECORD?         
         BNE   INVM50              NO, GO VALIDATE IT                           
*                                                                               
         OC    INVMSTEL(2),INVMSTEL                                             
         BNZ   *+10                                                             
         MVC   INVMSTEL,0(R5)      SAVE ELEM ON CHANGE MODE                     
*                                                                               
         USING PBMATELD,R5                                                      
*                                                                               
         CLC   WKTHALF,=AL2(D#MATSTA)                                           
         BNE   INVM20                                                           
         CHI   R6,1                                                             
         BNE   *+18                                                             
         CLC   PBMTSTAT,OLDCDATA+1                                              
         JNE   DIFFUERR                                                         
         B     INVM50              OK TO ADD AN INVOICE MAT STATUS ELEM         
         CLI   PBMTSTAT,0                                                       
         JNE   DIFFUERR                                                         
         B     INVM50              OK TO ADD AN INVOICE MAT STATUS ELEM         
*                                                                               
INVM20   CLC   WKTHALF,=AL2(D#DISSTA)                                           
         BNE   INVM25                                                           
         CHI   R6,1                                                             
         BNE   *+18                                                             
         CLC   PBMTDSTA,OLDCDATA+1                                              
         JNE   DIFFUERR                                                         
         B     INVM50              OK TO ADD AN INVOICE MAT STATUS ELEM         
         CLI   PBMTDSTA,0                                                       
         JNE   DIFFUERR                                                         
         B     INVM50              OK TO ADD AN INVOICE MAT STATUS ELEM         
*                                                                               
INVM25   DS    0H                  FOR FUTURE MATCHING STATUSES                 
*                                                                               
         B     INVM50              OK TO ADD AN INVOICE MAT STATUS ELEM         
         DROP  R5                                                               
*                                                                               
INVM50   LA    R4,INVMSTEL                                                      
         USING PBMATELD,R4                                                      
*                                                                               
         OC    INVMSTEL(2),INVMSTEL                                             
         BNZ   *+12                                                             
         MVI   PBMATELM,PBMATELQ   MATCHING STATUSES ELEM CODE                  
         MVI   PBMATLEN,PBMTLENQ   ELEM LENGTH                                  
*                                                                               
         CLC   WKTHALF,=AL2(D#MATSTA)                                           
         BNE   INVM54                                                           
         MVI   PBMTSTAT,PBMTSNIQ   INIT STATUS TO NO INVOICE                    
         LH    RE,HALF                                                          
         CHI   RE,0                                                             
         BE    INVM50U                                                          
         CHI   RE,1                                                             
         JNE   INVFLDER            INPUT LENGTH IS 1                            
         CLI   0(R2),PBMTSPDQ      PENDING?                                     
         BE    INVM50H                                                          
         CLI   0(R2),PBMTSMTQ      MATCHED?                                     
         BE    INVM50H                                                          
         CLI   0(R2),PBMTSDSQ      DISCREPANT?                                  
         JNE   INVFLDER                                                         
INVM50H  MVC   PBMTSTAT,0(R2)                                                   
         LA    R5,NEWREC+33                                                     
         USING PBDELEM,R5                                                       
         NI    PBDSTAT,X'FF'-X'40'                                              
         OI    ABCBITSW,MATSTACQ   INDICATE MATCH STAT BIT CHG'D                
         CLI   PBMTSTAT,PBMTSMTQ   MATCHED?                                     
         BNE   INVM50U                                                          
         OI    PBDSTAT,X'40'       TURN ON MATCH STATUS BIT                     
INVM50U  B     INVM_OIX                                                         
         DROP  R5                                                               
*                                                                               
INVM54   CLC   WKTHALF,=AL2(D#DISSTA)                                           
         BNE   INVM58                                                           
         MVI   PBMTDSTA,PBMTDNAQ   INIT STATUS TO NOT APPLICABLE                
         LH    RE,HALF                                                          
         CHI   RE,0                                                             
         BE    INVM54U                                                          
         CHI   RE,1                                                             
         JNE   INVFLDER            INPUT LENGTH IS 1                            
         CLI   0(R2),PBMTDNRQ      NEEDS REVIEW?                                
         BE    INVM54H                                                          
         CLI   0(R2),PBMTDRVQ      REVIEWING?                                   
         BE    INVM54H                                                          
         CLI   0(R2),PBMTDRSQ      RESOLVED?                                    
         JNE   INVFLDER                                                         
INVM54H  MVC   PBMTDSTA,0(R2)                                                   
INVM54U  B     INVM_OIX                                                         
*                                                                               
INVM58   B     INVMX               VALIDATED, NOW EXIT                          
*                                                                               
INVM_OIX OI    OTHELMSW,INVMSTSQ   MATCHING STATUSES DATA EDITED                
         OI    ADBCHGSW,X52CHGD    INVOICE MATCHING STATUS IS CHANGED           
INVMX    J     XCERRNUM                                                         
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
INVTRSTA NTR1  BASE=*,LABEL=*      INVOICE TEARSHEET RECEIVED STATUS            
*                                                                               
         MVC   WKTHALF,3(R3)       SAVE MAP CODE                                
*                                                                               
         TM    ABUPLDSW,IDSKUPLQ   PRISMA UPLOAD?                               
         JNZ   INVTR50                                                          
         BRAS  RE,CKCHGMOD         WILL POINT R5 TO REC+33 IF CHG MODE          
         BNE   INVTR50                                                          
         ZIC   R6,OLDCDATA                                                      
         CHI   R6,0                ANYTHING IN OLD DATA?                        
         BH    *+8                                                              
         B     INVTR50             OK TO ADD TEARSHEET RECEIVED STATUS          
*                                                                               
         CHI   R6,1                                                             
         JNE   INVFLDER            INVALID LENGTH (SHOULD NOT HAPPEN)           
         MVI   WKTFULL,0                                                        
         TM    PBDSTAT-PBDELEM(R5),X'10'                                        
         BZ    *+8                                                              
         MVI   WKTFULL,C'Y'                                                     
         CLC   WKTFULL(01),OLDCDATA+1                                           
         JNE   DIFFUERR                                                         
*                                                                               
INVTR50  LA    R5,NEWREC+33                                                     
         CLI   0(R5),X'20'                                                      
         BE    *+6                                                              
         DC    H'0'                BUY DESCRIPTION ELEM MUST BE THERE!          
*                                                                               
         NI    PBDSTAT-PBDELEM(R5),X'FF'-X'10'                                  
         OI    ABCBITSW,TSRSTACQ   INDICATE TS RECEIVED STAT BIT CHG'D          
         LH    RE,HALF                                                          
         CHI   RE,0                                                             
         BE    INVTRX                                                           
         CHI   RE,1                                                             
         JNE   INVFLDER            INPUT LENGTH IS 1                            
         CLI   0(R2),C'Y'          YES?                                         
         JNE   INVFLDER                                                         
         OI    PBDSTAT-PBDELEM(R5),X'10'                                        
*                                                                               
INVTRX   J     XCERRNUM                                                         
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
C2FACTOR NTR1  BASE=*,LABEL=*      COS2 FACTOR                                  
*                                                                               
         MVC   WKTHALF,3(R3)       SAVE MAP CODE                                
*                                                                               
         BRAS  RE,CKCHGMOD         WILL POINT R5 TO REC+33 IF CHG MODE          
         BNE   C2F_50                                                           
*                                                                               
         MVC   ERRORNUM,=AL2(NOFRZCER)                                          
         BRAS  RE,CKCLTFRZ         CLIENT FROZEN OPTION FOUND?                  
         JNE   EXIT                                                             
         XC    ERRORNUM,ERRORNUM                                                
*                                                                               
         ZIC   R6,OLDCDATA                                                      
         CHI   R6,0                ANYTHING IN OLD DATA?                        
         BH    *+8                                                              
         B     C2F_50              OK TO ADD COS2 FACTOR                        
*                                                                               
         SR    RF,RF                                                            
         IC    RF,OLDCDATA                                                      
         GOTOR VCASHVAL,DMCB,(6,OLDCDATA+1),(RF)                                
         CLI   0(R1),X'FF'                                                      
         JE    INVFLDER                                                         
         L     RF,4(R1)                                                         
         CVD   RF,DUB                                                           
         MVI   ELCODE,X'91'        COS2 FACTOR ELEM CODE                        
         BRAS  RE,NXTELEM                                                       
         JNE   DIFFUERR            STATE OF DATA IS DIFFERENT                   
         USING PCOS2FEL,R5                                                      
         CP    PCOS2FAC,DUB+3(5)                                                
         JNE   DIFFUERR            STATE OF DATA IS DIFFERENT                   
*                                                                               
C2F_50   SR    RF,RF                                                            
         ICM   RF,3,1(R3)                                                       
         SHI   RF,6                                                             
         GOTOR VCASHVAL,DMCB,(6,6(R3)),(RF)                                     
         CLI   0(R1),X'FF'                                                      
         JE    INVFLDER                                                         
         L     RF,4(R1)                                                         
         CVD   RF,DUB                                                           
         ZAP   SVE2FAC,DUB+3(5)                                                 
         OI    GENBYSW1,C2FOVRDQ   FACTOR IS OVERRIDDEN IN BUY                  
         OI    GENBYSW1,C2FC2$MQ   FACTOR IS VALIDATED (PBT MAP CODE)           
*                                                                               
C2F_X    J     XCERRNUM                                                         
*                                                                               
         DROP  RB,R5                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PURORDER NTR1  BASE=*,LABEL=*      PURCHASE ORDER #                             
*                                                                               
         XC    ERRORNUM,ERRORNUM                                                
*                                                                               
         CLI   DDLINKSW,C'F'       DRAFT INSERTION UPLOAD?                      
         BE    PO#_X                                                            
         CLI   LKDRFTSW,C'F'       DRAFT MODE UPLOAD?                           
         BE    PO#_X                                                            
*                                                                               
         BRAS  RE,CKBILPO#         CHECK BILLED WITHOUT PO#                     
         JE    *+12                                                             
         MVI   ERRORNUM+1,INVERR                                                
         J     PO#_X                                                            
*                                                                               
         CLI   SVPARM_3,VAL_PO#Q   VALIDATING PO# REQUEST FROM BUY?             
         BNE   PO#_05                                                           
         OC    SVPARM_3+1(3),SVPARM_3+1                                         
         BZ    PO#_08              LOOK UP PO# FOR NEW INSERTIONS               
         L     RE,SVPARM_3         ADDRESS OF OVERRIDING PO#                    
         MVC   WKPO#PNO(PO#DMXLQ),0(RE)                                         
         OC    WKPO#PNO(PO#DMXLQ),WKPO#PNO                                      
         BNZ   PO#_40                                                           
         LA    R5,REC+33                                                        
         MVI   ELCODE,PBYPOELQ                                                  
         BRAS  RE,NXTELEM          PO# ELEM FOUND?                              
         BNE   PO#_32E             MISSING PURCHASE ORDER#                      
         USING PBYPOELD,R5                                                      
         MVC   WKPO#SQ#,PBYPOSQ#   PURCHASE ORDER SEQUENCE #                    
         OI    WKPO#SW1,PO#1REMQ                                                
         B     PO#_40                                                           
*                                                                               
PO#_05   TM    OTHELMSW,PURORD#Q   PO# MAP CODES FOUND IN UPLOAD REQ?           
         BNZ   PO#_20                                                           
         MVC   SVMAPCOD,=AL2(D#PO#OLD)                                          
*                                                                               
PO#_08   XC    WKPO#PRD,WKPO#PRD                                                
         XC    WKPO#PNO,WKPO#PNO                                                
         LA    R5,NEWREC+33                                                     
         BRAS  RE,CKCHGMOD                                                      
         CLC   =C'ZZZ',BUYPR       POL BUY?                                     
         BE    PO#_10                                                           
         BRAS  RE,GETPOREC         GET PURCHASE ORDER RECORD                    
         BE    *+14                                                             
         XC    ERRORNUM,ERRORNUM   OK IF NOT FOUND FOR LOOK UP                  
         B     PO#_X                                                            
         BRAS  RE,PO#_PROC         PROCESS PURCHASE ORDER # ELEMS               
         B     PO#_X                                                            
*                                                                               
PO#_10   MVI   ELCODE,X'21'        PRD ELEMS FOR ZZZ BUYS                       
         BRAS  RE,NXTELEM                                                       
         BNE   PO#_X                                                            
         USING PPRELEM,R5                                                       
         MVC   WKPO#PRD,PPRCODE                                                 
         BRAS  RE,GETPOREC         GET PURCHASE ORDER RECORD                    
         BE    *+14                                                             
         XC    ERRORNUM,ERRORNUM   OK IF NOT FOUND FOR LOOK UP                  
         B     PO#_10              NOT FOUND, DO NEXT PRD CODE                  
         BRAS  RE,PO#_PROC         PROCESS PURCHASE ORDER # ELEMS               
         OC    ERRORNUM,ERRORNUM                                                
         BZ    PO#_10              NO ERROR, DO NEXT PRD CODE                   
         B     PO#_X                                                            
         DROP  R5                                                               
*                                                                               
PO#_20   MVC   WKTHALF,3(R3)       SAVE MAP CODE                                
         CLC   =AL2(D#PO#PRD),3(R3)                                             
         BNE   PO#_30                                                           
         XC    WKPO#PRD,WKPO#PRD                                                
         CLC   =C'ZZZ',BUYPR       POL BUY?                                     
         BNE   PO#_X                                                            
         MVC   WKPO#PRD,6(R3)                                                   
         OC    WKPO#PRD,SPACES                                                  
         B     PO#_X                                                            
*                                                                               
PO#_30   XC    WKPO#PNO,WKPO#PNO                                                
         SR    RE,RE                                                            
         ICM   RE,3,1(R3)                                                       
         SHI   RE,6                                                             
         CHI   RE,0                NO PURCHASE ORDER #?                         
         BH    PO#_34                                                           
         IC    RE,OLDCDATA                                                      
         CHI   RE,0                NO PURCHASE ORDER #?                         
         BH    *+12                                                             
PO#_32E  MVI   ERRORNUM+1,MSSNGERR                                              
         B     PO#_X                                                            
         OI    WKPO#SW1,PO#1REMQ                                                
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WKPO#PNO(0),OLDCDATA+1                                           
         B     PO#_40                                                           
*                                                                               
PO#_34   BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WKPO#PNO(0),6(R3)   PURCHASE ORDER #                             
*                                                                               
PO#_40   BRAS  RE,GETPOREC         GET PURCHASE ORDER RECORD                    
         BNE   PO#_X                                                            
         BRAS  RE,PO#_PROC         PROCESS PURCHASE ORDER # ELEMS               
*                                                                               
PO#_X    TM    WKAIOSW1,IO_GETRQ   GETREC IS CALLED?                            
         BZ    *+20                                                             
         NI    WKAIOSW1,X'FF'-IO_GETRQ                                          
         CLI   SVTRCODE,C'B'       NEW INSERTION?                               
         BE    *+8                                                              
         BRAS  RE,IORESTOR         RESTORE IO SEQUENCE                          
         CLI   SVPARM_3,VAL_PO#Q   VALIDATING PO# REQUEST FROM BUY?             
         BNE   *+12                                                             
         LH    RE,ERRORNUM                                                      
         ST    RE,DMCB+8           PASS BY ERROR NUMBER TO CALLER               
         J     EXIT                                                             
*                                                                               
PO#_PROC LR    R0,RE                                                            
         BRAS  RE,VALPONUM         VALIDATE PURCHASE ORDER #                    
         JNE   PO#_PX                                                           
         OC    WKSVELEM,WKSVELEM   GOT A VALID PO# DETAIL ELEM?                 
         JZ    PO#_PX                                                           
         LA    R1,WKSVELEM                                                      
         USING PO#DELM,R1                                                       
         L     RE,APO#TAB          ADDRESS OF PO# TABLE                         
         USING PO#PRDCD,RE                                                      
         SR    RF,RF                                                            
PO#_P30  CHI   RF,PO#_MAXQ                                                      
         JNH   *+6                                                              
         DC    H'0'                TABLE IS MAXED                               
         OC    0(PO#TABLQ,RE),0(RE)                                             
         JZ    PO#_P40                                                          
         LA    RF,1(RF)                                                         
         LA    RE,PO#TABLQ(RE)     POINT TO NEXT ENTRY                          
         J     PO#_P30                                                          
*                                                                               
PO#_P40  MVC   PO#PRDCD,WKPO#PRD                                                
         MVC   PO#SEQNO,PO#DID                                                  
         TM    PO#DACTV,PO#DUSDQ   ALREADY USED IN INSERTIONS?                  
         JNZ   *+8                                                              
         OI    PO#STATU,PO#_USEQ                                                
         TM    PO#DACTV,PO#DINAQ   PURCHASE ORDER # IS INACTIVE?                
         JZ    *+8                                                              
         OI    PO#STATU,PO#_REMQ                                                
         OC    WKPO#PNO,WKPO#PNO   PO# IS MANUALLY ENTERED (OVERRIDE)?          
         JZ    *+8                                                              
         OI    PO#STATU,BYPOOVRQ   SET OVERRIDE STATUS BIT                      
*                                                                               
         OI    GLBVALSW,BUYPO#VQ   SET PURCHASE ORDER # VALIDATED BIT           
*                                                                               
PO#_PX   LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
IORESTOR STCM  RE,15,SV_REG_E      RESTORE IO SEQUENCE                          
         MVC   SV_AREC_,AREC                                                    
         MVC   AREC,AWKAIO1                                                     
         BRAS  RE,PRT_GETR         RESTORE IO SEQUENCE                          
         MVC   AREC,SV_AREC_                                                    
         ICM   RE,15,SV_REG_E                                                   
         NI    WKAIOSW1,X'FF'-IO_GETRQ                                          
         BR    RE                                                               
*                                                                               
         DROP  RB,RE,R1                                                         
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKBILPO# NTR1  BASE=*,LABEL=*      CHECK BILLED WITHOUT PO#                     
*                                                                               
         CLI   SVTRCODE,C'B'       NEW INSERTION?                               
         JE    SETCCEQ                                                          
         LA    R5,REC+33                                                        
         USING PBILELEM,R5                                                      
         MVI   ELCODE,X'26'                                                     
         BRAS  RE,NXTELEM          BILLING ELEM FOUND?                          
         JNE   SETCCEQ                                                          
         OC    PBLDATE,PBLDATE     HAVE BILL DATE?                              
         JZ    *-14                                                             
         TM    PBBILST,X'80'       REVERSED?                                    
         JNZ   SETCCEQ                                                          
         TM    PBBILST,X'40'       REVERSAL?                                    
         JNZ   SETCCEQ                                                          
         DROP  R5                                                               
*                                                                               
         LA    R5,REC+33                                                        
         MVI   ELCODE,PBYPOELQ                                                  
         BRAS  RE,NXTELEM          PURCHASE ORDER ELEM FOUND?                   
         JE    SETCCEQ                                                          
*                                                                               
         J     SETCCNEQ            BILLED, CANNOT ATTACH PO# ELEM               
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GETPOREC NTR1  BASE=*,LABEL=*      GET PURCHASE ORDER RECORD                    
*                                                                               
         MVC   WKSVKEY,KEY         SAVE ORIGINAL KEY                            
*                                                                               
         LA    RF,REC                                                           
         CLI   SVTRCODE,C'B'       NEW INSERTION?                               
         BNE   *+8                                                              
         LA    RF,NEWREC                                                        
         XC    KEY,KEY                                                          
         LA    RE,KEY                                                           
         USING PPO#KEY,RE                                                       
         USING PBUYKEY,RF                                                       
         MVC   PPO#KAGY,PBUYKAGY                                                
         MVC   PPO#KMED,PBUYKMED                                                
         MVI   PPO#KRCD,PPO#KIDQ                                                
*                                                                               
         MVC   PPO#KCLT,PBUYKCLT                                                
         CLI   SVCLTPLV,P_POLVCQ   PURCHASE ORDER# AT CLIENT LEVEL?             
         JE    GETPO_20                                                         
*                                                                               
         MVC   PPO#KPRD,PBUYKPRD                                                
         CLC   WKPO#PRD,SPACES                                                  
         BNH   *+10                                                             
         MVC   PPO#KPRD,WKPO#PRD                                                
         CLI   SVCLTPLV,P_POLVPQ   PURCHASE ORDER# AT PRODUCT LEVEL?            
         JE    GETPO_20                                                         
*                                                                               
         MVC   PPO#KEST,PBUYKEST                                                
         DROP  RE,RF                                                            
*                                                                               
GETPO_20 BRAS  RE,PRT_RDHI                                                      
         CLC   KEY(L'PPO#KEY),KEYSAVE                                           
         BNE   GETPO_ER                                                         
*                                                                               
         MVC   SV_AREC_,AREC                                                    
         MVC   AREC,AWKAIO1                                                     
         BRAS  RE,PRT_GETR                                                      
         MVC   AREC,SV_AREC_                                                    
         OI    WKAIOSW1,IO_GETRQ   GETREC IS CALLED                             
         CLI   8(R1),0                                                          
         BE    GETPO_X                                                          
         B     GETPO_ER                                                         
*                                                                               
GETPO_X  MVC   KEY,WKSVKEY         RESTORE ORIGINAL KEY                         
         J     SETCCEQ                                                          
*                                                                               
GETPO_ER MVC   KEY,WKSVKEY         RESTORE ORIGINAL KEY                         
         MVC   ERRORNUM,=AL2(PO_RECNF)                                          
         J     SETCCNEQ                                                         
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VALPONUM NTR1  BASE=*,LABEL=*      VALIDATE PURCHASE ORDER #                    
*                                                                               
         XC    WKSVELEM,WKSVELEM   RETURN VALIDATED PO# DETAIL ELEM             
         L     R5,AWKAIO1                                                       
         LA    R5,PO#FIRST-PPO#KEY(R5)                                          
         USING PO#DELM,R5                                                       
VALPO_42 CLI   PO#DELID,0          END OF PURCHASE ORDER RECORD?                
         BE    VALPO_80                                                         
         CLI   PO#DELID,PO#DLIDQ   PO# DETAIL ELEM?                             
         BE    VALPO_48                                                         
VALPO_44 SR    RE,RE                                                            
         IC    RE,PO#DLEN                                                       
         AR    R5,RE                                                            
         B     VALPO_42                                                         
*                                                                               
VALPO_48 TM    WKPO#SW1,PO#1REMQ   REMOVING INACTIVE PO# FROM BUY?              
         BNZ   VALPO_52                                                         
         OC    WKPO#PNO,WKPO#PNO   LOOKING UP PO# FOR INSERTION?                
         BNZ   VALPO_56                                                         
         BRAS  RE,VPO#_FLT         APPLY FILTERING CONDITIONS                   
         BNE   VALPO_44                                                         
         TM    PO#DACTV,PO#DINAQ   PO# IS MARKED AS INACTIVE?                   
         BNZ   VALPO_44                                                         
         OC    WKSVELEM,WKSVELEM   ALREADY GOT A PO#?                           
         BZ    VALPO_62                                                         
*                                                                               
         TM    ABUPLDSW,IDSKUPLQ   PRISMA UPLOAD?                               
         JZ    *+14                                                             
         XC    WKSVELEM,WKSVELEM   SUPPRESS OVERLAPPING DATE FOR PRISMA         
         J     VALPO_80                                                         
*                                                                               
         B     VALPO#E3                                                         
*                                                                               
VALPO_52 OC    WKPO#SQ#,WKPO#SQ#   HAVE PURCHASE ORDER SEQUENCE #?              
         BZ    VALPO_56                                                         
         CLC   WKPO#SQ#,PO#DID     PURCHASE ORDER SEQUENCE # MATCH?             
         BNE   VALPO_44                                                         
         B     VALPO_58                                                         
*                                                                               
VALPO_56 SR    RE,RE                                                            
         IC    RE,PO#DLEN                                                       
         SHI   RE,PO#DHDLQ                                                      
         CHI   RE,1                PURCHASE ORDER # PRESENT?                    
         BNL   *+6                                                              
         DC    H'0'                                                             
         XC    WKTELEM,WKTELEM                                                  
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WKTELEM(0),PO#DPO#                                               
*                                                                               
         CLC   WKPO#PNO,WKTELEM    MATCH THAT OF UPLOAD INPUT?                  
         BNE   VALPO_44                                                         
         TM    WKPO#SW1,PO#1REMQ   REMOVING INACTIVE PO# FROM BUY?              
         BZ    *+16                                                             
VALPO_58 TM    PO#DACTV,PO#DINAQ   PO# IS MARKED AS INACTIVE?                   
         BZ    VALPO#E5                                                         
         B     *+12                                                             
         TM    PO#DACTV,PO#DINAQ   PO# IS MARKED AS INACTIVE?                   
         BNZ   VALPO#E2                                                         
         BRAS  RE,VPO#_FLT         APPLY FILTERING CONDITIONS                   
         BNE   VALPO#E4            INSERTION DATE IS NOT COVERED BY PO#         
*                                                                               
VALPO_62 SR    RE,RE                                                            
         IC    RE,PO#DLEN                                                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WKSVELEM(0),PO#DELM SAVE VALIDATED PO# DETAIL ELEM               
*                                                                               
         TM    WKPO#SW1,PO#1REMQ   REMOVING INACTIVE PO# FROM BUY?              
         BNZ   *+14                                                             
         OC    WKPO#PNO,WKPO#PNO   LOOKING UP PO# FOR INSERTION?                
         BZ    VALPO_44            CK FOR OVERLAPPING DATES                     
*                                                                               
VALPO_80 OC    WKSVELEM,WKSVELEM   GOT A VALID PO# DETAIL ELEM?                 
         BNZ   VALPO_84                                                         
         TM    WKPO#SW1,PO#1REMQ   REMOVING INACTIVE PO# FROM BUY?              
         BNZ   VALPO#E1            PO# NOT FOUND                                
         OC    WKPO#PNO,WKPO#PNO   LOOKING UP PO# FOR INSERTION?                
         BZ    VALPO#X                                                          
         B     VALPO#E1            PO# NOT FOUND                                
VALPO_84 LA    R5,WKSVELEM                                                      
         TM    WKPO#SW1,PO#1REMQ   REMOVING INACTIVE PO# FROM BUY?              
         BNZ   *+12                                                             
         TM    PO#DACTV,PO#DINAQ   PO# IS MARKED AS INACTIVE?                   
         BNZ   VALPO#E2                                                         
*                                                                               
VALPO#X  J     SETCCEQ                                                          
*                                                                               
VPO#_FLT LR    R0,RE                                                            
         TM    WKPO#SW1,PO#1REMQ   REMOVING INACTIVE PO# FROM BUY?              
         JNZ   *+12                                                             
         TM    PO#DACTV,PO#DINAQ   PO# IS MARKED AS INACTIVE?                   
         JNZ   X_RE_NEQ                                                         
         LA    RF,NEWREC                                                        
         USING PBUYKEY,RF                                                       
         CLC   PBUYKDAT,PO#DSTRT   BUY DATE >= PO# START DATE?                  
         JL    X_RE_NEQ                                                         
         CLC   PBUYKDAT,PO#DEND    BUY DATE <= PO# START DATE?                  
         JH    X_RE_NEQ                                                         
         DROP  RF                                                               
X_RE_EQ  CR    RE,RE                                                            
         J     *+6                                                              
X_RE_NEQ LTR   RE,RE                                                            
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
VALPO#E1 MVI   ERRORNUM+1,INVERR                                                
         J     SETCCNEQ                                                         
*                                                                               
VALPO#E2 MVC   ERRORNUM,=AL2(PO#INACT)                                          
         J     SETCCNEQ                                                         
*                                                                               
VALPO#E3 MVC   ERRORNUM,=AL2(OVRLAPDT)                                          
         J     SETCCNEQ                                                         
*                                                                               
VALPO#E4 MVC   ERRORNUM,=AL2(INSDTNPO)                                          
         J     SETCCNEQ                                                         
*                                                                               
VALPO#E5 MVC   ERRORNUM,=AL2(PO#NOREM)                                          
         J     SETCCNEQ                                                         
         DROP  RB,R5                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKCPMS   NTR1  BASE=*,LABEL=*      BYTE3 HAS ELEM CODE TO BE PROCESSED          
*                                                                               
         CLI   BUYMD,C'I'          INTERACTIVE?                                 
         JE    CCPMS02                                                          
         CLI   BUYMD,C'L'          SOCIAL?                                      
         JE    CCPMS02                                                          
         CLI   BUYMD,C'B'          MOBILE?                                      
         JE    CCPMS02                                                          
         CLI   BUYMD,C'D'          DIGITAL AUDIO?                               
         JE    CCPMS02                                                          
         CLI   BUYMD,C'V'          NATIONAL VIDEO (NVIDEO)?                     
         JE    CCPMS02                                                          
         CLI   BUYMD,C'W'          LOCAL VIDEO (LVIDEO)?                        
         JE    CCPMS02                                                          
         J     INVFLDER                                                         
*                                                                               
CCPMS02  XC    WKBUYELM,WKBUYELM                                                
         MVC   WKBUYELM(1),BYTE3   ELEM CODE                                    
*                                                                               
         CLI   DDLINKSW,C'C'       CHANGE INSERTION UPLOAD?                     
         BNE   CCPMS05                                                          
         ZIC   R6,OLDCDATA                                                      
         CHI   R6,0                ANYTHING IN OLD DATA?                        
         BH    CCPMS03                                                          
         LA    R5,REC+33                                                        
         CLI   0(R5),X'20'                                                      
         BE    *+6                                                              
         DC    H'0'                BUY DESCRIPTION ELEM MUST BE THERE!          
         MVC   ELCODE,BYTE3                                                     
         BRAS  RE,NXTELEM                                                       
         JE    DIFFUERR            CPM ELEM STATE IS DIFFERENT                  
         B     CCPMS05             OKAY TO ADD A CPM ELEM                       
*                                                                               
CCPMS03  GOTOR VCASHVAL,DMCB,(2,OLDCDATA+1),(R6)                                
         CLI   0(R1),X'FF'                                                      
         JE    INVFLDER                                                         
         L     R0,4(R1)                                                         
         CVD   R0,DUB                                                           
         LA    R5,REC+33                                                        
         CLI   0(R5),X'20'                                                      
         BE    *+6                                                              
         DC    H'0'                BUY DESCRIPTION ELEM MUST BE THERE!          
         MVC   ELCODE,BYTE3                                                     
         BRAS  RE,NXTELEM                                                       
         JNE   DIFFUERR            CPM ELEM HAS BEEN REMOVED                    
         CP    2(5,R5),DUB+3(5)                                                 
         JNE   DIFFUERR            CHANGED SINCE LAST DOWNLOAD                  
*                                                                               
CCPMS05  LH    R5,HALF             INPUT LENGTH                                 
         XC    DUB,DUB             FOR WORKING STORAGE                          
*                                                                               
         CHI   R5,0                                                             
         BE    CCPMS10             INPUT IS ZERO                                
         BH    CCPMS30             INPUT PRESENT, GO CHECK IT                   
         J     INVFLDER            NEGATIVE INPUT LENGTH, ERROR                 
*                                                                               
CCPMS10  MVI   DUB+1,X'FF'         SPECIAL DELETE CODE                          
         CLI   SVTRCODE,C'B'       ADDING EMPTY ELEM ON NEW BUY?                
         JE    INVFLDER                                                         
         B     CCPMS60             MUST BE REMOVING ELEM THEN                   
*                                                                               
CCPMS30  GOTOR VCASHVAL,DMCB,(2,(R2)),(R5)                                      
         CLI   0(R1),X'FF'                                                      
         JE    INVFLDER                                                         
         L     R0,4(R1)                                                         
         CVD   R0,DUB                                                           
         CP    DUB,=P'0'                                                        
         BE    CCPMS10             ZERO ENTERED - SET SPECIAL DEL CODE          
         JL    INVFLDER                                                         
         CP    DUB,=P'999999999'   MAX IS 999,999,999 FOR PL5                   
         JH    INVFLDER                                                         
*                                                                               
CCPMS60  CLI   WKBUYELM,X'A0'      ESTIMATED CPM?                               
         BE    CCPMS65                                                          
         CLI   WKBUYELM,X'A1'      ACTUAL CPM?                                  
         BE    CCPMS65                                                          
         DC    H'0'                NO OTHER ELEM CODE GOES HERE YET             
*                                                                               
CCPMS65  LA    R5,NEWREC+33        START BUILDING ELEMENT                       
         MVC   ELCODE,WKBUYELM                                                  
         BRAS  RE,NXTELEM                                                       
         JE    INVFLDER            CAN'T ALREADY HAVE ELEM                      
*                                                                               
         MVI   WKBUYELM+1,9        ELEMENT LENGTH                               
*                                                                               
         CLI   DUB+1,X'FF'         SPECIAL CODE PRESENT?                        
         BE    *+14                                                             
         ZAP   WKBUYELM+2(5),DUB   CPM VALUE IN PACKED FORMAT                   
         B     *+10                                                             
         MVC   WKBUYELM+2(1),DUB+1 MOVE IN DELETION CODE                        
*                                                                               
         BRAS  RE,ADDBUYEL                                                      
*                                                                               
CCPMSX   J     XCERRNUM                                                         
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKINSTA  NTR1  BASE=*,LABEL=*      INSERTION STATUS                             
*                                                                               
* FOR CHG UPLD, DO NOT COMPARE STATUS BYTE FOR BEST FOOD OR WEEK OF             
*                                                                               
         CLI   DDLINKSW,C'C'       CHANGE INSERTION UPLOAD?                     
         BNE   CINST20                                                          
         LA    R5,REC+33                                                        
         CLI   0(R5),X'20'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   PBDBFD-PBDELEM(R5),C'B'                                          
         BE    CINST20                                                          
         CLI   PBDBFD-PBDELEM(R5),C'W'                                          
         BE    CINST20                                                          
         CLC   PBDBFD-PBDELEM(L'PBDBFD,R5),OLDCDATA+1                           
         JNE   DIFFUERR                                                         
*                                                                               
CINST20  LH    RE,HALF             FIELD DATA LENGTH                            
         CHI   RE,4                                                             
         JH    INVFLDER                                                         
*                                                                               
         CLI   0(R2),C'S'          STEWARDSHIP INSERTION?                       
         BNE   *+8                                                              
         MVI   0(R2),C'T'          TREAT IT SAME AS TEST INSERTION              
*                                                                               
* FOR BEST FOOD DAY AND WEEK OF INSERTIONS, DO NOT WIPE OUT INDICATOR           
*                                                                               
         CLI   0(R2),C'L'          LIVE?                                        
         BNE   CINST30                                                          
         TM    SVESPROF+29,X'80'   LIVE STATUS ON TEST EST?                     
         BO    CINSTTEL            YES, THIS IS NOT ALLOWED                     
         LA    R5,NEWREC+33                                                     
         CLI   0(R5),X'20'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   PBDBFD-PBDELEM(R5),C'B'                                          
         BE    CINSTX                                                           
         CLI   PBDBFD-PBDELEM(R5),C'W'                                          
         BE    CINSTX                                                           
         MVI   PBDBFD-PBDELEM(R5),0                                             
         B     CINSTX                                                           
*                                                                               
* CANNOT HAVE TEST INSERTIONS FOR BEST FOOD DAY AND WEEK OF                     
*                                                                               
CINST30  CLI   0(R2),C'T'          TEST?                                        
         JNE   INVFLDER                                                         
         LA    R5,NEWREC+33                                                     
         CLI   0(R5),X'20'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   PBDBFD-PBDELEM(R5),C'B'                                          
         BE    CINSTTWB                                                         
         CLI   PBDBFD-PBDELEM(R5),C'W'                                          
         BE    CINSTTWB                                                         
         MVC   PBDBFD-PBDELEM(L'PBDBFD,R5),0(R2)                                
         B     CINSTX                                                           
*                                                                               
CINSTX   J     XCERRNUM                                                         
*                                                                               
CINSTTWB LHI   RE,257              TEST W OR B INSERTIONS NOT ALLOWED           
         B     CINSTEX                                                          
*                                                                               
CINSTTEL LHI   RE,258              DISALLOW TEST EST W/ LIVE STATUS             
*                                                                               
CINSTEX  STH   RE,ERRORNUM                                                      
         J     EXIT                                                             
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
BLDACHRG NTR1  BASE=*,LABEL=*      EXTRACT CHARGE MAP CODES                     
*                                                                               
         XC    ERRORNUM,ERRORNUM   SET TO NO ERROR                              
         LA    RE,WKBLOCK1         CHARGE DATA - BUY RECORD                     
         LHI   RF,500                                                           
         XCEFL                                                                  
         LA    RE,WKBLOCK2         CHARGE DATA - CHANGED DATA (OLD)             
         LHI   RF,500                                                           
         XCEFL                                                                  
*                                                                               
         ST    R3,WKTFULL                                                       
BLDAC08D LA    RE,BLDACMCT                                                      
BLDAC08F CLI   0(RE),X'FF'         END OF TABLE?                                
         BE    BLDAC08H                                                         
         CLC   3(2,R3),0(RE)       ADDITIONAL CHARGE MAP CODE?                  
         BNE   *+12                                                             
         BRAS  RE,NXTWFELM                                                      
         B     BLDAC08D                                                         
         LA    RE,2(RE)                                                         
         B     BLDAC08F                                                         
BLDAC08H ST    R3,SV_REG_3         ADDRESS OF END OF CHARGE MAP ELEMS           
         L     R3,WKTFULL                                                       
*                                                                               
         BRAS  RE,CKCHGMOD         CHANGE MODE?                                 
         BNE   BLDAC40                                                          
         MVI   ELCODE,X'44'                                                     
         LA    RF,WKBLOCK1                                                      
BLDAC14  BRAS  RE,NXTELEM                                                       
         BNE   BLDAC40                                                          
         MVC   0(32,RF),0(R5)      SAVE ADDITIONAL CHARGE ELEM                  
         CLI   (PACCD-PACELEM)(RF),0                                            
         BNE   *+8                                                              
         MVI   (PACCD-PACELEM)(RF),C'N'                                         
         CLI   (PACAC-PACELEM)(RF),0                                            
         BNE   *+8                                                              
         MVI   (PACAC-PACELEM)(RF),C'N'                                         
         LA    RF,32(RF)                                                        
         B     BLDAC14                                                          
*                                                                               
BLDAC40  MVI   ACHRGCNT,0          INIT ADDITIONAL CHARGE COUNTER               
*                                                                               
         USING PACELEM,R2                                                       
BLDAC50  CLC   =AL2(D#ACHCOD),3(R3)                                             
         BNE   BLDAC70                                                          
         OI    OTHELMSW,ACHARGEQ   PROCESSING ADDITIONAL CHARGES                
         BRAS  RE,BLDAC_TB         SET ADDITIONAL CHARGE TABLE ENTRY            
BLDAC50M SR    RE,RE                                                            
         ICM   RE,3,(LQ_LN-LQ_D)(R3)                                            
         SHI   RE,6                                                             
         CHI   RE,0                MAP CODE ONLY ELEM?                          
         BNH   BLDAC50P                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   PACCODE(0),6(R3)    ADDITIONAL CHARGE CODE                       
         OC    PACCODE,SPACES                                                   
*                                                                               
         CLC   PACCODE,=C'TC'      SPECIAL TC CHARGE CODE?                      
         JNE   *+12                                                             
         TM    GENBYSW1,MIDASTPQ   MIDAS TEST PUB?                              
         JZ    BLDAC_E2            ONLY MIDAS PUB ALLOWS TC CHARGE              
*                                                                               
         MVI   PACELEM+0,X'44'     ADDITIONAL CHARGE ELEM CODE                  
         MVI   PACELEM+1,32        ADDITIONAL CHARGE ELEM LENGTH                
BLDAC50P BRAS  RE,NXTWFELM                                                      
         CLC   =AL2(D#ACHCOD+1),3(R3)                                           
         BNE   BLDAC52                                                          
         BRAS  RE,BLDAC_CH         POINT TO CHANGE UPLOAD TABLE ENTRY           
         B     BLDAC50M                                                         
*                                                                               
BLDAC52  CLC   =AL2(D#ACHGRS),3(R3)                                             
         JNE   BLDAC_E1                                                         
         BRAS  RE,BLDAC_TB         SET ADDITIONAL CHARGE TABLE ENTRY            
BLDAC52M SR    R6,R6                                                            
         ICM   R6,3,(LQ_LN-LQ_D)(R3)                                            
         SHI   R6,6                                                             
         CHI   R6,0                MAP CODE ONLY ELEM?                          
         BNH   BLDAC52P                                                         
         MVI   PACGN,C'G'          GROSS BY DEFUALT                             
         LA    RF,6(R3)                                                         
         CLI   6(R3),C'G'          GROSS?                                       
         BE    *+16                                                             
         CLI   6(R3),C'N'          NET?                                         
         BNE   *+14                                                             
         MVI   PACGN,C'N'          NET                                          
         BCTR  R6,0                                                             
         LA    RF,1(RF)                                                         
         BRAS  RE,BLDAC_NM                                                      
         JNE   BLDAC_E2                                                         
         ZAP   PACAMT,DUB                                                       
BLDAC52P BRAS  RE,NXTWFELM                                                      
         CLC   =AL2(D#ACHGRS+1),3(R3)                                           
         BNE   BLDAC54                                                          
         BRAS  RE,BLDAC_CH         POINT TO CHANGE UPLOAD TABLE ENTRY           
         B     BLDAC52M                                                         
*                                                                               
BLDAC54  CLC   =AL2(D#ACHSAC),3(R3)                                             
         JNE   BLDAC_E1                                                         
         BRAS  RE,BLDAC_TB         SET ADDITIONAL CHARGE TABLE ENTRY            
BLDAC54M SR    RE,RE                                                            
         ICM   RE,3,(LQ_LN-LQ_D)(R3)                                            
         SHI   RE,6                                                             
         CHI   RE,0                MAP CODE ONLY ELEM?                          
         BNH   BLDAC54P                                                         
         MVC   PACAC,6(R3)         SUBJECT TO AGY COMMISSION Y/N                
BLDAC54P BRAS  RE,NXTWFELM                                                      
         CLC   =AL2(D#ACHSAC+1),3(R3)                                           
         BNE   BLDAC56                                                          
         BRAS  RE,BLDAC_CH         POINT TO CHANGE UPLOAD TABLE ENTRY           
         B     BLDAC54M                                                         
*                                                                               
BLDAC56  CLC   =AL2(D#ACHCPT),3(R3)                                             
         JNE   BLDAC_E1                                                         
         BRAS  RE,BLDAC_TB         SET ADDITIONAL CHARGE TABLE ENTRY            
BLDAC56M SR    RF,RF                                                            
         ICM   RF,3,(LQ_LN-LQ_D)(R3)                                            
         SHI   RF,6                                                             
         CHI   RF,0                MAP CODE ONLY ELEM?                          
         BNH   BLDAC56P                                                         
         GOTOR VCASHVAL,DMCB,(3,6(R3)),(RF)                                     
         CLI   0(R1),X'FF'                                                      
         JE    BLDAC_E2                                                         
         L     RE,4(R1)                                                         
         CVD   RE,DUB                                                           
         CP    DUB,=P'0'                                                        
         JL    BLDAC_E2                                                         
         CP    DUB,=P'100000'      MAX IS 100.000 (100%)                        
         JH    BLDAC_E2                                                         
         ZAP   PACACOM,DUB         AGY COMMISSION % (3 DECIMALS)                
BLDAC56P BRAS  RE,NXTWFELM                                                      
         CLC   =AL2(D#ACHCPT+1),3(R3)                                           
         BNE   BLDAC58                                                          
         BRAS  RE,BLDAC_CH         POINT TO CHANGE UPLOAD TABLE ENTRY           
         B     BLDAC56M                                                         
*                                                                               
BLDAC58  CLC   =AL2(D#ACHCDA),3(R3)                                             
         JNE   BLDAC_E1                                                         
         BRAS  RE,BLDAC_TB         SET ADDITIONAL CHARGE TABLE ENTRY            
BLDAC58M SR    RE,RE                                                            
         ICM   RE,3,(LQ_LN-LQ_D)(R3)                                            
         SHI   RE,6                                                             
         CHI   RE,0                MAP CODE ONLY ELEM?                          
         BNH   BLDAC58P                                                         
         MVC   PACCD,6(R3)         SUBJECT TO CASH DISCOUNT Y/N                 
BLDAC58P BRAS  RE,NXTWFELM                                                      
         CLC   =AL2(D#ACHCDA+1),3(R3)                                           
         BNE   BLDAC60                                                          
         BRAS  RE,BLDAC_CH         POINT TO CHANGE UPLOAD TABLE ENTRY           
         B     BLDAC58M                                                         
*                                                                               
BLDAC60  BRAS  RE,ACHRGNET         ADJUST CHARGE AMT (NET/GROSS)                
         BNE   BLDAC_EX                                                         
         CLI   DDLINKSW,C'C'       CHANGE INSERTION UPLOAD?                     
         BNE   BLDAC66                                                          
         BRAS  RE,BLDAC_TB                                                      
         OC    0(L'ACHRGEL1,R2),0(R2)                                           
         BZ    BLDAC66                                                          
         BRAS  RE,ACHRGNET         ADJUST CHARGE AMT (NET/GROSS)                
         BNE   BLDAC_EX                                                         
*                                                                               
BLDAC66  SR    RE,RE                                                            
         IC    RE,ACHRGCNT                                                      
         AHI   RE,1                                                             
         CHI   RE,10               MAX OF 10 ADDITIONAL CHARGES?                
         JH    BLDAC_E1                                                         
         STC   RE,ACHRGCNT                                                      
         B     BLDAC50             PROCESS NEXT SET OF CHARGE                   
*                                                                               
BLDAC70  CLI   DDLINKSW,C'C'       CHANGE INSERTION UPLOAD?                     
         BNE   BLDAC80                                                          
         LA    RE,WKBLOCK1                                                      
         LA    RF,WKBLOCK2                                                      
         LA    R1,10                                                            
BLDAC70H CLC   0(L'ACHRGEL1,RE),0(RF)                                           
         JNE   BLDAC_E3                                                         
         LA    RE,L'ACHRGEL1(RE)                                                
         LA    RF,L'ACHRGEL1(RF)                                                
         BCT   R1,BLDAC70H                                                      
*                                                                               
         LA    R5,REC+33                                                        
         MVI   ELCODE,X'25'        CK IF CHARGE HAS BEEN PAID                   
BLDAC72D BRAS  RE,NXTELEM                                                       
         BNE   BLDAC74                                                          
         CLI   1(R5),24+2          PAY ELEM WITH CHARGE CODE?                   
         BL    BLDAC72D                                                         
         LA    RE,WKBLOCK1         POINT TO SAVED CHARGE DATA IN REC            
         LA    RF,ACHRGEL1         POINT TO NEW CHARGE DATA                     
         LA    R1,10                                                            
BLDAC72K CLC   (PACCODE-PACELEM)(L'PACCODE,RE),(PACCODE-PACELEM)(RF)            
         BE    *+12                                                             
         CLI   0(RE),0             END OF CHARGE DATA IN REC?                   
         JNE   BLDAC_E4                                                         
         LA    RE,L'ACHRGEL1(RE)                                                
         LA    RF,L'ACHRGEL1(RF)                                                
         BCT   R1,BLDAC72K                                                      
*                                                                               
BLDAC74  LA    R5,REC+33                                                        
         MVI   ELCODE,X'26'        CK IF CHARGE HAS BEEN BILLED                 
BLDAC74D BRAS  RE,NXTELEM                                                       
         BNE   BLDAC76                                                          
         CLI   1(R5),23+2          BILL ELEM WITH CHARGE CODE?                  
         BL    BLDAC74D                                                         
         LA    RE,WKBLOCK1         POINT TO SAVED CHARGE DATA IN REC            
         LA    RF,ACHRGEL1         POINT TO NEW CHARGE DATA                     
         LA    R1,10                                                            
BLDAC74K CLC   (PACCODE-PACELEM)(L'PACCODE,RE),(PACCODE-PACELEM)(RF)            
         BE    *+12                                                             
         CLI   0(RE),0             END OF CHARGE DATA IN REC?                   
         JNE   BLDAC_E4                                                         
         LA    RE,L'ACHRGEL1(RE)                                                
         LA    RF,L'ACHRGEL1(RF)                                                
         BCT   R1,BLDAC74K                                                      
*                                                                               
BLDAC76  LA    RE,REC+33                                                        
         MVI   WKTBYTE,0                                                        
         USING PBDELEM,RE                                                       
         TM    PBDSTAT,X'40'       MATCHED TO INVOICE?                          
         BZ    BLDAC78                                                          
         OI    WKTBYTE,X'40'       MATCHED                                      
         DROP  RE                                                               
         MVI   ELCODE,X'25'                                                     
         BRAS  RE,NXTELEM          PAID ELEMENT FOUND?                          
         BNE   *+18                                                             
         OC    2(3,R5),2(R5)       PAID?                                        
         BZ    *-14                                                             
         OI    WKTBYTE,X'80'       PAID                                         
         TM    WKTBYTE,X'C0'       MATCH AND PAID?                              
         JM    BLDAC_E8            MIXED, NO CHANGES TO CHRGS ALLOWED           
*                                                                               
BLDAC78  DS    0H                  FUTURE CHANGE VALIDATION                     
*                                                                               
BLDAC80  LA    R2,ACHRGEL1                                                      
         LA    R6,10                                                            
         LA    RE,WKAIO1                                                        
         LHI   RF,4096                                                          
         XCEFL                                                                  
*                                                                               
BLDAC82F CLI   PACELEM,0           ADDITIONAL CHARGES TO BE ADDED?              
         BE    BLDAC82U                                                         
         OC    PACCODE,PACCODE                                                  
         JZ    BLDAC_E5                                                         
         CLC   PACCODE,=C'FX'      FOREIGN EXCHANGE CHARGE CODE?                
         BE    BLDAC82U                                                         
         L     RE,AWKAIO1                                                       
         CLI   3(RE),X'60'         ALREADY GOT CHARGE RECORD?                   
         BE    BLDAC82K                                                         
         XC    KEY,KEY                                                          
         MVC   KEY+0(2),AGYALPHA                                                
         MVC   KEY+2(1),BUYMD                                                   
         MVI   KEY+3,X'60'                                                      
         BRAS  RE,PRT_RDHI                                                      
         CLC   KEYSAVE(25),KEY     SAME RECORD?                                 
         JNE   BLDAC_E6                                                         
         MVC   SV_AREC_,AREC                                                    
         MVC   AREC,AWKAIO1                                                     
         BRAS  RE,PRT_GETR                                                      
         MVC   AREC,SV_AREC_                                                    
         CLI   8(R1),0                                                          
         JNE   BLDAC_E6                                                         
         USING PSPLELEM,R5                                                      
BLDAC82K L     R5,AWKAIO1                                                       
         LA    R5,(PSPLELEM-PSPLREC)(R5)                                        
         MVI   ELCODE,X'10'                                                     
         CLI   0(R5),X'10'                                                      
         JNE   BLDAC_E6                                                         
         CLC   PACCODE,PSPLCODE    CODE MATCH THAT OF CHARGE ELEM?              
         BE    BLDAC82M                                                         
         BRAS  RE,NXTELEM                                                       
         BE    *-14                                                             
         J     BLDAC_E6                                                         
*                                                                               
BLDAC82M CLI   PACCD,0             HAVE CASH DISCOUNT Y/N?                      
         BE    *+8                                                              
         CLI   PACCD,C'N'                                                       
         BE    *+12                                                             
         CLI   PACCD,C'Y'                                                       
         JNE   BLDAC_E2                                                         
*                                                                               
         CLI   PACAC,0             HAVE AGY COMMISSION Y/N?                     
         JE    BLDAC_E5                                                         
         CLI   PACAC,C'N'                                                       
         BE    *+12                                                             
         CLI   PACAC,C'Y'                                                       
         JNE   BLDAC_E2                                                         
*                                                                               
         OC    PACACOM,PACACOM     HAVE AGY COMMISSION PERCENTAGE?              
         BZ    *+12                                                             
         CLI   PACAC,C'N'                                                       
         JE    BLDAC_E7                                                         
*                                                                               
BLDAC82U LA    R2,L'ACHRGEL1(R2)                                                
         BCT   R6,BLDAC82F                                                      
*                                                                               
         LA    R2,ACHRGEL1                                                      
         LA    R6,10                                                            
BLDAC84H OC    0(2,R2),0(R2)                                                    
         BZ    BLDAC84K                                                         
         LA    R5,NEWREC+33                                                     
         MVI   ELCODE,X'FF'        GO TO END OF RECORD                          
         BRAS  RE,NXTELEM                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 VRECUP,DMCB,(1,NEWREC),(R2),(R5)                                 
*                                                                               
BLDAC84K LA    R2,L'ACHRGEL1(R2)   NEXT CHARGE ELEM                             
         BCT   R6,BLDAC84H                                                      
*                                                                               
         TM    OTHELMSW,ACHARGEQ   ADDITIONAL CHARGES PROCESSED?                
         BZ    *+12                                                             
         ST    R3,SVWRKELM         MAP CODES AFTER ADDITIONAL CHARGES           
         OI    ABCBITSW,ACHCHGDQ   INDICATE ADDITIONAL CHARGE CHANGED           
         J     SETCCEQ                                                          
*                                                                               
BLDAC_E1 MVC   ERRORNUM,=AL2(INVMAPCD)                                          
         J     BLDAC_EX                                                         
*                                                                               
BLDAC_E2 MVI   ERRORNUM+1,INVERR                                                
         J     BLDAC_EX                                                         
*                                                                               
BLDAC_E3 MVI   ERRORNUM+1,DCHGDUER                                              
         J     BLDAC_EX                                                         
*                                                                               
BLDAC_E4 MVC   ERRORNUM,=AL2(ACHNOREM)                                          
         J     BLDAC_EX                                                         
*                                                                               
BLDAC_E5 MVI   ERRORNUM+1,MSSNGERR                                              
         J     BLDAC_EX                                                         
*                                                                               
BLDAC_E6 MVI   ERRORNUM+1,NFNDERR                                               
         J     BLDAC_EX                                                         
*                                                                               
BLDAC_E7 MVI   ERRORNUM+1,COMNERR                                               
         J     BLDAC_EX                                                         
*                                                                               
BLDAC_E8 MVI   ERRORNUM+1,MATPDERR                                              
         J     BLDAC_EX                                                         
*                                                                               
BLDAC_EX MVC   SVWRKELM,SV_REG_3   POINT TO END OF CHARGE MAP ELEMS             
         OI    ABCBITSW,ACHVERRQ   INDICATE ADDITIONAL CHARGE ERROR'D           
         J     SETCCNEQ                                                         
*                                                                               
BLDAC_TB LA    R2,ACHRGEL1                                                      
         CLI   DDLINKSW,C'C'       CHANGE INSERTION UPLOAD?                     
         JNE   *+8                                                              
         LA    R2,WKBLOCK2                                                      
         SR    RF,RF                                                            
         IC    RF,ACHRGCNT                                                      
         MHI   RF,L'ACHRGEL1                                                    
         AR    R2,RF               POINT TO PROCESSING CHARGE                   
         BR    RE                                                               
*                                                                               
BLDAC_CH LA    R2,ACHRGEL1         POINT TO CHANGE UPLOAD TABLE ENTRY           
         SR    RF,RF                                                            
         IC    RF,ACHRGCNT                                                      
         MHI   RF,L'ACHRGEL1                                                    
         AR    R2,RF               POINT TO PROCESSING CHARGE                   
         BR    RE                                                               
*                                                                               
BLDAC_NM LR    R0,RE                                                            
         GOTOR VCASHVAL,DMCB,(2,0(RF)),(R6)                                     
         CLI   0(R1),X'FF'                                                      
         JE    X_RE_NEQ                                                         
         L     RE,4(R1)                                                         
         CVD   RE,DUB                                                           
         CP    DUB,=P'999999999'   MAX IS 9,999,999.99 FOR PL5                  
         JH    X_RE_NEQ                                                         
         CP    DUB,=P'-999999999'  NEGATIVE MAX?                                
         JL    X_RE_NEQ                                                         
         J     X_RE_EQ                                                          
*                                                                               
BLDACMCT DC    AL2(D#ACHCOD),AL2(D#ACHCOD+1)                                    
         DC    AL2(D#ACHGRS),AL2(D#ACHGRS+1)                                    
         DC    AL2(D#ACHSAC),AL2(D#ACHSAC+1)                                    
         DC    AL2(D#ACHCPT),AL2(D#ACHCPT+1)                                    
         DC    AL2(D#ACHCDA),AL2(D#ACHCDA+1)                                    
         DC    X'FF'                                                            
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
ACHRGNET NTR1  BASE=*,LABEL=*      GROSS UP CHARGE AMOUNT                       
*                                                                               
         USING PACELEM,R2                                                       
         CLI   PACGN,C'G'          GROSS?                                       
         BE    ACNETX                                                           
         CP    PACAMT,=P'0'        AMT IS ZERO?                                 
         BE    ACNETX                                                           
*                                                                               
         CLI   PACAC,C'N'          SUBJECT TO COMMISSION?                       
         BE    ACNETX              NO, NET AND GROSS ARE SAME                   
*                                                                               
         LA    RE,NEWREC+33                                                     
         CLI   DDLINKSW,C'C'       CHANGE UPLOADE MODE?                         
         BNE   *+8                                                              
         LA    RE,REC+33                                                        
         USING PBDELEM,RE                                                       
         CLI   0(RE),X'20'         BUY DESCRIPTION ELEM PRESENT?                
         BE    *+6                                                              
         DC    H'0'                                                             
         ZAP   WKTPL4,PBDACP                                                    
         CP    WKTPL4,=P'-1'       MINUS ONE IS 100% IN PBDAC                   
         BNE   *+10                                                             
         ZAP   WKTPL4,=P'100000'                                                
         DROP  RE                                                               
*                                                                               
         ZAP   WKTFULL2,WKTPL4     USE AGY COMMISSION IN BUY DESP ELEM          
         OC    PACACOM,PACACOM                                                  
         BZ    *+10                                                             
         ZAP   WKTFULL2,PACACOM                                                 
         ZAP   WKTDUB2,=P'100000'                                               
         SP    WKTDUB2,WKTFULL2    NET PCT                                      
*                                                                               
         CP    WKTDUB2,=P'0'                                                    
         BNE   ACNET25                                                          
         MVC   SVMAPCOD,=AL2(D#ACHGRS)                                          
         B     ACNET_E2                                                         
*                                                                               
ACNET25  ZAP   WKTFULL2,WKTDUB2    PUT IT IN FULL                               
         ZAP   WKTPL12,PACAMT                                                   
         MP    WKTPL12,=P'1000'                                                 
         DP    WKTPL12,WKTFULL2                                                 
         MP    WKTPL12(8),=P'100'                                               
         ZAP   WKTDUB2,WKTPL12(8)  INTEGER PART OF GROSS AMT                    
         ZAP   WKTPL12,WKTPL12+8(4)                                             
         MP    WKTPL12,=P'1000'    THREE DECIMAL PRECISION                      
         DP    WKTPL12,WKTFULL2                                                 
         CP    WKTPL12(8),=P'995'                                               
         BL    *+14                                                             
         AP    WKTDUB2,=P'100'     ROUND UP                                     
         B     ACNET45                                                          
         CP    WKTPL12(8),=P'-995'                                              
         BH    ACNET30                                                          
         AP    WKTDUB2,=P'-100'    ROUND UP (NEGATIVE AMT)                      
         B     ACNET45                                                          
*                                                                               
ACNET30  ZAP   WKTDUB,WKTPL12(8)                                                
         DP    WKTDUB,=P'10'       SEE IF THRID DECIMAL NEED ROUNDED            
         OI    WKTDUB+7,X'0F'      ALWAYS POSITIVE                              
         CP    WKTDUB+6(2),=P'5'                                                
         BL    ACNET40                                                          
         CP    WKTDUB(6),=P'0'     NEGATIVE?                                    
         BL    *+14                                                             
         AP    WKTDUB(6),=P'1'     ROUND UP                                     
         B     ACNET40                                                          
         AP    WKTDUB(6),=P'-1'    ROUND UP (NEGATIVE AMT)                      
*                                                                               
ACNET40  AP    WKTDUB2,WKTDUB(6)                                                
*                                                                               
ACNET45  CP    WKTDUB2,=P'999999999'                                            
         BNH   ACNET50             MAX IS RECHED FOR PL5                        
         MVC   SVMAPCOD,=AL2(D#ACHGRS)                                          
         J     BLDAC_E2                                                         
*                                                                               
ACNET50  ZAP   PACAMT,WKTDUB2+3(5) CALCULATED GROSS AMT                         
*                                                                               
ACNETX   J     SETCCEQ                                                          
*                                                                               
ACNET_E2 MVI   ERRORNUM+1,NETACERR                                              
         J     SETCCNEQ                                                         
*                                                                               
         DROP  RB,R2                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* FULL     - WILL RETURN ADDRESS OF ROUTINE                                     
* SVMAPCOD - WILL RETURN CORRESPONDING MAP CODE                                 
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKKEYWRD NTR1  BASE=*,LABEL=*      R3 POINTS WORKER ELEM                        
*                                                                               
         XC    ERRORNUM,ERRORNUM   INIT ERROR NUMBER                            
         XC    SVMAPCOD,SVMAPCOD   INIT MAP CODE                                
*                                                                               
         CLI   DDLINKSW,C'C'       CHANGE UPLOAD MODE?                          
         BNE   CKKW25                                                           
         ZICM  R5,1(R3),(3)                                                     
         AHI   R5,-6               LENGTH OF DATA                               
         XC    OLDCDATA,OLDCDATA   INIT ORIGINAL DATA FOR CHANGE MODE           
*                                                                               
         CLC   =AL2(D#PO#PRD),3(R3)                                             
         BE    CKKW25              NO ODD-EVEN PAIR FOR PO# PRD CODE            
*                                                                               
         CHI   R5,0                EMPTY DATA?                                  
         BE    CKKW25              YES, THIS IS A MAP CODE ONLY ELEM            
*                                                                               
         CHI   R5,0                                                             
         BH    *+6                                                              
         DC    H'0'                BAD LENGTH!                                  
         CHI   R5,255                                                           
         BNH   *+6                                                              
         DC    H'0'                CAN ONLY HANDLE 255 BYTES NOW                
         STC   R5,OLDCDATA         LENGTH OF ORIGINAL DATA                      
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   OLDCDATA+1(0),6(R3) SAVE ORIGINAL DATA                           
*                                                                               
CKKW25   LA    RE,KEYWTAB                                                       
CKKW30   CLI   0(RE),X'FF'         END OF KEYWORD TAB?                          
         JE    SETCCNEQ            YES, KEYWORD NOT FOUND                       
*                                                                               
         CLC   3(2,R3),0(RE)       MAP CODE MATCHED?                            
         BNE   CKKW60              NO, TRY NEXT ENTRY IN TABLE                  
*                                                                               
         MVC   FULL,2(RE)          ADDRESS FOR KEYWORD EDIT ROUTINE             
         MVC   SVMAPCOD,0(RE)                                                   
         J     SETCCEQ             PASS BY ADDRESS OF ROUTINE IN FULL           
*                                                                               
CKKW60   LA    RE,06(RE)           NEXT ENTRY IN KEYWORD TAB                    
         B     CKKW30                                                           
*                                                                               
KEYWTAB  DS    0H                                                               
         DC    AL2(D#REGCO1),AL4(REGCOM)    REGULAR COMMENTS 1                  
         DC    AL2(D#REGCO2),AL4(REGCOM)    REGULAR COMMENTS 2                  
         DC    AL2(D#REGCO3),AL4(REGCOM)    REGULAR COMMENTS 3                  
         DC    AL2(D#REGCO4),AL4(REGCOM)    REGULAR COMMENTS 4                  
         DC    AL2(D#REGCO5),AL4(REGCOM)    REGULAR COMMENTS 5                  
*                                                                               
         DC    AL2(D#INSCO1),AL4(IOCOM)     IC=              1                  
         DC    AL2(D#INSCO2),AL4(IOCOM)     IC=              2                  
         DC    AL2(D#INSCO3),AL4(IOCOM)     IC=              3                  
         DC    AL2(D#INSCO4),AL4(IOCOM)     IC=              4                  
         DC    AL2(D#INSCO5),AL4(IOCOM)     IC=              5                  
*                                                                               
         DC    AL2(D#POSIN1),AL4(PICOM)     PI=              1                  
         DC    AL2(D#POSIN2),AL4(PICOM)     PI=              2                  
         DC    AL2(D#POSIN3),AL4(PICOM)     PI=              3                  
         DC    AL2(D#POSIN4),AL4(PICOM)     PI=              4                  
         DC    AL2(D#POSIN5),AL4(PICOM)     PI=              5                  
*                                                                               
         DC    AL2(D#SRCOM1),AL4(SRCOM)     SRC=             1                  
         DC    AL2(D#SRCOM2),AL4(SRCOM)     SRC=             2                  
         DC    AL2(D#SRCOM3),AL4(SRCOM)     SRC=             3                  
         DC    AL2(D#SRCOM4),AL4(SRCOM)     SRC=             4                  
         DC    AL2(D#SRCOM5),AL4(SRCOM)     SRC=             5                  
*                                                                               
         DC    AL2(D#COMPCT),AL4(EDTCAC)    AC=                                 
         DC    AL2(D#PLCOST),AL4(EDTPLCOS)  PC=                                 
         DC    AL2(D#DSCPCT),AL4(EDTCCD)    CD=                                 
         DC    AL2(D#BBLDAT),AL4(EDTCBLDT)  BD=                                 
         DC    AL2(D#PBLDAT),AL4(EDTCPDDT)  PD=                                 
         DC    AL2(D#IORDAT),AL4(EDTCIODT)  ID=                                 
         DC    AL2(D#INSDA2),AL4(EDTCD2)    D2=                                 
         DC    AL2(D#SHPDAT),AL4(EDTCSHDT)  SD=                                 
         DC    AL2(D#TAXPCT),AL4(EDTTAX)    TAX=                                
         DC    AL2(D#CONUVL),AL4(EDTCU)     CU=                                 
         DC    AL2(D#REFNUM),AL4(EDTREF)    REF=                                
         DC    AL2(D#SPECFH),AL4(EDTSFH)    SFH=                                
         DC    AL2(D#ESTIMP),AL4(EDTIMP)    EIMPS=                              
         DC    AL2(D#ACTIMP),AL4(EDTAIMP)   AIMPS=                              
         DC    AL2(D#ONSDAT),AL4(EDTOSDT)   OSD=                                
         DC    AL2(D#DEFCIR),AL4(EDTDLC)    DLC=                                
         DC    AL2(D#REPNTS),AL4(EDTRPT)    RPT=                                
         DC    AL2(D#MCLXDT),AL4(EDTEXDT)   EXDATE=                             
         DC    AL2(D#MCLXDY),AL4(EDTEXD)    EXDAYS=                             
         DC    AL2(D#CONLIE),AL4(EDTCLE)    CLE=                                
         DC    AL2(D#SPCDAT),AL4(EDTCLDT)   CLD=                                
         DC    AL2(D#SITELO),AL4(EDTSITEL)  SITE=                               
*                                                                               
         DC    AL2(D#SPREP),AL4(EDTSREP)    SREP=                               
         DC    AL2(D#FSINS),AL4(EDTFSI)     FSI=                                
         DC    AL2(D#VIEWS),AL4(EDTPVWS)    PV=                                 
         DC    AL2(D#CLICK),AL4(EDTCTUS)    CT=                                 
*                                                                               
         DC    AL2(D#TSHAPR),AL4(EDTTSHT)   TSHEET - APPROVED                   
         DC    AL2(D#TSHSTA),AL4(EDTTSHT)          - STATUS                     
         DC    AL2(D#REPROQ),AL4(EDTTSHT)          - REP QUALITY                
         DC    AL2(D#TSHNOT),AL4(EDTTSHT)          - PAGE NOTATION              
         DC    AL2(D#TSHCO1),AL4(EDTTSHT)          - COMMENTS 1                 
         DC    AL2(D#TSHCO2),AL4(EDTTSHT)          - COMMENTS 2                 
         DC    AL2(D#TSHCO3),AL4(EDTTSHT)          - COMMENTS 3                 
         DC    AL2(D#TSHCO4),AL4(EDTTSHT)          - COMMENTS 4                 
*                                                                               
         DC    AL2(D#ECPM),AL4(EDTECPM)     ESTIMATED CPM                       
         DC    AL2(D#ACPM),AL4(EDTACPM)     ACTUAL CPM                          
*                                                                               
         DC    AL2(D#INSSTA),AL4(EDTINSTA)  INSERTION STATUS                    
*                                                                               
         DC    AL2(D#ICNUM),AL4(EDTINTC)    INTERNET CONTRACT NUMBER            
         DC    AL2(D#TOTRAT),AL4(EDTINTC)   TOTAL RATE (2 DECIMALS)             
         DC    AL2(D#TOTIMP),AL4(EDTINTC)   TOTAL IMPRESSIONS                   
         DC    AL2(D#TOTCPM),AL4(EDTINTC)   TOTAL CPM                           
         DC    AL2(D#ICINUM),AL4(EDTINTC)   NUMBER OF INSERTION                 
*                                                                               
         DC    AL2(D#GST),AL4(EDTGST)       GST=                                
         DC    AL2(D#PST),AL4(EDTPST)       PST/                                
*                                                                               
         DC    AL2(D#ACHCOD),AL4(EDTACHRG)  ADDTIONAL CHARGE                    
*                                                                               
******** DC    AL2(D#______),AL4(EDTTRAFF)  TRAFF=                              
*                                                                               
         DC    AL2(D#ISSNM),AL4(EDTISSNM)   ISS=                                
*                                                                               
         DC    AL2(D#MATSTA),AL4(EDTINVM)   MATCHING STATUS                     
         DC    AL2(D#DISSTA),AL4(EDTINVM)   DISCREPANCY STATUS                  
*                                                                               
         DC    AL2(D#TEAREC),AL4(EDTITSR)   TEARSHEET RECEIVED                  
*                                                                               
         DC    AL2(D#C2FACT),AL4(EDTCOS2F)  COS2 FACTOR                         
*                                                                               
         DC    AL2(D#PO#PRD),AL4(EDT_PO#)   PURCHASE ORDER #                    
         DC    AL2(D#PO#OLD),AL4(EDT_PO#)   PURCHASE ORDER #                    
*                                                                               
         DC    AL2(D#C2$NEW),AL4(EDTCOS2$)  COS2 $                              
         DC    AL2(D#C2$OLD),AL4(EDTCOS2$)  COS2 $                              
*                                                                               
         DC    X'FF'                                                            
*                                                                               
* FIELDS EDITED IN BUY PROGRAM, BUT NOT DEFINED IN ADBUYER:                     
*                                                                               
* - OPEN RATE                (OR=    )        NOT NEEDED IN ADBUYER             
* - MANUAL IO                (MANIO= )        NOT NEEDED IN ADBUYER             
* - UPLOAD ID                (UPID=  )        NOT NEEDED IN ADBUYER             
* - TRAFFIC STATUS           (TRAFF= )        ?                                 
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKMIDSFD NTR1  BASE=*,LABEL=*      R3 POINTS WORKER ELEM                        
*                                                                               
         CLI   BUYMD,C'M'          MEDIA M?                                     
         BE    CKMSF05                                                          
         CLI   BUYMD,C'S'          MEDIA S?                                     
         BE    CKMSF05                                                          
         CLI   BUYMD,C'T'          MEDIA T?                                     
         BNE   CKMSF10                                                          
CKMSF05  CLC   =AL2(D#SPCDAT),3(R3)                                             
         JE    SETCCNEQ                                                         
         CLC   =AL2(D#ONSDAT),3(R3)                                             
         JE    SETCCNEQ                                                         
         J     SETCCEQ                                                          
*                                                                               
CKMSF10  CLI   BUYMD,C'O'          MEDIA O?                                     
         BNE   CKMSF20                                                          
         CLC   =AL2(D#SPCDAT),3(R3)                                             
         JE    SETCCNEQ                                                         
         CLC   =AL2(D#ONSDAT),3(R3)                                             
         JE    SETCCNEQ                                                         
         J     SETCCEQ                                                          
*                                                                               
CKMSF20  CLI   BUYMD,C'I'          INTERACTIVE?                                 
         JE    CKMSF22                                                          
         CLI   BUYMD,C'L'          SOCIAL?                                      
         JE    CKMSF22                                                          
         CLI   BUYMD,C'B'          MOBILE?                                      
         JE    CKMSF22                                                          
         CLI   BUYMD,C'D'          DIGITAL AUDIO?                               
         JE    CKMSF22                                                          
         CLI   BUYMD,C'V'          NATIONAL VIDEO (NVIDEO)?                     
         JE    CKMSF22                                                          
         CLI   BUYMD,C'W'          LOCAL VIDEO (LVIDEO)?                        
         JE    CKMSF22                                                          
         J     CKMSF30                                                          
*                                                                               
CKMSF22  CLC   =AL2(D#SPCDAT),3(R3)                                             
         JE    SETCCNEQ                                                         
         CLC   =AL2(D#ONSDAT),3(R3)                                             
         JE    SETCCNEQ                                                         
         J     SETCCEQ                                                          
*                                                                               
CKMSF30  DS    0H                  FOR FUTURE USES                              
         J     SETCCEQ                                                          
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* IF MYERR IS X'FF' AND ERRAREA IS X'FF' MESSAGE IS INTERNAL (BUYMSG)           
* OTHERWISE USE GETTEXT                                                         
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
BLDERREL NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R3,VTIA             FIRST 4096 BYTES HAVE WORKER REC             
         LA    R3,4(R3)            POINT TO WORKER ELEM                         
*                                                                               
BDERR20  CLI   0(R3),0             ELEM CODES EXIST?                            
         JNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R3),LQ_RDATQ      RETURNED DATA HEADER ELEM?                   
         JE    *+12                                                             
         BRAS  RE,NXTWFELM                                                      
         J     BDERR20                                                          
*                                                                               
         MVC   WKTHALF,1(R3)       ELEM LENGTH                                  
*                                                                               
         CLI   PCVERSN#,X'04'      HIGHER THAN 4.X.X.X?                         
         JL    BDERR22                                                          
         CLI   DDLINKSW,C'F'       DRAFT INSERTION UPLOAD?                      
         JNE   BDERR22                                                          
         CLC   REQTOKEN,SPACES     HAVE REQUEST TOKEN?                          
         JNH   BDERR22                                                          
         MVI   0(R3),LQ_RAWDQ      REPLY TOKEN                                  
         MVI   1(R3),0                                                          
         MVI   2(R3),6+L'REQTOKEN  LENGTH                                       
         MVC   3(2,R3),=AL2(D#QTOKEN)                                           
         MVI   5(R3),LD_CHARQ      DATA TYPE - EBCDIC CHARACTERS                
         MVC   6(L'REQTOKEN,R3),REQTOKEN                                        
         BRAS  RE,NXTWFELM                                                      
         LH    RE,WKTHALF                                                       
         SHI   RE,L'REQTOKEN                                                    
         STH   RE,WKTHALF          NEW RETURNED DATA HEADER ELEM LENGTH         
*                                                                               
BDERR22  OC    SVMAPCOD,SVMAPCOD   CANNOT FIND FLD NUMBER IN ERROR?             
         BZ    BDERR30                                                          
         MVI   0(R3),LQ_RAWDQ      REPLY ERROR FIELD NUMBER                     
         MVI   1(R3),0                                                          
         MVI   2(R3),8             ERROR ELEM LENGTH                            
         LH    RF,WKTHALF                                                       
         AHI   RF,-8                                                            
         STH   RF,WKTHALF          NEW RETURNED DATA HEADER ELEM LENGTH         
         MVC   3(2,R3),=AL2(D#ERRNUM)                                           
         MVI   5(R3),LD_UBINQ      DATA TYPE - BINARY                           
         MVC   6(2,R3),SVMAPCOD    MAP CODE (WHERE ERROR OCCURED)               
         BRAS  RE,NXTWFELM                                                      
*                                                                               
BDERR30  MVI   0(R3),LQ_RAWDQ      REPLY ERROR MESSGE (TEXT)                    
         MVC   3(2,R3),=AL2(D#ERRDSC)                                           
         MVI   5(R3),LD_CHARQ      DATA TYPE - EBCDIC CHARACTERS                
*                                                                               
         LH    RE,ERRORNUM                                                      
         CHI   RE,X'FF'                                                         
         BNE   BDERR50                                                          
         CLI   ERRAREA,X'FF'                                                    
         BNE   BDERR35                                                          
*                                                                               
BDERR32  OC    BUYMSG,SPACES       MAKE SURE NO TRAILING NULLS                  
         LA    R4,60                                                            
         LA    RF,BUYMSG+60-1      POINT TO LAST CHAR OF MSG                    
         CLI   0(RF),C' '                                                       
         BNE   *+12                                                             
         BCTR  R4,0                TRAILING SPACES ARE STRIPPED                 
         BCTR  RF,0                                                             
         B     *-12                                                             
         BCTR  R4,0                                                             
         CHI   R4,0                                                             
         BH    *+6                                                              
         DC    H'0'                INVALID MSG LENGTH                           
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   6(0,R3),BUYMSG                                                   
         LH    RE,WKTHALF                                                       
         AHI   R4,6+1              6 FOR OVERHEAD AND 1 FOR EX                  
         STCM  R4,3,1(R3)          LENGTH                                       
         SR    RE,R4                                                            
         STH   RE,WKTHALF          NEW RETURNED DATA HEADER ELEM LENGTH         
         B     BDERR80                                                          
*                                                                               
BDERR35  DC    H'0'                UNKNOWN ERROR                                
*                                                                               
BDERR50  LH    R6,ERRORNUM                                                      
         BRAS  RE,GET_ETXT                                                      
         B     BDERR32                                                          
*                                                                               
BDERR80  BRAS  RE,NXTWFELM                                                      
         MVI   0(R3),LQ_RDATQ      RETURNED DATA HEADER ELEM CODE               
         MVC   1(2,R3),WKTHALF     NEW LENGTH                                   
*                                                                               
BDERRX   J     EXIT                                                             
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
BLDDDEL  NTR1  BASE=*,LABEL=*      ONLY 1 DOWNLOAD DATA ELEM IS BUILT           
*                                                                               
         L     R3,VTIA             FIRST 4096 BYTES HAVE WORKER REC             
         LA    R3,4(R3)            POINT TO WORKER ELEM                         
         XC    BDDWORK,BDDWORK                                                  
         XC    BDDABKEY,BDDABKEY                                                
         MVI   BDDSW,0                                                          
*                                                                               
BDDD20   CLI   0(R3),0             ELEM CODES EXIST?                            
         JNE   *+6                                                              
         DC    H'0'                                                             
         CLI   DDLINKSW,C'F'       DRAFT INSERTION UPLOAD?                      
         BE    BDDD25                                                           
         CLI   DDLINKSW,C'N'       NEW INSERTION UPLOAD?                        
         BE    BDDD25                                                           
*                                                                               
         CLI   0(R3),LQ_RQSTQ      REQUEST DATA ELEM?                           
         JNE   BDDD25                                                           
         CLC   3(2,R3),=AL2(D#INSKEY)                                           
         JNE   BDDD25                                                           
         ZICM  RE,1(R3),(3)                                                     
         CHI   RE,255                                                           
         BNH   *+6                                                              
         DC    H'0'                INVALID INSERTION KEY (>255)                 
         CHI   RE,0                                                             
         BH    *+6                                                              
         DC    H'0'                INVALID INSERTION KEY (<0)                   
         STC   RE,BDDWORK                                                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   BDDWORK+1(0),6(R3)                                               
*                                                                               
BDDD25   CLI   LKDRFTSW,C'F'       DRAFT CHANGE MODE?                           
         BNE   BDDD26                                                           
         CLI   0(R3),LQ_RQSTQ      REQUEST DATA ELEM?                           
         JNE   BDDD26                                                           
         CLC   3(2,R3),=AL2(D#ADBKEY)                                           
         JNE   BDDD26                                                           
         ZICM  RE,1(R3),(3)                                                     
         CHI   RE,6+20             "ADBUYER ONLY" KEY IS <20?                   
         BNH   *+6                                                              
         DC    H'0'                INVALID "ADBUYER ONLY" KEY (>20)             
         CHI   RE,6                                                             
         BH    *+6                                                              
         DC    H'0'                INVALID "ADBUYER ONLY" KEY (<0)              
         STC   RE,BDDABKEY                                                      
         AHI   RE,-6-1             6 FOR OVERHEAD AND 1 FOR EX                  
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   BDDABKEY+1(0),6(R3)                                              
*                                                                               
BDDD26   CLI   0(R3),LQ_DLDDQ      DOWLOAD DATA ELEM?                           
         JNE   *+8                                                              
         OI    BDDSW,DLDATABQ      DOWNLOAD DATA ALREADY CONSTRUCTED            
         CLI   0(R3),LQ_RAWDQ      REPLY DATA ELEM?                             
         JNE   BDDD27                                                           
         CLC   3(2,R3),=AL2(D#INSKEY)                                           
         JNE   *+8                                                              
         OI    BDDSW,INSKEYBQ      REPLY INS. KEY ALREADY CONSTRUCTED           
BDDD27   CLI   0(R3),LQ_RDATQ      RETURNED DATA HEADER ELEM?                   
         JE    BDDD30                                                           
         BRAS  RE,NXTWFELM                                                      
         J     BDDD20                                                           
*                                                                               
BDDD30   MVC   BDDHALF,1(R3)       SAVE RETURNED DATA HDR ELEM LENGTH           
         LH    RE,BDDHALF                                                       
         CHI   RE,100              ENOUGH ROOM?                                 
         BNL   *+6                                                              
         DC    H'0'                RETURN RECORD IS TOO SMALL                   
*                                                                               
         TM    BDDSW,DLDATABQ      DOWNLOAD DATA ELEM BUILD?                    
         BO    BDDD35              YES, NO NEED TO BUILD ANOTHER                
*                                                                               
         MVI   0(R3),LQ_DLDDQ      DOWNLOAD DATA ELEM CODE                      
         MVI   1(R3),0                                                          
         MVI   2(R3),5             2ND BYTE OF ELEM LENGTH                      
*                                                                               
         CLI   DDLINKSW,C'K'       IDESK INSERTION UPLOAD?                      
         BE    *+12                                                             
         TM    ABUPLDSW,IDSKUPLQ   IDESK INSERTION UPLOAD?                      
         BZ    *+14                                                             
         MVC   3(2,R3),=AL2(E#IDKRPY)                                           
         B     BDDD33                                                           
*                                                                               
         CLI   DDLINKSW,C'F'       DRAFT INS?                                   
         JNE   *+10                                                             
         MVC   3(2,R3),=AL2(E#INSDER)                                           
         CLI   DDLINKSW,C'N'       NEW INS?                                     
         JNE   *+10                                                             
         MVC   3(2,R3),=AL2(E#INSADD)                                           
         CLI   DDLINKSW,C'C'       CHANGE INS?                                  
         JNE   *+10                                                             
         MVC   3(2,R3),=AL2(E#INSCHA)                                           
         CLI   DDLINKSW,C'D'       DELETE INS?                                  
         JNE   *+10                                                             
         MVC   3(2,R3),=AL2(E#INSDEL)                                           
*                                                                               
BDDD33   LH    RE,BDDHALF                                                       
         AHI   RE,-5               RECALCULATE RETURNED DATE HDR EL LEN         
         STH   RE,BDDHALF                                                       
         BRAS  RE,NXTWFELM                                                      
*                                                                               
BDDD35   CLI   LKDRFTSW,C'F'       DRAFT CHANGE MODE?                           
         BNE   BDDD40                                                           
         MVI   0(R3),LQ_RAWDQ      REPLY DRAFT CHANGE ACTION                    
         MVI   1(R3),0                                                          
         MVI   2(R3),7             LENGTH                                       
         MVC   3(2,R3),=AL2(D#DACTN)                                            
         MVI   5(R3),LD_CHARQ      DATA TYPE - EBCDIC CHARACTERS                
         MVI   6(R3),C'D'                                                       
         BRAS  RE,NXTWFELM                                                      
         LH    RF,BDDHALF                                                       
         AHI   RF,-7               RECALCULATE RETURNED DATA HDR EL LEN         
         STH   RF,BDDHALF                                                       
*                                                                               
         OC    BDDABKEY,BDDABKEY   NEED TO REPLY "ADBUYER ONLY" KEY?            
         BZ    BDDD40                                                           
         MVI   0(R3),LQ_RAWDQ      REPLY "ADBUYER ONLY" KEY                     
         ZIC   RE,BDDABKEY                                                      
         STCM  RE,3,1(R3)          ELEM LENGTH                                  
         LH    RF,BDDHALF                                                       
         SR    RF,RE               RECALCULATE RETURNED DATA HDR EL LEN         
         STH   RF,BDDHALF                                                       
         MVC   3(2,R3),=AL2(D#ADBKEY)                                           
         MVI   5(R3),LD_CHARQ      DATA TYPE - EBCDIC CHARACTERS                
         AHI   RE,-6-1             6 FOR OVERHEAD AND 1 FOR EX                  
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   6(0,R3),BDDABKEY+1  INSERTION KEY                                
         BRAS  RE,NXTWFELM                                                      
*                                                                               
* NOTE THAT REPLY INS KEY ELEM FOR ADD INS UPLOAD IS NOT BUILD HERE             
* BECAUSE ON A SUCCESSFUL ADD, REPLY INS KEY ELEM WILL BE BUILD IN              
* T41117.  REPLY INS KEY ELEM CANNOT BE BUILT IF ERROR OCCURED                  
* WHEN DOING AN ADD (I.E. THERE'S NO SERIAL NUMBER!)                            
*                                                                               
BDDD40   TM    BDDSW,INSKEYBQ      REPLY INS. KEY ELEM BUILD?                   
         BO    BDDD80              YES, NO NEED TO BUILD ANOTHER                
         CLI   DDLINKSW,C'F'       DRAFT INSERTION UPLOAD?                      
         BE    BDDD80                                                           
         CLI   DDLINKSW,C'N'       NEW INSERTION UPLOAD?                        
         BE    BDDD80                                                           
*                                                                               
* NEED TO REPLY INS. KEY FOR CHANGE AND DELETE UPLOAD MODES                     
*                                                                               
BDDD50   MVI   0(R3),LQ_RAWDQ      REPLY INSERTION KEY                          
         ZIC   RE,BDDWORK                                                       
         STCM  RE,3,1(R3)          ELEM LENGTH                                  
         LH    RF,BDDHALF                                                       
         SR    RF,RE               RECALCULATE RETURNED DATE HDR EL LEN         
         STH   RF,BDDHALF                                                       
         MVC   3(2,R3),=AL2(D#INSKEY)                                           
         MVI   5(R3),LD_CHARQ      DATA TYPE - EBCDIC CHARACTERS                
         AHI   RE,-7               6 FOR OVERHEAD AND 1 FOR EX                  
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   6(0,R3),BDDWORK+1   INSERTION KEY                                
         BRAS  RE,NXTWFELM                                                      
*                                                                               
BDDD80   MVI   0(R3),LQ_RDATQ      REBUILD RETURNED DATA HEADER ELEM            
         MVC   1(2,R3),BDDHALF                                                  
*                                                                               
BDDDX    J     EXIT                                                             
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
FNLZTSHT NTR1  BASE=*,LABEL=*      FINALIZE TEARSHEET ELEMS                     
*                                                                               
         CLI   DDLINKSW,C'D'                                                    
         BE    FZTSX               DELETE MODE DOESN'T APPLY HERE               
         CLI   DDLINKSW,C'F'                                                    
         BE    FZTSX               DRAFT INS MODE DOESN'T APPLY HERE            
*                                                                               
         TM    OTHELMSW,TEARSHET   TEARSHEET VALUES EDITED?                     
         BNZ   *+12                YES                                          
         TM    OTHELMSW,TEARSCOM   TEARSHEET COMMENTS EDITED?                   
         BZ    FZTSX               NO TEARSHEET ELEM BUILD, DONE                
*                                                                               
         MVI   WKTBYTE,0                                                        
         CLI   DDLINKSW,C'C'       CHANGE MODE?                                 
         BNE   FZTS25                                                           
         LA    R5,REC+33           NEED TO KNOW WHAT DATA IS CHANGED            
         MVI   ELCODE,X'95'                                                     
         BRAS  RE,NXTELEM                                                       
         BNE   *+8                                                              
         MVI   WKTBYTE,C'Y'        YES, X'95' ELEM FOUND                        
*                                                                               
FZTS25   LA    R6,TSHTELEM                                                      
         USING PTSHTEL,R6                                                       
         MVC   PTSHCDAT,BTODAY     TODAY'S DATE (BINARY)                        
         MVC   PTSHPID(2),SVPID                                                 
         MVC   PTSHBID,BUYNM                                                    
         OC    PTSHBID,SPACES      BUYER'S ID (NO TRAILING NULLS)               
         OC    PTSHPAGE,SPACES                                                  
*                                                                               
         CLI   WKTBYTE,C'Y'        NEED TO CK WITH ORIGINAL VALUES?             
         BNE   FZTS80                                                           
         CLC   PTSHIND1,14(R5)     SPACE DESCP STATUS CHANGED?                  
         BE    *+8                                                              
         OI    PTSHCIN1,X'02'                                                   
         CLC   PTSHIND2,15(R5)     CAPTION STATUS CHANGED?                      
         BE    *+8                                                              
         OI    PTSHCIN1,X'04'                                                   
         CLC   PTSHIND3,16(R5)     POSITION STATUS CHANGED?                     
         BE    *+8                                                              
         OI    PTSHCIN1,X'08'                                                   
         CLC   PTSHIND4,17(R5)     INSERTION DATE STATUS CHANGED?               
         BE    *+8                                                              
         OI    PTSHCIN1,X'10'                                                   
         CLC   PTSHIND5,18(R5)     ZONES STATUS CHANGED?                        
         BE    *+8                                                              
         OI    PTSHCIN1,X'20'                                                   
         CLC   PTSHREPO,23(R5)     REPRODUCTION QUALITY CHANGED?                
         BE    *+8                                                              
         OI    PTSHCIN2,X'01'                                                   
         CLC   PTSHSTAT,13(R5)     STAUS (APPROVED OR NOT) CHANGED?             
         BE    *+8                                                              
         OI    PTSHCIN1,X'01'                                                   
*                                                                               
* NOTE: PTSHCIN2 FOR COMMENT (X'02') IS SET IN TSHEET ROUTINE                   
*                                                                               
FZTS80   LA    R5,NEWREC+33        ADDING TEARSHEET ELEM                        
         MVI   ELCODE,X'95'                                                     
         BRAS  RE,NXTELEM                                                       
         BNE   *+6                                                              
         DC    H'0'                CANNOT HAVE ONE ALREADY!                     
         LA    R5,NEWREC+33                                                     
         MVI   ELCODE,X'FF'        GO TO END OF RECORD                          
         BRAS  RE,NXTELEM                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   TSHTELEM,X'95'                                                   
         BE    *+6                                                              
         DC    H'0'                BAD TEARSHEET ELEM IS BUILT                  
*                                                                               
         GOTO1 VRECUP,DMCB,(1,NEWREC),TSHTELEM,(R5)                             
*                                                                               
         LA    R5,NEWREC+33        ADDING TEARSHEET COMMENT ELEM(S)             
         MVI   ELCODE,X'69'                                                     
         BRAS  RE,NXTELEM                                                       
         BNE   *+6                                                              
         DC    H'0'                CANNOT HAVE ONE ALREADY!                     
         LA    R5,NEWREC+33                                                     
         LA    R2,TSHTC1EL                                                      
         LA    R3,4                4 SETS OF TSHEET COMMENTS TO PROC            
FZTS85H  OC    2(66,R2),2(R2)                                                   
         BZ    FZTS85K                                                          
         LA    R5,NEWREC+33                                                     
         MVI   ELCODE,X'FF'        GO TO END OF RECORD                          
         BRAS  RE,NXTELEM                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 VRECUP,DMCB,(1,NEWREC),(R2),(R5)                                 
*                                                                               
FZTS85K  LA    R2,68(R2)           NEXT COMMENT                                 
         BCT   R3,FZTS85H                                                       
*                                                                               
FZTSX    J     EXIT                                                             
*                                                                               
         DROP  RB,R6                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
FNLZICON NTR1  BASE=*,LABEL=*      FINALIZE INTERNET CONTRACT ELEM              
*                                                                               
         CLI   DDLINKSW,C'D'                                                    
         BE    FZICX               DELETE MODE DOESN'T APPLY HERE               
         CLI   DDLINKSW,C'F'                                                    
         BE    FZICX               DRAFT INS MODE DOESN'T APPLY HERE            
*                                                                               
         TM    OTHELMSW,INTRNCON   INTERNET CONTRACT DATA EDITED?               
         BZ    FZICX                                                            
*                                                                               
         LA    RE,INTCONEL                                                      
         USING PICONEL,RE                                                       
         CLC   PICCONN,SPACES      INTERNET CONTRACT CODE PRESENT?              
         BNE   FZIC50                                                           
         MVI   ERRORNUM+1,MSSNGERR MISSING INPUT FIELD                          
         MVC   SVMAPCOD,=AL2(D#ICNUM)                                           
         TM    WKERRSW,ERRORFND    DOWNLOAD DATA ELEM & INS. KEY BUILT?         
         BO    *+12                                                             
         OI    WKERRSW,ERRORFND    ERROR ENCOUNTERED                            
         BRAS  RE,BLDDDEL                                                       
         BRAS  RE,BLDERREL         BUILD ERROR ELEM                             
         J     XCERRNUM                                                         
*                                                                               
* ADDING INTERNET CONTRACT ELEM                                                 
*                                                                               
FZIC50   LA    R5,NEWREC+33                                                     
         MVI   ELCODE,X'FF'        GO TO END OF RECORD                          
         BRAS  RE,NXTELEM                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 VRECUP,DMCB,(1,NEWREC),INTCONEL,(R5)                             
*                                                                               
FZICX    J     EXIT                                                             
*                                                                               
         DROP  RB,RE                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
FNLZINVM NTR1  BASE=*,LABEL=*      FINALIZE INV MATCHING STATUSES ELEM          
*                                                                               
         CLI   DDLINKSW,C'D'                                                    
         BE    FZINVMX             DELETE MODE DOESN'T APPLY HERE               
         CLI   DDLINKSW,C'F'                                                    
         BE    FZINVMX             DRAFT INS MODE DOESN'T APPLY HERE            
*                                                                               
         TM    OTHELMSW,INVMSTSQ   INVOICE MATCHING STATUSES EDITED?            
         BZ    FZINVMX                                                          
*                                                                               
         B     FZINVM50            IF NEED TO CK ERROR, COMPLETE BELOW          
*                                                                               
******** LA    RE,INVMSTEL                                                      
******** USING PBMATELD,RE                                                      
******** MVI   ERRORNUM+1,MSSNGERR MISSING INPUT FIELD                          
******** MVC   SVMAPCOD,=AL2(D#MATSTA)                                          
******** TM    WKERRSW,ERRORFND    DOWNLOAD DATA ELEM & INS. KEY BUILT?         
******** BO    *+12                                                             
******** OI    WKERRSW,ERRORFND    ERROR ENCOUNTERED                            
******** BRAS  RE,BLDDDEL                                                       
******** BRAS  RE,BLDERREL         BUILD ERROR ELEM                             
******** J     XCERRNUM                                                         
******** DROP  RE                                                               
*                                                                               
* ADDING INVOICE MATCHING STATUSES ELEM                                         
*                                                                               
FZINVM50 LA    R5,NEWREC+33                                                     
         MVI   ELCODE,X'FF'        GO TO END OF RECORD                          
         BRAS  RE,NXTELEM                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 VRECUP,DMCB,(1,NEWREC),INVMSTEL,(R5)                             
*                                                                               
FZINVMX  J     EXIT                                                             
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
FNLZERRS NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   FZERSW1,0           INIT FINAL ERROR SWITCH                      
         XC    ERRORNUM,ERRORNUM   CLR ERROR NUMBER                             
*                                                                               
         LA    R5,NEWREC+33                                                     
         MVI   ELCODE,X'89'        EXTENSION DAYS ELEM CODE                     
         BRAS  RE,NXTELEM                                                       
         BNE   FZER10D                                                          
         CLI   2(R5),C'X'          SPECIAL DELETE CODE?                         
         BE    FZER10D                                                          
         OI    FZERSW1,X89FOUND                                                 
         LA    R5,NEWREC+33                                                     
         MVI   ELCODE,X'96'        EXTENSION DATE ELEM CODE                     
         BRAS  RE,NXTELEM                                                       
         BNE   *+14                                                             
         CLC   2(3,R5),=3X'FF'     SPECIAL DELETE CODE?                         
         BE    FZER10D                                                          
         LA    R5,REC+33                                                        
         BRAS  RE,NXTELEM                                                       
         BNE   *+8                                                              
         OI    FZERSW1,X96FOUND    FOUND IN REC                                 
*                                                                               
FZER10D  LA    R5,NEWREC+33                                                     
         MVI   ELCODE,X'96'        EXTENSION DATE ELEMCODE                      
         BRAS  RE,NXTELEM                                                       
         BNE   FZER10P                                                          
         CLC   2(3,R5),=3X'FF'     SPECIAL DELETE CODE?                         
         BE    FZER10P                                                          
         OI    FZERSW1,X96FOUND                                                 
         LA    R5,NEWREC+33                                                     
         MVI   ELCODE,X'89'        EXTENSION DAYS ELEM CODE                     
         BRAS  RE,NXTELEM                                                       
         BNE   *+12                                                             
         CLI   2(R5),C'X'          SPECIAL DELETE CODE?                         
         BE    FZER10P                                                          
         LA    R5,REC+33                                                        
         BRAS  RE,NXTELEM                                                       
         BNE   *+8                                                              
         OI    FZERSW1,X89FOUND    FOUND IN REC                                 
*                                                                               
FZER10P  TM    FZERSW1,X89FOUND+X96FOUND                                        
         BO    FZERR001                                                         
*                                                                               
FZER20   LA    R5,NEWREC+33                                                     
         MVI   ELCODE,X'A6'        ISSUE NAME ELEM CODE                         
         BRAS  RE,NXTELEM                                                       
         BNE   FZER20D                                                          
         CLI   2(R5),X'FF'         SPECIAL DELETE CODE?                         
         BE    FZER20D                                                          
         OI    FZERSW1,XA6FOUND                                                 
         LA    R5,NEWREC+33                                                     
         CLI   PBDIDAT2-PBDELEM(R5),X'FF'                                       
         BE    FZER20D                                                          
         OC    PBDIDAT2-PBDELEM(L'PBDIDAT2,R5),PBDIDAT2-PBDELEM(R5)             
         BZ    FZER20D                                                          
         OI    FZERSW1,X2DFOUND    2ND INS DATE FOUND IN REC                    
*                                                                               
FZER20D  LA    R5,NEWREC+33                                                     
         CLI   PBDIDAT2-PBDELEM(R5),X'FF'                                       
         BE    FZER20P                                                          
         OC    PBDIDAT2-PBDELEM(L'PBDIDAT2,R5),PBDIDAT2-PBDELEM(R5)             
         BZ    FZER20P                                                          
         OI    FZERSW1,X2DFOUND                                                 
         LA    R5,NEWREC+33                                                     
         MVI   ELCODE,X'A6'        ISSUE NAME ELEM CODE                         
         BRAS  RE,NXTELEM                                                       
         BNE   *+12                                                             
         CLI   2(R5),X'FF'         SPECIAL DELETE CODE?                         
         BE    FZER20P                                                          
         LA    R5,REC+33                                                        
         BRAS  RE,NXTELEM                                                       
         BNE   *+8                                                              
         OI    FZERSW1,XA6FOUND    FOUND IN REC                                 
*                                                                               
FZER20P  TM    FZERSW1,XA6FOUND+X2DFOUND                                        
         BO    FZERR002                                                         
*                                                                               
FZER30   DS    0H                  FOR FUTURE USES                              
*                                                                               
         J     EXIT                                                             
*                                                                               
FZERR001 MVI   ERRORNUM+1,EXDERR2  CANNOT HAVE BOTH EXDAYS/EXDATE               
         BRAS  RE,RPLY_ERR                                                      
         B     FZER20              CK NEXT POSSIBLE ERROR                       
*                                                                               
FZERR002 MVI   ERRORNUM+1,ISSNMERR CANNOT HAVE BOTH 2ND INSDT & ISSNM           
         BRAS  RE,RPLY_ERR                                                      
         B     FZER30              CK NEXT POSSIBLE ERROR                       
*                                                                               
RPLY_ERR LR    R0,RE                                                            
         OC    ERRORNUM,ERRORNUM   HAVE ERROR NUMBER?                           
         JZ    RPLY_ERX                                                         
         TM    WKERRSW,ERRORFND    DOWNLOAD DATA ELEM & INS. KEY BUILT?         
         JNZ   *+12                                                             
         OI    WKERRSW,ERRORFND    ERROR ENCOUNTERED                            
         BRAS  RE,BLDDDEL                                                       
         BRAS  RE,BLDERREL         BUILD ERROR ELEM                             
         XC    ERRORNUM,ERRORNUM   CLR ERROR NUMBER FOR NEXT ROUND              
RPLY_ERX LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKBUYCLE NTR1  BASE=*,LABEL=*      CHECK CONTRACT LINE EQUIVALENCY              
*                                                                               
         CLI   BUYMD,C'N'                                                       
         BNE   BUYCLE_E            ONLY ALLOWED FOR NEWSPAPER                   
*                                                                               
         CLI   LKDRFTSW,C'F'       DRAFT MODE?                                  
         BE    BUYCLE_X            USE SPACE DESP CALC'D CLE                    
*                                                                               
         CLC   PBDSPACE(2),=C'* '                                               
         BNH   BUYCLE_E                                                         
         CLC   PBDSPACE(2),=X'7B00'                                             
         BE    BUYCLE_E            TREAT AS NONE-SPACE BUY                      
         CLC   PBDSPACE(2),=C'# '                                               
         BE    BUYCLE_E            TREAT AS NON-SPACE BUY                       
*                                                                               
         MVI   ERRORNUM+1,CLERQERR DEFAULT ERROR                                
*                                                                               
         LA    R4,0(R2)            POINT TO INPUT                               
         LH    R5,HALF             INPUT LENGTH                                 
         LTR   R5,R5                                                            
         JNP   EXIT                                                             
         BRAS  RE,CSTLNS1                                                       
         CP    PBDUNITS,=P'0'                                                   
         BNE   *+10                                                             
         ZAP   PBDUNITS,=P'-1'     SET 0 TO -1 TO PREVENT LOOKUP                
*                                                                               
BUYCLE_X J     EXIT                                                             
*                                                                               
BUYCLE_E MVI   ERRORNUM+1,CLEERR   CLE NOT ALLOWRD FOR THIS SPACE DESP          
         J     EXIT                                                             
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDTSR    NTR1  BASE=*,LABEL=*      EDIT  SPECIAL REP (PBSREPEL)                 
*                                                                               
         CLI   SVTRCODE,C'B'       BUYING?                                      
         BE    EDTSR2                                                           
*                                                                               
         CLC   =C'0000',0(R2)      SREP=0000 (TO REMOVE SREP)?                  
         JNE   EDTSR1                                                           
         BRAS  RE,CKCHGMOD         CHANGE MODE?                                 
         JE    *+6                                                              
         DC    H'0'                                                             
         MVI   ELCODE,X'80'                                                     
         BRAS  RE,NXTELEM                                                       
         JNE   EDTSRX              DONE, NO SREP TO REMOVE                      
*                                                                               
* MAKE SURE PAYMENTS NET TO ZERO                                                
*                                                                               
EDTSR1   GOTO1 VGETINS,DMCB,REC,PVALUES,REC+7                                   
         OC    PGROSS,PGROSS                                                    
         BZ    EDTSR2                                                           
         TM    ABUPLDSW,IDSKUPLQ   PRISMA INSERTION UPLOAD?                     
         JNZ   XCERRNUM            SUPPRESS PRISMA SREP ERROR ON CHG            
         MVI   ERRORNUM+1,SRPDERR                                               
         B     EDTSRX                                                           
*                                                                               
EDTSR2   LA    R4,0(R2)            POINT TO INPUT                               
         MVI   ERRORNUM+1,INVERR                                                
         LH    R5,HALF                                                          
         LTR   R5,R5                                                            
         BNP   EDTSRX                                                           
         CHI   R5,4                                                             
         BH    EDTSRX                                                           
         LR    R1,R5               SAVE LENGTH                                  
*                                                                               
EDTSR5   CLI   0(R4),C'0'          CHK FOR NUMERICS                             
         BL    EDTSRX                                                           
         CLI   0(R4),C'9'                                                       
         BH    EDTSRX                                                           
         LA    R4,1(R4)                                                         
         BCT   R1,EDTSR5                                                        
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R2)                                                      
         OI    DUB,X'0F'                                                        
         UNPK  WORK(4),DUB                                                      
         CLC   WORK(4),=C'0000'                                                 
         BNE   EDTSR8                                                           
*                                                                               
         CLI   SVTRCODE,C'B'       BUYING?                                      
         JE    XCERRNUM            IGNORE SREP=0000 ON NEW BUYS                 
*                                                                               
         MVI   WORK,C'X'           SPECIAL DELETE CODE                          
         B     EDTSR10                                                          
*                                                                               
EDTSR8   XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),BUYMD      MEDIA                                        
         MVI   KEY+3,X'11'                                                      
         MVC   KEY+4(4),WORK                                                    
         MVC   KEYSAVE(L'KEY),KEY                                               
*                                                                               
         BRAS  RE,PRT_RDHI                                                      
         CLC   KEY(9),KEYSAVE                                                   
         BE    EDTSR10             FOUND - OK                                   
*                                                                               
         MVI   ERRORNUM+1,NFNDERR                                               
         B     EDTSRX                                                           
*                                                                               
EDTSR10  LA    R5,NEWREC+33        NOW STORE IN PBSREPEL                        
         MVI   ELCODE,X'80'                                                     
         BRAS  RE,NXTELEM                                                       
         BNE   *+8                 MUST ADD ONE                                 
         J     INVFLDER            CAN'T ALREADY HAVE REP ELEM                  
*                                                                               
         XC    WKBUYELM,WKBUYELM                                                
         MVC   WKBUYELM+00(02),=X'800A'                                         
         MVC   WKBUYELM+02(04),WORK                                             
         BRAS  RE,ADDBUYEL                                                      
         J     XCERRNUM                                                         
*                                                                               
EDTSRX   J     EXIT                                                             
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CSTLNS1  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   ERRORNUM+1,INVERR   DEFAULT ERROR CODE                           
*                                                                               
         MVI   PBDUIND,0                                                        
         ZAP   PBDUNITS,=P'0'                                                   
         ZAP   PBDCLMS,=P'0'                                                    
*                                                                               
CDTLNS1  STM   R4,R5,DUB           SAVE PTR/LEN                                 
CDTLNS2  CLI   0(R4),C'0'          VERIFY NUMERIC                               
         BL    CDTLNS3                                                          
         CLI   0(R4),C'9'                                                       
         BH    CDTLNSX                                                          
         LA    R4,1(R4)                                                         
         BCT   R5,CDTLNS2                                                       
*                                                                               
CDTLNS3  L     RE,DUB              START OF NUMBER                              
         SR    R4,RE               R4 = LENGTH                                  
         BNP   CDTLNSX                                                          
         BCTR  R4,R0                                                            
         EX    R4,CPACKLN                                                       
         CP    WORK(5),=P'99999'   MAX LINES OR INCHES                          
         BH    CDTLNSX             THAT CAN BE CARRIED IN PBDUNITS              
         ZAP   PBDUNITS,WORK(5)                                                 
*                                                                               
         MVI   PBDUIND,C'L'                                                     
         LTR   R5,R5                                                            
         BZ    CDTLNS8             NO MORE INPUT                                
         LA    R4,1(RE,R4)         R4 = BYTE PAST NUMBER                        
         CLI   0(R4),C'L'                                                       
         BE    CDTLNS4                                                          
         CLI   0(R4),C'/'                                                       
         BE    CDTLNS5                                                          
         MVI   PBDUIND,C'I'                                                     
         CLI   0(R4),C'I'                                                       
         BE    CDTLNS4                                                          
         CLI   0(R4),C'X'          NEW SAU NNXNN.NN                             
*                                  COLUMNS X INCHES (2 DECIMALS)                
         BNE   CDTLNS3C            NO                                           
         CP    PBDUNITS,=P'14'     MAX COLS =14                                 
         BH    CDTLNSX             TREAT AS SPACE                               
         CP    PBDUNITS,=P'0'                                                   
         BNH   CDTLNSX                                                          
         LA    R4,1(R4)                                                         
         BCT   R5,*+8              NO MORE INPUT                                
         B     CDTLNSX             NO MUST BE SPACE                             
         CLC   0(2,R4),=C'FD'                                                   
         BNE   CDTLNS3B                                                         
         CLI   2(R4),C' '                                                       
         BNL   CDTLNSX             INPUT AFTER FD                               
         L     R5,APUBIO                                                        
*                                                                               
         USING PUBKEY,R5                                                        
         CLI   PUBKCOD,X'81'       SEE IF PUB THERE                             
         BE    CDTLNS32                                                         
         DROP  R5                                                               
*                                                                               
         XC    KEY,KEY             MUST REREAD PUB                              
         MVC   KEY+27(4),SVPUBDA                                                
*                                                                               
         MVC   SV_AREC_,AREC                                                    
         MVC   AREC,APUBIO                                                      
         BRAS  RE,PUB_GETR                                                      
         MVC   AREC,SV_AREC_                                                    
*                                                                               
         MVC   BYTE,DMCB+8                                                      
         NC    BYTE,DMOUTBTS                                                    
         BZ    *+6                                                              
         DC    H'0'                MUST FIND PUB                                
*                                                                               
CDTLNS32 LA    R5,33(R5)                                                        
         MVI   ELCODE,X'20'                                                     
         BRAS  RE,NXTELEM                                                       
         BE    CDTLNS3A                                                         
         B     CDTLNSX             CAN'T FIND PROD ELEM                         
         USING PUBGENEL,R5                                                      
*                                                                               
CDTLNS3A OC    PUBFD,PUBFD         SEE IF I HAVE FD                             
         BZ    CDTLNSX             NO - TREAT AS SPACE                          
         ZAP   PBDCLMS,PBDUNITS                                                 
         ZAP   DUB,PBDUNITS                                                     
         MP    DUB,PUBFD                                                        
         CP    DUB,=P'99999'       MAX IS 999.99 COL INCHES                     
         BH    CDTLNSX                                                          
         ZAP   PBDUNITS,DUB                                                     
         NI    PBDUIND,X'BF'       MAKE I LOWER CASE                            
         B     CDTLNS8                                                          
         DROP  R5                                                               
*                                                                               
CDTLNS3B GOTO1 VCASHVAL,DMCB,(2,0(R4)),(R5)                                     
         CLI   DMCB,X'FF'                                                       
         BE    CDTLNSX                                                          
         OC    DMCB+4(4),DMCB+4                                                 
         BZ    CDTLNSX                                                          
         ZAP   PBDCLMS,PBDUNITS                                                 
         L     R0,DMCB+4                                                        
         CVD   R0,DUB                                                           
         CP    DUB,=P'0'           CAN'T BE NEGATIVE                            
         BNH   CDTLNSX                                                          
         MP    DUB,PBDCLMS                                                      
         CP    DUB,=P'99999'       MAX COLUMN INCHES                            
         BH    CDTLNSX                                                          
         NI    PBDUIND,X'BF'       MAKE LOWER CASE I                            
         ZAP   PBDUNITS,DUB                                                     
         B     CDTLNS8                                                          
*                                                                               
CDTLNS3C CLI   0(R4),C'.'          CK FOR DECIMAL POINT                         
         BNE   CDTLNSX             TREAT AS SPACE                               
         BCT   R5,*+8                                                           
         B     CDTLNSX             NO MORE INPUT                                
         LA    R4,1(R4)                                                         
         CLI   0(R4),C'I'                                                       
         BNE   CDTLNS3E                                                         
         BCT   R5,*+8                                                           
         B     CDTLNS8             TREAT NN.I AS NN (NO DECIMALS)               
*                                                                               
         B     CDTLNSX             INPUT AFTER I TREAT AS SPACE                 
*                                                                               
CDTLNS3E ST    R4,DUB                                                           
*                                                                               
CDTLNS3F CLI   0(R4),C'I'                                                       
         BE    CDTLNS3G                                                         
         CLI   0(R4),C'0'                                                       
         BL    CDTLNSX                                                          
         CLI   0(R4),C'9'                                                       
         BH    CDTLNSX                                                          
         LA    R4,1(R4)                                                         
         BCT   R5,CDTLNS3F                                                      
         B     CDTLNSX             IF DOESN'T END WITH I ASSUME SPACE           
*                                                                               
CDTLNS3G BCTR  R5,0                MUST DECREMENT R5                            
         LTR   R5,R5                                                            
         BNZ   CDTLNSX             MORE INPUT - TREAT AS SPACE                  
         L     RE,DUB              START OF NUMBER                              
         SR    R4,RE               R4 = LENGTH                                  
         BCTR  R4,R0               ADJUST FOR I                                 
         EX    R4,CPACKLN                                                       
         ZAP   DUB,WORK(5)                                                      
         CP    DUB,=P'0'           SEE IF NN.00  INPUT                          
         BE    CDTLNS8             TREAT AS NNI                                 
         XC    WORK(13),WORK       BUILD LINE FOR CASHVAL                       
         LH    R5,HALF             DATA LENGTH                                  
         LA    R4,0(R2)                                                         
*                                                                               
CDTLNS3I BCTR  R5,0                                                             
         BCTR  R5,0                SO I WON'T MOVE THE I                        
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(R4)                                                    
         LA    R5,1(R5)            RESTORE FROM THE EX                          
*                                                                               
         GOTO1 VCASHVAL,DMCB,(2,WORK),(R5)                                      
         CLI   DMCB,X'FF'                                                       
         BE    CDTLNSX             IF ERROR TREAT AS SPACE                      
         L     R0,DMCB+4                                                        
         CVD   R0,DUB                                                           
         CP    DUB,=P'99999'                                                    
         BH    CDTLNSX             CAN'T FIT IN PBDUNITS                        
         ZAP   PBDUNITS,DUB                                                     
         NI    PBDUIND,X'BF'       LOWER CASE 'I'                               
         B     CDTLNS8                                                          
*                                                                               
CDTLNS4  BCT   R5,*+8              NO MORE INPUT AFTER L OR I                   
         B     CDTLNS8                                                          
         CLI   1(R4),C'/'          '/'  MUST BE NEXT                            
         BNE   CDTLNSX                                                          
         LA    R4,1(R4)                                                         
*                                                                               
CDTLNS5  LA    R4,1(R4)                                                         
*                                                                               
CDTLNS6  BCT   R5,*+8              NO MORE INPUT                                
         B     CDTLNSX                                                          
         ST    R4,DUB              START OF NUMBER                              
*                                                                               
CDTLNS6A CLI   0(R4),C'0'                                                       
         BL    CDTLNSX                                                          
         CLI   0(R4),C'9'                                                       
         BH    CDTLNSX                                                          
         LA    R4,1(R4)                                                         
         BCT   R5,CDTLNS6A                                                      
         L     RE,DUB              START OF NUMBER                              
         SR    R4,RE               R4 = LENGTH                                  
         BCTR  R4,R0                                                            
         EX    R4,CPACKLN                                                       
         CP    WORK(5),=P'999'     MAX COLUMNS IS 999                           
         BH    CDTLNSX             CAN'T FIT IN PDBCLMS                         
         MVI   ERRORNUM+1,DVSERR   DIVISIBILITY ERROR                           
         ZAP   PBDCLMS,WORK(5)                                                  
         BZ    CDTLNSX                                                          
         ZAP   DUB,PBDUNITS                                                     
         BZ    CDTLNSX                                                          
         DP    DUB,PBDCLMS                                                      
         CP    DUB+8-L'PBDCLMS(L'PBDCLMS),=P'0'                                 
         BNE   CDTLNSX                                                          
*                                                                               
CDTLNS8  J     XCERRNUM                                                         
*                                                                               
CDTLNSX  J     EXIT                                                             
*                                                                               
CPACKLN  PACK  WORK(5),0(0,RE)                                                  
*                                                                               
         DROP  RB                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VALPLCOS NTR1  BASE=*,LABEL=*      VALIDATE PLANNED COST                        
*                                                                               
         XC    WKTELEM,WKTELEM     PLANNED COST INPUT DATA                      
         XC    WKBUYELM,WKBUYELM   VALIDATED PLANNED COST ELEM                  
P        USING BYPCELD,WKBUYELM                                                 
*                                                                               
         CLI   DDLINKSW,0          ADBUYER UPLOAD?                              
         BE    VPLCO12                                                          
         LH    RE,HALF             INPUT LENGTH                                 
         LTR   RE,RE                                                            
         BZ    VPLCO14                                                          
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WKTELEM(0),6(R3)                                                 
         B     VPLCO14                                                          
*                                                                               
VPLCO12  L     RE,SVPARM_3         ADDRESS OF OVERRIDING PLANNED COST           
         MVC   WKTELEM(15),0(RE)                                                
*                                                                               
VPLCO14  CLC   WKTELEM(15),SPACES  HAVE PLANNED COST INPUT DATA?                
         BNH   VPLCO70                                                          
         CLC   =C'NONE',WKTELEM    REMOVE PLANNED COST?                         
         BE    VPLCO70                                                          
         CLC   =C'COPYRATE',WKTELEM                                             
         BNE   *+12                                                             
         MVI   WKBUYELM,X'FF'      INDICATE TO USE C'COPYRATE' FEATURE          
         B     VPLCO_X                                                          
*                                                                               
         MVI   P.BYPCNIND,0                                                     
         MVI   P.BYPCIND,C' '                                                   
         ZAP   P.BYPCCST,=P'0'                                                  
*                                                                               
         LA    RF,WKTELEM+15-1                                                  
         BRAS  RE,LAST_CHR                                                      
         LA    R5,WKTELEM                                                       
         LR    R6,R5               POINT TO START OF INPUT                      
         SR    RF,R5                                                            
         LR    R5,RF                                                            
         AHI   R5,1                INPUT LENGTH                                 
         GOTO1 VCASHVAL,DMCB,(R6),(R5)                                          
         CLI   0(R1),X'FF'                                                      
         BNE   VPLCO64                                                          
*                                                                               
VPLCO30  CLI   0(R6),C'S'          GROSS=NET?                                   
         BNE   VPLCO32                                                          
         MVI   P.BYPCIND,C'S'                                                   
         LA    R6,1(R6)                                                         
         BCTR  R5,0                                                             
*                                                                               
VPLCO32  CLI   0(R6),C'C'          COMMISSION ONLY?                             
         BNE   VPLCO34                                                          
         CLI   P.BYPCIND,C' '      ANY INPUT?                                   
         JNE   VPLCO_E1                                                         
         MVI   P.BYPCIND,C'C'                                                   
         LA    R6,1(R6)                                                         
         BCTR  R5,0                                                             
*                                                                               
VPLCO34  CLI   0(R6),C'N'          NET TO BE GROSSED UP?                        
         BNE   VPLCO35                                                          
         MVI   P.BYPCNIND,C'N'                                                  
         LA    R6,1(R6)                                                         
         BCTR  R5,0                                                             
         B     VPLCO30                                                          
*                                                                               
VPLCO35  CLI   0(R6),C'R'          ROADSIDE?                                    
         BNE   VPLCO36                                                          
         MVI   P.BYPCIND,C'R'                                                   
         LA    R6,1(R6)                                                         
         BCTR  R5,0                                                             
         B     VPLCO30                                                          
*                                                                               
VPLCO36  CLI   SVESPROF+28,C'C'    'C' RATE ESTIMATE?                           
         BNE   VPLCO38                                                          
         CLI   P.BYPCIND,C'C'      'C' INPUT?                                   
         BE    VPLCO40                                                          
         CLI   P.BYPCIND,C' '      ANY INPUT?                                   
         JNE   VPLCO_E1                                                         
         MVI   P.BYPCIND,C'C'      NO INPUT - SET TO 'C'                        
         B     VPLCO40                                                          
*                                                                               
VPLCO38  CLI   P.BYPCIND,C'C'      'C' RATE ONLY FOR 'C' RATE EST?              
         JE    VPLCO_E1                                                         
*                                                                               
VPLCO40  GOTO1 VCASHVAL,DMCB,(R6),(R5)                                          
         CLI   0(R1),X'FF'                                                      
         JE    VPLCO_E1                                                         
*                                                                               
VPLCO64  L     R0,4(R1)                                                         
         C     R0,=F'999999999'    MAX 9,999,999.99?                            
         JH    VPLCO_E1                                                         
         CVD   R0,DUB                                                           
         ZAP   P.BYPCCST,DUB                                                    
         CP    P.BYPCCST,=P'0'     'FREE' PLANNED COST?                         
         BE    VPLCO66                                                          
*                                                                               
         CLI   SVTRCODE,C'C'       CHANGING INSERTION?                          
         BNE   VPLCO65                                                          
         CLI   P.BYPCNIND,C'N'     ENTERED AS NET?                              
         BNE   VPLCO65                                                          
         CP    PBDCOS,=P'0'        RATE IS FREE?                                
         BE    *+14                                                             
         CP    PBDCOS,=P'1'        RATE IS 0.01 (WILL BE SET TO 0)?             
         BNE   VPLCO65                                                          
         CP    REC+33+(PBDACP-PBDELEM)(L'PBDACP),=P'0'                          
         BE    VPLCO65                                                          
         CP    REC+33+(PBDACP-PBDELEM)(L'PBDACP),=P'1'                          
         BE    VPLCO65                                                          
         ZAP   DUB,P.BYPCCST                                                    
         CVB   R1,DUB                                                           
         M     R0,=F'100000'                                                    
         ZAP   DUB,REC+33+(PBDACP-PBDELEM)(L'PBDACP)                            
         CVB   RF,DUB                                                           
         S     RF,=F'100000'       =NET PCT                                     
         LCR   RF,RF                                                            
         BNP   VPLCO65                                                          
         SLDA  R0,1                                                             
         DR    R0,RF                                                            
         LTR   R1,R1                                                            
         BNP   *+8                                                              
         AHI   R1,1                                                             
         SRA   R1,1                                                             
         CVD   R1,DUB              RETURN GROSS                                 
         ZAP   P.BYPCCST,DUB                                                    
*                                                                               
VPLCO65  CP    PBDCOS,=P'0'        RATE IS FREE?                                
         BE    VPLCO66                                                          
         CP    PBDCOS,=P'1'        RATE IS 0.01 (WILL BE SET TO 0)?             
         BE    VPLCO66                                                          
         CLC   P.BYPCIND,PBDCOSIN  PC COST INDICATORS SAME AS RATE?             
         JNE   VPLCO_E2                                                         
         CLC   P.BYPCNIND,PBDCTYP  PC COST INDICATORS SAME AS RATE?             
         JNE   VPLCO_E2                                                         
         CLC   P.BYPCTYP,PBDCOSTY  PC COST INDICATORS SAME AS RATE?             
         JNE   VPLCO_E2                                                         
*                                                                               
VPLCO66  MVI   P.BYPCELM,BYPCIDQ   NEW PLANNED COST ELEM CODE & LENGTH          
         MVI   P.BYPCLEN,BYPCELQ                                                
         XC    PBDPLCOS,PBDPLCOS   CLEAR OLD PLANNED COST DATA                  
         B     VPLCO_X                                                          
*                                                                               
VPLCO70  MVI   ELCODE,BYPCIDQ                                                   
         LA    R5,NEWREC+33                                                     
         BRAS  RE,NXTELEM          PLANNED COST ELEM FOUND?                     
         BE    *+20                                                             
         CLI   SVTRCODE,C'B'       BUYING?                                      
         BE    VPLCO_X                                                          
         MVI   P.BYPCIND,C'X'      SPECIAL ELEM DELETION CODE                   
         B     VPLCO66                                                          
         MVI   2(R5),C'X'          SPECIAL ELEM DELETION CODE                   
*                                                                               
VPLCO_X  J     SETCCEQ                                                          
*                                                                               
VPLCO_E1 MVC   ERRORNUM,=AL2(INVRTERR)                                          
         J     SETCCNEQ                                                         
VPLCO_E2 MVC   ERRORNUM,=AL2(RTINCONS)                                          
         J     SETCCNEQ                                                         
*                                                                               
LAST_CHR CLI   0(RF),0                                                          
         JE    *+8                                                              
         CLI   0(RF),C' '                                                       
         JH    *+10                                                             
         BCTR  RF,0                                                             
         J     *-18                                                             
         BR    RE                                                               
*                                                                               
         DROP  P                                                                
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GLOBALS  DS    0D                                                               
         LTORG                                                                  
         DROP                                                                   
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE PPBUYWRK1                                                      
         EJECT                                                                  
*                                                                               
         ORG   NEWREC              MAP BUY RECORD TO NEWREC                     
*                                                                               
       ++INCLUDE PPBUYWRK2                                                      
         PRINT ON                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
WORK18D  DSECT                                                                  
*                                                                               
SPACES   DS    CL255               C' '                                         
*                                                                               
SVPARM_3 DS    F                                                                
SVPARM_4 DS    F                                                                
RELOBY18 DS    F                                                                
SV_REG_3 DS    F                                                                
SV_REG_E DS    F                                                                
*                                                                               
AWKAIO1  DS    A                                                                
APO#TAB  DS    A                   ADDRESS OF PO# TABLE                         
*                                                                               
SV_AREC_ DS    A                                                                
SVWRKELM DS    F                                                                
WKTFULL  DS    F                                                                
WKTFULL2 DS    F                                                                
*                                                                               
WKTHALF  DS    H                                                                
WKTHALF2 DS    H                                                                
*                                                                               
WKTBYTE  DS    X                                                                
WKTBYTE2 DS    X                                                                
WKTDUB   DS    D                                                                
WKTDUB2  DS    D                                                                
WKTPL4   DS    PL4                                                              
WKTPL12  DS    PL12                                                             
*                                                                               
WKTELEM  DS    CL255                                                            
*                                                                               
WKERRSW  DS    X                   ERROR SWITCH                                 
ERRORFND EQU   X'80'               ERROR FOUND                                  
*                                                                               
BDDHALF  DS    H                                                                
BDDWORK  DS    XL50                                                             
BDDABKEY DS    XL20                                                             
BDDSW    DS    X                                                                
DLDATABQ EQU   X'80'               DOWNLOAD DATA ELEM IS BUILT                  
INSKEYBQ EQU   X'40'               REPLY KEY ELEM IS BUILT                      
*                                                                               
FZERSVRE DS    F                                                                
FZERSW1  DS    X                   FINALIZING ERROR SWTICH 1                    
X89FOUND EQU   X'80'                                                            
X96FOUND EQU   X'40'                                                            
XA6FOUND EQU   X'20'               ISSUE NAME (ELEM CODE 'A6')                  
X2DFOUND EQU   X'10'               2ND INS DATE (IN MAIN ELEM)                  
*                                                                               
SVMAPCOD DS    XL2                 SAVE MAP CODE                                
ERRORNUM DS    H                   ERROR NUMBER SET BY EDIT ROUTINES            
*                                                                               
OLDCDATA DS    CL256               1 BYTE LENGTH + 255 BYTES OLD DATA           
*                                                                               
OTHELMSW DS    X                   OTHER ELEMENT SWITCH                         
TEARSHET EQU   X'80'               TEARSHEET                                    
TEARSCOM EQU   X'40'               TEARSHEET COMMENT                            
ACHARGEQ EQU   X'20'               ADDITIONAL CHARGE                            
INTRNCON EQU   X'10'               INTERNET CONTRACT                            
INVMSTSQ EQU   X'08'               INVOICE MATCHING STATUSES                    
PURORD#Q EQU   X'04'               PURCHASE ORDER #                             
PLANCOSQ EQU   X'02'               PLANNED COST - NEW                           
COS2$$$Q EQU   X'01'               COS2 $                                       
*                                                                               
TSHTELEM DS    XL39                TEARSHEET ELEM                               
TSHTC1EL DS    XL68                TEARSHEET COMMENT ELEM 1                     
TSHTC2EL DS    XL68                TEARSHEET COMMENT ELEM 2                     
TSHTC3EL DS    XL68                TEARSHEET COMMENT ELEM 3                     
TSHTC4EL DS    XL68                TEARSHEET COMMENT ELEM 4                     
*                                                                               
INTCONEL DS    XL36                INTERNET CONTRACT ELEM                       
*                                                                               
ACHRGEL1 DS    XL32                ADDITIONAL CHARGES                           
ACHRGEL2 DS    XL32                                                             
ACHRGEL3 DS    XL32                                                             
ACHRGEL4 DS    XL32                                                             
ACHRGEL5 DS    XL32                                                             
ACHRGEL6 DS    XL32                                                             
ACHRGEL7 DS    XL32                                                             
ACHRGEL8 DS    XL32                                                             
ACHRGEL9 DS    XL32                                                             
ACHRGELA DS    XL32                                                             
*                                                                               
ACHRGCNT DS    X                   ADDITIONAL CHARGE COUNTER, MAX OF 10         
*                                                                               
ACHRGCOD DS    10CL2               TABLE OF BILLED/PAID CHARGES                 
*                                                                               
INVMSTEL DS    XL(PBMTLENQ)        INVOICE MATCHING STATUSES ELEM               
*                                                                               
WKSVKEY  DS    XL(L'KEY)                                                        
WKSVAIO  DS    XL(L'AREC)                                                       
WKSVELEM DS    XL256                                                            
WKDATESW DS    C                   DATE VALIDATION SWITCH                       
*                                                                               
WKAIOSW1 DS    X                   AIO SWITCH                                   
IO_GETRQ EQU   X'80'               GETREC IS CALLED                             
*                                                                               
WKPO#SW1 DS    X                   PURCHASE ORDER SWITCH                        
PO#1REMQ EQU   X'80'               REMOVING INACTIVE PO# IN BUY                 
WKPO#PRD DS    CL(L'PPO#KPRD)      PURCHASE ORDER - PRD CODE                    
WKPO#PNO DS    CL(PO#DMXLQ)        PURCHASE ORDER - PURCHASE ORDER #            
WKPO#SQ# DS    XL(L'PO#DID)        PURCHASE ORDER - SEQUENCE #                  
*                                                                               
SVIDKPRF DS    CL16                IDESK CONTROL PROFILE VALUES                 
*                                                                               
WKBUYELM DS    XL256                                                            
WKBLOCK1 DS    XL500                                                            
WKBLOCK2 DS    XL500                                                            
*                                                                               
WKAIO1   DS    XL4096                                                           
*                                                                               
WORK18X  EQU   *                                                                
         EJECT                                                                  
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'160PPBUY18   01/25/21'                                      
         END                                                                    
