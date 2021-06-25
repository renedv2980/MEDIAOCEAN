*          DATA SET DEUNVUP    AT LEVEL 008 AS OF 09/09/19                      
*PROCESS USING(WARN(15))                                                        
*PHASE DEUNVUPA                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE CARDS                                                                  
*INCLUDE SCANNER                                                                
*INCLUDE DATCON                                                                 
*INCLUDE DATVAL                                                                 
*INCLUDE GETDAY                                                                 
*INCLUDE ADDAY                                                                  
*INCLUDE NETWEEK                                                                
*INCLUDE DMDMGRL                                                                
*INCLUDE DMUTLCT                                                                
*INCLUDE LOADER                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRTREC                                                                 
*INCLUDE SMTP                                                                   
         TITLE 'UPDATE DEMO FORMULAS WITH UNIVERSE VALUES'                      
***********************************************************************         
* THIS PROGRAM TAKES AS INPUT A UNIVERSE FILE PRODUCED BY THE DAILY   *         
* NATIONAL CONVERSION.  IT BUILDS DEMO FORMULA RECORDS FOR THE        *         
* UNIVERSE VALUES, UPDATES THE CONTROL FILE, AND REBUILDS THE         *         
* 'DMASTER' DATASPACE TABLE.                                          *         
*                                                                     *         
* VALID PARAMETER CARDS:                                              *         
*  WRITE=YES/NO                                                       *         
*  TRACE=YES/NO                                                       *         
*  DEBUG=YES/NO     YES: UPDATE CTFILE EVEN IF UNIVERSES ARE THE SAME *         
*  DEMTABS=                                                           *         
*  DDSIO=                                                             *         
*  DSPACE=                                                            *         
***********************************************************************         
DEUNVUPD CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,DEUNVUPD,=V(REGSAVE),R8,R9                                     
*                                                                               
         ENTRY SSB                                                              
*                                                                               
         EXTRACT ASIDFLD,'S',FIELDS=(ASID)                                      
         LA    R1,ASIDFLD                                                       
         L     R5,0(R1)                                                         
         LOCASCB ASID=(R5)                                                      
         L     R5,ASCBASSB-ASCB(R1)                                             
*                                                                               
         SAM31 ,                   SWITCH TO 31-BIT MODE                        
*                                                                               
         L     R5,ASSBJSAB-ASSB(R5) R5 = A(JSAB)                                
         USING JSAB,R5                                                          
         MVC   SUBJJBID,JSABJBID   SAVE JOBID/JOBNAME IN E-MAIL SUBJECT         
         MVC   SUBJJBNM,JSABJBNM                                                
         DROP  R5                                                               
*                                                                               
         SAM24 ,                   SWITCH BACK TO 24-BIT MODE                   
*                                                                               
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
         MVC   TITLEJBN,SUBJJBNM   JOBNAME                                      
         MVC   TITLEJB#,SUBJJBID+3 JOB #                                        
         MVC   TITLE,TITLE1                                                     
*                                                                               
         BAS   RE,VALPAR           VALIDATE INPUT PARAMETERS                    
*                                                                               
         LA    R7,COMFACS                                                       
         USING COMFACSD,R7                                                      
         BAS   RE,GETCOMF          FILL IN NEEDED COMFACS ADDRESSES             
*                                                                               
         BAS   RE,DFORMULS         CREATE/UPDATE DFORMULA RECS FOR UNVS         
         BNE   *+8                                                              
         BAS   RE,BLDSPACE         REBUILD THE DATASPACE IF CONTROL             
*                                    FILE WAS UPDATED                           
         XBASE                                                                  
         EJECT                                                                  
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* CREATE/UPDATE DFORMULA RECORDS FOR UNIVERSES.                                 
* CREATE ONE SET FOR BROADCAST, AND ANOTHER SET FOR CABLE.                      
***********************************************************************         
*                                                                               
DFORMULS NTR1                                                                   
*                                                                               
         BAS   RE,INDBLOCK         INITIALIZE NEEDED FIELDS IN DBLOCK           
*                                                                               
*                                  OPEN THE CONTROL FILE                        
         GOTO1 CDATAMGR,DMCB,(0,=C'DMOPEN'),=C'CONTROL ',CTFLIST,IO             
*                                                                               
         CLI   WRITE,C'Y'          WRITE=YES?                                   
         BNE   DFRM05                                                           
         GOTO1 CDATAMGR,DMCB,(0,=C'ENQCTL'),(C'E',=C'CTRL')                     
         TM    8(R1),X'04'         ENQ SUCCESSFUL?                              
         BO    *+6                                                              
         DC    H'0'                NO                                           
*                                                                               
DFRM05   MVI   FILEUPDT,NO         CTFILE HASN'T BEEN UPDATED YET               
*                                                                               
         SR    R6,R6               CONTAINS ERROR CODE                          
         OPEN  FILIN               INPUT FILE WITH DEMO RECORDS                 
*                                  OPEN THE CONTROL FILE                        
         GET   FILIN,UREC          THE FIRST RECORD HOLDS ONLY THE BOOK         
         MVC   BOOK,UREC                                                        
*                                                                               
DFRM10   GET   FILIN,UREC                                                       
         AP    TOTINPUT,=P'1'                                                   
*                                  GET DEMO NUMBER                              
         XC    WORK,WORK              CREATE FAKE SCREEN FIELD                  
         MVC   WORK+8(L'UNAME),UNAME  CONTAINING DEMO NAME                      
         MVI   WORK,L'UNAME+8                                                   
         MVI   WORK+5,L'UNAME                                                   
         XC    DUB,DUB                                                          
         GOTO1 CDEMOVAL,DMCB,WORK,DUB,DBLOCK                                    
         CLI   DMCB+4,0                                                         
         BNE   *+6                                                              
         DC    H'0'                INVALID DEMO                                 
         MVC   DEMONUM,DUB+2       SAVE DEMO NUMBER                             
*                                                                               
         MVI   MEDIA,BROADCAST     BUILD AND PUT DFORMULA RECORD                
         BAS   RE,BLDREC           FOR BROADCAST                                
         BAS   RE,PUTREC                                                        
*                                                                               
         MVI   MEDIA,CABLE         BUILD AND PUT DFORMULA RECORD                
         BAS   RE,BLDREC           FOR CABLE                                    
         BAS   RE,PUTREC                                                        
*                                                                               
         B     DFRM10                                                           
*                                                                               
DFRMDONE CLI   WRITE,C'Y'          WRITE=YES?                                   
         BNE   DFRMDON5                                                         
         GOTO1 CDATAMGR,DMCB,(0,=C'ENQCTL'),(C'D',=C'CTRL')                     
*                                  CLOSE THE CONTROL FILE                       
DFRMDON5 GOTO1 CDATAMGR,DMCB,(0,=C'DMCLSE'),=C'CONTROL ',CTFLIST,IO             
*                                                                               
         CLOSE FILIN                                                            
*                                                                               
         MVC   P(34),=C'TOTAL NUMBER OF ADDED RECORDS =   '                     
         EDIT  TOTADD,(8,P+34),ZERO=NOBLANK                                     
         GOTO1 =V(PRINTER)                                                      
         MVC   P(34),=C'TOTAL NUMBER OF CHANGED RECORDS = '                     
         EDIT  TOTCHG,(8,P+34),ZERO=NOBLANK                                     
         GOTO1 =V(PRINTER)                                                      
         MVC   P(34),=C'TOTAL NUMBER OF INPUT RECORDS =   '                     
         EDIT  TOTINPUT,(8,P+34),ZERO=NOBLANK                                   
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         CLI   FILEUPDT,YES        SET CONDITION CODE                           
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* INITIALIZE DBLOCK                                                   *         
***********************************************************************         
*                                                                               
INDBLOCK NTR1                                                                   
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'NTI'                                                   
         MVC   DBCOMFCS,=A(COMFACS)                                             
         MVI   DBSELMED,C'N'                                                    
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* BUILD RECORD KEY AND ELEMENTS                                       *         
***********************************************************************         
*                                                                               
BLDREC NTR1                                                                     
*                                                                               
         LA    RE,CTREC            CLEAR RECORD AREA                            
         LA    RF,L'CTREC                                                       
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         LA    R4,CTREC                                                         
         USING CTGREC,R4                                                        
         XC    CTGKEY,CTGKEY                                                    
         MVI   CTGKTYP,CTGKTEQU                                                 
*                                                                               
         CLI   MEDIA,BROADCAST                                                  
         BNE   *+8                                                              
         MVI   CTGKFILE,C'P'                                                    
*                                                                               
         CLI   MEDIA,CABLE                                                      
         BNE   *+8                                                              
         MVI   CTGKFILE,C'C'                                                    
*                                                                               
         MVI   CTGKMED,C'N'                                                     
         MVI   CTGKSRC,C'N'                                                     
         MVC   CTGKAGY,=X'FFFF'                                                 
         MVI   CTGKCODE,X'FF'                                                   
*                                                                               
         BAS   RE,GETYBOOK         GET FIRST BOOK OF THE YEAR IN DUB            
         MVC   CTGKSTRT,DUB                                                     
         XC    CTGKSTRT,=X'FFFF'                                                
*                                                                               
         CLI   MEDIA,BROADCAST                                                  
         BNE   *+8                                                              
         MVI   CTGKDEMO,C'U'       UNIVERSE MODIFIER FOR BROADCAST              
*                                                                               
         CLI   MEDIA,CABLE                                                      
         BNE   *+8                                                              
         MVI   CTGKDEMO,C'L'       UNIVERSE MODIFIER FOR CABLE                  
*                                                                               
         MVC   CTGKDEMO+1(1),DEMONUM    DEMO NUMBER                             
*                                                                               
         XC    CTGLEN,CTGLEN                                                    
         MVI   CTGSTAT,0                                                        
*                                                                               
         LA    R5,ELEM             ADD ACTIVITY ELEMENT X'01'                   
         XC    ELEM,ELEM                                                        
         USING CTACTD,R5                                                        
         MVI   CTACTEL,CTACTELQ                                                 
         MVI   CTACTLEN,CTACTLNQ                                                
         MVC   CTACTDT,TODAY                                                    
         GOTO1 ADDELEM                                                          
         DROP  R5                                                               
*                                                                               
         LA    R5,ELEM             ADD DESCRIPTION ELEMENT X'02'                
         XC    ELEM,ELEM                                                        
         USING CTDSCD,R5                                                        
         MVI   CTDSCEL,CTDSCELQ                                                 
         MVC   CTDSC(L'UNAME),UNAME                                             
         LA    R1,CTDSC+L'UNAME-1       REMOVE SPACES AT END OF NAME            
         LHI   R0,L'UNAME                                                       
BLDR10   CLI   0(R1),C' '                                                       
         BNE   BLDR15                                                           
         SHI   R1,R1                                                            
         BCT   R0,BLDR10                                                        
         DC    H'0'                DEMO NAME SHOULD BE FILLED IN                
BLDR15   AHI   R0,2                ADD OVERHEAD TO ELEMNT LENGTH                
         STC   R0,CTDSCLEN                                                      
         GOTO1 ADDELEM                                                          
         DROP  R5                                                               
*                                                                               
         LA    R5,ELEM             ADD PRECISION ELEMENT X'03'                  
         XC    ELEM,ELEM                                                        
         USING CTPRECSD,R5                                                      
         MVI   CTPRECDE,CTPRECDQ                                                
         MVI   CTPRELEN,CTPRELNQ                                                
         CLI   MEDIA,BROADCAST                                                  
         BNE   *+8                                                              
         MVI   CTPRECCD,X'44'      PRECISION IN (0000) FOR BROADCAST            
         CLI   MEDIA,CABLE                                                      
         BNE   *+8                                                              
         MVI   CTPRECCD,X'40'      PRECISION IN UNITS FOR CABLE                 
         MVI   CTPRECFL,0                                                       
         GOTO1 ADDELEM                                                          
         DROP  R5                                                               
*                                                                               
         LA    R5,ELEM             ADD POLISH FORMULA ELEMENT X'04'             
         XC    ELEM,ELEM                                                        
         USING CTDPFCD,R5                                                       
         MVI   CTDPFCDE,CTDPFCDQ                                                
         MVI   CTDPFORM,X'FF'      FORMULA IS A CONSTANT                        
         ICM   R3,15,UVALUE        UNIVERSE VALUE                               
         SR    R2,R2                                                            
         LHI   RF,10                                                            
         DR    R2,RF               DIVIDE BY 10                                 
         CHI   R2,5                                                             
         BL    *+8                                                              
         AHI   R3,1                AND ROUND                                    
         C     R3,=X'0000FFFF'                                                  
         BNH   *+6                                                              
         DC    H'0'                UNIVERSE VALUE TOO BIG                       
         STCM  R3,3,CTDPFORM+1     ADD UNIVERSE VALUE                           
*                                                                               
         CLI   MEDIA,BROADCAST     FOR BROADCAST, FORMULA IS JUST               
         BNE   BLDR20               THE UNIVERSE VALUE                          
         MVI   CTDPFORM+3,1        ONLY ONE VALUE IN THE FORMULA                
         MVI   CTDPFORM+4,C'='     END OF FORMULA                               
         MVI   CTDPFLEN,CTDPFCFQ+5   5 IS LENGTH OF DATA IN CTDPFORM            
         B     BLDR25                                                           
*                                                                               
BLDR20   CLI   MEDIA,CABLE         FOR CABLE, FORMULA IS                        
         BE    *+6                   UNIVERSE VALUE*10000                       
         DC    H'0'                INVALID MEDIA                                
         MVI   CTDPFORM+3,X'FF'    SIGNAL A CONSTANT (10000)                    
         MVC   CTDPFORM+4(2),=AL2(10000)                                        
         MVC   CTDPFORM+6(2),=X'0102'   VALUE 1, VALUE 2                        
         MVI   CTDPFORM+8,C'*'          MULTIPLIED                              
         MVI   CTDPFORM+9,C'='     END OF FORMULA                               
         MVI   CTDPFLEN,CTDPFCFQ+10  10 IS LENGTH OF DATA IN CTDPFORM           
                                                                                
BLDR25   GOTO1 ADDELEM                                                          
         DROP  R5                                                               
*                                                                               
         LA    R5,ELEM             ADD INPUT FORMULA ELEMENT X'05'              
         XC    ELEM,ELEM                                                        
         USING CTDINCD,R5                                                       
         MVI   CTDINCDE,CTDINCDQ                                                
         MVI   CTDINCS#,1          SEQUENCE # 1                                 
         EDIT  (R3),(5,CTDINIF),ALIGN=LEFT,ZERO=NOBLANK                         
         LA    R1,CTDINIF+5-1      LAST CHARACTER                               
         LHI   R0,5                LENGTH OF VALUE IN CHARACTER FORMAT          
BLDRC50  TM    0(R1),X'F0'                                                      
         BO    BLDRC55                                                          
         SHI   R1,1                                                             
         BCT   R0,BLDRC50          VALUE SHOULD BE FILLED IN                    
         DC    H'0'                                                             
*                                                                               
BLDRC55  CLI   MEDIA,CABLE                                                      
         BNE   BLDRC59                                                          
         MVC   1(6,R1),=C'*10000'     UPDATE FORMULA                            
         AHI   R0,6                   AND ELEMENT LENGTH                        
*                                                                               
BLDRC59  AHI   R0,CTDINCFQ                                                      
         STC   R0,CTDINLEN         ELEMENT LENGTH                               
         GOTO1 ADDELEM                                                          
         DROP  R5                                                               
*                                                                               
         LA    R5,ELEM             ADD GENCON ACTIVITY ELEMENT 'F1'             
         XC    ELEM,ELEM                                                        
         USING ACTVD,R5                                                         
         XC    ACTVEL(ACTVLENQ),ACTVEL                                          
         MVI   ACTVEL,X'F1'                                                     
         MVI   ACTVLEN,20                                                       
         MVC   ACTVADDT,TODAY      DATE ADDED                                   
         MVC   ACTVCHDT,TODAY      DATE LAST CHANGED                            
         MVC   ACTVADID,=X'0115'   USER ID (I COPIED  THIS FROM THE             
         OI    ACTVADFL,X'80'      EXISTING RECORDS)                            
         GOTO1 ADDELEM                                                          
         DROP  R5                                                               
*                                                                               
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* PUT RECORD TO CTFILE, IF NEEDED                                     *         
***********************************************************************         
*                                                                               
PUTREC   NTR1                                                                   
*                                                                               
         MVI   BYTE,0              ASSUME WRITE=NO                              
         CLI   WRITE,C'Y'                                                       
         BNE   *+8                                                              
         MVI   BYTE,X'80'          WRITE=YES: READ FOR UPDATE                   
         GOTO1 CDATAMGR,DMCB,(BYTE,=C'DMREAD'),CTFILE,CTREC,IO                  
         CLI   DMCB+8,0                                                         
         BE    WRTREC              CTFILE RECORD WAS FOUND                      
*                                                                               
         TM    DMCB+8,X'FF'-X'10'  RECORD NOT FOUND?                            
         BZ    ADDREC              CORRECT: GO ADD IT                           
         LHI   R6,1                FATAL DATAMGR ERROR ON DMREAD                
         B     PUTRERR             CLOSE CTFILE                                 
*                                                                               
ADDREC   AP    TOTADD,=P'1'                                                     
*                                                                               
         CLI   TRACE,C'Y'          TRACE=YES?                                   
         BNE   ADDREC10                                                         
         GOTO1 =V(PRTREC),DMCB,CTREC,(28,25),V(PRINT),V(HEXOUT),       +        
               C'DOME',=C'ADDING NEW RECORD'                                    
         MVI   P,0                                                              
         GOTO1 =V(PRINTER)                                                      
*                                                                               
ADDREC10 CLI   WRITE,C'Y'          WRITE=YES?                                   
         BNE   ADDREC20                                                         
*                                                                               
         GOTO1 CDATAMGR,DMCB,(BYTE,=C'DMADD'),CTFILE,,CTREC                     
         CLI   DMCB+8,0                                                         
         BE    ADDREC18                                                         
         LHI   R6,2                DMADD FAILED                                 
         B     PUTRERR             CLOSE CTFILE                                 
*                                                                               
ADDREC18 MVI   FILEUPDT,YES        CTFILE HAS BEEN UPDATED                      
*                                                                               
ADDREC20 B     PUTRECX                                                          
*                                                                               
WRTREC   CLI   DEBUG,C'Y'          DEBUG MODE?                                  
         BE    WRTREC10            YES.UPDATE RECORDS EVEN IF NO CHANGE         
         BAS   RE,COMPREC          NEW RECORD SAME AS EXISTING RECD?            
         BE    PUTRECX             YES. DON'T UPDATE                            
*                                                                               
         CLI   TRACE,C'Y'          TRACE=YES?                                   
         BNE   WRTREC10                                                         
         GOTO1 =V(PRTREC),DMCB,IO,(28,25),V(PRINT),V(HEXOUT),          +        
               C'DOME',=C'EXISTING DFORMULA RECORD'                             
         MVI   P,0                                                              
         GOTO1 =V(PRINTER)                                                      
*                                                                               
WRTREC10 AP    TOTCHG,=P'1'                                                     
         CLI   TRACE,C'Y'          TRACE=YES?                                   
         BNE   WRTREC20                                                         
*                                                                               
         GOTO1 =V(PRTREC),DMCB,CTREC,(28,25),V(PRINT),V(HEXOUT),       +        
               C'DOME',=C'REPLACING WITH NEW RECORD'                            
         MVI   P,0                                                              
         GOTO1 =V(PRINTER)                                                      
*                                                                               
WRTREC20 CLI   WRITE,C'Y'          WRITE=YES?                                   
         BNE   WRTREC30            NO                                           
*                                                                               
         GOTO1 CDATAMGR,DMCB,(0,=C'DMWRT'),CTFILE,CTREC,CTREC                   
         CLI   DMCB+8,0                                                         
         BE    WRTREC28                                                         
         LHI   R6,3                DMWRT FAILED                                 
         B     PUTRERR             CLOSE CTFILE                                 
*                                                                               
WRTREC28 MVI   FILEUPDT,YES        CTFILE HAS BEEN UPDATED                      
*                                                                               
WRTREC30 DS    0X                                                               
*                                                                               
PUTRECX  B     EXIT                                                             
*                                                                               
PUTRERR  GOTO1 CDATAMGR,DMCB,(0,=C'DMCLSE'),=C'CONTROL ',CTFLIST,IO             
         ABEND 342,DUMP            FATAL ERROR ENCOUNTERED: LOOK AT R6          
         EJECT                                                                  
***********************************************************************         
* REBUILD THE DATASPACE IF NEEDED                                     *         
***********************************************************************         
*                                                                               
BLDSPACE NTR1                                                                   
*                                                                               
         MVC   P(33),=C'** REBUILDING THE ''DMASTER'' TABLE'                    
         GOTO1 =V(PRINTER)                                                      
                                                                                
         MVI   CTFILEW,C'N'        OPEN CTFILE NON-UPDATIVE                     
         MVI   CTRCVRW,C'X'        DON'T OPEN CTRCVR AT ALL                     
         OI    SSOSTAT2,SSOSNRCV   NO RECOVERY                                  
         GOTO1 CDATAMGR,DMCB,(0,=C'DMOPEN'),=C'CONTROL ',CTFLIST,IO             
*                                                                               
BLDSPC4  XC    TBLPARM,TBLPARM                                                  
         MVI   TBLPARM,X'D3'       'DMASTER' TABLE                              
         MVI   TBLPARM+4,X'FF'                                                  
         GOTO1 CDEMADDR,DMCB,(X'FE',TBLPARM),COMFACS,,0                         
*                                                                               
         MVC   P(32),=C'** FINISHED REBUILDING DATASPACE'                       
         GOTO1 =V(PRINTER)                                                      
*                                   CLOSE THE CONTROL FILE                      
         GOTO1 CDATAMGR,DMCB,(0,=C'DMCLSE'),=C'CONTROL ',CTFLIST,IO             
*                                                                               
         BAS   RE,SNDEMAIL          SEND EMAIL                                  
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* COMPARE RECORD IN CTREC WITH THE OLD RECORD FROM THE CTFILE.        *         
* AT INPUT IO HAS THE OLD RECORD FROM CTFILE AND                      *         
*          CTREC HAS THE NEWLY BUILT RECORD.                          *         
* SET CC TO EQUAL IF RECORD IS THE SAME.                              *         
***********************************************************************         
*                                                                               
COMPREC  NTR1                                                                   
*                                                                               
         LA    R4,CTREC+CTGDATA-CTGREC      R4-> ELEMENT IN CTREC               
         LA    R6,IO+CTGDATA-CTGREC         R6-> ELEMENT IN IO                  
*                                                                               
         CLI   0(R4),CTACTELQ    DON'T COMPARE ACTIVITY ELEMENT X'01'           
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   0(R6),CTACTELQ                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
NEW01    USING CTACTD,R4                                                        
OLD01    USING CTACTD,R6                                                        
         ZIC   R1,NEW01.CTACTLEN                                                
         AR    R4,R1                                                            
         ZIC   R1,OLD01.CTACTLEN                                                
         AR    R6,R1                                                            
*                                                                               
         CLI   0(R4),CTDSCELQ    DON'T COMPARE DESCRIPTION ELEM X'02'           
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   0(R6),CTDSCELQ                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
NEW02    USING CTDSCD,R4                                                        
OLD02    USING CTDSCD,R6                                                        
         ZIC   R1,NEW02.CTDSCLEN                                                
         AR    R4,R1                                                            
         ZIC   R1,OLD02.CTDSCLEN                                                
         AR    R6,R1                                                            
*                                                                               
         CLI   0(R4),CTPRECDQ    COMPARE PRECISION ELEMENT X'03'                
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   0(R6),CTPRECDQ                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
NEW03    USING CTPRECSD,R4                                                      
OLD03    USING CTPRECSD,R6                                                      
         CLC   NEW03.CTPRELEN,OLD03.CTPRELEN                                    
         BNE   COMPNEQ                                                          
         ZIC   R1,NEW03.CTPRELEN                                                
         SHI   R1,3              2 FOR OVERHEAD, 1 FOR EX                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   NEW03.CTPRECCD(0),OLD03.CTPRECCD                                 
         BNE   COMPNEQ                                                          
         ZIC   R1,NEW03.CTPRELEN                                                
         AR    R4,R1                                                            
         AR    R6,R1                                                            
*                                                                               
         CLI   0(R4),CTDPFCDQ    COMPARE POLISH FORMULA ELEMENT X'04'           
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   0(R6),CTDPFCDQ                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
NEW04    USING CTDPFCD,R4                                                       
OLD04    USING CTDPFCD,R6                                                       
         CLC   NEW04.CTDPFLEN,OLD04.CTDPFLEN                                    
         BNE   COMPNEQ                                                          
         ZIC   R1,NEW04.CTDPFLEN                                                
         SHI   R1,CTDPFCFQ+1     2 FOR OVERHEAD, 1 FOR EX                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   NEW04.CTDPFORM(0),OLD04.CTDPFORM                                 
         BNE   COMPNEQ                                                          
         ZIC   R1,NEW04.CTDPFLEN                                                
         AR    R4,R1                                                            
         AR    R6,R1                                                            
*                                                                               
         CLI   0(R4),CTDINCDQ    COMPARE INPUT FORMULA ELEMENT X'05'            
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   0(R6),CTDINCDQ                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
NEW05    USING CTDINCD,R4                                                       
OLD05    USING CTDINCD,R6                                                       
         CLC   NEW05.CTDINLEN,OLD05.CTDINLEN                                    
         BNE   COMPNEQ                                                          
         ZIC   R1,NEW05.CTDINLEN                                                
         SHI   R1,CTDINCFQ+1     2 FOR OVERHEAD, 1 FOR EX                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   NEW05.CTDINIF(0),OLD05.CTDINIF                                   
         BNE   COMPNEQ                                                          
         ZIC   R1,NEW05.CTDINLEN                                                
         AR    R4,R1                                                            
         AR    R6,R1                                                            
*                                                                               
         CLI   0(R4),X'F1'       DON'T COMPARE GENCON ACTIVITY ELEM'F1'         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   0(R6),X'F1'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
NEWF1    USING ACTVD,R4                                                         
OLDF1    USING ACTVD,R6                                                         
         ZIC   R1,NEWF1.ACTVLEN                                                 
         AR    R4,R1                                                            
         ZIC   R1,OLDF1.ACTVLEN                                                 
         AR    R6,R1                                                            
*                                                                               
         CLI   0(R4),0                                                          
         BE    *+6                                                              
         DC    H'0'              SHOULD BE END OF RECORD                        
         CLI   0(R6),0                                                          
         BE    *+6                                                              
         DC    H'0'              SHOULD BE END OF RECORD                        
*                                                                               
COMPEQ   CR    RB,RB                                                            
         B     EXIT                                                             
COMPNEQ  CHI   RB,0                                                             
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* FILL IN NEEDED COMFACS ADDRESSES                                    *         
***********************************************************************         
GETCOMF  NTR1                                                                   
*                                                                               
         GOTO1 =V(LOADER),DMCB,=CL8'T00ADE',0,0     LOAD DEMADDR                
         OC    4(4,R1),4(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   CDEMADDR,4(R1)                                                   
*                                                                               
         GOTO1 (RF),(R1),=CL8'T00AE0',0,0     LOAD DEMOCON                      
         OC    4(4,R1),4(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   CDEMOCON,4(R1)                                                   
*                                                                               
         GOTO1 (RF),(R1),=CL8'T00AD9',0,0     LOAD DEMOVAL                      
         OC    4(4,R1),4(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   CDEMOVAL,4(R1)                                                   
*                                                                               
         GOTO1 (RF),(R1),=CL8'T00AD0',0,0     LOAD DEMDISP                      
         OC    4(4,R1),4(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   CT00AD0,4(R1)                                                    
*                                                                               
         GOTO1 (RF),(R1),DEMTABS,0,0     LOAD DEMTABS                           
         OC    4(4,R1),4(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   CDEMTABS,4(R1)                                                   
*                                                                               
         GOTO1 CDEMTABS,DMCB,UNIVYRS    GET A(UNIVERSE YEARS TABLE)             
         ICM   R4,15,0(R1)         R4=A(MONTH/DATES TABLE)                      
         JZ    *+2                 A(UNIVYRS) TABLE NOT AVAILABLE ?!?           
         ST    R4,AUNIVYRS         SAVE A(UNIVYRS) TABLE                        
         MVC   UNIVYRLN,6(R1)      SAVE L'(TABLE ENTRY)                         
*                                                                               
         IF (OC,TODAY,TODAY,Z)     IF OVERRIDE DATE= IS NOT PRESENT:            
           GOTO1 CDATCON,DMCB,(5,0),(3,TODAY) GET TODAY'S DATE                  
         ENDIF ,                                                                
         GOTO1 CDATCON,DMCB,(3,TODAY),DUB   DUB = TODAY (TYPE-0)                
*                                                                               
* BY THE BEGINNING OF AUGUST IN ANY GIVEN YEAR, THE UNIVYRS TABLE               
* SHOULD HAVE BEEN UPDATED WITH THE START OF THE *FOLLOWING* CALENDAR           
* YEAR. IF IT ISN'T, SEND A WARNING E-MAIL!                                     
*                                                                               
         IF (CLI,TODAY+1,GE,MON_AUG)        IF AUG THROUGH DEC...               
           GOTO1 CADDAY,DMCB,(C'Y',DUB),DUB2,F'1'  BUMP TO NEXT YEAR            
           MVC   DUB,DUB2                                                       
         ENDIF ,                                                                
         GOTO1 CDATCON,DMCB,DUB,(20,DUB2)   CONVERT TO YYYYMMDD                 
         PACK  DUB,DUB2(4)                  ISOLATE THE YEAR...                 
         CVB   R0,DUB                       ...AND CONVERT TO BINARY            
         IF (CH,R0,GT,UYYEAR-UNVYRD(R4))    COMPARE W/ 1ST TABLE ENTRY          
           GOTO1 CDATAMGR,DMCB,(0,=C'OPMSG'),=C'AUTONOTE*US-DEMOSTEAM:W+        
               ARNING: DEMTABS UNIVYRS TABLE MUST BE UPDATED BEFORE ANN+        
               UAL UE CHANGE!'                                                  
         ENDIF ,                                                                
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GET THE FIRST BOOK OF THE YEAR.                                     *         
* INPUT: 'BOOK' HAS THE CURRENT BOOK.                                 *         
* OUTPUT: DUB(2) HAS THE FIRST BOOK OF THE YEAR 'BOOK' BELONGS TO.    *         
***********************************************************************         
*                                                                               
GETYBOOK NTR1                                                                   
*                                                                               
         L     R4,AUNIVYRS         R4=A(MONTH/DATES TABLE)                      
         LH    R5,UNIVYRLN         R5 HAS LENGTH OF TABLE ENTRY                 
         USING UNVYRD,R4                                                        
*                                                                               
GETY10   CLC   =X'FF',0(R4)                                                     
         BNE   *+6                                                              
         DC    H'0'                BOOK NOT FOUND. CALENDAR IS NOT UP           
*                                   TO DATE.                                    
         GOTO1 CDATCON,DMCB,(4,UYSTDT),(0,WORK),0   CONVERT UNIVERSE BK         
         GOTO1 =V(NETWEEK),DMCB,WORK,CGETDAY,CADDAY    FOR NETWEEK              
         MVC   DUB(1),4(R1)                                                     
         MVC   DUB+1(1),8(R1)     DUB(2) = START YEAR + WEEK NO                 
*                                                                               
GETY20   CLC   BOOK,DUB                                                         
         BNL   GETY30                                                           
         AR    R4,R5                                                            
         B     GETY10                                                           
                                                                                
GETY30   DS    0X                 DUB HAS THE FIRST WEEK OF THE YEAR            
*                                                                               
GETYX    B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* ADD ELEMENT IN 'ELEM' TO RECORD IN 'CTREC.                          *         
***********************************************************************         
*                                                                               
ADDELEM  NTR1                                                                   
         GOTO1 CHELLO,DMCB,(C'P',=CL8'CTFILE'),CTREC,ELEM,0                     
         ZIC   R1,DMCB+12                                                       
         LTR   R1,R1                                                            
         BZ    ADDELEMX                                                         
         DC    H'0'                ERROR CODE IN R1                             
*                                                                               
ADDELEMX B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE PARAMETER CARDS                                            *         
***********************************************************************         
*                                                                               
VALPAR   NTR1                                                                   
*                                  ASSUME WRITE=NO                              
         MVI   CTFILEW,C'N'        OPEN CTFILE NON-UPDATIVE                     
         MVI   CTRCVRW,C'X'        DON'T OPEN CTRCVR AT ALL                     
         OI    SSOSTAT2,SSOSNRCV   NO RECOVERY                                  
*                                                                               
VALPAR02 DS    0H                                                               
         GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         MVC   P(L'CARD),CARD      PRINT PARAMETER CARD                         
         GOTO1 =V(PRINTER)                                                      
         CLI   CARD,C'*'           SKIP CARDS COMMENTED OUT                     
         BE    VALPAR02                                                         
         LA    R1,PARAMS           R1=A(PARAMETER TABLE)                        
         SR    RE,RE                                                            
VALPAR04 ICM   RE,1,0(R1)          TEST FOR END OF TABLE                        
         BZ    VALPAR08            YES - NOT A VALID CONTROL CARD               
         EX    RE,*+8              MATCH CARD DATA TO TABLE ENTRY               
         BE    VALPAR06                                                         
         CLC   CARD(0),4(R1)                                                    
         AHI   R1,L'PARAMS         BUMP TO NEXT TABLE ENTRY                     
         B     VALPAR04                                                         
*                                                                               
VALPAR06 SR    RF,RF               PROCESS PARAMETER CARD                       
         ICM   RF,7,1(R1)          RF=A(PROCESS/VALIDATION ROUTINE)             
         LA    R1,CARD+1(RE)       R1=A(DATA VALUE)                             
         GOTO1 (RF),(R1)           CALL PROCESS/VALIDATE ROUTINE                
         B     VALPAR02                                                         
*                                                                               
VALPAR08 MVC   P(20),=C'INVALID CONTROL CARD'                                   
         GOTO1 =V(PRINTER)                                                      
         DC    H'0'                INVALID PARAMETER CARD                       
         SPACE 3                                                                
PARAMS   DS    0XL24               ** TABLE OF PARAMETER CARDS **               
         DC    AL1(05),AL3(PARDDSIO),CL20'DDSIO='                               
         DC    AL1(06),AL3(PARDSPAC),CL20'DSPACE='                              
         DC    AL1(07),AL3(PARDEMTB),CL20'DEMTABS='                             
         DC    AL1(05),AL3(PARWRITE),CL20'WRITE='                               
         DC    AL1(05),AL3(PARTRACE),CL20'TRACE='                               
         DC    AL1(05),AL3(PARDEBUG),CL20'DEBUG='                               
         DC    AL1(05),AL3(PARTODAY),CL20'TODAY='                               
         DC    AL1(01),AL3(PAREXIT),CL20'/*'                                    
         DC    AL1(0)              END-OF-TABLE                                 
         EJECT                                                                  
PARDDSIO L     RF,=V(DDSIO)        DDSIO=                                       
         MVC   0(8,RF),0(R1)                                                    
         BR    RE                                                               
*                                                                               
PARDSPAC MVC   SSODSPAC,0(R1)      DSPACE=                                      
         BR    RE                                                               
*                                                                               
PARDEMTB MVC   DEMTABS,0(R1)       DEMTABS=                                     
         BR    RE                                                               
*                                                                               
PARWRITE MVC   WRITE,0(R1)         WRITE=                                       
         CLI   WRITE,C'Y'          TEST LIVE RUN                                
         BNER  RE                                                               
         MVI   CTFILEW,C'U'                                                     
         MVI   CTRCVRW,C'U'                                                     
         NI    SSOSTAT2,X'FF'-(SSOSNRCV+SSOSROLC)                               
         BR    RE                                                               
*                                                                               
PARTRACE MVC   TRACE,0(R1)         TRACE=                                       
         BR    RE                                                               
*                                                                               
PARDEBUG MVC   DEBUG,0(R1)         DEBUG=                                       
         BR    RE                                                               
*                                                                               
PARTODAY DS    0H                  TODAY=                                       
         ST    RE,SAVERE                                                        
         LR    RF,R1               A(PARM VALUE)                                
         GOTO1 =V(DATVAL),DMCB,0(RF),DUB                                        
         OC    DMCB(4),DMCB                                                     
         JZ    *+2                 INVALID DATE= PARAMETER                      
         GOTO1 =V(DATCON),DMCB,DUB,(3,TODAY)                                    
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
PAREXIT  DS    0H                                                               
         MVI   P,0                                                              
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         CLI   WRITE,C' '          WRITE= PARAMETER CARD IS REQUIRED            
         BNE   EXIT                                                             
*                                                                               
         MVC   P(27),=C'MISSING WRITE= CONTROL CARD'                            
         GOTO1 =V(PRINTER)                                                      
         DC    H'0'                MISSING WRITE= PARAMETER CARD                
         EJECT                                                                  
***********************************************************************         
* SEND EMAIL ON CHANGE OF UNIVERSES                                   *         
***********************************************************************         
*                                                                               
SNDEMAIL NTR1                                                                   
*                                                                               
         GOTO1 =V(SMTP),DMCB,('SMTPAINI',0)    ATTACH AND INIT. JESMAIL         
*                                                                               
         GOTO1 =V(SMTP),DMCB,('SMTPAPRS',SENDTO),('SUBJCTLQ',SUBJECT)           
*                                                                               
         LARL  R2,EMAILTXT         ADD TO E-MAIL BODY                           
SNDEM10  DS    0H                                                               
         GOTO1 =V(SMTP),DMCB,('SMTPAPTL',(R2))                                  
         AHI   R2,80                                                            
         CLI   0(R2),X'FF'                                                      
         BNE   SNDEM10                                                          
*                                                                               
         GOTO1 =V(SMTP),DMCB,('SMTPASND',0)   SEND THE BUFFERED E-MAIL          
*                                                                               
         GOTO1 =V(SMTP),DMCB,('SMTPAEND',0)   DETACH JESMAIL                    
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*---------------------------------------------------------------------*         
         LTORG                                                                  
         SPACE 3                                                                
DUB      DS    D                                                                
DUB2     DS    D                                                                
DMCB     DS    6F                                                               
SAVERE   DS    A                                                                
AUNIVYRS DS    A                   A(UNIVYRS) TABLE IN DEMTABS                  
UNIVYRLN DS    H                   L'UNIVYRS TABLE ENTRY                        
BYTE     DS    X                                                                
WRITE    DC    C' '                WRITE=Y/N                                    
TRACE    DC    C'N'                TRACE=Y/N                                    
DEBUG    DC    C'N'                DEBUG=Y/N                                    
CARD     DS    CL80                PARAMETER CARD                               
WORK     DS    CL80                                                             
KEY      DS    XL25                CTFILE KEY                                   
ELEM     DS    XL255               BUILD ELEMENT HERE                           
TOTINPUT DC    PL8'0'              TOTAL NUMBER OF INPUT RECORDS                
TOTADD   DC    PL8'0'              TOTAL NUMBER OF ADDED RECORDS                
TOTCHG   DC    PL8'0'              TOTAL NUMBER OF CHANGED RECORDS              
*                                                                               
TODAY    DC    XL3'00'             TODAY'S DATE                                 
DEMONUM  DS    XL1                 1-BYTE DEMO NUMBER                           
BOOK     DS    XL2                                                              
MEDIA    DS    C                                                                
BROADCAST EQU  C'B'                                                             
CABLE     EQU  C'C'                                                             
FILEUPDT DS    C                   WAS THE CONTROL FILE UPDATED?                
YES      EQU   C'Y'                YES                                          
NO       EQU   C'N'                NO                                           
TBLPARM  DS    XL5                 PARAMETER TO DEMADDR                         
*                                                                               
UREC     DS    CL12                UNIVERSE INFO RECORD FROM INPUT FILE         
         ORG   UREC                                                             
UNAME    DS    CL8                 DEMO NAME                                    
UVALUE   DS    AL4                 UNIVERSE VALUE                               
*                                                                               
DEMTABS  DC    CL8'T00AD1'                                                      
*                                                                               
ASIDFLD  DC    F'0'                                                             
*                                                                               
CTFLIST  DS    0C                                                               
CTFILEW  DC    C'?'                                                             
CTFILE   DC    C'CTFILE '                                                       
CTRCVRW  DC    C'?'                                                             
CTRCVR   DC    C'CTRCVR '                                                       
         DC    C'X'                                                             
*                                                                               
TITLE1   DC    CL60' '                                                          
         ORG   TITLE1                                                           
         DC    C'UPDATE DFORMULA UNIVERSE RECORDS: JOB '                        
TITLEJBN DS    CL8                                                              
         DC    C'(J'                                                            
TITLEJB# DS    CL5                                                              
         DC    C')'                                                             
         ORG                                                                    
*                                                                               
SENDTO   DC    C'US-DEMOSTEAM@MEDIAOCEAN.COM:'                                  
*                                                                               
SUBJECT  DC    C'Notification of change to National universes. '                
SUBJJBID DS    CL8                                                              
         DC    C' '                                                             
SUBJJBNM DS    CL8                                                              
SUBJCTLQ EQU   *-SUBJECT                                                        
*                                                                               
EMAILTXT DS    0D                                                               
 DC CL80'You have received this email because a new set of National'            
 DC CL80'universe DFORMULA records have been updated to the live'               
 DC CL80'Control file.  The live DMASTER dataspace table has been'              
 DC CL80'rebuilt to reflect these changes.'                                     
 DC CL80' '                                                                     
 DC CL80'NOTE: This should only happen **ONCE** at the beginning of a'          
 DC CL80'new NTI year, usually close to Labor Day weekend. If you have'         
 DC CL80'received this email at any other time, or if you receive this'         
 DC CL80'email multiple times within a week, then you must investigate'         
 DC CL80'the cause.'                                                            
 DC CL80' '                                                                     
 DC CL80'To keep all the files in sync, you need to copy the newly'             
 DC CL80'updated DFORMULA records from the live Control file to the'            
 DC CL80'test Control file.  To do so, follow these steps:'                     
 DC CL80' '                                                                     
 DC CL80'1. Dump the newly updated records from the live Control'               
 DC CL80'file using program CTCONUN. See DEIS.DDS.JCL(CONUNIV)'                 
 DC CL80'for an example.  Be sure to specify the live Control file,'            
 DC CL80'and the correct BOOK= card with the book in the format YYWW.'          
 DC CL80'The book should be the first week of the new year.'                    
 DC CL80'You can see the book by displaying the newly added records'            
 DC CL80'in C/SFM (leave the book field blank to get the latest book).'         
 DC CL80'Example:'                                                              
 DC CL80'Record  DFORMULA  Action  DISPLAY   Key'                               
 DC CL80'Print             Output            Destn             Others'          
 DC CL80'File C   Subfile N   Source N   Start book 1136      Demo'             
 DC CL80'LHOMES   L1     (FF)'                                                  
 DC CL80' '                                                                     
 DC CL80'2. Copy the records from the output file of step 2, to the'            
 DC CL80'test Control file using program DECTUPD. See'                          
 DC CL80'DEIS.DDS.JCL(CTUPDATE) for an example. You may'                        
 DC CL80'want to run it first with WRITE=NO, check the trace, and'              
 DC CL80'then with WRITE=YES.'                                                  
 DC CL80' '                                                                     
 DC CL80'3. Rebuild the DMASTER table in the test dataspace.'                   
 DC CL80'This can be done in SPOT/SFM, DMTEST REPORT NOW,...'                   
 DC CL80'FUNCTION CLRTABS, FILE CODE DMASTER'                                   
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
         DS    0D                                                               
         DC    C'*SSB*SSB*SSB*SSB*SSB*SSB*SSB*SSB'                              
SSB      DS    0F                                                               
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
         ORG   SSB                                                              
         DC    XL(SSOOFFX-SSOOFF)'00'                                           
         ORG   SSOXTND                                                          
         DC    X'FF'               SET EXTENDED SSB                             
         ORG                                                                    
         SPACE 3                                                                
FILIN    DCB   DDNAME=FILIN,DSORG=PS,RECFM=FB,LRECL=12,EODAD=DFRMDONE, +        
               MACRF=GM                                                         
         SPACE 3                                                                
         DS    0H                                                               
       ++INCLUDE DEDBLOCK                                                       
         SPACE 3                                                                
         EJECT                                                                  
*---------------------------------------------------------------------*         
* COMMON FACILITIES LIST FOR DEMO LOOK-UPS                                      
COMFACS  DS    0D                                                               
         DC    V(DATAMGR)                                                       
         DC    4A(0)                                                            
         DC    V(HELLO)                                                         
         DC    V(SCANNER)                                                       
         DC    2A(0)                                                            
         DC    V(HEXOUT)                                                        
         DC    A(0)                                                             
         DC    V(DATVAL)                                                        
         DC    V(DATCON)                                                        
         DC    2A(0)                                                            
         DC    V(ADDAY)                                                         
         DC    V(GETDAY)                                                        
         DC    3A(0)                                                            
         DC    A(0)                                                             
         DC    3A(0)                                                            
         DC    A(0)                V(DEMADDR)                                   
         DC    A(0)                V(DEMDISP)                                   
         DC    A(0)                V(DEMTABS)                                   
         DC    A(0)                V(DEMTABOF)                                  
         DC    A(0)                CT00AD3-D8 TABLES IN DATASPACE               
         DC    5A(0)                                                            
         DC    A(0)                V(DEMOUT)                                    
         DC    A(0)                V(DEMEL)                                     
         DC    A(0)                V(DEMAINT)                                   
         DC    A(0)                V(DEMAND)                                    
         DC    A(0)                V(DEMOMATH)                                  
         DC    A(0)                V(DEMOVAL)                                   
         DC    A(0)                                                             
         DC    A(0)                V(PERVAL)                                    
         DC    24A(0)                                                           
         DC    V(HELEN)                                                         
         DC    2A(0)                                                            
         DC    V(LOCKUP)                                                        
         DC    V(MASTC)            MASTC                                        
         DC    V(LOCKSPC)          LOCKSPACE                                    
         DC    5A(0)                                                            
         DC    A(0)                DEMOCON                                      
         DC    18A(0)                                                           
         EJECT                                                                  
*---------------------------------------------------------------------*         
IO       DS    XL1000              I/O AREA FOR CTFILE RECORD                   
*                                                                               
CTREC    DS    XL1000              BUILD CONTROL RECORD HERE                    
*                                                                               
         ORG                                                                    
         EJECT                                                                  
*---------------------------------------------------------------------*         
         TITLE 'MASTER CONTROL - STORAGE AND DSECTS'                            
MASTC    CSECT                                                                  
       ++INCLUDE DDMASTC                                                        
*---------------------------------------------------------------------*         
         EJECT                                                                  
DEUNVUPD CSECT                                                                  
*                                                                               
* DDCOMFACS                                                                     
* CTGENFILE                                                                     
* DDACTIVD                                                                      
* DEDEMTABD                                                                     
* DDDPRINT                                                                      
* DDSMTPD                                                                       
* DDMONYREQU                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDACTIVD                                                       
       ++INCLUDE DEDEMTABD                                                      
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE DDSMTPD                                                        
       ++INCLUDE DDMONYREQU                                                     
         PRINT ON                                                               
         SPACE 3                                                                
*                                                                               
         IHAASCB                                                                
         IHAASSB                                                                
         IAZJSAB                                                                
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008DEUNVUP   09/09/19'                                      
         END                                                                    
