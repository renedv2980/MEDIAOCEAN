*          DATA SET CTADDREC   AT LEVEL 001 AS OF 10/15/19                      
*PHASE CADDRECA                                                                 
*INCLUDE CARDS                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTERL                                                               
         TITLE '- CONTROL SYSTEM ADD RECORD FROM INPUT FILE'                    
CTADDREC CSECT                                                                  
         PRINT NOGEN                                                            
         ENTRY SSB                                                              
         ENTRY UTL                                                              
         NBASE WORKL,**ADDREC,WORK=AWORKC,CLEAR=YES                             
         USING WORKD,RC                                                         
         BASR  RA,0                                                             
         AHI   RA,LITERALS-*                                                    
         USING LITERALS,RA                                                      
         USING SSBD,SSB                                                         
         L     R9,VCPRINT                                                       
         USING DPRINT,R9                                                        
         MVI   CHRULT,X'BF'                                                     
                                                                                
         GOTOR VALPAR              VALIDATE INPUT PARAMETERS                    
         OPEN  (CONTAPE,INPUT,GENTAPE,INPUT)                                    
         GOTOR VDMGR,DMCB,DMOPEN,DMCONSYS,DMFLIST,IO                            
                                                                                
         MVC   TITLE,SPACES                                                     
         MVC   TITLE(L'TITLE2),TITLE2                                           
         ZAP   LINE,PNINETY                                                     
         ZAP   PAGE,PONE                                                        
                                                                                
         GOTOR ADDGEN              Add to GENDIR/GENFIL                         
*        GOTOR ADDCON              Add to CTFILE                                
*        GOTOR GET                 READ RECORDS AND POST TO BUFFER              
*        GOTOR PUT                 WRITE BACK ALL DELETED RECORDS               
                                                                                
         GOTO1 VDMGR,DMCB,DMCLSE,DMCONSYS,0,IO                                  
                                                                                
         CLOSE (CONTAPE,,GENTAPE)                                               
                                                                                
AGYDELX  XBASE ,                                                                
         DROP  RB                                                               
                                                                                
AWORKC   DC    A(WORKC)                                                         
         EJECT                                                                  
***********************************************************************         
* READ ALL AGENCY BASED CONTROL FILE RECORDS                          *         
***********************************************************************         
ADDGEN   NTR1  BASE=*,LABEL=*                                                   
                                                                                
ADDG000  GET   GENTAPE,IOL                                                      
         CLI   TRACE,C'Y'                                                       
         JNE   ADDG010                                                          
         GOTOR VHEXOUT,DMCB,IO,P+1,L'GENDRKEY,HEXTOG                            
         GOTOR VPRINTER                                                         
*                                                                               
ADDG010  MVC   GENDRKEY,IO                                                      
         GOTOR VDMGR,DMCB,(DMIND,DMREAD),GENDIR,GENDRKEY,AIO2                   
         JE    *+2                                                              
         XC    DISK,DISK                                                        
         CLI   WRITE,C'Y'                                                       
         BNE   ADDG000                                                          
         GOTOR VDMGR,DMCB,(DMIND,DMADDREC),GENFIL,DISK,IO,IOWORK                
         CLI   8(R1),0             Error ?                                      
         JNE   *+2                                                              
         J     ADDG000                                                          
GENX     J     EXIT                                                             
CONX     J     EXIT                                                             
                                                                                
***********************************************************************         
* READ ALL AGENCY BASED CONTROL FILE RECORDS                          *         
***********************************************************************         
*&&DO                                                                           
GET      NTR1  BASE=*,LABEL=*                                                   
                                                                                
         GOTOR VBUFFRIN,PARM,('BUFFAINI',BUFFER),BUFKEY,COMFACS                 
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         XC    TSTDELS(TSTDELL),TSTDELS                                         
                                                                                
         GOTOR GETACS              GET ACCESS RECORDS                           
         GOTOR GETIDS              GET USER-ID RECORDS                          
         GOTOR GETLST              GET LIST RECORDS                             
         GOTOR GETUPR              GET USER PROFILE RECORDS                     
         GOTOR GETRPR              GET REPORT PROFILE RECORDS                   
         GOTOR GETPWD              GET PASSWORD RECORDS                         
         GOTOR GETSAC              GET ACCESS RECORDS                           
         GOTOR GETSAF              GET FIELD CONTROL RECORDS                    
         GOTOR GETSAO              GET OPTION CONTROL RECORDS                   
         GOTOR GETOFF              GET OFFICE RECORDS                           
         GOTOR GETDEP              GET DEPARTMENT RECORDS                       
         GOTOR GETPER              GET PERSON RECORDS                           
         GOTOR GETPAG              GET PERSON ACCESS GROUP RECORDS              
         GOTOR GETLAG              GET LIMIT ACCESS GROUP RECORDS               
         GOTOR GETLAL              GET LIMIT ACCESS LIST RECORDS                
         GOTOR GETTSA              GET T/S APPROVER GROUP RECORDS               
*&&US*&& GOTOR GETFAX              GET FAX INFORMATION RECORDS                  
         GOTOR GETDRU              GET DRIVER USER RECORDS                      
*&&US*&& GOTOR GETCPP              GET CPP EXTRACT RULES RECORDS                
         GOTOR GETWRI              GET WRITER DEFINITION RECORDS                
         GOTOR GETRFP              GET RFP GROUP RECORDS                        
*&&UK*&& GOTOR GETNAR              GET NARRATIVE RECORDS                        
*&&UK*&& GOTOR GETEXC              GET EXCHANGE RECORDS                         
*&&US*&& GOTOR GETMOF              GET MEDIA OFFICE RECORDS                     
         GOTOR GETBDR              GET ESS BDE REFORM RECORDS                   
         GOTOR GETXTA              GET EXTRACT CONTROL RECORD (AGENCY)          
         GOTOR GETXTS              GET EXTRACT CONTROL RECORD (ESS)             
         GOTOR GETXTF              GET EXTRACT CONTROL RECORD (XFILE)           
         GOTOR GETXSA              GET EXTRACT ESS/AGENCY PASSIVE               
         GOTOR GETXAF              GET EXTRACT AGENCY/FILE PASSIVE              
         GOTOR GETXSF              GET EXTRACT ESS/FILE PASSIVE                 
                                                                                
GETX     J     EXIT                                                             
         DROP  RB                                                               
                                                                                
***********************************************************************         
* READ BACK BUFFERIN FILE AND TRACE/DELETE CTFILE/GENFIL UPDATES      *         
***********************************************************************         
PUT      NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   P(L'UPDLIT),UPDLIT                                               
         CLI   WRITE,YES                                                        
         BE    *+10                                                             
         MVC   P(L'GETLIT),GETLIT                                               
         GOTOR VPRINTER                                                         
         XC    BUFREC(BUFRECL),BUFREC                                           
         GOTOR VBUFFRIN,PARM,('BUFFARDH',BUFFER),BUFREC,COMFACS                 
         B     PUT04                                                            
PUT02    GOTOR VBUFFRIN,PARM,('BUFFASEQ',BUFFER),BUFREC,COMFACS                 
PUT04    BNE   PUT30                                                            
*                                                                               
         CLI   TRACE,C'Y'                                                       
         BNE   PUT06                                                            
         GOTOR VHEXOUT,DMCB,BUFKEY,P+1,BUFRECL,HEXTOG                           
         GOTOR VPRINTER                                                         
*                                                                               
PUT06    TM    BUFDSTAT,BUFDSRBD   TEST RECORD TO BE DELETED                    
         BZ    PUT02               NO, NEXT                                     
                                                                                
         CLI   OUTPUT,C'T'                                                      
         BE    PUT08                                                            
         CLI   WRITE,YES                                                        
         BNE   PUT08                                                            
         AP    COMTALLY,=P'1'                                                   
         CP    COMTALLY,COMLIMIT                                                
         BL    PUT08                                                            
         GOTOR VDMGR,DMCB,DMCOMMIT,0                                            
         ZAP   COMTALLY,=P'0'                                                   
                                                                                
PUT08    CLI   BUFKTYPE,BUFKTGEN   TEST FILE TYPE                               
         BH    PUT20                                                            
                                                                                
PUT10    MVC   IO(L'CT5KEY),BUFDRKEY                                            
         GOTOR VDMGR,DMCB,(DMIND,DMREAD),CTFILE,IO,IO                           
         BE    PUT12                                                            
         TM    BUFDSTAT,BUFDSPSV                                                
         BO    PUT02                                                            
         DC    H'0'                                                             
PUT12    AP    CTFILED,PONE                                                     
         CLI   OUTPUT,C'D'                                                      
         BE    PUT14                                                            
         ICM   R1,3,IO+CT5LEN-CT5REC                                            
         LA    R1,4(,R1)                                                        
         SLL   R1,16                                                            
         ST    R1,IOL                                                           
         PUT   CONTAPE,IOL                                                      
         CLI   OUTPUT,C'T'                                                      
         BE    PUT02                                                            
PUT14    OI    IO+(CT5STAT-CT5KEY),X'80'                                        
         CLI   UNDO,YES                                                         
         BNE   *+8                                                              
         NI    IO+(CT5STAT-CT5KEY),X'7F'                                        
         CLI   WRITE,YES                                                        
         BNE   PUT02                                                            
         GOTOR VDMGR,DMCB,DMWRT,CTFILE,IO,IO                                    
         BE    PUT02                                                            
         DC    H'0'                                                             
                                                                                
PUT20    MVC   IO(L'GRPKEY),BUFDRKEY                                            
         GOTOR VDMGR,DMCB,(DMIND,DMREAD),GENDIR,IO,IO                           
         JNE   *+2                                                              
         CLI   OUTPUT,C'T'                                                      
         BE    PUT22                                                            
         OI    IO+(GRPKSTAT-GRPKEY),X'80'                                       
         CLI   UNDO,YES                                                         
         BNE   *+8                                                              
         NI    IO+(GRPKSTAT-GRPKEY),X'7F'                                       
         AP    GENDIRD,PONE                                                     
         CLI   WRITE,YES                                                        
         BNE   PUT22                                                            
         GOTOR VDMGR,DMCB,DMWRT,GENDIR,IO,IO                                    
         JNE   *+2                                                              
                                                                                
PUT22    TM    BUFDSTAT,BUFDSPSV                                                
         BO    PUT30                                                            
                                                                                
         MVC   DUB(L'GRPKDA),IO+(GRPKDA-GRPKEY)                                 
         GOTOR VDMGR,DMCB,(DMIND,DMGETREC),GENFIL,DUB,IO,WORK                   
         JNE   *+2                                                              
         CLI   OUTPUT,C'D'                                                      
         BE    PUT24                                                            
         ICM   R1,3,IO+GERLEN-GEXCD                                             
         LA    R1,4(,R1)                                                        
         SLL   R1,16                                                            
         ST    R1,IOL                                                           
         PUT   GENTAPE,IOL                                                      
         CLI   OUTPUT,C'T'                                                      
         BE    PUT02                                                            
PUT24    OI    IO+(GRPSTAT-GRPKEY),X'80'                                        
         CLI   UNDO,C'Y'                                                        
         BNE   *+8                                                              
         NI    IO+(GRPSTAT-GRPKEY),X'7F'                                        
         AP    GENFILD,PONE                                                     
         CLI   WRITE,YES                                                        
         BNE   PUT02                                                            
         GOTOR VDMGR,DMCB,DMPUTREC,GENFIL,DUB,IO,WORK                           
         BE    PUT02                                                            
         DC    H'0'                                                             
                                                                                
PUT30    ZAP   LINE,PNINETY                                                     
         GOTOR VPRINTER                                                         
         MVC   P(L'CTFILELD),CTFILELD                                           
         OI    CTFILED+L'CTFILED-1,X'0F'                                        
         UNPK  P+L'CTFILELD(7),CTFILED                                          
         GOTOR VPRINTER                                                         
         MVC   P(L'GENDIRLD),GENDIRLD                                           
         OI    GENDIRD+L'GENDIRD-1,X'0F'                                        
         UNPK  P+L'GENDIRLD(7),GENDIRD                                          
         GOTOR VPRINTER                                                         
         MVC   P(L'GENFILLD),GENFILLD                                           
         OI    GENFILD+L'GENFILD-1,X'0F'                                        
         UNPK  P+L'GENFILLD(7),GENFILD                                          
         GOTOR VPRINTER                                                         
                                                                                
PUTX     J     EXIT                                                             
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* READ ACCESS RECORDS AND POST TO BUFFER                              *         
***********************************************************************         
                                                                                
GETACS   NTR1  LABEL=NO                                                         
         MVC   P(L'RECCON01),RECCON01                                           
         GOTOR VPRINTER                                                         
                                                                                
         CLI   MODE,C'O'           SKIP IF ORPHAN MODE                          
         JE    GETACS10                                                         
                                                                                
         LA    R2,DELETE           R2=A(DELETE LIST)                            
         J     GETACS04                                                         
                                                                                
GETACS02 CLI   0(R2),C' '          TEST END OF DELETE LIST                      
         JE    EXIT                                                             
                                                                                
         USING CT5REC,IO                                                        
GETACS04 XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,0(R2)                                                   
         GOTOR VDMGR,DMCB,(DMIND,DMREAD),CTFILE,CT5REC,CT5REC                   
         JE    GETACS05                                                         
         CLI   UNDO,C'Y'                                                        
         JNE   GETACS06                                                         
         TM    DMCB+8,X'02'                                                     
         JNO   GETACS06                                                         
*                                                                               
GETACS05 GOTOR PUTBUF,DMCB,('BUFDSRBD',CT5KALPH),CT5KEY,               *        
               ('BUFKTACS',L'BUFKAGYA-1),0                                      
         MVC   P(L'ACSLIT),ACSLIT                                               
         MVC   P+L'ACSLIT+1(L'CT5KALPH),CT5KALPH                                
         MVC   P+L'ACSLIT+L'CT5KALPH+2(L'WILDEL),WILDEL                         
         GOTOR VPRINTER                                                         
         J     GETACS08                                                         
                                                                                
GETACS06 MVC   P(L'ERROR2),ERROR2  INVALID AGENCY                               
         MVC   P+L'ERROR2(L'CT5KALPH),0(R2)                                     
         GOTOR VPRINTER                                                         
         GOTOR PUTBUF,DMCB,('BUFDSNOF',CT5KALPH),0,                    *        
               ('BUFKTACS',L'BUFKAGYA-1),0                                      
                                                                                
GETACS08 AHI   R2,L'CT5KALPH+1     BUMP TO NEXT AGENCY                          
         J     GETACS02                                                         
                                                                                
         USING CT5REC,IO                                                        
GETACS10 XC    CT5KEY,CT5KEY       ORPHAN MODE. ADD IDS TO KEEP                 
         MVI   CT5KTYP,CT5KTYPQ                                                 
         GOTOR VDMGR,DMCB,(DMIND,DMRDHI),CTFILE,CT5REC,CT5REC                   
         J     GETACS14                                                         
                                                                                
GETACS12 GOTOR VDMGR,DMCB,(DMIND,DMRSEQ),CTFILE,CT5REC,CT5REC                   
                                                                                
GETACS14 JNE   *+2                                                              
         CLI   CT5KTYP,CT5KTYPQ                                                 
         JNE   EXIT                                                             
         GOTOR PUTBUF,DMCB,(0,CT5KALPH),CT5KEY,                        *        
               ('BUFKTACS',L'BUFKAGYA-1),0                                      
         MVC   P(L'ACSLIT),ACSLIT                                               
         MVC   P+L'ACSLIT+1(L'CT5KALPH),CT5KALPH                                
         MVC   P+L'ACSLIT+L'CT5KALPH+2(L'WILKEPT),WILKEPT                       
         GOTOR VPRINTER                                                         
         J     GETACS12                                                         
                                                                                
***********************************************************************         
* READ ID RECORDS AND POST TO BUFFER                                  *         
***********************************************************************         
GETIDS   NTR1  LABEL=NO                                                         
         MVC   P(L'RECCON02),RECCON02                                           
         GOTOR VPRINTER                                                         
*                                                                               
         USING CTIKEY,IO                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVI   CTIKID,1                                                         
         GOTOR VDMGR,DMCB,(DMIND,DMRDHI),CTFILE,CTIKEY,CTIKEY                   
         J     GETIDS04                                                         
GETIDS02 GOTOR VDMGR,DMCB,(DMIND,DMRSEQ),CTFILE,CTIKEY,CTIKEY                   
GETIDS04 JNE   *+2                                                              
         CLI   CTIKTYP,CTIKTYPQ    C'I' ID Record                               
         JNE   EXIT                                                             
         OC    CTIKSPAR,CTIKSPAR   Binary zeroes                                
         JNZ   GETIDS02                                                         
         MVC   SAVEKEY,IO          Hold ID record key                           
         MVI   FLAG,0              Reset FLAG                                   
*                                                                               
         XC    WORK(2),WORK                                                     
         LA    RF,CTIDATA                                                       
         USING CTAGYD,RF                                                        
GETIDS06 CLI   CTAGYEL,0                                                        
         JE    GETIDS12                                                         
         CLI   CTAGYEL,CTDSCELQ                                                 
         JNE   *+14                                                             
         MVC   WORK(2),CTDSC-CTDSCD(RF)                                         
         J     GETIDS08                                                         
         CLI   CTAGYEL,CTAGYELQ                                                 
         JE    GETIDS10                                                         
GETIDS08 LLC   R0,CTAGYLEN                                                      
         AR    RF,R0                                                            
         J     GETIDS06                                                         
*                                                                               
GETIDS10 MVC   WORK+2(L'CTAGYID),CTAGYID                                        
         MVI   BUFDSTAT,BUFDSRBD   RECORD TO BE DELETED                         
         GOTOR TSTDELA,CTAGYID     TEST AGENCY TO BE DELETED                    
         JNE   GETIDS12            NO                                           
*&&US                                                                           
         BRAS  RE,KREPID           REP SYSTEM ID MAY STILL BE NEEDED            
         JNE   GETIDS13            DO NOT DELETE, CONTAINS DARE INFO            
*&&                                                                             
         J     GETIDS20                                                         
*                                                                               
GETIDS12 CLI   MODE,C'O'           TEST ORPHAN MODE                             
         JNE   GETIDS02            NO, NEXT                                     
GETIDS13 MVI   BUFDSTAT,0          ADD BUT NOT TO DELETE                        
*                                                                               
GETIDS20 GOTOR PUTBUF,DMCB,(BUFDSTAT,CTIKID),CTIKEY,                   *        
               ('BUFKTIDC',L'BUFKUSER-1),WORK+2                                 
*                                                                               
         MVC   WORK+4(L'CTIKID),CTIKID                                          
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         OC    CTIKNUM,WORK                                                     
         JZ    *+2                                                              
         OI    BUFDSTAT,BUFDSPSV   INDICATE PASSIVE                             
         GOTOR PUTBUF,DMCB,(BUFDSTAT,CTIKNUM),CTIKEY,                  *        
               ('BUFKTID#',L'BUFKUSID-1),WORK+2                                 
         USING CT9BKEY,IO                                                       
         XC    CT9BKEY,CT9BKEY                                                  
         MVI   CT9BKTYP,CT9BKTYQ                                                
         MVI   CT9BKSUB,CT9BKS01                                                
         MVC   CT9BKAGY,WORK+2                                                  
         MVC   CT9BKNUM,WORK                                                    
         MVC   DUB(2),WORK+2                                                    
         MVC   DUB+2(2),WORK                                                    
         GOTOR PUTBUF,DMCB,(BUFDSTAT,DUB),CT9BKEY,                     *        
               ('BUFKTIDA',L'BUFKUSAG-1),WORK+2                                 
*                                                                               
         MVC   P(L'UIDLIT),UIDLIT                                               
         LA    R1,P+L'UIDLIT+1                                                  
         MVI   0(R1),C'('                                                       
         MVC   1(L'CTAGYID,R1),WORK+2                                           
         MVI   1+L'CTAGYID(R1),C')'                                             
         AHI   R1,2+L'CTAGYID                                                   
         MVC   0(L'CTIKID,R1),WORK+4                                            
         AHI   R1,L'CTIKID-1                                                    
         CLI   0(R1),C' '                                                       
         JH    *+8                                                              
         BRCT  R1,*-8                                                           
         SR    RF,RF                                                            
         ICM   RF,3,WORK                                                        
         CVD   RF,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         MVI   1(R1),C'/'                                                       
         UNPK  2(5,R1),DUB                                                      
*                                                                               
         MVC   8(L'WILKEPT,R1),WILKEPT                                          
         TM    BUFDSTAT,BUFDSRBD   TEST DELETING                                
         JZ    *+10                NO, SKIP                                     
         MVC   8(L'WILDEL,R1),WILDEL                                            
*&&US                                                                           
         TM    FLAG,FLAGDAPA          DARE Partner found                        
         JZ    *+10                   NO                                        
         MVC   P+55(L'DAPALIT),DAPALIT                                          
*                                                                               
         TM    FLAG,FLAGDAPR          DARE Prefix found                         
         JZ    *+10                   NO                                        
         MVC   P+55(L'DAPRLIT),DAPRLIT                                          
*&&                                                                             
         GOTOR VPRINTER                                                         
         MVC   IO(L'SAVEKEY),SAVEKEY                                            
         J     GETIDS02                                                         
                                                                                
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* BUFFERIN ROUTINES                                                   *         
***********************************************************************         
*&&DO                                                                           
PUTBUF   NTR1  LABEL=NO                                                         
         LR    R2,R1                                                            
         XC    BUFREC(BUFRECL),BUFREC                                           
         MVC   BUFKTYPE,8(R2)                                                   
         SR    RF,RF                                                            
         ICM   RF,7,9(R2)                                                       
         L     R1,0(R2)                                                         
         BASR  RE,0                                                             
         EX    RF,8(RE)                                                         
         J     *+10                                                             
         MVC   BUFKDATA(0),0(R1)                                                
         MVC   BUFDSTAT,0(R2)      SET RECORD STATUS                            
         ICM   RE,15,12(R2)                                                     
         JZ    *+10                                                             
         MVC   BUFDAGY,0(RE)                                                    
         ICM   RE,15,4(R2)                                                      
         JZ    PUTBUF02                                                         
         CLI   BUFKTYPE,BUFKTGEN   TEST FILE TYPE                               
         JH    *+14                                                             
         MVC   BUFDRKEY(L'CTIKEY),0(RE)                                         
         J     PUTBUF02                                                         
         MVC   BUFDRKEY(L'GRPKEY),0(RE)                                         
PUTBUF02 GOTOR BUFPUT                                                           
         J     EXIT                                                             
                                                                                
BUFGET   LR    R0,RE                                                            
         GOTOR VBUFFRIN,PARM,('BUFFAGET',BUFFER),BUFREC,COMFACS                 
         LR    RE,R0                                                            
         BR    RE                                                               
                                                                                
BUFPUT   LR    R0,RE                                                            
         GOTOR VBUFFRIN,PARM,('BUFFAPUT',BUFFER),BUFREC,COMFACS                 
         LR    RE,R0                                                            
         BER   RE                                                               
         DC    H'0'                                                             
*&&                                                                             
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* VALIDATE PARAMETER CARDS                                            *         
***********************************************************************         
                                                                                
VALPAR   NTR1  BASE=*,LABEL=*                                                   
         XC    RUNVALS(RUNVALL),RUNVALS                                         
         MVC   TITLE,SPACES                                                     
         MVC   TITLE(L'TITLE1),TITLE1                                           
         ZAP   LINE,PNINETY                                                     
         ZAP   PAGE,PONE                                                        
         MVI   CTFILEW,C'N'                                                     
         MVI   GENFILW,C'N'                                                     
         MVI   GENDIRW,C'N'                                                     
         MVI   CTRCVRW,C'X'                                                     
                                                                                
VALPAR02 GOTOR VCARDS,DMCB,C,RE00                                               
         MVC   P(L'C),C                                                         
         GOTOR VPRINTER                                                         
         LA    R1,PARAMS           R1=A(PARAMETER TABLE)                        
         SR    RE,RE                                                            
VALPAR04 ICM   RE,1,0(R1)          TEST FOR END OF TABLE                        
         BZ    VALPAR08            YES - NOT A VALID CONTROL CARD               
         EX    RE,*+8              MATCH CARD DATA TO TABLE ENTRY               
         BE    VALPAR06                                                         
         CLC   C(0),4(R1)                                                       
         AHI   R1,L'PARAMS         BUMP TO NEXT TABLE ENTRY                     
         B     VALPAR04                                                         
                                                                                
VALPAR06 SR    RF,RF               PROCESS PARAMETER CARD                       
         ICM   RF,7,1(R1)          RF=A(PROCESS/VALIDATION ROUTINE)             
         LA    R1,C+1(RE)          R1=A(DATA VALUE)                             
         GOTOR (RF),(R1)           CALL PROCESS/VALIDATE ROUTINE                
         B     VALPAR02                                                         
                                                                                
VALPAR08 MVC   P(L'ERROR3),ERROR3                                               
         GOTOR VPRINTER                                                         
         ABEND 1                                                                
                                                                                
VALPAR10 CLI   MODE,C'O'                                                        
         BE    VALPAR12                                                         
         B     VALPAR16                                                         
         CLI   DELETE,C' '                                                      
         BH    VALPAR12                                                         
         MVC   P(L'ERROR1),ERROR1  DELETE CARD MISSING                          
         GOTOR VPRINTER                                                         
         ABEND 1                                                                
                                                                                
VALPAR12 CLI   OUTPUT,C' '                                                      
         BH    VALPAR14                                                         
         B     VALPAR16                                                         
         MVC   P(L'ERROR4),ERROR4  OUTPUT CARD MISSING                          
         GOTOR VPRINTER                                                         
         ABEND 1                                                                
                                                                                
VALPAR14 CLI   OUTPUT,C'D'                                                      
         BE    VALPAR16                                                         
         CLI   OUTPUT,C'T'                                                      
         BNE   VALPAR16                                                         
         CLI   WRITE,YES                                                        
         BNE   VALPAR16                                                         
         MVI   WRITE,NO                                                         
         MVI   CTFILEW,C'N'                                                     
         MVI   GENFILW,C'N'                                                     
         MVI   GENDIRW,C'N'                                                     
         MVC   P(L'WARN1),WARN1    WRITE=Y IGNORED                              
         GOTOR VPRINTER                                                         
VALPAR16 CLI   WRITE,C' '                                                       
         BH    *+8                                                              
         MVI   WRITE,NO                                                         
         CLI   TRACE,C' '                                                       
         BH    *+8                                                              
         MVI   TRACE,C'N'                                                       
         J     EXIT                                                             
         DROP  RB                                                               
                                                                                
PARAMS   DS    0XL24               ** TABLE OF PARAMETER CARDS **               
                                                                                
         DC    AL1(05),AL3(PARDDIO),CL20'DDSIO='                                
         DC    AL1(06),AL3(PARDSPA),CL20'DSPACE='                               
         DC    AL1(05),AL3(PARTRAC),CL20'TRACE='                                
         DC    AL1(07),AL3(PARRCVR),CL20'RECOVER='                              
         DC    AL1(08),AL3(PARRCVR),CL20'RECOVERY='                             
         DC    AL1(05),AL3(PARWRIT),CL20'WRITE='                                
*        DC    AL1(06),AL3(PARDELS),CL20'DELETE='                               
         DC    AL1(04),AL3(UNDODEL),CL20'UNDO='                                 
         DC    AL1(06),AL3(PAROUTP),CL20'OUTPUT='                               
         DC    AL1(01),AL3(VALPAR10),CL20'/*'                                   
                                                                                
PARAMSX  DC    AL1(0)                                                           
                                                                                
***********************************************************************         
* PARAMETER VALIDATION ROUTINES                                       *         
***********************************************************************         
PARDDIO  L     RF,VDDSIO           DDSIO=                                       
         MVC   0(8,RF),0(R1)                                                    
         BR    RE                                                               
                                                                                
PARDSPA  MVC   SSODSPAC,0(R1)      DSPACE=                                      
         BR    RE                                                               
                                                                                
PARTRAC  MVC   TRACE,0(R1)         TRACE=                                       
         BR    RE                                                               
                                                                                
PARDELS  MVC   DELETE,0(R1)        DELETE=                                      
         MVI   MODE,C'L'            =A1,A2,A3,...ETC                            
         CLC   DELETE(6),=C'ORPHAN'                                             
         BNER  RE                                                               
         MVI   MODE,C'O'            =ORPHAN                                     
         XC    DELETE,DELETE                                                    
         BR    RE                                                               
                                                                                
PAROUTP  MVC   OUTPUT,0(R1)        OUTPUT=                                      
         CLI   0(R1),C'D'           =DISK                                       
         BER   RE                                                               
         CLI   0(R1),C'T'           =TAPE                                       
         BER   RE                                                               
         CLI   0(R1),C'B'           =BOTH                                       
         BER   RE                                                               
         J     VALPAR08                                                         
                                                                                
PARWRIT  MVC   WRITE,0(R1)         WRITE=                                       
         CLI   WRITE,NO                                                         
         BER   RE                                                               
         MVI   CTFILEW,C'U'                                                     
         MVI   GENFILW,C'U'                                                     
         MVI   GENDIRW,C'U'                                                     
         OI    SSOSTAT2,SSOSGALO+SSOSLOCK                                       
         CLI   WRITE,YES            =Y IF LIVE RUN                              
         BER   RE                                                               
         J     VALPAR08                                                         
                                                                                
PARRCVR  OI    SSOSTAT2,SSOSNRCV   RECOVERY=                                    
         CLI   0(R1),C'N'                                                       
         BER   RE                                                               
         MVI   CTRCVRW,C'U'                                                     
         NI    SSOSTAT2,X'FF'-(SSOSNRCV+SSOSROLC)                               
         CLI   0(R1),C'Y'                                                       
         BER   RE                                                               
         OI    SSOSTAT2,SSOSROLC                                                
         CLI   0(R1),C'C'          TEST RECOVER COPIES TOO                      
         BER   RE                                                               
         CLI   0(R1),C'F'          TEST FULL RECOVERY                           
         JNE   VALPAR08                                                         
         OI    SSOFLAG1,SSOFRCVR                                                
         BR    RE                                                               
                                                                                
UNDODEL  MVC   UNDO,0(R1)          UNDO=Y/N                                     
         CLI   UNDO,C'N'                                                        
         BER   RE                                                               
         MVC   WILDEL,WILRES       Change message                               
         MVI   DMIND,X'08'         read for deleted records                     
         BR    RE                                                               
                                                                                
***********************************************************************         
* Constants and variables                                                       
***********************************************************************         
K        EQU   1024                                                             
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
                                                                                
LITERALS DS    0D                                                               
                                                                                
         LTORG                                                                  
                                                                                
COMFACS  DS    0F                                                               
VDMGR    DC    V(DATAMGR)                                                       
                                                                                
VBUFFRIN DC    V(BUFFERIN)                                                      
VCARDS   DC    V(CARDS)                                                         
VCPRINT  DC    V(CPRINT)                                                        
VDDSIO   DC    V(DDSIO)                                                         
VHEXOUT  DC    V(HEXOUT)                                                        
VPRINTER DC    V(PRINTER)                                                       
                                                                                
AIDWORK  DC    A(IDWORK)                                                        
AIO2     DC    A(IO2)                                                           
                                                                                
GENTAPE  DCB   DSORG=PS,MACRF=(GM),RECFM=VB,DDNAME=GENTAPE,EODAD=GENX           
CONTAPE  DCB   DSORG=PS,MACRF=(GM),RECFM=VB,DDNAME=CONTAPE,EODAD=CONX           
                                                                                
DMOPEN   DC    C'OPEN    '                                                      
DMCLSE   DC    C'DMCLSE  '                                                      
DMREAD   DC    C'DMREAD  '                                                      
DMRDHI   DC    C'DMRDHI  '                                                      
DMRSEQ   DC    C'DMRSEQ  '                                                      
DMWRT    DC    C'DMWRT   '                                                      
DMADDREC DC    C'ADDREC  '                                                      
DMGETREC DC    C'GETREC  '                                                      
DMPUTREC DC    C'PUTREC  '                                                      
DMCONSYS DC    C'CONTROL '                                                      
DMCOMMIT DC    C'COMMIT  '                                                      
                                                                                
DMFLIST  DS    0C                                                               
CTFILEW  DC    C'?'                                                             
CTFILE   DC    C'CTFILE '                                                       
GENDIRW  DC    C'?'                                                             
GENDIR   DC    C'GENDIR '                                                       
GENFILW  DC    C'?'                                                             
GENFIL   DC    C'GENFIL '                                                       
CTRCVRW  DC    C'?'                                                             
CTRCVR   DC    C'CTRCVR '                                                       
         DC    C'X'                                                             
                                                                                
RECCON01 DC    C'Reading Access records (CT5REC)'                               
RECCON02 DC    C'Reading ID records (CTIREC)'                                   
RECCON03 DC    C'Reading List records (CTWREC)'                                 
RECCON04 DC    C'Reading User Profile records (CTUREC)'                         
RECCON05 DC    C'Reading Report Profile records (CTPREC)'                       
RECCON06 DC    C'Reading Password records (CT0REC)'                             
RECCON07 DC    C'Reading Access records (SAASSUBQ)'                             
RECCON08 DC    C'Reading Field Control records (SAFCSUBQ)'                      
RECCON09 DC    C'Reading Option Control records (SAOCSUBQ)'                     
RECCON10 DC    C'Reading Office records (SAOFSUBQ)'                             
RECCON11 DC    C'Reading Department records (SADPSUBQ)'                         
RECCON12 DC    C'Reading Person records (SAPESUBQ)'                             
RECCON13 DC    C'Reading Person Access Group records (SAAGSUBQ)'                
RECCON14 DC    C'Reading Limit Access Group records (SALASUBQ)'                 
RECCON15 DC    C'Reading Limit Access List records (SALMSUBQ)'                  
RECCON16 DC    C'Reading Time Sheet Approver records (SAAPSUBQ)'                
RECCON17 DC    C'Reading FAX Information records (CTFXREC)'                     
RECCON19 DC    C'Reading DRIVER User records (CT02REC)'                         
RECCON20 DC    C'Reading CPP Extract Rules records (CTXREC)'                    
RECCON21 DC    C'Reading Writer Definition records (CT$REC)'                    
                                                                                
RECGEN01 DC    C'Reading RFP Group records (GRPKSTYQ)'                          
RECGEN02 DC    C'Reading Narrative records (GNKRECQ)'                           
RECGEN03 DC    C'Reading Exchange records (GEKRECQ)'                            
RECGEN04 DC    C'Reading Media Office records (GEGENOFF)'                       
RECGEN05 DC    C'Reading ESS BDE Reform records (GEGENBDR)'                     
RECGEN06 DC    C'Reading Extract Control Agency records (GXAKRECQ)'             
RECGEN07 DC    C'Reading Extract Control ESS records (GXSKRECQ)'                
RECGEN08 DC    C'Reading Extract Control File records (GXFKRECQ)'               
RECGEN09 DC    C'Reading Extract Passive ESS Agy records (GXSARECQ)'            
RECGEN10 DC    C'Reading Extract Passive Agy File records (GXAFRECQ)'           
RECGEN11 DC    C'Reading Extract Passive ESS File records (GXSFRECQ)'           
                                                                                
ACSLIT   DC    C'Access record'                                                 
UIDLIT   DC    C'User-ID record'                                                
LSTLIT   DC    C'List record'                                                   
UPRLIT   DC    C'User Profile record'                                           
RPRLIT   DC    C'Report Profile record'                                         
PW#LIT   DC    C'Password Number record'                                        
PWCLIT   DC    C'Password Code record'                                          
PWNLIT   DC    C'Password Name record'                                          
SACLIT   DC    C'Security Access record'                                        
SAFLIT   DC    C'Field Control record'                                          
SAOLIT   DC    C'Option Control record'                                         
OFFLIT   DC    C'Office record'                                                 
DEPLIT   DC    C'Department record'                                             
PERLIT   DC    C'Person record'                                                 
PAGLIT   DC    C'Person Access Group record'                                    
LAGLIT   DC    C'Limit Access Group record'                                     
LALLIT   DC    C'Limit Access List record'                                      
TSALIT   DC    C'Time Sheet Approver record'                                    
RFPLIT   DC    C'RFP Group record'                                              
NARLIT   DC    C'Narrative record'                                              
EXCLIT   DC    C'Exchange record'                                               
FAXLIT   DC    C'FAX Information record'                                        
DRULIT   DC    C'DRIVER User record'                                            
CPPLIT   DC    C'CPP Extract Rules record'                                      
WRILIT   DC    C'Writer Definition record'                                      
TRMLIT   DC    C'Terminal record'                                               
MOFLIT   DC    C'Media Office record'                                           
BDRLIT   DC    C'ESS BDE Reform record'                                         
XTRLIT   DC    C'Extract Control record'                                        
                                                                                
TITLE1   DC    C'Control Cards'                                                 
TITLE2   DC    C'Add record Trace'                                              
                                                                                
ERROR1   DC    C'DELETE= card missing - Run aborted'                            
ERROR2   DC    C'Invalid Agency Alpha-ID='                                      
ERROR3   DC    C'Invalid control card'                                          
ERROR4   DC    C'OUTPUT=DISK/TAPE/BOTH card missing - Run aborted'              
                                                                                
WARN1    DC    C'WRITE=YES ignored when OUTPUT=TAPE'                            
                                                                                
WILDEL   DC    C'will be deleted '                                              
WILRES   DC    C'will be restored'                                              
WILKEPT  DC    C'will be kept'                                                  
ALLLIT   DC    C'All'                                                           
UPDLIT   DC    C'Updating files for real'                                       
GETLIT   DC    C'Reading BUFFERIN file to check file keys'                      
DAPALIT  DC    C'DARE Partner Code found'                                       
DAPRLIT  DC    C'DARE Prefix Record found'                                      
                                                                                
CTFILELD DC    C'Total CTFILE records='                                         
CTFILED  DC    PL4'0'                                                           
GENDIRLD DC    C'Total GENDIR records='                                         
GENDIRD  DC    PL4'0'                                                           
GENFILLD DC    C'Total GENFIL records='                                         
GENFILD  DC    PL4'0'                                                           
                                                                                
COMTALLY DC    PL3'0'                                                           
COMLIMIT DC    PL3'100'                                                         
                                                                                
PONE     DC    P'1'                                                             
PNINETY  DC    P'90'                                                            
RE00     DC    C'RE00'                                                          
HEXTOG   DC    C'TOG'                                                           
                                                                                
*BUFFER   BUFFD TYPE=D,KEYLEN=BUFKEYL,COMLEN=BUFDTAL,BUFFERS=200                
                                                                                
***********************************************************************         
* SSB and ULT. Entry points for DMDMGRL (connection to DDSIO)                   
***********************************************************************         
         DS    0D                                                               
         DC    C'*SSB*SSB*SSB*SSB*SSB*SSB*SSB*SSB'                              
SSB      DS    0F                                                               
         DC    XL(SSOOFFX-SSOOFF)'00'                                           
         ORG   SSB+(SSOXTND-SSBD)                                               
         DC    X'FF'               SET EXTENED SSB                              
         ORG   SSB+(SSOSTAT2-SSBD)                                              
         DC    AL1(SSOSNRCV)       SET NO RECOVERY                              
         ORG                                                                    
*                                                                               
         DS    0D                                                               
         DC    CL16'*UTL*UTL*UTL*UTL'                                           
UTL      DC    (TUTLXALN)X'00'                                                  
         ORG   UTL+(TSYS-UTLD)                                                  
         DC    X'0A'               CONTROL                                      
         ORG                                                                    
*                                                                               
         DS    0D                                                               
         DC    C'**AIO2**AIO2**AIO2**AIO2**AIO2**'                              
IO2LNQ   EQU   2000                                                             
IO2      DS    (IO2LNQ)X                                                        
         DS    0D                                                               
         DC    C'*IDWORK**IDWORK**IDWORK**IDWORK*'                              
IDWORKMX EQU   1000                                                             
IDWORK   DS    (IDWORKMX)XL12      LIST OF DESTINATION/COMPATIBLE IDS           
                                                                                
         DC    C'**WORKC***WORKC***WORKC***WORKC*'                              
WORKC    DS    (200*K)X                                                         
         EJECT                                                                  
WORKD    DSECT ,                   ** WORK AREA **                              
                                                                                
DUB      DS    D                                                                
DMCB     DS    6F                                                               
PARM     DS    6F                                                               
WORK     DS    XL256                                                            
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
EXITCC   DS    X                                                                
DISK     DS    F                                                                
IOWORK   DS    XL64                                                             
GENDRKEY DS    XL32                                                             
                                                                                
FLAG     DS    X                                                                
FLAGDAPA EQU   X'80'               Dare Partner Code Found                      
FLAGDAPR EQU   X'40'               Dare Prefix Record Found                     
                                                                                
TSTDELS  DS    0X                                                               
TSTAGY   DS    CL2                                                              
TSTAGYKD DS    C                                                                
TSTUID   DS    XL2                                                              
TSTUIDAG DS    CL2                                                              
TSTUIDKD DS    C                                                                
TSTDELL  EQU   *-TSTDELS                                                        
                                                                                
RUNVALS  DS    0X                  ** RUN TIME PARAMETERS **                    
DELETE   DS    CL72                                                             
MODE     DS    C                                                                
OUTPUT   DS    C                                                                
TRACE    DS    C                                                                
WRITE    DS    C                                                                
UNDO     DS    C                   Default is NO                                
DMIND    DS    X                                                                
RUNVALL  EQU   *-RUNVALS                                                        
                                                                                
C        DS    CL80                                                             
                                                                                
SAVEKEY  DS    CL42                saved key                                    
                                                                                
IOL      DS    F                                                                
IO       DS    XL2048                                                           
                                                                                
WORKL    EQU   *-WORKD                                                          
                                                                                
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
CT$RECD  DSECT                     ** WRITER DEFINITION RECORDS **              
CT$KEY   DS    0XL25                                                            
CT$KTYPE DS    X                                                                
CT$KTYPQ EQU   X'01'                                                            
         DS    XL11                                                             
CT$KAGY  DS    CL2                                                              
CT$KSYS  DS    X                                                                
CT$KPRG  DS    X                                                                
CT$KPHAS DS    X                                                                
CT$KNAME DS    CL8                                                              
CT$END   DS    0X                                                               
       ++INCLUDE CTGENRFP                                                       
       ++INCLUDE GEGENNAR                                                       
       ++INCLUDE GEGENEXC                                                       
       ++INCLUDE GEGENBDR                                                       
       ++INCLUDE GEGENXTR                                                       
       ++INCLUDE GEGENREF                                                       
*&&US                                                                           
       ++INCLUDE GEGENEDI                                                       
       ++INCLUDE GEGENOFF                                                       
*&&                                                                             
CTLSTD   DSECT                                                                  
         ORG   CTLSTDTA                                                         
CTLSTIDC DS    CL(L'CTIKID)                                                     
CTLSTID# DS    XL(L'CTIKNUM)                                                    
       ++INCLUDE DDBUFFD                                                        
       ++INCLUDE DDDPRINTL                                                      
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE FASYSLSTD                                                      
       ++INCLUDE FADSECTS                                                       
         PRINT OFF                                                              
       ++INCLUDE SEACSFILE                                                      
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001CTADDREC  10/15/19'                                      
         END                                                                    
