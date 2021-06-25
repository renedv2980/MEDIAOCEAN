*          DATA SET CTCONVID4  AT LEVEL 221 AS OF 05/01/02                      
*PHASE CONVID4                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE DMUTLCT                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTERL                                                               
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE GETFACT                                                                
*INCLUDE CARDS                                                                  
         TITLE 'CONVID4 - ID RECORD CLEAR EXTERNAL'                             
CONVID4  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE WORKX-WORKD,**CONV**,RA,WORK=A(WORKC),CLEAR=YES                  
         USING WORKD,RC            RC=A(GLOBAL W/S)                             
         L     R9,VCPRINT                                                       
         USING DPRINT,R9                                                        
         MVC   TITLE(26),=C'ID RECORD CLEARANCE REPORT'                         
         B     MAIN                                                             
         EJECT                                                                  
***********************************************************************         
* MAIN CONTROL CODE                                                   *         
***********************************************************************         
MAIN     BAS   RE,GENINIT          GENERAL INTIALISATION                        
         BAS   RE,VALCARDS         VALIDATE JCL CARD DATA LINE                  
         BNE   MERR                  EXIT IF ERROR                              
*                                                                               
         BAS   RE,READAGY          READ DDIN & BUILD AGENCY TABLE               
         BNE   MERR                  EXIT IF ERROR                              
*                                                                               
         BAS   RE,READUID          READ CTFILE & BUILD USER-ID TABLE            
         BNE   MERR                  EXIT IF ERROR                              
*                                                                               
         BAS   RE,READCTF          READ CONTROL FILE RECORDS                    
         BNE   MERR                  EXIT IF ERROR                              
         B     MXIT                                                             
*                                                                               
MERR     MVI   RETCODE,0                                                        
         B     MXIT                                                             
*                                                                               
MXIT     XBASE RC=RETCODE,RL=1                                                  
         EJECT                                                                  
***********************************************************************         
* GENERAL INITIALISATION                                              *         
***********************************************************************         
GENINIT  NTR1  ,                                                                
         MVI   RETCODE,X'FF'                                                    
         MVI   ERROR,0                                                          
         MVI   CONTROLF,0                                                       
         MVI   LASTTYPE,0                                                       
         L     RF,=A(AGYTAB)                                                    
         ST    RF,AAGYTAB                                                       
         ST    RF,AAGYTABL                                                      
         L     RF,=A(AGYTABX)                                                   
         ST    RF,AAGYTABX                                                      
         L     RF,=A(UIDTAB)                                                    
         ST    RF,AUIDTAB                                                       
         ST    RF,AUIDTABL                                                      
         L     RF,=A(UIDTABX)                                                   
         ST    RF,AUIDTABX                                                      
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE JCL CARD DATA LINES AS DEFINED IN CARDTBL                  *         
***********************************************************************         
         SPACE 1                                                                
VALCARDS NTR1                                                                   
*                                                                               
VCLP1    GOTO1 VCARDS,DMCB,P,=C'RE00'                                           
         CLC   =C'/*',P            IF END OF JCL                                
         BE    VCEND                 CHECK REQUIRED CARDS INPUT                 
*                                                                               
         LA    RE,CARDTBL          INITIALISE TABLE POINTER                     
*                                                                               
VCLP2    CLI   0(RE),0             END OF TABLE                                 
         BE    VCERR1              CARD NOT IN TABLE                            
         SR    RF,RF                                                            
         IC    RF,CLENGTH(RE)                                                   
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   P(0),CSTRING(RE)    COMPARE STRING                               
         BE    VCLP2X                                                           
         LA    RE,L'CARDTBL(RE)    GET NEXT ENTRY                               
         B     VCLP2                                                            
*                                                                               
VCLP2X   SR    RF,RF               MATCH FOUND                                  
         IC    RF,CROUTINE(RE)                                                  
         SLL   RF,2                                                             
         B     *+0(RF)             BRANCH TO PROCESSING ROUTINE                 
*                                    FROM JUMP TABLE                            
         B     VCLIST                                                           
         B     VCUPDATE                                                         
*                                  CARD DATA ERROR CONDITIONS                   
VCEND    B     VCYES                                                            
*                                  CARD DATA ERROR CONDITIONS                   
VCERR1   GOTO1 VPRINTER            INVALID CARD                                 
         MVI   ERROR,1                                                          
         BAS   RE,ERRPRT                                                        
         B     VCNO                                                             
*                                                                               
VCNO     B     NO                  EXIT ERROR CONDITION                         
*                                                                               
VCYES    B     YES                 EXIT OK                                      
         EJECT                                                                  
***********************************************************************         
* ROUTINES TO PROCESS EACH JCL CARD DATA LINE                         *         
***********************************************************************         
         SPACE 1                                                                
VCLIST   EQU   *                   LIST=                                        
         CLC   P+5(3),=C'ALL'                                                   
         BNE   VCLP1                                                            
         OI    CONTROLF,LISTALL                                                 
         B     VCLP1                                                            
*                                                                               
VCUPDATE EQU   *                   UPDATE=                                      
         CLC   P+7(3),=C'YES'                                                   
         BNE   VCLP1                                                            
         OI    CONTROLF,UPDATEY                                                 
         B     VCLP1                                                            
         EJECT                                                                  
***********************************************************************         
* READ AGENCY LIST FROM DDIN AND BUILD TABLE                          *         
***********************************************************************         
READAGY  NTR1  ,                                                                
         L     R2,AAGYTAB                                                       
         OPEN  (DDIN,INPUT)                                                     
*                                                                               
RAGY010  GET   DDIN,LINEBUFF                                                    
         B     RAGY020                                                          
EODADDI  CLOSE (DDIN)                                                           
         B     RAGYOK                                                           
*                                                                               
RAGY020  MVC   0(2,R2),LINEBUFF+2  SAVE AGENCY ALPHA                            
         MVC   2(1,R2),LINEBUFF    SAVE +/- UPDATE CODE                         
         LA    R2,3(R2)                                                         
         C     R2,AAGYTABX                                                      
         BL    *+6                                                              
         DC    H'00'                                                            
         B     RAGY010                                                          
*                                                                               
RAGYOK   ST    R2,AAGYTABL                                                      
         B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* READ CONTROL FILE USER ID RECORDS & BUILD VALID ID TABLE            *         
***********************************************************************         
READUID  NTR1  ,                                                                
         ZAP   LINE,=P'99'                                                      
         MVC   P(19),=C'USER-ID/AGENCY LIST'                                    
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         MVI   WORK+00,C'N'        OPEN CONTROL FILE                            
         MVC   WORK+01(7),CTFILE                                                
         MVI   WORK+08,C'X'                                                     
         GOTO1 VDATAMGR,DMCB,DMOPEN,CONTROL,WORK,IO                             
*                                                                               
         LA    R2,IO                                                            
         USING CTIREC,R2                                                        
         XC    CTIKEY,CTIKEY                                                    
         GOTO1 ,DMCB,DMRDHI,CTFILE,CTIKEY,CTIKEY                                
*                                                                               
RUID010  GOTO1 VDATAMGR,DMCB       GET FIRST/NEXT RECORD                        
         LA    R0,DMRSEQ                                                        
         ST    R0,DMCB                                                          
         CLI   8(R1),0                                                          
         BE    *+14                                                             
         TM    8(R1),X'80'                                                      
         BNZ   RUID2000                                                         
         DC    H'0'                                                             
*                                                                               
         CLI   CTIKTYP,CTIKTYPQ                                                 
         BE    RUID020                                                          
         B     RUID010                                                          
*                                                                               
RUID020  OC    CTIKID,CTIKID                                                    
         BZ    RUID010                                                          
         OC    CTIKID(8),CTIKID                                                 
         BZ    RUID200                                                          
         MVC   P(10),CTIKID                                                     
         MVC   USERALPH,CTIKID                                                  
*                                  EXTRACT USER ID NUMBER                       
         LA    R3,CTIDATA                                                       
         SR    R0,R0                                                            
         USING CTDSCD,R3                                                        
RUID030  CLI   CTDSCEL,0                                                        
         BNE   RUID040                                                          
         MVC   P+60(30),=C'MISSING NUMBER POINTER ELEMENT'                      
         GOTO1 VHEXOUT,PARM,(R2),P,25,=C'TOG'                                   
         GOTO1 VPRINTER                                                         
         B     RUID010                                                          
RUID040  CLI   CTDSCEL,CTDSCELQ                                                 
         BNE   RUID050                                                          
         MVC   USERNO,CTDSC        SAVE USER ID NUMBER                          
         B     RUID100                                                          
RUID050  IC    R0,CTDSCLEN                                                      
         AR    R3,R0                                                            
         B     RUID030                                                          
         DROP  R3                                                               
*                                  PROCESS AGENCY ALPHA ELEMENT                 
RUID100  LA    R3,CTIDATA                                                       
         SR    R0,R0                                                            
         USING CTAGYD,R3                                                        
RUID110  CLI   CTAGYEL,0                                                        
         BNE   RUID112                                                          
         MVC   P+60(22),=C'MISSING AGENCY ELEMENT'                              
         GOTO1 VHEXOUT,PARM,(R2),P,25,=C'TOG'                                   
         GOTO1 VPRINTER                                                         
         B     RUID010                                                          
RUID112  CLI   CTAGYEL,CTAGYELQ                                                 
         BNE   RUID120                                                          
         B     RUID130                                                          
RUID120  IC    R0,CTAGYLEN                                                      
         AR    R3,R0                                                            
         B     RUID110                                                          
*                                  GET CTAGYD DATA                              
RUID130  EQU   *                                                                
         MVC   P+12(2),CTAGYID                                                  
         MVC   AGYALPH,CTAGYID                                                  
         BAS   RE,VALAGY                                                        
         BNE   RUID150                                                          
         CLI   AGYIND,C'+'                                                      
         BNE   RUID140                                                          
         TM    CONTROLF,LISTALL                                                 
         BNO   RUID1000                                                         
         GOTO1 VPRINTER                                                         
         B     RUID1000                                                         
RUID140  MVC   P+16(20),=C'AGENCY TO BE REMOVED'                                
         GOTO1 VPRINTER                                                         
         B     RUID010                                                          
RUID150  MVC   P+16(16),=C'AGENCY NOT FOUND'                                    
         GOTO1 VPRINTER                                                         
         B     RUID010                                                          
         DROP  R3                                                               
*                                                                               
RUID200  LA    R3,CTIDATA                                                       
         SR    R0,R0                                                            
         USING CTAGYD,R3                                                        
RUID210  CLI   CTAGYEL,0                                                        
         BNE   RUID212                                                          
         MVC   P+60(22),=C'MISSING AGENCY ELEMENT'                              
         GOTO1 VHEXOUT,PARM,(R2),P,25,=C'TOG'                                   
         GOTO1 VPRINTER                                                         
         B     RUID010                                                          
RUID212  CLI   CTAGYEL,CTAGYELQ                                                 
         BNE   RUID220                                                          
         B     RUID230                                                          
RUID220  IC    R0,CTAGYLEN                                                      
         AR    R3,R0                                                            
         B     RUID210                                                          
*                                  GET CTAGYD DATA                              
RUID230  EQU   *                                                                
         MVC   AGYALPH,CTAGYID                                                  
         BAS   RE,VALAGY                                                        
         BNE   RUID010                                                          
         CLI   AGYIND,C'+'                                                      
         BNE   RUID010                                                          
         B     RUID010                                                          
         DROP  R3                                                               
*                                                                               
RUID1000 L     RF,AUIDTABL                                                      
         MVC   0(10,RF),USERALPH      SAVE USER ID ALPHA                        
         MVC   10(2,RF),USERNO        SAVE USER ID NUMBER                       
         LA    RF,12(RF)                                                        
         C     RF,AUIDTABX                                                      
         BL    *+6                                                              
         DC    H'00'                                                            
         ST    RF,AUIDTABL                                                      
         B     RUID010                                                          
*                                                                               
RUID2000 MVC   P,SPACES                                                         
         GOTO1 VPRINTER                                                         
         MVC   P(26),=C'END OF USER-ID/AGENCY LIST'                             
         GOTO1 VPRINTER                                                         
         B     RUIDOK                                                           
*                                                                               
RUIDNO   B     NO                                                               
*                                                                               
RUIDOK   B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* READ CONTROL FILE RECORDS AND BUILD LISTS                           *         
***********************************************************************         
READCTF  NTR1  ,                                                                
         MVI   WORK+00,C'N'        OPEN CONTROL FILE                            
         MVC   WORK+01(7),CTFILE                                                
         MVI   WORK+08,C'X'                                                     
         GOTO1 VDATAMGR,DMCB,DMOPEN,CONTROL,WORK,IO                             
*                                                                               
         OPEN  (TAPEOUT,OUTPUT)                                                 
         LA    R2,IO                                                            
         XC    IOKEY,IOKEY                                                      
         GOTO1 VDATAMGR,DMCB,DMRDHI,CTFILE,IOKEY,IO                             
         B     RCTF030                                                          
*                                                                               
RCTF010  GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,IOKEY,IO                             
         CLI   8(R1),0                                                          
         BE    RCTF020                                                          
         B     RCTF040                                                          
*                                                                               
RCTF020  GOTO1 VDATAMGR,DMCB,DMRSEQ,CTFILE,IOKEY,IO                             
*                                                                               
RCTF030  CLI   8(R1),0                                                          
         BE    RCTF050                                                          
*                                                                               
RCTF040  TM    8(R1),X'80'                                                      
         BNZ   RCTF2000                                                         
         DC    H'0'                                                             
*                                  PROCESS EACH RECORD TYPE                     
RCTF050  MVC   IOKEY,IO              ACTION FROM LOOK UP TABLE                  
         LA    R4,ACTNTBL          INITIALISE TABLE POINTER                     
*                                                                               
RCTF100  CLI   0(R4),0             END OF TABLE                                 
         BE    RCTF120               RECORD TYPE NOT IN TABLE                   
         CLC   IO(1),ATYPE(R4)     COMPARE RECORD TYPE                          
         BE    RCTF110                                                          
         LA    R4,L'ACTNTBL(R4)    GET NEXT ENTRY                               
         B     RCTF100                                                          
*                                  MATCH FOUND                                  
RCTF110  MVC   P,SPACES                                                         
         CLC   ATYPE(1,R4),LASTTYPE                                             
         BE    RCTF112                                                          
         ZAP   LINE,=P'99'                                                      
         GOTO1 VPRINTER                                                         
         MVC   P(30),AHEAD(R4)     IF NEW TYPE PRINT HEADING                    
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         MVC   LASTTYPE,ATYPE(R4)    AND SAVE TYPE                              
RCTF112  MVC   P(1),IO             MATCH FOUND                                  
         SR    RF,RF                                                            
         IC    RF,AROUTINE(R4)                                                  
         SLL   RF,2                                                             
         B     *+0(RF)             BRANCH TO PROCESSING ROUTINE                 
*                                    FROM JUMP TABLE                            
         B     AUID                USERID RECORDS                               
         B     AACC                ACCESS RECORDS                               
         B     AAUT                AUTHORISATION RECORDS                        
         B     APRO                PROFILE RECORDS                              
         B     ATER                TERMINAL RECORDS                             
         B     ASLS                SYSLIST RECORDS                              
         B     ATWX                TWX ADDRESSEE RECORDS                        
         B     ACOM                COMMENT (BOOK) RECORDS                       
         B     AINT                INTERFACE RECORDS                            
         B     ADSC                DSCHEME RECORDS                              
         B     ADRU                DRIVER USER RECORDS                          
         B     ADNA                DEMO NAME RECORDS                            
         B     ADFO                DEMO FORMULA RECORDS                         
         B     AMAR                MARKET AUTHORISATION RECORDS                 
         B     ADCO                DEMO CODE RECORDS                            
         B     ADAD                DEMO ADJUSTMENT RECORDS                      
         B     ADAC                DEMO AGENCY CONTROL RECORDS                  
         B     AEXR                CPP EXTRACT RULES RECORDS                    
         B     APRF                CPP PROJECTION FORMULA RECORDS               
*                                  TYPE NOT IN ACTION TABLE                     
RCTF120  B     RCTF010                                                          
         GOTO1 VPRINTER                                                         
         MVI   ERROR,1                                                          
         BAS   RE,ERRPRT                                                        
         B     RCTFNO                                                           
*                                  COMPLETE RECORD PROCESSING                   
         USING CTIREC,R2                                                        
RCTF1000 SR    RE,RE                                                            
         ICM   RE,3,CTILEN                                                      
         LA    RE,4(RE)                                                         
         SLL   RE,16                                                            
         STCM  RE,15,IOL                                                        
         TM    CONTROLF,UPDATEY                                                 
         BNO   RCTF010                                                          
         PUT   TAPEOUT,IOL                                                      
         B     RCTF010                                                          
*                                                                               
RCTF2000 MVC   P,SPACES                                                         
         GOTO1 VPRINTER                                                         
         MVC   P(13),=C'END OF REPORT'                                          
         GOTO1 VPRINTER                                                         
         CLOSE (TAPEOUT)                                                        
         B     RCTFOK                                                           
*                                                                               
RCTFNO   B     NO                  EXIT ERROR CONDITION                         
*                                                                               
RCTFOK   B     YES                 EXIT OK                                      
         EJECT                                                                  
***********************************************************************         
* PROCESS USER ID RECORDS                                             *         
***********************************************************************         
         SPACE 1                                                                
         USING CTIREC,R2                                                        
AUID     EQU   *                                                                
         OC    CTIKID,CTIKID                                                    
         BZ    AUIDOK              IGNORE ID# HEADER RECORD                     
         OC    CTIKID(8),CTIKID                                                 
         BZ    AUID010                                                          
         MVC   USERALPH,CTIKID     PROCESS IDALPHA RECORD                       
         MVC   P+2(10),USERALPH                                                 
         BAS   RE,VALUAL                                                        
         BNE   AUID300                                                          
         B     AUID100                                                          
AUID010  MVC   USERNO,CTIKNUM      PROCESS ID# RECORD                           
         GOTO1 VHEXOUT,PARM,CTIKNUM,P+2,2,=C'TOG'                               
         BAS   RE,VALUNO                                                        
         BNE   AUID300                                                          
         B     AUID100                                                          
*                                  PROCESS ELEMENTS                             
AUID100  LA    R3,CTIDATA                                                       
         SR    R0,R0                                                            
AUID110  CLI   0(R3),0                                                          
         BE    AUIDOK                                                           
         CLI   0(R3),CTPASELQ      PRINCIPLE ID                                 
         BE    AUID130                                                          
         CLI   0(R3),CTIDELQ       COMPATIBLE ID                                
         BE    AUID140                                                          
         CLI   0(R3),CTVALELQ      DESTINATION ID                               
         BE    AUID150                                                          
AUID120  IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     AUID110                                                          
*                                  PRINCIPLE ID                                 
AUID130  EQU   *                                                                
         MVC   USERNO,2(R3)                                                     
         BAS   RE,VALUNO                                                        
         BE    AUID120                                                          
         BAS   RE,DELETE                                                        
         GOTO1 VHEXOUT,PARM,USERNO,P+30,2,=C'TOG'                               
         MVC   P+36(28),=C'PRINCIPLE ID ELEMENT DELETED'                        
         GOTO1 VPRINTER                                                         
         B     AUID110                                                          
*                                  COMPATIBLE ID                                
AUID140  EQU   *                                                                
         OC    2(2,R3),2(R3)       IGNORE LIST ENTRY                            
         BZ    AUID120                                                          
         MVC   USERALPH,2(R3)                                                   
         BAS   RE,VALUAL                                                        
         BE    AUID120                                                          
         BAS   RE,DELETE                                                        
         MVC   P+30(10),USERALPH                                                
         MVC   P+42(29),=C'COMPATIBLE ID ELEMENT DELETED'                       
         GOTO1 VPRINTER                                                         
         B     AUID110                                                          
*                                  DESTINATION ID                               
AUID150  EQU   *                                                                
         OC    2(2,R3),2(R3)       IGNORE LIST ENTRY                            
         BZ    AUID120                                                          
         MVC   USERALPH,2(R3)                                                   
         BAS   RE,VALUAL                                                        
         BE    AUID120                                                          
         BAS   RE,DELETE                                                        
         MVC   P+30(10),USERALPH                                                
         MVC   P+42(30),=C'DESTINATION ID ELEMENT DELETED'                      
         GOTO1 VPRINTER                                                         
         B     AUID110                                                          
*                                                                               
AUID300  MVC   P+30(28),=C'USER ID RECORD TO BE REMOVED'                        
         GOTO1 VPRINTER                                                         
         B     RCTF010                                                          
*                                                                               
AUIDOK   B     RCTF1000                                                         
         EJECT                                                                  
***********************************************************************         
* PROCESS ACCESS RECORDS                                              *         
***********************************************************************         
         SPACE 1                                                                
         USING CT5REC,R2                                                        
AACC     EQU   *                                                                
         MVC   AGYALPH,CT5KALPH                                                 
         MVC   P+2(2),AGYALPH                                                   
         BAS   RE,VALAGY                                                        
         BE    *+6                                                              
         DC    H'00'                                                            
         CLI   AGYIND,C'+'                                                      
         BNE   AACC300                                                          
         B     AACC100                                                          
*                                  PROCESS ELEMENTS                             
AACC100  LA    R3,CT5DATA                                                       
         SR    R0,R0                                                            
AACC110  CLI   0(R3),0                                                          
         BE    AACCOK                                                           
         CLI   0(R3),CTDSCELQ      PRINCIPLE ID                                 
         BE    AACC130                                                          
         CLI   0(R3),CTAGDELQ      AGENCY GROUP DETAILS                         
         BE    AACC140                                                          
AACC120  IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     AACC110                                                          
*                                  PRINCIPLE ID                                 
AACC130  EQU   *                                                                
         MVC   USERNO,2(R3)                                                     
         BAS   RE,VALUNO                                                        
         BE    AACC120                                                          
*        BAS   RE,DELETE                                                        
         GOTO1 VHEXOUT,PARM,USERNO,P+30,2,=C'TOG'                               
         MVC   P+36(20),=C'PRINCIPLE ID INVALID'                                
         GOTO1 VPRINTER                                                         
*        B     AACC110                                                          
         B     AACC120                                                          
*                                  AGENCY GROUP DETAILS                         
         USING CTAGDD,R3                                                        
AACC140  EQU   *                                                                
         MVC   USERNO,CTAGDHG                                                   
         BAS   RE,VALUNO                                                        
         BE    AACC142                                                          
*        BAS   RE,DELETE                                                        
         GOTO1 VHEXOUT,PARM,USERNO,P+30,2,=C'TOG'                               
         MVC   P+36(18),=C'HOLDING ID INVALID'                                  
         GOTO1 VPRINTER                                                         
*        B     AACC110                                                          
         B     AACC120                                                          
AACC142  MVC   USERNO,CTAGDAG                                                   
         BAS   RE,VALUNO                                                        
         BE    AACC120                                                          
*        BAS   RE,DELETE                                                        
         GOTO1 VHEXOUT,PARM,USERNO,P+30,2,=C'TOG'                               
         MVC   P+36(23),=C'GROUP MASTER ID INVALID'                             
         GOTO1 VPRINTER                                                         
*        B     AACC110                                                          
         B     AACC120                                                          
         DROP  R3                                                               
*                                                                               
AACC300  MVC   P+30(27),=C'ACCESS RECORD TO BE REMOVED'                         
         GOTO1 VPRINTER                                                         
         B     RCTF010                                                          
*                                                                               
AACCOK   B     RCTF1000                                                         
         EJECT                                                                  
***********************************************************************         
* PROCESS AUTHORISATION RECORDS                                       *         
***********************************************************************         
         SPACE 1                                                                
         USING CT0REC,R2                                                        
AAUT     EQU   *                                                                
         MVC   P+2(2),CT0KAGY                                                   
         MVC   AGYALPH,CT0KAGY     CHECK AGENCY VALID                           
         BAS   RE,VALAGY                                                        
         BNE   AAUT300                                                          
         OC    CT0KEYS(12),CT0KEYS                                              
         BZ    AAUT002                                                          
         MVC   P+6(20),CT0KEYS     PROCESS AUTH NAME RECORD                     
         B     AAUT100                                                          
*                                                                               
AAUT002  OC    CT0KCODE(8),CT0KCODE                                             
         BZ    AAUT010                                                          
         MVC   P+6(10),CT0KCODE    PROCESS AUTH ALPHA RECORD                    
         B     AAUT100                                                          
*                                  PROCESS AUTH# RECORD                         
AAUT010  GOTO1 VHEXOUT,PARM,CT0KNUM,P+6,2,=C'TOG'                               
         B     AAUT100                                                          
*                                  PROCESS ELEMENTS                             
AAUT100  LA    R3,CT0DATA                                                       
         MVI   PIDREAD,0                                                        
         MVI   PIDDEL,0                                                         
         SR    R0,R0                                                            
AAUT110  CLI   0(R3),0                                                          
         BE    AAUTOK                                                           
         CLI   0(R3),CTIDELQ       COMPATIBLE ID                                
         BE    AAUT130                                                          
AAUT120  IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     AAUT110                                                          
*                                  COMPATIBLE ID                                
AAUT130  EQU   *                                                                
         CLI   PIDDEL,1            IF PID DELETED DELETE ALL ID ELS             
         BE    AAUT132                                                          
         OC    2(2,R3),2(R3)       IGNORE LIST ENTRY                            
         BZ    AAUT120                                                          
         CLI   PIDREAD,0                                                        
         BE    AAUT134                                                          
         MVC   USERALPH,2(R3)                                                   
         BAS   RE,VALUAL                                                        
         BE    AAUT120                                                          
AAUT132  MVC   P+30(10),2(R3)                                                   
         BAS   RE,DELETE                                                        
         MVC   P+42(29),=C'COMPATIBLE ID ELEMENT DELETED'                       
         GOTO1 VPRINTER                                                         
         B     AAUT110                                                          
*                                                                               
AAUT134  MVI   PIDREAD,1           PROCESS PRINCIPLE (FIRST) ID                 
         MVC   USERALPH,2(R3)                                                   
         BAS   RE,VALUAL                                                        
         BE    AAUT120                                                          
         MVI   PIDDEL,1                                                         
         MVC   P+30(10),2(R3)                                                   
         BAS   RE,DELETE                                                        
         MVC   P+42(28),=C'PRINCIPLE ID ELEMENT DELETED'                        
         GOTO1 VPRINTER                                                         
         B     AAUT110                                                          
*                                                                               
AAUT300  MVC   P+30(34),=C'AUTHORISATION RECORD TO BE REMOVED'                  
         GOTO1 VPRINTER                                                         
         B     RCTF010                                                          
*                                                                               
AAUTOK   B     RCTF1000                                                         
         EJECT                                                                  
***********************************************************************         
* PROCESS PROFILE RECORDS                                             *         
***********************************************************************         
         SPACE 1                                                                
         USING CTPREC,R2                                                        
APRO     EQU   *                                                                
         MVC   P+2(1),CTPKSYS                                                   
         MVC   P+4(2),CTPKPROG                                                  
         MVC   USERNO,CTPKORIG                                                  
         GOTO1 VHEXOUT,PARM,CTPKORIG,P+8,2,=C'TOG'                              
         BAS   RE,VALUNO                                                        
         BNE   APRO300                                                          
         B     APRO100                                                          
*                                  PROCESS ELEMENTS                             
APRO100  LA    R3,CTPDATA                                                       
         SR    R0,R0                                                            
APRO110  CLI   0(R3),0                                                          
         BE    APROOK                                                           
         CLI   0(R3),CTDCOELQ      DESTINATION CODE                             
         BE    APRO130                                                          
APRO120  IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     APRO110                                                          
*                                  DESTINATION CODE                             
         USING CTDCOD,R3                                                        
APRO130  EQU   *                                                                
         MVC   USERALPH,CTDCODE                                                 
         BAS   RE,VALUAL                                                        
         BE    APRO120                                                          
         BAS   RE,DELETE                                                        
         MVC   P+30(10),USERALPH                                                
         MVC   P+42(32),=C'DESTINATION CODE ELEMENT DELETED'                    
         GOTO1 VPRINTER                                                         
         B     APRO110                                                          
         DROP  R3                                                               
*                                                                               
APRO300  MVC   P+30(28),=C'PROFILE RECORD TO BE REMOVED'                        
         GOTO1 VPRINTER                                                         
         B     RCTF010                                                          
*                                                                               
APROOK   B     RCTF1000                                                         
         EJECT                                                                  
***********************************************************************         
* PROCESS TERMINAL RECORDS                                            *         
***********************************************************************         
         SPACE 1                                                                
         USING CTTREC,R2                                                        
ATER     EQU   *                                                                
         OC    CTTKSPAR(24),CTTKSPAR                                            
         BZ    ATEROK              IGNORE TERM# HEADER RECORD                   
         OC    CTTKTID(8),CTTKTID                                               
         BZ    ATER010                                                          
*                                  PROCESS TERM ALPHA RECORD                    
         CLI   CTTKTID-1,0                                                      
         BE    *+10                                                             
         MVC   P+2(1),CTTKTID-1                                                 
         MVC   P+4(18),CTTKTID                                                  
         B     ATER100                                                          
*                                  PROCESS TERM# RECORD                         
ATER010  GOTO1 VHEXOUT,PARM,CTTKPASS+8,P+2,2,=C'TOG'                            
         B     ATER100                                                          
*                                  PROCESS ELEMENTS                             
ATER100  LA    R3,CTTDATA                                                       
         MVI   PIDREAD,0                                                        
         MVI   PIDDEL,0                                                         
         SR    R0,R0                                                            
ATER110  CLI   0(R3),0                                                          
         BE    ATEROK                                                           
         CLI   0(R3),CTPIDELQ      PRINCIPLE ID                                 
         BE    ATER130                                                          
         CLI   0(R3),CTIDELQ       COMPATIBLE ID                                
         BE    ATER140                                                          
         CLI   0(R3),CTPRTELQ      PRINTER ID                                   
         BE    ATER150                                                          
         CLI   0(R3),CTPRQELQ      PRINTERQ ENTRY                               
         BE    ATER160                                                          
         CLI   0(R3),CTTRMELQ      TERMINAL DEFN                                
         BE    ATER170                                                          
ATER120  IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     ATER110                                                          
*                                  PRINCIPLE ID                                 
ATER130  EQU   *                                                                
         MVI   PIDREAD,1                                                        
         MVC   USERALPH,2(R3)                                                   
         BAS   RE,VALUAL                                                        
         BE    ATER120                                                          
         MVI   PIDDEL,1                                                         
         MVC   P+30(10),2(R3)                                                   
         BAS   RE,DELETE                                                        
         MVC   P+42(28),=C'PRINCIPLE ID ELEMENT DELETED'                        
         GOTO1 VPRINTER                                                         
         B     ATER110                                                          
*                                  COMPATIBLE ID                                
ATER140  EQU   *                                                                
         CLI   PIDDEL,1            IF PID DELETED DELETE ALL ID ELS             
         BE    ATER142                                                          
         OC    2(2,R3),2(R3)       IGNORE LIST ENTRY                            
         BZ    ATER120                                                          
         CLI   PIDREAD,0                                                        
         BE    ATER130                                                          
         MVC   USERALPH,2(R3)                                                   
         BAS   RE,VALUAL                                                        
         BE    ATER120                                                          
ATER142  MVC   P+30(10),2(R3)                                                   
         BAS   RE,DELETE                                                        
         MVC   P+42(29),=C'COMPATIBLE ID ELEMENT DELETED'                       
         GOTO1 VPRINTER                                                         
         B     ATER110                                                          
*                                  PRINTER ID                                   
         USING CTPRTD,R3                                                        
ATER150  EQU   *                                                                
         MVC   USERNO,CTPRTID                                                   
         BAS   RE,VALUNO                                                        
         BE    ATER120                                                          
         BAS   RE,DELETE                                                        
         GOTO1 VHEXOUT,PARM,USERNO,P+30,2,=C'TOG'                               
         MVC   P+36(26),=C'PRINTER ID ELEMENT DELETED'                          
         GOTO1 VPRINTER                                                         
         B     ATER110                                                          
         DROP  R3                                                               
*                                  PRINTERQ ENTRY                               
         USING CTPRQD,R3                                                        
ATER160  EQU   *                                                                
         MVC   USERNO,CTPRQUSR                                                  
         BAS   RE,VALUNO                                                        
         BE    ATER120                                                          
         BAS   RE,DELETE                                                        
         GOTO1 VHEXOUT,PARM,USERNO,P+30,2,=C'TOG'                               
         MVC   P+36(30),=C'PRINTERQ ENTRY ELEMENT DELETED'                      
         GOTO1 VPRINTER                                                         
         B     ATER110                                                          
         DROP  R3                                                               
*                                  TERMINAL DEFINITION                          
         USING CTTRMD,R3                                                        
ATER170  EQU   *                                                                
         MVC   AGYALPH,CTTRMAGY    CHECK AGENCY VALID                           
         BAS   RE,VALAGY                                                        
         BE    ATER120                                                          
         MVC   CTTRMAGY,SPACES                                                  
         MVC   P+30(2),AGYALPH                                                  
         MVC   P+34(24),=C'AGENCY ALPHA BLANKED OUT'                            
         GOTO1 VPRINTER                                                         
         B     ATER120                                                          
         DROP  R3                                                               
*                                                                               
ATER300  MVC   P+30(29),=C'TERMINAL RECORD TO BE REMOVED'                       
         GOTO1 VPRINTER                                                         
         B     RCTF010                                                          
*                                                                               
ATEROK   B     RCTF1000                                                         
         EJECT                                                                  
***********************************************************************         
* PROCESS SYSLIST RECORDS                                             *         
***********************************************************************         
         SPACE 1                                                                
         USING CTWREC,R2                                                        
ASLS     EQU   *                                                                
         MVC   P+2(9),CTWKAGY                                                   
         MVC   AGYALPH,CTWKAGY     CHECK AGENCY VALID                           
         BAS   RE,VALAGY                                                        
         BNE   ASLS300                                                          
         CLI   CTWKREC,CTWKRUSR    PROCESS IF USERID LIST                       
         BE    ASLS100                                                          
         CLI   CTWKREC,CTWKRIDG      OR IDGROUP                                 
         BNE   ASLSOK                                                           
         B     ASLS100                                                          
*                                  PROCESS ELEMENTS                             
ASLS100  LA    R3,CTWDATA                                                       
         SR    R0,R0                                                            
ASLS110  CLI   0(R3),0                                                          
         BE    ASLSOK                                                           
         CLI   0(R3),CTLSTELQ      LIST                                         
         BE    ASLS130                                                          
ASLS120  IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     ASLS110                                                          
*                                  LIST                                         
         USING CTLSTD,R3                                                        
ASLS130  EQU   *                                                                
         MVC   USERALPH,CTLSTDTA                                                
         BAS   RE,VALUAL                                                        
         BE    ASLS120                                                          
         BAS   RE,DELETE                                                        
         MVC   P+30(10),USERALPH                                                
         MVC   P+42(20),=C'LIST ELEMENT DELETED'                                
         GOTO1 VPRINTER                                                         
         B     ASLS110                                                          
         DROP  R3                                                               
*                                                                               
ASLS300  MVC   P+30(28),=C'SYSLIST RECORD TO BE REMOVED'                        
         GOTO1 VPRINTER                                                         
         B     RCTF010                                                          
*                                                                               
ASLSOK   B     RCTF1000                                                         
         EJECT                                                                  
***********************************************************************         
* PROCESS TWX ADDRESSEE RECORDS                                       *         
***********************************************************************         
         SPACE 1                                                                
         USING CTAREC,R2                                                        
ATWX     EQU   *                                                                
         MVC   USERNO,CTAKUSER                                                  
         GOTO1 VHEXOUT,PARM,USERNO,P+2,2,=C'TOG'                                
         MVC   P+6(7),CTAKID                                                    
         BAS   RE,VALUNO                                                        
         BNE   ATWX300                                                          
         B     ATWX100                                                          
*                                  PROCESS ELEMENTS                             
ATWX100  LA    R3,CTADATA                                                       
         SR    R0,R0                                                            
ATWX110  CLI   0(R3),0                                                          
         BE    ATWXOK                                                           
         CLI   0(R3),CTPASELQ      ID# POINTERS                                 
         BE    ATWX130                                                          
ATWX120  IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     ATWX110                                                          
*                                  ID# POINTER                                  
         USING CTPASD,R3                                                        
ATWX130  EQU   *                                                                
         MVC   USERNO,CTPASDTA                                                  
         BAS   RE,VALUNO                                                        
         BE    ATWX120                                                          
         BAS   RE,DELETE                                                        
         GOTO1 VHEXOUT,PARM,USERNO,P+30,2,=C'TOG'                               
         MVC   P+36(18),=C'ID# ELEMENT DELETED'                                 
         GOTO1 VPRINTER                                                         
         B     ATWX110                                                          
         DROP  R3                                                               
*                                                                               
ATWX300  MVC   P+30(34),=C'TXW ADDRESSEE RECORD TO BE REMOVED'                  
         GOTO1 VPRINTER                                                         
         B     RCTF010                                                          
*                                                                               
ATWXOK   B     RCTF1000                                                         
         EJECT                                                                  
***********************************************************************         
* PROCESS COMMENT (BOOK) RECORDS                                      *         
***********************************************************************         
         SPACE 1                                                                
         USING CTCREC,R2                                                        
ACOM     EQU   *                                                                
         MVC   USERNO,CTCKUSER                                                  
         GOTO1 VHEXOUT,PARM,USERNO,P+2,2,=C'TOG'                                
         MVC   P+6(12),CTCKSYS                                                  
         BAS   RE,VALUNO                                                        
         BNE   ACOM300                                                          
         B     ACOMOK                                                           
*                                                                               
ACOM300  MVC   P+30(35),=C'COMMENT (BOOK) RECORD TO BE REMOVED'                 
         GOTO1 VPRINTER                                                         
         B     RCTF010                                                          
*                                                                               
ACOMOK   B     RCTF1000                                                         
         EJECT                                                                  
***********************************************************************         
* PROCESS INTERFACE RECORDS                                           *         
***********************************************************************         
         SPACE 1                                                                
         USING CTZREC,R2                                                        
AINT     EQU   *                                                                
         MVC   USERNO,CTZUSER                                                   
         GOTO1 VHEXOUT,PARM,USERNO,P+2,2,=C'TOG'                                
         MVC   P+6(4),CTZSYS                                                    
         BAS   RE,VALUNO                                                        
         BNE   AINT300                                                          
         B     AINTOK                                                           
*                                                                               
AINT300  MVC   P+30(30),=C'INTERFACE RECORD TO BE REMOVED'                      
         GOTO1 VPRINTER                                                         
         B     RCTF010                                                          
*                                                                               
AINTOK   B     RCTF1000                                                         
         EJECT                                                                  
***********************************************************************         
* PROCESS DSCHEME RECORDS                                             *         
***********************************************************************         
         SPACE 1                                                                
         USING CTDSREC,R2                                                       
ADSC     EQU   *                                                                
         MVC   P+2(4),CTDSKAGY                                                  
         MVC   AGYALPH,CTDSKAGY    CHECK AGENCY VALID                           
         BAS   RE,VALAGY                                                        
         BNE   ADSC300                                                          
         B     ADSCOK                                                           
*                                                                               
ADSC300  MVC   P+30(28),=C'DSCHEME RECORD TO BE REMOVED'                        
         GOTO1 VPRINTER                                                         
         B     RCTF010                                                          
*                                                                               
ADSCOK   B     RCTF1000                                                         
         EJECT                                                                  
***********************************************************************         
* PROCESS DRIVER USER RECORDS                                         *         
***********************************************************************         
         SPACE 1                                                                
         USING CT02REC,R2                                                       
ADRU     EQU   *                                                                
         MVC   P+2(10),CT02KAGY                                                 
         MVC   AGYALPH,CT02KAGY    CHECK AGENCY VALID                           
         BAS   RE,VALAGY                                                        
         BNE   ADRU300                                                          
         B     ADRUOK                                                           
*                                                                               
ADRU300  MVC   P+30(32),=C'DRIVER USER RECORD TO BE REMOVED'                    
         GOTO1 VPRINTER                                                         
         B     RCTF010                                                          
*                                                                               
ADRUOK   B     RCTF1000                                                         
         EJECT                                                                  
***********************************************************************         
* PROCESS DEMO NAME RECORDS                                           *         
***********************************************************************         
         SPACE 1                                                                
         USING CTDREC,R2                                                        
ADNA     EQU   *                                                                
         MVC   P+2(6),CTDKFILE                                                  
         MVC   AGYALPH,CTDKAGY     CHECK AGENCY VALID                           
         BAS   RE,VALAGY                                                        
         BNE   ADNA300                                                          
         B     ADNAOK                                                           
*                                                                               
ADNA300  MVC   P+30(30),=C'DEMO NAME RECORD TO BE REMOVED'                      
         GOTO1 VPRINTER                                                         
         B     RCTF010                                                          
*                                                                               
ADNAOK   B     RCTF1000                                                         
         EJECT                                                                  
***********************************************************************         
* PROCESS DEMO FORMULA RECORDS                                        *         
***********************************************************************         
         SPACE 1                                                                
         USING CTGREC,R2                                                        
ADFO     EQU   *                                                                
         MVC   P+2(10),CTGKFILE                                                 
         MVC   AGYALPH,CTGKAGY     CHECK AGENCY VALID                           
         BAS   RE,VALAGY                                                        
         BNE   ADFO300                                                          
         B     ADFOOK                                                           
*                                                                               
ADFO300  MVC   P+30(34),=C'DEMO FORMULA RECORD TO BE REMOVED'                   
         GOTO1 VPRINTER                                                         
         B     RCTF010                                                          
*                                                                               
ADFOOK   B     RCTF1000                                                         
         EJECT                                                                  
***********************************************************************         
* PROCESS MARKET AUTHORISATION RECORDS                                *         
***********************************************************************         
         SPACE 1                                                                
         USING CTKREC,R2                                                        
AMAR     EQU   *                                                                
         MVC   P+2(4),CTKKUSER                                                  
         MVC   AGYALPH,CTKKUSER    CHECK AGENCY VALID                           
         BAS   RE,VALAGY                                                        
         BNE   AMAR300                                                          
         B     AMAROK                                                           
*                                                                               
AMAR300  MVC   P+30(34),=C'MARKET AUTH. RECORD TO BE REMOVED'                   
         GOTO1 VPRINTER                                                         
         B     RCTF010                                                          
*                                                                               
AMAROK   B     RCTF1000                                                         
         EJECT                                                                  
***********************************************************************         
* DEMO CODE RECORDS                                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING CTNREC,R2                                                        
ADCO     EQU   *                                                                
         MVC   P+2(5),CTNKFILE                                                  
         MVC   AGYALPH,CTNKAGY     CHECK AGENCY VALID                           
         BAS   RE,VALAGY                                                        
         BNE   ADCO300                                                          
         B     ADCOOK                                                           
*                                                                               
ADCO300  MVC   P+30(31),=C'DEMO CODE RECORD TO BE REMOVED'                      
         GOTO1 VPRINTER                                                         
         B     RCTF010                                                          
*                                                                               
ADCOOK   B     RCTF1000                                                         
         EJECT                                                                  
***********************************************************************         
* DEMO ADJUSTMENT RECORDS                                             *         
***********************************************************************         
         SPACE 1                                                                
         USING CTQREC,R2                                                        
ADAD     EQU   *                                                                
         MVC   P+2(11),CTQKSRC                                                  
         MVC   AGYALPH,CTQKAGY     CHECK AGENCY VALID                           
         BAS   RE,VALAGY                                                        
         BNE   ADAD300                                                          
         B     ADADOK                                                           
*                                                                               
ADAD300  MVC   P+30(36),=C'DEMO ADJUSTMENT RECORD TO BE REMOVED'                
         GOTO1 VPRINTER                                                         
         B     RCTF010                                                          
*                                                                               
ADADOK   B     RCTF1000                                                         
         EJECT                                                                  
***********************************************************************         
* DEMO AGENCY CONTROL RECORDS                                         *         
***********************************************************************         
         SPACE 1                                                                
         USING CTRREC,R2                                                        
ADAC     EQU   *                                                                
         MVC   P+2(10),CTRKSRC                                                  
         MVC   AGYALPH,CTRKAGY     CHECK AGENCY VALID                           
         BAS   RE,VALAGY                                                        
         BNE   ADAC300                                                          
         B     ADACOK                                                           
*                                                                               
ADAC300  MVC   P+30(40),=C'DEMO AGENCY CONTROL RECORD TO BE REMOVED'            
         GOTO1 VPRINTER                                                         
         B     RCTF010                                                          
*                                                                               
ADACOK   B     RCTF1000                                                         
         EJECT                                                                  
***********************************************************************         
* CPP EXTRACT RULES RECORDS                                           *         
***********************************************************************         
         SPACE 1                                                                
         USING CTXREC,R2                                                        
AEXR     EQU   *                                                                
         MVC   P+2(15),CTXKAGY                                                  
         MVC   AGYALPH,CTXKAGY     CHECK AGENCY VALID                           
         BAS   RE,VALAGY                                                        
         BNE   AEXR300                                                          
         B     AEXROK                                                           
*                                                                               
AEXR300  MVC   P+30(38),=C'CPP EXTRACT RULES RECORD TO BE REMOVED'              
         GOTO1 VPRINTER                                                         
         B     RCTF010                                                          
*                                                                               
AEXROK   B     RCTF1000                                                         
         EJECT                                                                  
***********************************************************************         
* CPP PROJECTION FORMULA RECORDS                                      *         
***********************************************************************         
         SPACE 1                                                                
         USING CTYREC,R2                                                        
APRF     EQU   *                                                                
         MVC   P+2(5),CTYKAGY                                                   
         MVC   AGYALPH,CTYKAGY     CHECK AGENCY VALID                           
         BAS   RE,VALAGY                                                        
         BNE   APRF300                                                          
         B     APRFOK                                                           
*                                                                               
APRF300  MVC   P+30(43),=C'CPP PROJECTION FORMULA RECORD TO BE REMOVED'         
         GOTO1 VPRINTER                                                         
         B     RCTF010                                                          
*                                                                               
APRFOK   B     RCTF1000                                                         
         EJECT                                                                  
***********************************************************************         
* CHECK ID AGENCY ALPHA IN VALID AGENCY LIST                          *         
***********************************************************************         
VALAGY   NTR1  ,                                                                
         L     RF,AAGYTAB                                                       
         MVI   AGYIND,C'+'         OVERIDE 00 OR FF OR SPACES                   
         OC    AGYALPH,AGYALPH                                                  
         BZ    VAGYOK                                                           
         CLC   AGYALPH,=XL2'FFFF'                                               
         BE    VAGYOK                                                           
         CLC   AGYALPH,SPACES                                                   
         BE    VAGYOK                                                           
VAGY010  CLC   0(2,RF),AGYALPH                                                  
         BE    VAGY020                                                          
         LA    RF,3(RF)                                                         
         C     RF,AAGYTABL                                                      
         BNL   VAGYNO                                                           
         B     VAGY010                                                          
*                                                                               
VAGY020  MVC   AGYIND,2(RF)                                                     
         B     VAGYOK                                                           
*                                                                               
VAGYNO   B     NO                                                               
*                                                                               
VAGYOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* CHECK USER ID ALPHA CODE IN VALID LIST                              *         
***********************************************************************         
VALUAL   NTR1  ,                                                                
         L     RF,AUIDTAB                                                       
         CLC   USERALPH,=CL10'ALL       '                                       
         BE    VUALOK                                                           
VUAL010  CLC   0(10,RF),USERALPH                                                
         BE    VUALOK                                                           
         LA    RF,12(RF)                                                        
         C     RF,AUIDTABL                                                      
         BNL   VUALNO                                                           
         B     VUAL010                                                          
*                                                                               
VUALNO   B     NO                                                               
*                                                                               
VUALOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* CHECK USER ID NUMBER IN VALID LIST                                  *         
***********************************************************************         
VALUNO   NTR1  ,                                                                
         L     RF,AUIDTAB                                                       
         OC    USERNO,USERNO                                                    
         BZ    VUNOOK                                                           
         CLC   USERNO,=XL2'FFFF'                                                
         BE    VUNOOK                                                           
VUNO010  CLC   10(2,RF),USERNO                                                  
         BE    VUNOOK                                                           
         LA    RF,12(RF)                                                        
         C     RF,AUIDTABL                                                      
         BNL   VUNONO                                                           
         B     VUNO010                                                          
*                                                                               
VUNONO   B     NO                                                               
*                                                                               
VUNOOK   B     YES                                                              
         SPACE 2                                                                
***********************************************************************         
* DELETE ELEMENT AT (R3) FROM RECORD AT (R2)                          *         
***********************************************************************         
DELETE   NTR1  ,                                                                
         XC    WORK,WORK                                                        
         SR    R4,R4                                                            
         IC    R4,1(R3)                                                         
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(R3)                                                    
         BCTR  R4,0                                                             
         GOTO1 VHELLO,PARM,(C'D',CTFILE),(WORK,(R2)),((R4),WORK+2)              
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* PRINT ERROR MESSAGE                                                 *         
***********************************************************************         
         SPACE 1                                                                
ERRPRT   NTR1                                                                   
         LA    RE,ERRTAB                                                        
         SR    RF,RF                                                            
         ICM   RF,1,ERROR                                                       
         MH    RF,=H'40'                                                        
         AR    RE,RF                                                            
         MVC   P,SPACES                                                         
         MVC   P+13(10),=C'*** ERROR '                                          
         MVC   P+23(L'ERRMSG0),0(RE)                                            
         GOTO1 VPRINTER                                                         
         MVI   ERROR,0                                                          
         XIT1                                                                   
         SPACE 2                                                                
*                                                                               
YES      SR    RC,RC               RETURN CC EQUAL                              
NO       LTR   RC,RC               RETURN CC NOT EQUAL                          
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
* CARDTBL DEFINES JOB CONTROL INPUT PARAMETER CARDS                             
* AL1    LENGTH OF CARD NAME                                                    
* AL1    ACTION ROUTINE NUMBER                                                  
* XL1    ACTION FLAGS                                                           
* CL8    CARD NAME                                                              
*                                                                               
CARDTBL  DS    0CL14                                                            
         DC    AL1(05,01),X'00',CL11'LIST='                                     
         DC    AL1(07,02),X'00',CL11'UPDATE='                                   
CARDTBLX DC    AL1(00)                                                          
CLENGTH  EQU   0                                                                
CROUTINE EQU   1                                                                
CFLAG    EQU   2                                                                
CSTRING  EQU   3                                                                
         SPACE 2                                                                
* ACTNTBL DEFINES ACTION TO BE TAKEN FOR ECH CONTROL FILE RECORD TYPE           
* AL1    ACTION ROUTINE NUMBER                                                  
* XL1    RECORD TYPE                                                            
* XL1    ACTION FLAGS                                                           
* CL30   HEADING                                                                
*                                                                               
ACTNTBL  DS    0CL33                                                            
         DC    AL1(01),C'I',X'00',CL30'USER-ID RECORD REPORT'                   
         DC    AL1(02),C'5',X'00',CL30'ACCESS RECORD REPORT'                    
         DC    AL1(03),C'0',X'00',CL30'AUTHORISATION RECORD REPORT'             
         DC    AL1(04),C'P',X'00',CL30'PROFILE RECORD REPORT'                   
         DC    AL1(05),C'T',X'00',CL30'TERMINAL RECORD REPORT'                  
         DC    AL1(06),C'W',X'00',CL30'SYSLIST RECORD REPORT'                   
         DC    AL1(07),C'A',X'00',CL30'TWX ADDRESSEE RECORD REPORT'             
         DC    AL1(08),C'C',X'00',CL30'COMMENT (BOOK) RECORD REPORT'            
         DC    AL1(09),C'Z',X'00',CL30'INTERFACE TAPE RECORD REPORT'            
         DC    AL1(10),X'03',X'00',CL30'DSCHEME RECORD REPORT'                  
         DC    AL1(11),X'02',X'00',CL30'DRIVER USER RECORD REPORT'              
         DC    AL1(12),C'D',X'00',CL30'DEMO NAME RECORD REPORT'                 
         DC    AL1(13),C'G',X'00',CL30'DEMO FORMULA RECORD REPORT'              
         DC    AL1(14),C'K',X'00',CL30'MARKET AUTH. RECORD REPORT'              
         DC    AL1(15),C'N',X'00',CL30'DEMO CODE RECORD REPORT'                 
         DC    AL1(16),C'Q',X'00',CL30'DEMO ADJUSTMENT RECORD REPORT'           
         DC    AL1(17),C'R',X'00',CL30'DEMO CONTROL RECORD REPORT'              
         DC    AL1(18),C'X',X'00',CL30'CPP EXTRACT RULES REC. REPORT'           
         DC    AL1(19),C'Y',X'00',CL30'CPP PROJ. FORMULA REC. REPORT'           
ACTNTBLX DC    AL1(00)                                                          
AROUTINE EQU   0                                                                
ATYPE    EQU   1                                                                
AFLAGS   EQU   2                                                                
AHEAD    EQU   3                                                                
         EJECT                                                                  
* FASYSLST                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASYSLST                                                       
         PRINT ON                                                               
         SPACE 1                                                                
         EJECT                                                                  
YAUTH    DC    X'000F'                                                          
NAUTH    DC    X'0000'                                                          
XAUTH    DC    X'FFFF'                                                          
MAXLEN   DC    H'999'                                                           
         SPACE 1                                                                
COMFACS  DS    0A                                                               
VDATAMGR DC    V(DATAMGR)                                                       
VCPRINT  DC    V(CPRINT)                                                        
VPRINT   DC    V(PRINT)                                                         
VPRINTER DC    V(PRINTER)                                                       
VHELLO   DC    V(HELLO)                                                         
VHEXOUT  DC    V(HEXOUT)                                                        
VGETFACT DC    V(GETFACT)                                                       
VCARDS   DC    V(CARDS)                                                         
*                                                                               
ERRTAB   DS    0H                  ERROR REPORT STRINGS                         
ERRMSG0  DC    CL40'DATASET NOT FOUND IN PAN LIBRARY'                           
ERRMSG1  DC    CL40'INVALID CONTROL CARD INPUT'                                 
ERRMSG2  DC    CL40'INVALID INPUT FILE LINE'                                    
         SPACE 1                                                                
TAPEOUT  DCB   DDNAME=TAPEOUT,DSORG=PS,MACRF=(PM),RECFM=VB,            *        
               BLKSIZE=8200,LRECL=2048,BUFNO=2                                  
DDIN     DCB   DDNAME=DDIN,DSORG=PS,RECFM=FB,MACRF=GM,                 *        
               BLKSIZE=3200,LRECL=80,EODAD=EODADDI                              
         SPACE 1                                                                
DMOPEN   DC    C'OPEN   '                                                       
DMREAD   DC    C'DMREAD '                                                       
DMRSEQ   DC    C'DMRSEQ '                                                       
DMRDHI   DC    C'DMRDHI '                                                       
         SPACE 1                                                                
CONTROL  DC    C'CONTROL'                                                       
CTFILE   DC    C'CTFILE '                                                       
         EJECT                                                                  
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* FASYSLSTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FASYSLSTD                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
         SPACE 1                                                                
* DDDPRINTL                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDDPRINTL                                                      
         PRINT ON                                                               
         SPACE 1                                                                
WORKD    DSECT                     ** GLOBAL WORKING STORAGE **                 
*                                                                               
DUB      DS    D                                                                
DMCB     DS    6F                                                               
PARM     DS    6F                                                               
ASYSLST  DS    A                                                                
RETCODE  DS    XL1                                                              
PASSFLAG DS    XL1                                                              
CONTROLF DS    XL1                                                              
LISTALL  EQU   X'01'                                                            
UPDATEY  EQU   X'02'                                                            
AGYIND   DS    XL1                                                              
MVSPARM  DS    XL1                                                              
ERROR    DS    XL1                 FLAG TO INDIATE INVALID INPUT                
LASTTYPE DS    XL1                 LAST RECORD TYPE PROCESSED                   
PIDREAD  DS    XL1                 FLAG FIRST/PRINCIPLE COMPAT ID READ          
PIDDEL   DS    XL1                 FLAG PRINCIPLE ID DELETED                    
*                                  AGENCY ALPHA TABLE POINTERS                  
AAGYTAB  DS    A                                                                
AAGYTABL DS    A                                                                
AAGYTABX DS    A                                                                
*                                  USER ID TABLE POINTERS                       
AUIDTAB  DS    A                                                                
AUIDTABL DS    A                                                                
AUIDTABX DS    A                                                                
*                                                                               
USERALPH DS    CL10                USER-ID ALPHA CODE                           
USERNO   DS    XL2                 USER-ID NUMBER                               
AGYALPH  DS    CL2                 AGENCY ALPHA CODE                            
LINEBUFF DS    XL80                                                             
WORK     DS    XL256                                                            
IOKEY    DS    XL(L'CTIKEY)                                                     
IOL      DS    F                                                                
IO       DS    2000X                                                            
IO2      DS    2000X                                                            
WORKX    DS    0D                                                               
         SPACE 1                                                                
WORKC    CSECT                     ** WORKING STORAGE POOL 1 **                 
         DS    (64*1024)X                                                       
         SPACE 1                                                                
AGYTAB   CSECT                     ** WORKING STORAGE POOL 2 **                 
         DS    (10000)XL3                                                       
AGYTABX  EQU   *                                                                
         SPACE 1                                                                
UIDTAB   CSECT                     ** WORKING STORAGE POOL 3 **                 
         DS    (10000)XL12                                                      
UIDTABX  EQU   *                                                                
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'221CTCONVID4 05/01/02'                                      
         END                                                                    
