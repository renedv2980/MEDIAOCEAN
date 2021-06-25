*          DATA SET CTGEN0C    AT LEVEL 007 AS OF 12/04/18                      
*PHASE TA0B0CA                                                                  
*INCLUDE SCANNER                                                                
*                                                                               
         TITLE 'CTGEN0C - FILE MAINTENANCE - PROVER RECORDS'                    
GEN0C    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**GE0C**,RA,RR=RE                                              
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         USING TWAD,R5             R5=A(TWA)                                    
         USING SAVAREA,R6          R6=A(GLOBAL SAVE AREA)                       
         LA    R2,IOKEY                                                         
         USING CTPRREC,R2          R2=A(RECORD KEY)                             
         L     RC,AAPLOCAL                                                      
         USING LOCALD,RC           RC=A(LOCAL W/S)                              
         ST    RE,APRELO                                                        
*                                                                               
         MVI   MODE,0              SET OLD CMPRSD DATES                         
         MVI   BYTE,0              STATUS FROM RECORD X'40'=NEW CMPRSD          
         MVI   BYTE1,0                                                          
*                                                                               
         L     R0,=V(SCINKEY)                                                   
         AR    R0,RE                                                            
         ST    R0,VSCINKEY                                                      
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
         ST    RD,APWORKA                                                       
*                                                                               
         ZIC   RF,APMODE                                                        
         SLL   RF,2                                                             
         B     *+0(RF)                                                          
*                                                                               
         B     VALKEY                                                           
         B     VALREC                                                           
         B     DISKEY                                                           
         B     DISREC                                                           
         B     DELREC                                                           
         B     RESREC                                                           
         B     VALSEL                                                           
         B     GETSEL                                                           
         B     DISSEL                                                           
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     LSTSCR                                                           
         B     VALREQ                                                           
         B     PRTREP                                                           
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE KEY OF PROVER TYPE RECORD                       *         
***********************************************************************         
         SPACE 1                                                                
VALKEY   LA    R2,IOKEY                                                         
         XC    CTPRKEY,CTPRKEY                                                  
         MVI   CTPRKTYP,CTPRKTYQ                                                
*                                  VALIDATE PROGRAM TYPE                        
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,PRVPTYH                                                    
         BNE   VALKEYX                                                          
         CLI   FVIFLD,CTPRKPFQ                                                  
         BE    VKEY010                                                          
         CLI   FVIFLD,CTPRKPSQ                                                  
         BE    VKEY010                                                          
         B     EIIF                                                             
*                                                                               
VKEY010  EQU   *                                                                
         MVC   CTPRKPTY,FVIFLD                                                  
         MVI   CTPRKPST,0                                                       
*                                  VALIDATE PROGRAM SUB TYPE                    
         MVI   FVMINL,0                                                         
         GOTO1 AFVAL,PRVPSTH                                                    
         CLI   FVILEN,0                                                         
         BNE   EIIF                                                             
         CLI   CTPRKPTY,CTPRKPFQ   TEST FOR FACPAK PROGRAM TYPE                 
         BE    VKFP                                                             
*                                  VALIDATE STEREO PROGRAM NAME                 
VKSP     EQU   *                                                                
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,PRVPNMH                                                    
         BNE   VALKEYX                                                          
         MVC   CTPRKPNM,FVIFLD                                                  
VKSPX    EQU   *                                                                
         B     VKLA                                                             
*                                  VALIDATE FACPAK PROGRAM NAME                 
VKFP     EQU   *                                                                
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,PRVSYSH       FACPAK SYSTEM NAME                           
         BNE   VALKEYX                                                          
         GOTO1 AVALSYS,PRVSYSH     VALIDATE SYSTEM NAME                         
         BNE   VALKEYX                                                          
         MVC   CTPRKSYS,APWORK                                                  
*                                                                               
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,PRVPNMH       FACPAK PROGRAM NAME                          
         BNE   VALRECX                                                          
         GOTO1 AVALPGM,APPARM,(CTPRKSYS,PRVPNMH)                                
         BNE   VALRECX                                                          
         MVC   CTPRKPRG,APWORK                                                  
         L     R1,0(R1)                                                         
         MVC   APWORK(7),0(R1)                                                  
         GOTO1 DISPFLD,PRVPNMH     MOVE PGM NAME TO FIELD                       
*                                                                               
VKFPX    EQU   *                                                                
         B     VKLA                                                             
*                                                                               
VKLA     EQU   *                                                                
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,PRVLANGH      LANGUAGE CODE                                
         BNE   VALKEYX                                                          
         GOTO1 AVALLNG,PRVLANGH    VALIDATE LANGUAGE NAME                       
         BNE   VALKEYX                                                          
         MVC   CTPRKLAN,APWORK                                                  
*                                                                               
         L     R1,APPARM           A(LANGUAGE TABLE ENTRY)                      
         MVI   APWORK,C' '                                                      
         MVC   APWORK+1(L'PRVLANG-1),APWORK                                     
         MVC   APWORK(L'LANGFUL),LANGFUL-LANGTABD(R1)                           
         GOTO1 DISPFLD,PRVLANGH    DISPLAY FULL NAME                            
*                                                                               
         XI    CTPRKLAN,X'FF'      INVERT LANGUAGE                              
VKLAX    EQU   *                                                                
*                                                                               
*                                  READ RECORD WITH THIS KEY                    
VKR      MVC   APRECKEY(L'CTPRKEY),CTPRKEY                                      
         LA    R1,IORDD+IOCONFIL+IO1                                            
         CLI   APACTN,ACTDIS                                                    
         BE    *+8                                                              
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO                                                              
         BL    VALKEYX             I/O ERROR EXIT                               
         BNE   VKR1                                                             
         MVI   APINDS,APIOKDIS+APIOKCHA+APIOKDEL                                
         NI    BYTE,255-CTPRSNCD                                                
         TM    CTPRSTAT,CTPRSNCD   TEST IF RECORD HAS NEW CMPRSD DATES          
         BZ    VALKEYY                                                          
         OI    BYTE,CTPRSNCD       SET RECORD HAS NEW CMPRSD DATES              
         B     VALKEYY                                                          
VKR1     TM    IOERR,IOEDEL        TEST RECORD IS DELETED                       
         BZ    VKR2                                                             
         MVI   APINDS,APIOKDIS+APIOKRES                                         
         NI    BYTE,255-CTPRSNCD                                                
         TM    CTPRSTAT,CTPRSNCD   TEST IF RECORD HAS NEW CMPRSD DATES          
         BZ    VALKEYY                                                          
         OI    BYTE,CTPRSNCD       SET RECORD HAS NEW CMPRSD DATES              
         B     VALKEYY                                                          
VKR2     MVI   APINDS,APIOKADD                                                  
VALKEYY  MVC   FVMSGNO,=AL2(FVFOK)                                              
VALKEYX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD OR CHANGE A PROVER TYPE RECORD                       *         
***********************************************************************         
         SPACE 1                                                                
VALREC   L     R2,AIOAREA1                                                      
         CLI   APACTN,ACTADD                                                    
         BE    VRADD                                                            
*                                  CHANGE FUNCTION - SAVE SYSTEM ELMS.          
VRCHA    LA    R3,CTPRDATA         AND STRIP DOWN RECORD                        
*                                                                               
VRCHA10  CLI   0(R3),0                                                          
         BE    VRCHAX                                                           
         CLI   0(R3),X'01'                                                      
         BE    VRCHA30                                                          
         CLI   0(R3),CTPRVELQ                                                   
         BE    VRCHA30                                                          
*                                  GET NEXT ELEMENT                             
VRCHA20  ZIC   RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     VRCHA10                                                          
*                                                                               
VRCHA30  SR    R0,R0               DELETE ELEMENT                               
         ICM   R0,1,0(R3)                                                       
         GOTO1 VHELLO,APPARM,(C'D',CTFILE),((R0),CTPRREC),0,0                   
         CLI   APPARM+12,0                                                      
         BE    *+6                                                              
         DC    H'00'                                                            
         B     VRCHA10                                                          
*                                                                               
VRCHAX   B     VRDATA                                                           
*                                                                               
VRADD    XC    CTPRREC(256),CTPRREC ADD FUNCTION                                
         MVC   CTPRKEY,APRECKEY                                                 
         LA    R0,CTPRDATA+1-CTPRREC                                            
         STCM  R0,3,CTPRLEN                                                     
*                                                                               
VRDATA   EQU   *                                                                
         L     R2,AIOAREA1                                                      
         MVC   CTPRSTAT,MODE       SET NEW/OLD CMPRSD DATES                     
         TM    CTPRSTAT,CTPRSNCD                                                
         BZ    *+8                                                              
         OI    BYTE,CTPRSNCD       SET RECORD HAS NEW CMPRSD DATES              
*                                                                               
VRPV     EQU   *                                                                
         LA    R3,APELEM                                                        
         USING CTPRVD,R3                                                        
         XC    CTPRVEL(CTPRVLNQ),CTPRVEL                                        
         MVI   CTPRVEL,CTPRVELQ                                                 
         MVI   CTPRVLEN,CTPRVLNQ                                                
*                                  VALIDATE DATE RANGE                          
VRDR     EQU   *                                                                
         MVI   FVMINL,0                                                         
         GOTO1 AFVAL,PRVDATRH                                                   
         BL    VRDRX                                                            
         XC    APWORK,APWORK                                                    
         ZIC   R0,FVILEN                                                        
         MVC   APBYTE,CULANG                                                    
         GOTO1 VPERVAL,APPARM,((R0),FVIFLD),(APBYTE,APWORK)                     
         CLI   4(R1),PVRCOK                                                     
         BNE   EIDR                                                             
         MVC   CTPRVDFR,APWORK+PVALNSTA-PERVALD  NEW CMPRSD DATES               
         MVC   CTPRVDTO,APWORK+PVALNEND-PERVALD                                 
         TM    BYTE,CTPRSNCD                                                    
         BO    VRDRX                                                            
         OI    CTPRVDFR,X'80'      CONVERT TO OLD CMPRSD DATES                  
         OI    CTPRVDTO,X'80'                                                   
VRDRX    EQU   *                                                                
*                                  VALIDATE VERSION.LEVEL                       
VRVL     EQU   *                                                                
         MVI   FVMINL,0                                                         
         GOTO1 AFVAL,PRVVLVH                                                    
         BNE   VRVLX                                                            
         MVI   FVINDX,0            SCAN INPUT FIELD LIST                        
         GOTO1 VSCANNER,APPARM,FVIHDR,(2,BLOCKI),C',=.='                        
         CLI   4(R1),2                                                          
         BL    EMIF                                                             
         BH    EIIF                                                             
         MVI   FVINDX,1                                                         
         LA    R9,BLOCKI                                                        
         CLI   0(R9),1                                                          
         BL    EFTS                                                             
         CLI   0(R9),3                                                          
         BH    EFTL                                                             
         TM    2(R9),X'80'                                                      
         BZ    EFNN                                                             
         ICM   RF,15,4(R9)                                                      
         CLM   RF,15,=AL4(255)                                                  
         BH    EFTB                                                             
         STC   RF,CTPRVVER                                                      
         LA    R9,L'BLOCKI(R9)                                                  
         ZIC   RF,FVINDX           DO NEXT FIELD IN SCAN LIST                   
         LA    RF,1(RF)                                                         
         STC   RF,FVINDX                                                        
         CLI   0(R9),1                                                          
         BL    EFTS                                                             
         CLI   0(R9),3                                                          
         BH    EFTL                                                             
         TM    2(R9),X'80'                                                      
         BZ    EFNN                                                             
         ICM   RF,15,4(R9)                                                      
         CLM   RF,15,=AL4(255)                                                  
         BH    EFTB                                                             
         STC   RF,CTPRVLEV                                                      
         B     VRVLX                                                            
VRVLX    EQU   *                                                                
*                                  VALIDATE PQ REPORT 1 ID                      
VRR1     EQU   *                                                                
         MVI   FVMINL,0                                                         
         GOTO1 AFVAL,PRVREP1H                                                   
         BL    VRR1X                                                            
         MVI   FVINDX,0            SCAN INPUT FIELD LIST                        
         GOTO1 VSCANNER,APPARM,FVIHDR,(2,BLOCKI),C',=,='                        
         CLI   4(R1),2                                                          
         BL    EMIF                                                             
         BH    EIIF                                                             
         MVI   FVINDX,1                                                         
         LA    R9,BLOCKI           SCAN INTO SUBID,SEQNUM                       
*                                                                               
         CLI   0(R9),2             SUBID MUST BE 2 OR 3 CHARS                   
         BL    EFTS                                                             
         CLI   0(R9),3                                                          
         BH    EFTL                                                             
*                                                                               
         MVC   CTPRVPQ1(3),12(R9)  RETURN SUB ID                                
*                                                                               
         LA    RE,CTPRVPQ1         MASSAGE SUBID                                
         LA    RF,3                                                             
         CLI   0(RE),C'A'                                                       
         BNL   *+8                                                              
         MVI   0(RE),C'.'                                                       
         LA    RE,1(RE)                                                         
         BCT   RF,*-16                                                          
*                                                                               
         ZIC   RF,FVINDX           DO NEXT FIELD IN SCAN LIST                   
         LA    RF,1(RF)                                                         
         STC   RF,FVINDX                                                        
         LA    R9,L'BLOCKI(R9)                                                  
         CLI   0(R9),0             SEQNUM MUST HAVE NON-ZERO LENGTH             
         BE    EIIF                                                             
         TM    2(R9),X'80'         MUST BE NUMERIC                              
         BZ    EFNN                                                             
         OC    4(4,R9),4(R9)       MUST BE NON-ZERO                             
         BZ    EIIF                                                             
         CLC   4(4,R9),=F'65000'   MUST BE <= 65000                             
         BH    EFTB                                                             
*                                                                               
         MVC   CTPRVPQ1+3(2),6(R9) RETURN SEQUENCE NUMBER                       
         B     VRR1X                                                            
VRR1X    EQU   *                                                                
*                                  VALIDATE PQ REPORT 2 ID                      
VRR2     EQU   *                                                                
         MVI   FVMINL,0                                                         
         GOTO1 AFVAL,PRVREP2H                                                   
         BL    VRR2X                                                            
         MVI   FVINDX,0            SCAN INPUT FIELD LIST                        
         GOTO1 VSCANNER,APPARM,FVIHDR,(2,BLOCKI),C',=,='                        
         CLI   4(R1),2                                                          
         BL    EMIF                                                             
         BH    EIIF                                                             
         MVI   FVINDX,1                                                         
         LA    R9,BLOCKI           SCAN INTO SUBID,SEQNUM                       
*                                                                               
         CLI   0(R9),2             SUBID MUST BE 2 OR 3 CHARS                   
         BL    EFTS                                                             
         CLI   0(R9),3                                                          
         BH    EFTL                                                             
*                                                                               
         MVC   CTPRVPQ2(3),12(R9)  RETURN SUB ID                                
*                                                                               
         LA    RE,CTPRVPQ2         MASSAGE SUBID                                
         LA    RF,3                                                             
         CLI   0(RE),C'A'                                                       
         BNL   *+8                                                              
         MVI   0(RE),C'.'                                                       
         LA    RE,1(RE)                                                         
         BCT   RF,*-16                                                          
*                                                                               
         ZIC   RF,FVINDX           DO NEXT FIELD IN SCAN LIST                   
         LA    RF,1(RF)                                                         
         STC   RF,FVINDX                                                        
         LA    R9,L'BLOCKI(R9)                                                  
         CLI   0(R9),0             SEQNUM MUST HAVE NON-ZERO LENGTH             
         BE    EIIF                                                             
         TM    2(R9),X'80'         MUST BE NUMERIC                              
         BZ    EFNN                                                             
         OC    4(4,R9),4(R9)       MUST BE NON-ZERO                             
         BZ    EIIF                                                             
         CLC   4(4,R9),=F'65000'   MUST BE <= 65000                             
         BH    EFTB                                                             
*                                                                               
         MVC   CTPRVPQ2+3(2),6(R9) RETURN SEQUENCE NUMBER                       
         B     VRR2X                                                            
VRR2X    EQU   *                                                                
*                                  VALIDATE PQ REPORT 2 ID                      
VRHI     EQU   *                                                                
         NI    CTPRVFL1,X'FF'-CTPRVFHI                                          
         MVI   FVMINL,0                                                         
         GOTO1 AFVAL,PRVHIGHH                                                   
         BL    VRHIX                                                            
         CLI   FVIFLD,C'N'                                                      
         BE    VRHIX                                                            
         CLI   FVIFLD,C'Y'                                                      
         BNE   EIIF                                                             
         OI    CTPRVFL1,CTPRVFHI                                                
VRHIX    EQU   *                                                                
*                                                                               
VRPVX    EQU   *                                                                
         GOTO1 AADDELS,CTPRREC                                                  
*                                                                               
VRMS     LA    R3,APELEM                                                        
         USING GMSGEL,R3           R3=A(MESSAGE ELEMENT)                        
         XC    APELEM,APELEM                                                    
         MVI   APELEM,GMSGELC                                                   
         MVI   APELEM+1,0                                                       
         GOTO1 ADELELS,CTPRREC     REMOVE OLD ELEMENTS                          
         XC    APELEM,APELEM                                                    
         MVI   GMSGEL,GMSGELC                                                   
         MVI   GMSGSEV,C' '                                                     
*                                                                               
         MVI   FVMINL,0                                                         
         GOTO1 AFVAL,PRVMSGH       MESSAGE TEXT                                 
         BNE   VRMSX                                                            
*                                                                               
         ZIC   R1,FVXLEN                                                        
         STH   R1,APHALF           SAVE MSG LENGTH FOR CHECK                    
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   GMSGTXT(0),FVIFLD                                                
         LA    R1,GMSGFXDL+1(R1)   ADD FIXED LEN +1 FOR EXECUTABLE              
         STC   R1,GMSGELL                                                       
         GOTO1 AADDELS,CTPRREC                                                  
         DROP  R3                                                               
VRMSX    EQU   *                                                                
*                                                                               
VRTX     EQU   *                                                                
         MVI   APELEM,GMTXTELC     EXPANDED TEXT ELEMENTS                       
         MVI   APELEM+1,0                                                       
         GOTO1 ADELELS,CTPRREC     REMOVE OLD ELEMENTS                          
         LA    R4,PRVTXT1H         1ST EXPANDED TEXT LINE                       
         LA    R3,APELEM                                                        
         USING GMTXTD,R3                                                        
         XC    APELEM,APELEM       INITIALISE ELEMENT                           
         MVI   GMTXTEL,GMTXTELC                                                 
         MVI   GMTXTLNO,1          RELATIVE LINE NUMBER                         
*                                                                               
VRTX010  GOTO1 AFVAL,(R4)                                                       
         BNE   VRTX020             IGNORE BLANK LINES                           
         ZIC   R1,FVXLEN                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   GMTXTLIN(0),FVIFLD                                               
         AH    R1,=Y(GMTXTFXD+1)   FULL ELEMENT LENGTH                          
         STC   R1,GMTXTELL                                                      
         GOTO1 AADDELS,CTPRREC                                                  
         XC    GMTXTLIN(L'PRVTXT1),GMTXTLIN                                     
*                                                                               
VRTX020  CLI   GMTXTLNO,MAXTXTNO   CHECK FOR MORE LINES                         
         BNL   VRTXX                                                            
         ZIC   R1,GMTXTLNO         REDEFINE ELEMENT LINE NUMBER                 
         LA    R1,1(R1)                                                         
         STC   R1,GMTXTLNO                                                      
         AH    R4,TXTLEN           POINT TO NEXT SCREEN FIELD                   
         B     VRTX010                                                          
*                                                                               
VRTXX    EQU   *                                                                
         DROP  R3                                                               
*                                                                               
         B     VRUPD                                                            
*                                  SAVE UPDATED RECORD                          
VRUPD    GOTO1 ASETACT,CTPRREC                                                  
         LA    R1,IOWRITE+IOCONFIL+IO1                                          
         CLI   APACTN,ACTADD                                                    
         BNE   *+8                                                              
         LA    R1,IOADD+IOCONFIL+IO1                                            
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALRECX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY KEY OF PROVER TYPE RECORD                        *         
***********************************************************************         
         SPACE 1                                                                
DISKEY   LA    R2,APRECKEY                                                      
         MVC   PRVPTY,CTPRKPTY                                                  
         XC    PRVPST,PRVPST                                                    
         CLI   CTPRKPTY,CTPRKPFQ                                                
         BE    DKFP                                                             
*                                                                               
DKSP     EQU   *                                                                
         MVC   PRVPNM,CTPRKPNM                                                  
DKSPX    EQU   *                                                                
         B     DKLA                                                             
*                                                                               
DKFP     EQU   *                                                                
         GOTO1 ADISSYS,CTPRKSYS    GET FACPAK SYSTEM                            
         GOTO1 DISPFLD,PRVSYSH                                                  
         GOTO1 ADISPGM,APPARM,(CTPRKSYS,CTPRKPRG)                               
         MVC   PRVPNM(7),APWORK    MOVE PGM NAME TO FIELD                       
DKFPX    EQU   *                                                                
         B     DKLA                                                             
*                                                                               
DKLA     EQU   *                                                                
         CLI   CTPRKLAN,0                                                       
         BE    DKLAX                                                            
         MVC   APWORK(1),CTPRKLAN                                               
         XI    APWORK,X'FF'        INVERT LANGUAGE                              
         GOTO1 ADISLNG,APWORK      GET LANGUAGE TABLE ENTRY                     
*                                                                               
         L     R1,APPARM           A(LANGUAGE TABLE ENTRY)                      
         MVI   APWORK,C' '                                                      
         MVC   APWORK+1(L'PRVLANG-1),APWORK                                     
         MVC   APWORK(L'LANGFUL),LANGFUL-LANGTABD(R1)                           
         GOTO1 DISPFLD,PRVLANGH    DISPLAY FULL NAME                            
DKLAX    EQU   *                                                                
*                                                                               
DISKEYX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY PROVER TYPE RECORD                               *         
***********************************************************************         
         SPACE 1                                                                
DISREC   L     R2,AIOAREA1                                                      
         TWAXC PRVDATRH                                                         
         NI    BYTE,255-CTPRSNCD                                                
         TM    CTPRSTAT,CTPRSNCD   TEST IF RECORD HAS NEW CMPRSD DATES          
         BZ    *+8                                                              
         OI    BYTE,CTPRSNCD       SET RECORD HAS NEW CMPRSD DATES              
*                                                                               
         LA    R3,CTPRDATA         DISPLAY EACH ELEMENT OF RECORD               
DREC010  CLI   0(R3),0             E-O-R                                        
         BE    DREC100                                                          
         CLI   0(R3),CTPRVELQ                                                   
         BE    DRPV                                                             
         CLI   0(R3),GMSGELC                                                    
         BE    DRMS                                                             
*                                                                               
DREC020  SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     DREC010                                                          
*                                                                               
         USING CTPRVD,R3                                                        
DRPV     EQU   *                                                                
         OC    CTPRVDFR,CTPRVDFR                                                
         BZ    DRPV010                                                          
         MVI   BYTE1,02                                                         
         TM    BYTE,CTPRSNCD       TEST RECORD HAS NEW CMPRSD DATES             
         BZ    *+8                                                              
         MVI   BYTE1,14                                                         
         OI    BYTE1,X'20'                                                      
         MVI   BYTE2,17            MMMDD/YY                                     
         GOTO1 VDATCON,APPARM,(BYTE1,CTPRVDFR),(BYTE2,APWORK),CTPRVDTO          
         MVC   PRVDATR,APWORK                                                   
         OI    PRVDATRH+(FVOIND-FVIHDR),FVOXMT                                  
DRPV010  EDIT  CTPRVVER,(3,PRVVLV),ZERO=NOBLANK,ALIGN=LEFT                      
         LA    RF,PRVVLV                                                        
         LA    R0,3                                                             
DRPV020  CLI   0(RF),0                                                          
         BE    DRPV030                                                          
         CLI   0(RF),C' '                                                       
         BE    DRPV030                                                          
         LA    RF,1(RF)                                                         
         BCT   R0,DRPV020                                                       
DRPV030  MVI   0(RF),C'.'                                                       
         LA    RF,1(RF)                                                         
         EDIT  CTPRVLEV,(3,(RF)),ZERO=NOBLANK,ALIGN=LEFT                        
         OC    CTPRVPQ1,CTPRVPQ1                                                
         BZ    DRPV040                                                          
         MVC   PRVREP1(3),CTPRVPQ1                                              
         MVI   PRVREP1+3,C','                                                   
         SR    RF,RF                                                            
         ICM   RF,3,CTPRVPQ1+3                                                  
         EDIT  (RF),(5,PRVREP1+4),ZERO=NOBLANK,ALIGN=LEFT                       
DRPV040  OC    CTPRVPQ2,CTPRVPQ2                                                
         BZ    DRPV050                                                          
         MVC   PRVREP2(3),CTPRVPQ2                                              
         MVI   PRVREP2+3,C','                                                   
         SR    RF,RF                                                            
         ICM   RF,3,CTPRVPQ2+3                                                  
         EDIT  (RF),(5,PRVREP2+4),ZERO=NOBLANK,ALIGN=LEFT                       
*                                                                               
DRPV050  EQU   *                                                                
         MVI   PRVHIGH,C'N'                                                     
         TM    CTPRVFL1,CTPRVFHI                                                
         BZ    DRPV060                                                          
         MVI   PRVHIGH,C'Y'                                                     
*                                                                               
DRPV060  EQU   *                                                                
         B     DREC020                                                          
         DROP  R3                                                               
*                                                                               
         USING GMSGEL,R3                                                        
DRMS     EQU   *                                                                
         ZIC   R1,GMSGELL          NOTE MSG ELEM ALWAYS FIXED POSN              
         SH    R1,=Y(GMSGFXDL+1)                                                
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PRVMSG(0),GMSGTXT                                                
         B     DREC020                                                          
         DROP  R3                                                               
*                                                                               
DREC100  EQU   *                                                                
*                                                                               
DRTX     EQU   *                                                                
         XC    APELEM,APELEM                                                    
         MVI   APELEM,GMTXTELC     LOOK FOR EXPANDED TEXT                       
         GOTO1 AGETELS,CTPRREC                                                  
         ICM   R3,15,APPARM        R3=A(TEXT ELEMENT)                           
         USING GMTXTD,R3                                                        
         BZ    DRTXX                                                            
*                                                                               
DRTX010  MVI   APWORK,C' '                                                      
         MVC   APWORK+1(L'PRVTXT1-1),APWORK                                     
         ZIC   R1,GMTXTELL         GET ELEMENT LENGTH                           
         SH    R1,=Y(GMTXTFXD+1)   EX TEXT LENGTH                               
         BNM   *+6                                                              
         DC    H'0'                CORRUPT ELEMENT LENGTH                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   APWORK(0),GMTXTLIN                                               
         ZIC   R1,GMTXTLNO         LINE NO WITHIN TEXT BLOCK                    
         CLI   GMTXTLNO,MAXTXTNO   CHECK LINE WITHIN LIMITS                     
         BNH   *+6                                                              
         DC    H'0'                ODD RELATIVE LINE NUMBER                     
         BCTR  R1,0                OFFSET LINE NUMBER                           
         MH    R1,TXTLEN           R1=OFFSET INTO TWA LINES                     
         LA    R0,PRVTXT1H                                                      
         AR    R1,R0               R1=A(TEXT HEADER)                            
         GOTO1 DISPFLD             DISPLAY THE FIELD                            
         BAS   RE,NEXTEL           GET NEXT ELEMENT R3=A(CURRENT EL)            
         BE    DRTX010                                                          
*                                                                               
DRTXX    EQU   *                                                                
         DROP  R3                                                               
*                                                                               
DREC200  EQU   *                                                                
*                                                                               
         GOTO1 ADISACT,CTPRREC     DISPLAY ACTIVITY DATE                        
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DELETE A PROVER TYPE RECORD                              *         
***********************************************************************         
         SPACE 1                                                                
DELREC   L     R2,AIOAREA1                                                      
         GOTO1 ASETACT,CTPRREC                                                  
         OI    CTPRSTAT,X'80'                                                   
         GOTO1 AIO,IOWRITE+IOCONFIL+IO1                                         
         BE    *+6                                                              
         DC    H'0'                                                             
DELRECX  B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO RESTORE A DELETED PROVER TYPE RECORD                     *         
***********************************************************************         
         SPACE 1                                                                
RESREC   L     R2,AIOAREA1                                                      
         GOTO1 ASETACT,CTPRREC                                                  
         NI    CTPRSTAT,X'FF'-X'80'                                             
         GOTO1 AIO,IOWRITE+IOCONFIL+IO1                                         
         BE    *+6                                                              
         DC    H'0'                                                             
RESRECX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE SELECT PARAMETERS FOR ACCESS RECORDS            *         
***********************************************************************         
         SPACE 1                                                                
VALSEL   LA    R0,LSTACT1H         SET ADDRESS OF FIRST LIST LINE               
         ST    R0,APPARM+0                                                      
         LA    R1,LSTACT2H         SET LIST LINE LENGTH                         
         SR    R1,R0                                                            
         STH   R1,APPARM+6                                                      
*                                                                               
         LA    R2,APRECKEY         SET UP FIRST LIST RECORD KEY                 
         XC    CTPRKEY,CTPRKEY                                                  
         MVI   CTPRKTYP,CTPRKTYQ                                                
         XC    SELKEY,SELKEY       CLEAR SELECT DATA FIELDS                     
*                                                                               
VSPT     EQU   *                                                                
         GOTO1 AFVAL,LSTPTYH                                                    
         BNE   VSPTX                                                            
         MVC   SELPTY,FVIFLD                                                    
VSPTX    EQU   *                                                                
*                                                                               
VSPS     EQU   *                                                                
         GOTO1 AFVAL,LSTPSTH                                                    
         BNE   VSPSX                                                            
         MVC   SELPST,FVIFLD                                                    
VSPSX    EQU   *                                                                
*                                                                               
         CLI   SELPTY,CTPRKPFQ                                                  
         BE    VSFP                                                             
*                                                                               
VSSP     EQU   *                                                                
         GOTO1 AFVAL,LSTPNMH                                                    
         BNE   VSSPX                                                            
         MVC   SELPNM,FVIFLD                                                    
         CLI   SELPTY,CTPRKPFQ                                                  
VSSPX    EQU   *                                                                
*                                                                               
         CLI   SELPTY,CTPRKPSQ                                                  
         BE    VSLA                                                             
*                                  VALIDATE FACPAK PROGRAM NAME                 
VSFP     EQU   *                                                                
         GOTO1 AFVAL,LSTSYSH       FACPAK SYSTEM NAME                           
         BNE   VSFPX                                                            
         GOTO1 AVALSYS,LSTSYSH     VALIDATE SYSTEM NAME                         
         BNE   VALSELX                                                          
         MVC   SELSYS,APWORK                                                    
         GOTO1 ADISSYS,SELSYS      GET FACPAK SYSTEM                            
         GOTO1 DISPFLD,LSTSYSH                                                  
*                                                                               
         GOTO1 AFVAL,LSTPNMH       FACPAK PROGRAM NAME                          
         BNE   VSFPX                                                            
         GOTO1 AVALPGM,APPARM,(CTPRKSYS,LSTPNMH)                                
         BNE   VALSELX                                                          
         MVC   SELPRG,APWORK                                                    
         L     R1,0(R1)                                                         
         MVC   APWORK(7),0(R1)                                                  
         GOTO1 DISPFLD,LSTPNMH     MOVE PGM NAME TO FIELD                       
VSFPX    EQU   *                                                                
*                                                                               
VSLA     EQU   *                                                                
         GOTO1 AFVAL,LSTLANGH                                                   
         BNE   VSLAX                                                            
         GOTO1 AVALLNG,LSTLANGH    VALIDATE LANGUAGE NAME                       
         BNE   VALSELX                                                          
         MVC   SELLNG,APWORK                                                    
         XI    SELLNG,X'FF'        INVERT LANGUAGE CODE                         
         L     R1,APPARM           A(LANGUAGE TABLE ENTRY)                      
         MVI   APWORK,C' '                                                      
         MVC   APWORK+1(L'LSTLANG-1),APWORK                                     
         MVC   APWORK(L'LANGFUL),LANGFUL-LANGTABD(R1)                           
         GOTO1 DISPFLD,LSTLANGH    DISPLAY FULL NAME                            
VSLAX    EQU   *                                                                
*                                                                               
VALSELY  EQU   *                                                                
         LA    R0,LSTACT1H         SET ADDRESS OF FIRST LIST LINE               
         ST    R0,APPARM+0                                                      
         LA    R1,LSTACT2H         SET LIST LINE LENGTH                         
         SR    R1,R0                                                            
         STH   R1,APPARM+6                                                      
         MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALSELX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GET NEXT LIST-SELECT RECORD FOR ACCESS RECORDS                      *         
***********************************************************************         
         SPACE 1                                                                
GETSEL   LA    R2,IOKEY                                                         
         MVC   CTPRKEY,APRECKEY                                                 
         TM    APINDS,APILRERD     TEST SEQUENCE BROKEN                         
         BZ    GSEL010                                                          
         GOTO1 AIO,IOCONFIL+IORD+IO1                                            
         BE    GSEL020                                                          
         B     GETSELN                                                          
GSEL010  TM    APINDS,APILNSEQ     TEST READ OR READ HIGH                       
         BNZ   GSEL020                                                          
         LA    R1,IOCONFIL+IOHI+IO1                                             
         B     *+8                                                              
GSEL020  LA    R1,IOCONFIL+IOSQ+IO1                                             
         GOTO1 AIO                                                              
         BNE   GETSELN                                                          
         L     R2,AIOAREA1                                                      
         CLI   CTPRKTYP,CTPRKTYQ                                                
         BNE   GETSELN                                                          
GSPT     EQU   *                                                                
         OC    SELPTY,SELPTY                                                    
         BZ    GSPTX                                                            
         CLC   SELPTY,CTPRKPTY                                                  
         BNE   GSEL020                                                          
GSPTX    EQU   *                                                                
*                                                                               
GSPS     EQU   *                                                                
         OC    SELPST,SELPST                                                    
         BZ    GSPSX                                                            
         CLC   SELPST,CTPRKPST                                                  
         BNE   GSEL020                                                          
GSPSX    EQU   *                                                                
*                                                                               
         CLI   CTPRKPTY,CTPRKPFQ                                                
         BE    GSFP                                                             
*                                                                               
GSSP     EQU   *                                                                
         OC    SELSYS,SELSYS                                                    
         BNZ   GSEL020                                                          
         OC    SELPNM,SELPNM                                                    
         BZ    GSSPX                                                            
         CLC   SELPNM,CTPRKPNM                                                  
         BNE   GSEL020                                                          
GSSPX    EQU   *                                                                
         B     GSLA                                                             
*                                                                               
GSFP     EQU   *                                                                
         OC    SELSYS,SELSYS                                                    
         BZ    GSFP010                                                          
         CLC   SELSYS,CTPRKSYS                                                  
         BNE   GSEL020                                                          
GSFP010  OC    SELPRG,SELPRG                                                    
         BZ    GSFPX                                                            
         CLC   SELPRG,CTPRKPRG                                                  
         BNE   GSEL020                                                          
GSFPX    EQU   *                                                                
         B     GSLA                                                             
*                                                                               
GSLA     EQU   *                                                                
         OC    SELLNG,SELLNG                                                    
         BZ    GSLAX                                                            
         CLC   SELLNG,CTPRKLAN                                                  
         BNE   GSEL020                                                          
GSLAX    EQU   *                                                                
*                                                                               
         MVI   APFLAG,0            SET SYSTEM # SELECT SEARCH FLAG              
         LA    R3,CTPRDATA         GET ELEMENTS FOR SELECT FILTER               
GSEL030  CLI   0(R3),0             E-O-R                                        
         BE    GSEL100                                                          
*                                                                               
GSEL040  SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     GSEL030                                                          
*                                                                               
GSEL100  EQU   *                   RECORD SELECTED                              
*                                                                               
GETSELY  MVC   APRECKEY(L'CTPRKEY),CTPRKEY                                      
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     GETSELX                                                          
*                                                                               
GETSELN  MVI   APMODE,APMEOFS      SET NO MORE RECORDS TO COME                  
*                                                                               
GETSELX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY LIST/SELECT LINE FOR ACCESS RECORDS                         *         
***********************************************************************         
         SPACE 1                                                                
DISSEL   L     R2,AIOAREA1                                                      
         L     R4,APPARM                                                        
         USING LISTD,R4            R4=A(LIST/SELECT LINE)                       
         NI    BYTE,255-CTPRSNCD                                                
         TM    CTPRSTAT,CTPRSNCD   TEST IF RECORD HAS NEW CMPRSD DATES          
         BZ    *+8                                                              
         OI    BYTE,CTPRSNCD       SET RECORD HAS NEW CMPRSD DATES              
         MVC   LISTPTY,CTPRKPTY                                                 
         MVC   LISTPST,CTPRKPST                                                 
         CLI   CTPRKPTY,CTPRKPFQ                                                
         BE    DSFP                                                             
*                                                                               
DSSP     EQU   *                                                                
         MVC   LISTPNM,CTPRKPNM                                                 
DSSPX    EQU   *                                                                
         B     DSLA                                                             
*                                                                               
DSFP     EQU   *                                                                
         GOTO1 ADISSYS,CTPRKSYS    GET FACPAK SYSTEM                            
         MVC   LISTSYS,APWORK                                                   
         GOTO1 ADISPGM,APPARM,(CTPRKSYS,CTPRKPRG)                               
         MVC   LISTPNM(7),APWORK                                                
DSFPX    EQU   *                                                                
         B     DSLA                                                             
*                                                                               
DSLA     EQU   *                                                                
         CLI   CTPRKLAN,0                                                       
         BE    DSLAX                                                            
         MVC   APWORK(1),CTPRKLAN                                               
         XI    APWORK,X'FF'        INVERT LANGUAGE                              
         GOTO1 ADISLNG,APWORK      GET LANGUAGE NAME                            
         L     R1,APPARM           A(LANGUAGE TABLE ENTRY)                      
         MVC   LISTLANG,LANGSHR-LANGTABD(R1)                                    
         OI    APINDS,APILRERD     SET SEQUENCE BROKEN                          
DSLAX    EQU   *                                                                
*                                                                               
         LA    R3,CTPRDATA         READ EACH ELEMENT OF RECORD                  
*                                                                               
DSEL010  CLI   0(R3),0             E-O-R                                        
         BE    DISSELX                                                          
         CLI   0(R3),CTPRVELQ                                                   
         BE    DSPV                                                             
*                                                                               
DSEL020  SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     DSEL010                                                          
*                                                                               
         USING CTPRVD,R3                                                        
DSPV     EQU   *                                                                
         OC    CTPRVDFR,CTPRVDFR                                                
         BZ    DSPV010                                                          
         MVI   BYTE1,02                                                         
         TM    BYTE,CTPRSNCD       TEST RECORD HAS NEW CMPRSD DATES             
         BZ    *+8                                                              
         MVI   BYTE1,14                                                         
         OI    BYTE1,X'20'                                                      
         MVI   BYTE2,17            MMMDD/YY                                     
         GOTO1 VDATCON,APPARM,(BYTE1,CTPRVDFR),(BYTE2,APWORK),CTPRVDTO          
         MVC   LISTDATR,APWORK                                                  
DSPV010  EDIT  CTPRVVER,(3,LISTVLV),ZERO=NOBLANK,ALIGN=LEFT                     
         LA    RF,LISTVLV                                                       
         LA    R0,3                                                             
DSPV020  CLI   0(RF),0                                                          
         BE    DSPV030                                                          
         CLI   0(RF),C' '                                                       
         BE    DSPV030                                                          
         LA    RF,1(RF)                                                         
         BCT   R0,DSPV020                                                       
DSPV030  MVI   0(RF),C'.'                                                       
         LA    RF,1(RF)                                                         
         EDIT  CTPRVLEV,(3,(RF)),ZERO=NOBLANK,ALIGN=LEFT                        
         B     DSEL020                                                          
         DROP  R3                                                               
*                                                                               
DISSELX  EQU   *                                                                
         B     EXIT                                                             
         SPACE 2                                                                
         DROP  R4                                                               
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO HANDLE LAST FOR SCREEN (ENABLE PROGRAM FUNCTION KEYS)    *         
***********************************************************************         
         SPACE 1                                                                
LSTSCR   MVI   APMODE,APMPFKS                                                   
LSTSCRX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE REPORT REQUEST SCREEN FOR ACCESS RECORDS        *         
***********************************************************************         
         SPACE 1                                                                
VALREQ   L     R9,AREP                                                          
         USING REPD,R9             R9=A(REPORT WORK AREA)                       
*                                                                               
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,REPREQH       VALIDATE REQUESTOR                           
         BNE   VALREQX                                                          
         MVC   INUSER,FVIFLD       SET REQUESTOR                                
*                                                                               
         GOTO1 AVALWHEN,REPWHENH   VALIDATE WHEN                                
         BNE   VALREQX                                                          
*                                                                               
         GOTO1 AVALDEST,REPDESTH   VALIDATE DESTINATION ID                      
         BNE   VALREQX                                                          
*                                                                               
         GOTO1 AVALOTYP,REPOTYPH   VALIDATE OUTPUT TYPE                         
         BNE   VALREQX                                                          
*                                  VALIDATE COUNTRY FILTER                      
*                                                                               
VALREQ4  MVC   REPDESC,REPDESCL    SET REPORT DESCRIPTION                       
         MVI   REPHEADI,REPHSPAC+REPHCLRA                                       
         MVI   REPMIDSI,REPMSPAC+REPMCLRA                                       
         MVI   REPFOOTN,0                                                       
         LA    R0,REPSPEC                                                       
         ST    R0,REPAPHS                                                       
         MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALREQX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PRINT PROVER TYPE REPORT                                 *         
***********************************************************************         
         SPACE 1                                                                
PRTREP   L     R9,AREP                                                          
         LA    R2,IOKEY                                                         
         XC    APRECKEY,APRECKEY                                                
         XC    CTPRKEY,CTPRKEY                                                  
         MVI   CTPRKTYP,CTPRKTYQ                                                
         LA    R1,IOHI+IOCONFIL+IO1                                             
         B     PRTREP2                                                          
*                                  GET RECORD (SEQUENCE IS BROKEN)              
PRTREP1  LA    R2,IOKEY                                                         
         MVC   CTPRKEY,APRECKEY                                                 
         GOTO1 AIO,IORD+IOCONFIL+IO1                                            
         BNE   PRTREPX                                                          
         LA    R1,IOSQ+IOCONFIL+IO1                                             
PRTREP2  GOTO1 AIO                                                              
         BNE   PRTREPX                                                          
         L     R2,AIOAREA1                                                      
         CLI   CTPRKTYP,CTPRKTYQ   TEST IF A PROVER TYPE RECORD                 
         BNE   PRTREPX                                                          
         MVC   APRECKEY(L'CTPRKEY),CTPRKEY                                      
*                                                                               
         LA    R3,CTPRDATA         PRINT EACH ELEMENT OF RECORD                 
PRTREP3  CLI   0(R3),0             E-O-R                                        
         BE    PRTREP6                                                          
*                                                                               
PRTREP4  SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     PRTREP3                                                          
*                                                                               
PRTREP6  EQU   *                                                                
*                                  PRINT REPORT LINE                            
         GOTO1 VREPORT,REPD                                                     
         B     PRTREP1                                                          
*                                                                               
PRTREPX  MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
         EJECT                                                                  
         DROP  R9                                                               
         EJECT                                                                  
**********************************************************************          
* GENERAL FIELD XMT IF CHANGED                                       *          
* R1=A(TWAHDR)                                                       *          
* APWORK MUST CONTAIN THE NEW TEXT                                   *          
**********************************************************************          
         SPACE 1                                                                
DISPFLD  ZIC   RF,FVTLEN-FVIHDR(R1)                                             
         SH    RF,=Y(L'FVIHDR)                                                  
         TM    FVATRB-FVIHDR(R1),FVAXTND                                        
         BZ    *+8                                                              
         SH    RF,=Y(L'FVIHDR)     KNOCK OFF HEADER EXTENSION                   
         BCTR  RF,0                                                             
         EX    RF,DISPFLDC         COMPARE FIELD CONTENTS                       
         BER   RE                                                               
         OI    FVOIND-FVIHDR(R1),FVOXMT                                         
         EX    RF,DISPFLDM         MOVE IN NEW FIELD                            
         BR    RE                                                               
         SPACE 1                                                                
DISPFLDC CLC   L'FVIHDR(0,R1),APWORK                                            
DISPFLDM MVC   L'FVIHDR(0,R1),APWORK                                            
         EJECT                                                                  
***********************************************************************         
* FIND NEXT ELEMENT OF SAME CODE WITHIN RECORD                        *         
* NTRY R3=A(CURRENT ELEMENT)                                          *         
*      APELEM = ELCODE TO FIND                                        *         
* EXIT CC EQ - FOUND - R3=A(NEW ELEMENT)                              *         
*      CC NE - NOT FOUND                                              *         
***********************************************************************         
         SPACE 1                                                                
NEXTEL   ZIC   RF,1(R3)            L'ELEMENT                                    
         AR    R3,RF               A(NEXT ELEMNT)                               
         ICM   RF,1,1(R3)          L'ELEMENT                                    
         BNZ   *+8                                                              
         LTR   RB,RB               FORCE CC NON ZERO                            
         BR    RE                                                               
         CLC   0(1,R3),APELEM                                                   
         BR    RE                                                               
         EJECT                                                                  
*                                  GETTXT MESSAGE # ERROR EXITS                 
EIIF     MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     NO                  INPUT FIELD ERROR                            
EFTL     MVC   FVMSGNO,=AL2(FVFLONG)                                            
         B     NO                  INPUT FIELD TOO LONG                         
EFNN     MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     NO                  INPUT FIELD NOT NUMERIC                      
EFTS     MVC   FVMSGNO,=AL2(FVFSHRT)                                            
         B     NO                  INPUT FIELD TOO SHORT                        
EFNH     MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     NO                  INPUT FIELD ERROR                            
EMIF     MVC   FVMSGNO,=AL2(FVFNONE)                                            
         B     NO                  MISSING FIELD                                
EIIO     MVC   FVMSGNO,=AL2(FVFIOER)                                            
         B     NO                  I/O ERROR                                    
ERNF     MVC   FVMSGNO,=AL2(FVFERNF)                                            
         B     NO                  RECORD NOT FOUND                             
EDIF     MVC   FVMSGNO,=AL2(FVFDUPE)                                            
         B     NO                  DUPLICATE                                    
ERAE     MVC   FVMSGNO,=AL2(FVFERAE)                                            
         B     NO                  ALREADY EXISTS                               
EIRT     MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     NO                  ?? ALREADY EXISTS                            
ERTB     MVC   FVMSGNO,=AL2(CE#RECTB)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  RECORD TO BIG                                
EFTB     MVC   FVMSGNO,=AL2(CE#FVMAX)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  FIELD VALUE > MAX                            
EIDR     MVC   FVMSGNO,=AL2(32)                                                 
         MVI   FVMSGNO,X'FF'                                                    
         B     NO                  INVALID DATE RANGE                           
*                                                                               
YES      SR    RC,RC               RETURN CC EQUAL                              
NO       LTR   RC,RC               RETURN CC NOT EQUAL                          
         B     EXIT                                                             
         SPACE 1                                                                
         SPACE 1                                                                
PROGTAB  EQU   *                   EXTRA VALID PROGRAM ENTRIES                  
         DC    CL8'OFFLINE'                                                     
         DC    CL8'ALL'                                                         
         DC    H'0'                                                             
         EJECT                                                                  
REPDESCL DC    C'ACCESS LIST'                                                   
         SPACE 1                                                                
REPSPEC  DS    0X                                                               
         SPEC  H1,1,RUN                                                         
         SPEC  H1,57,C'PROVER TYPE LIST'                                        
         SPEC  H2,57,C'----------------'                                        
         SPEC  M1,1,C'ID  CTY  PRINCIPAL   GROUP'                               
         SPEC  M2,1,C'--  ---  ID          ID   '                               
         SPEC  M1,34,C'HOLDING     CLIENT  SYSTEM SPECIFICATIONS'               
         SPEC  M2,34,C'ID          TYPE    ---------------------'               
         SPEC  H1,100,AGYNAME                                                   
         SPEC  H2,100,AGYADD                                                    
         SPEC  H3,100,REQUESTOR                                                 
         SPEC  H3,120,PAGE                                                      
         SPEC  H4,100,REPORT                                                    
         SPEC  END                                                              
         EJECT                                                                  
         LTORG                                                                  
         SPACE 1                                                                
CTFILE   DC    C'CTFILE '                                                       
SPACES   DC    80C' '                                                           
FFILL    DC    80X'FF'                                                          
CAPFILL  DC    (L'APWORK)X'40'                                                  
         SPACE 1                                                                
TXTLEN   DC    Y(PRVTXT2-PRVTXT1)                      L LINE                   
MAXTXTNO EQU   (PRVTXTL-PRVTXT1)/(PRVTXT2-PRVTXT1)+1   N LINES                  
         EJECT                                                                  
* CTGENWRK                                                                      
       ++INCLUDE CTGENWRK                                                       
* GEGENMSG                                                                      
       ++INCLUDE GEGENMSG                                                       
         EJECT                                                                  
TWAD     DSECT                                                                  
         ORG   GENTABH                                                          
       ++INCLUDE CTGENAAD                                                       
         ORG   GENTABH                                                          
       ++INCLUDE CTGENA3D                                                       
         ORG   GENTABH                                                          
       ++INCLUDE CTGENB6D                                                       
         ORG                                                                    
         EJECT                                                                  
LISTD    DSECT                     ** LIST/SELECT LINE LAYOUT **                
LISTACTH DS    XL8                                                              
LISTACT  DS    CL(L'LSTACT1)       ACTION FIELD                                 
LISTLINH DS    XL8                                                              
LISTLIN  DS    0CL(L'LSTLIN1)                                                   
LISTPTY  DS    CL1                 PROGRAM TYPE                                 
         DS    CL2                                                              
LISTPST  DS    CL1                 PROGRAM SUB TYPE                             
         DS    CL2                                                              
LISTSYS  DS    CL7                 FACPAK SYSTEM NAME                           
         DS    CL1                                                              
LISTPNM  DS    CL8                 PROGRAM NAME                                 
         DS    CL1                                                              
LISTLANG DS    CL3                 LANGUAGE                                     
         DS    CL2                                                              
LISTDATR DS    CL20                DATE RANGE                                   
         DS    CL1                                                              
LISTVLV  DS    CL7                 VERSION.LEVEL                                
         ORG   LISTLIN+L'LISTLIN                                                
         SPACE 2                                                                
REPD     DSECT                     ** PRINT LINE LAYOUT **                      
         ORG   REPP1                                                            
LINEPTY  DS    CL1                 PROGRAM TYPE                                 
         DS    CL2                                                              
LINEPST  DS    CL1                 PROGRAM SUB TYPE                             
         DS    CL2                                                              
LINEPNM  DS    CL8                 PROGRAM TNAME                                
         DS    CL2                                                              
LINELANG DS    CL13                PROGRAM LANGUAGE                             
         DS    CL2                                                              
         ORG   REPP2                                                            
         DS    CL45                                                             
         EJECT                                                                  
LOCALD   DSECT                     ** DSECT TO COVER LOCAL W/S **               
DUB      DS    6D                                                               
FULL     DS    F                                                                
HALF     DS    F                                                                
HALF1    DS    F                                                                
MODE     DS    X                   SET TO X'40' FOR NEW CMPRSD DATES            
BYTE     DS    X                                                                
BYTE1    DS    X                                                                
BYTE2    DS    X                                                                
WORK     DS    CL255                                                            
SELKEY   DS    0XL13               SELECT KEY PARAMETERS                        
SELPTY   DS    CL1                 PROGRAM TYPE                                 
SELPST   DS    CL1                 PROGRAM SUB TYPE                             
SELPNM   DS    CL8                 PROGRAM NAME                                 
SELSYS   DS    XL1                 FACPAK SYSTEM CODE                           
SELPRG   DS    XL1                 FACPAK PROGRAM CODE                          
SELLNG   DS    XL1                 LANGAUGE CODE                                
         ORG   SELKEY+L'SELKEY                                                  
VSCINKEY DS    V                   UNSCAN ROUTINE ADDRESS                       
FLDCNT   DS    X                   SUB-FIELD COUNT                              
*                                                                               
OVSYS    DS    XL256                                                            
BLOCKI   DS    20CL32              SCAN BLOCK                                   
         ORG   BLOCKI                                                           
BLOCKO   DS    20CL20              UNSCAN BLOCK                                 
         ORG                                                                    
*                                                                               
LOCALX   EQU   *                   END OF LOCAL WORKING STORAGE                 
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007CTGEN0C   12/04/18'                                      
         END                                                                    
