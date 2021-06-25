*          DATA SET CTGEN1D    AT LEVEL 003 AS OF 08/22/00                      
*PHASE TA0B1DA                                                                  
*                                                                               
         TITLE 'CTGEN1D - FILE MAINTENANCE - EXTRACT LOG RECORDS'               
GEN1D    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**GEN1D*,RA,RR=RE                                              
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         USING TWAD,R5             R5=A(TWA)                                    
         USING SAVAREA,R6          R6=A(GLOBAL SAVE AREA)                       
         LA    R2,IOKEY                                                         
         USING GXTRD,R2            R2=A(RECORD KEY)                             
         L     RC,AAPLOCAL                                                      
         USING LOCALD,RC           RC=A(LOCAL W/S)                              
         ST    RE,APRELO                                                        
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
* ROUTINE TO VALIDATE KEY OF EXTRACT RECOVERY SYSTEM LOG RECORD       *         
***********************************************************************         
         SPACE 1                                                                
VALKEY   LA    R2,IOKEY                                                         
         USING GXTRD,R2            R2=A(XLOG RECORD KEY)                        
         XC    GXKEY,GXKEY                                                      
         MVI   GXKREC,GXRKRECQ     RECORD TYPE                                  
*                                                                               
VKSYS    MVI   FVMINL,1            READ SYSTEM FIELD                            
         GOTO1 AVALSE,XLGSYSH                                                   
         BNE   VALKEYX                                                          
         L     R1,0(R1)                                                         
         USING SELISTD,R1                                                       
         MVC   XLGSYS(7),SENAME    DISPLAY FULL SYSTEM NAME                     
         OI    XLGSYSH+6,X'80'                                                  
         MVC   GXRKSEN,APWORK                                                   
         DROP  R1                                                               
*                                                                               
VKDATE   EQU   *                                                                
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,XLGDATH                                                    
         BNE   VKTIME                                                           
         ZIC   R0,FVILEN                                                        
         CLC   XLGDAT,SPACES                                                    
         BE    VKTIME                                                           
         MVC   APBYTE,CULANG                                                    
         OI    APBYTE,PVINSGLO+PVINSGLS                                         
         GOTO1 VPERVAL,APPARM,((R0),FVIFLD),(APBYTE,APWORK)                     
         CLI   4(R1),PVRCONE                                                    
         BNE   EIIF                                                             
         MVC   GXRKDAT,APWORK+PVALCSTA-PERVALD                                  
         XC    GXRKDAT,FFS                                                      
*                                                                               
VKTIME   EQU   *                                                                
         MVI   FVMINL,1                                                         
         GOTO1 VALTIM,XLGTIMH                                                   
         BNE   VKREAD                                                           
         CLC   XLGTIM,SPACES                                                    
         BE    VKREAD                                                           
         MVC   GXRKTMC,APFULL                                                   
         XC    GXRKTMC,FFS                                                      
*                                  READ RECORD                                  
VKREAD   MVC   APRECKEY(GXKEYL),GXKEY                                           
         LA    R1,IOHID+IOGENDIR                                                
         CLI   APACTN,ACTDIS                                                    
         BE    *+8                                                              
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO                                                              
         BL    VALKEYX             I/O ERROR EXIT                               
         CLC   GXKEY(GXRKDAT-GXKEY),APRECKEY                                    
         BE    VKR010                                                           
         MVI   APINDS,APIOKADD     NO RECORD WITH THIS KEY                      
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     VALKEYX                                                          
*                                                                               
VKR010   EQU   *                                                                
         MVC   APRECKEY(GXKEYL),GXKEY                                           
         LA    R1,IORDD+IOGENDIR                                                
         CLI   APACTN,ACTDIS                                                    
         BE    *+8                                                              
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO                                                              
         BL    VALKEYX             I/O ERROR EXIT                               
         BNE   *+12                                                             
         MVI   APINDS,APIOKDIS+APIOKCHA+APIOKDEL                                
         B     VKR020                                                           
         TM    IOERR,IOEDEL        TEST RECORD IS DELETED                       
         BNZ   *+12                                                             
         MVI   APINDS,APIOKADD     NOT DEL THEREFORE NO REC                     
         B     VALKEYY                                                          
         MVI   APINDS,APIOKDIS+APIOKRES                                         
*                                                                               
VKR020   LA    R1,IOGET+IOGENFIL+IO1                                            
         CLI   APACTN,ACTDIS                                                    
         BE    *+8                                                              
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO                 GET RECORD                                   
         TM    IOERR,IOERRS-IOEDEL DELETED IS ONLY SAFE ERR                     
         BZ    *+6                                                              
         DC    H'0'                ERROR ON GET OF D/A RECORD                   
         MVC   APRECDA,IODA        SAVE DISK ADDRESS                            
*                                                                               
VALKEYY  EQU  *                                                                 
*                                  UPDATE SCREEN KEY FIELDS                     
         OI    XLGSYSH+(FVOIND-FVIHDR),FVOXMT                                   
         OI    XLGDATH+(FVOIND-FVIHDR),FVOXMT                                   
         OI    XLGTIMH+(FVOIND-FVIHDR),FVOXMT                                   
         GOTO1 ADISSE,GXRKSEN                                                   
         MVC   XLGSYS,APWORK                                                    
         XC    APWORK,APWORK                                                    
         MVC   APHALF,GXRKDAT                                                   
         XC    APHALF,FFS                                                       
         GOTO1 VDATCON,APPARM,(2,APHALF),(X'51',APWORK)                         
         MVC   XLGDAT,APWORK                                                    
         MVC   APFULL,GXRKTMC                                                   
         XC    APFULL,FFS                                                       
         GOTO1 TIMEOUT,APPARM,APFULL,APWORK                                     
         MVC   XLGTIM,APWORK                                                    
         MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALKEYX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD OR CHANGE AN XLOG RECORD                             *         
***********************************************************************         
         SPACE 1                                                                
VALREC   L     R2,AIOAREA1                                                      
         MVC   GXKEY,APRECKEY                                                   
         CLI   APACTN,ACTADD                                                    
         BNE   VRCHG                                                            
*                                                                               
VRADD    MVC   GXKEY,APRECKEY                                                   
         MVC   GXFLEN,=AL2(GXFIRST)                                             
         XC    GXFSTAT(GXFIRST-GXKEYL),GXFSTAT                                  
         B     VRDATA                                                           
*                                                                               
VRCHG    EQU   *                                                                
         B     VRDATA                                                           
*                                                                               
VRDATA   EQU   *                                                                
*                                  UPDATE RECORD                                
VRUPD    GOTO1 ASETACT,GXTRD       DEFINE ACTIVITY ELEMENT                      
*                                                                               
         LA    R1,IOADD+IOGENFIL+IO1                                            
         CLI   APACTN,ACTADD                                                    
         BE    *+8                                                              
         LA    R1,IOPUT+IOGENFIL+IO1                                            
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   APACTN,ACTADD       DON'T UPDATE GENDIR ON ADD                   
         BE    VALRECOK                                                         
         LA    R2,IOKEY                                                         
         GOTO1 AIO,IOWRITE+IOGENDIR                                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VALRECOK MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     DISREC                                                           
*                                                                               
VALRECX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY KEY OF XLOG RECORD                               *         
***********************************************************************         
         SPACE 1                                                                
DISKEY   LA    R2,APRECKEY                                                      
         GOTO1 ADISSE,GXRKSEN                                                   
         MVC   XLGSYS,APWORK                                                    
         XC    APWORK,APWORK                                                    
         MVC   APHALF,GXRKDAT                                                   
         XC    APHALF,FFS                                                       
         GOTO1 VDATCON,APPARM,(2,APHALF),(X'51',APWORK)                         
         MVC   XLGDAT,APWORK                                                    
         MVC   APFULL,GXRKTMC                                                   
         XC    APFULL,FFS                                                       
         GOTO1 TIMEOUT,APPARM,APFULL,APWORK                                     
         MVC   XLGTIM,APWORK                                                    
         SPACE 1                                                                
DISKEYX  B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO DISPLAY XLOG RECORD                                      *         
***********************************************************************         
         SPACE 1                                                                
DISREC   L     R2,AIOAREA1                                                      
*                                                                               
         TWAXC XLGTSKH                                                          
         XC    XLGFFL,XLGFFL                                                    
         OI    XLGFFLH+(FVOIND-FVIHDR),FVOXMT                                   
         XC    XLGTFL,XLGTFL                                                    
         OI    XLGTFLH+(FVOIND-FVIHDR),FVOXMT                                   
*                                  CLEAR TASK TABLE LINES                       
         USING TSKTD,RF                                                         
         LA    RF,XLGACT1H         SET ADDRESS OF FIRST TABLE LINE              
         LA    R1,XLGTENDH         SET ADDRESS END OF TABLE                     
         LA    RE,XLGACT2H         SET LIST LINE LENGTH                         
         SR    RE,RF                                                            
DREC010  CR    RF,R1                                                            
         BNL   DREC020                                                          
         XC    TSKTLIN,TSKTLIN                                                  
         OI    TSKTLINH+(FVOIND-FVIHDR),FVOXMT                                  
         AR    RF,RE                                                            
         B     DREC010                                                          
         DROP  RF                                                               
*                                                                               
DREC020  LA    R3,APELEM                                                        
         USING GXRSEL,R3                                                        
         MVI   GXRSEL,GXRSELQ                                                   
         MVI   GXRSELL,0                                                        
         GOTO1 AGETELS,GXTRD                                                    
         ICM   R3,15,APPARM                                                     
         BZ    DRECTSKT                                                         
         EDIT  GXRSNUM,(3,XLGTSK),ZERO=NOBLANK,ALIGN=LEFT                       
         GOTO1 TIMEOUT,APPARM,GXRSFTM,APWORK                                    
         MVC   XLGFTM,APWORK                                                    
         GOTO1 TIMEOUT,APPARM,GXRSTTM,APWORK                                    
         MVC   XLGTTM,APWORK                                                    
         GOTO1 VHEXOUT,APPARM,GXRSFDA,APWORK,4,=C'TOG'                          
         MVC   XLGFDA,APWORK                                                    
         GOTO1 VHEXOUT,APPARM,GXRSTDA,APWORK,4,=C'TOG'                          
         MVC   XLGTDA,APWORK                                                    
         TM    GXRSFFL,X'01'                                                    
         BNO   *+8                                                              
         MVI   XLGFFL,C'*'                                                      
         TM    GXRSTFL,X'01'                                                    
         BNO   *+8                                                              
         MVI   XLGTFL,C'*'                                                      
*                                                                               
DRECTSKT EQU   *                                                                
         USING TSKTD,R4                                                         
         LA    R4,XLGACT1H         SET ADDRESS OF FIRST TABLE LINE              
         LA    R1,XLGACT2H         SET TABLE LINE LENGTH                        
         SR    R1,R4                                                            
         STH   R1,TSKTLEN                                                       
         ST    R4,TSKTSTA                                                       
         LA    R8,GXRSLDA                                                       
         MVI   TSKTCNT,0                                                        
*                                                                               
DTSK010  CLI   TSKTCNT,GXRSTMAX                                                 
         BNL   DREC100                                                          
         LA    RF,TSKTDA1                                                       
         GOTO1 VHEXOUT,APPARM,(R8),(RF),4,=C'TOG'                               
         LA    RF,TSKTDA2                                                       
         LA    R8,L'GXRSLDA(R8)                                                 
         GOTO1 VHEXOUT,APPARM,(R8),(RF),4,=C'TOG'                               
         LA    RF,TSKTDA3                                                       
         LA    R8,L'GXRSLDA(R8)                                                 
         GOTO1 VHEXOUT,APPARM,(R8),(RF),4,=C'TOG'                               
         LA    RF,TSKTDA4                                                       
         LA    R8,L'GXRSLDA(R8)                                                 
         GOTO1 VHEXOUT,APPARM,(R8),(RF),4,=C'TOG'                               
         LA    RF,TSKTDA5                                                       
         LA    R8,L'GXRSLDA(R8)                                                 
         GOTO1 VHEXOUT,APPARM,(R8),(RF),4,=C'TOG'                               
         LA    RF,TSKTDA6                                                       
         LA    R8,L'GXRSLDA(R8)                                                 
         GOTO1 VHEXOUT,APPARM,(R8),(RF),4,=C'TOG'                               
         LA    R8,L'GXRSLDA(R8)                                                 
         ZIC   RF,TSKTCNT                                                       
         AH    RF,=H'6'                                                         
         STC   RF,TSKTCNT                                                       
         L     R4,TSKTSTA                                                       
         AH    R4,TSKTLEN                                                       
         ST    R4,TSKTSTA                                                       
         LA    RF,XLGTENDH                                                      
         CR    R4,RF                                                            
         BNH   DTSK010                                                          
         DC    H'00'                                                            
         DROP  R3,R4                                                            
*                                                                               
DREC100  EQU   *                                                                
*                                                                               
         GOTO1 ADISACT,GXTRD       DISPLAY ACTIVITY DATE                        
*                                                                               
DISRECX  EQU   *                                                                
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DELETE AN XLOG RECORD                                    *         
***********************************************************************         
         SPACE 1                                                                
DELREC   LA    R2,IOKEY                                                         
         OI    GXDSTAT,X'80'       SET DELETE FLAG IN DIRECTORY                 
         GOTO1 AIO,IOWRITE+IOGENDIR                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOAREA1                                                      
         GOTO1 ASETACT,GXTRD                                                    
         OI    GXFSTAT,X'80'       SET DELETE FLAG IN RECORD                    
         GOTO1 AIO,IOPUT+IOGENFIL+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
DELRECX  B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO RESTORE A DELETED XLOG RECORD                            *         
***********************************************************************         
         SPACE 1                                                                
RESREC   LA    R2,IOKEY                                                         
         NI    GXDSTAT,X'FF'-X'80' UNSET DELETE                                 
         GOTO1 AIO,IOWRITE+IOGENDIR                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOAREA1                                                      
         GOTO1 ASETACT,GXTRD                                                    
         NI    GXFSTAT,X'FF'-X'80' UNSET DELETE                                 
         GOTO1 AIO,IOWRITE+IOGENFIL+IO1                                         
         BE    *+6                                                              
         DC    H'0'                                                             
RESRECX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE SELECT PARAMETERS                               *         
***********************************************************************         
         SPACE 1                                                                
VALSEL   LA    R2,APRECKEY                                                      
         XC    GXKEY,GXKEY                                                      
         MVI   GXKMAJ,X'FF'        FLAG FOR FIRST PASS                          
         MVI   GXKREC,GXRKRECQ                                                  
         XC    SELKEY,SELKEY                                                    
*                                                                               
*                                                                               
VALSELY  MVC   FVMSGNO,=AL2(FVFOK)                                              
         LA    R0,LSTACT1H         SET ADDRESS OF FIRST LIST LINE               
         ST    R0,APPARM+0                                                      
         LA    R1,LSTACT2H         SET LIST LINE LENGTH                         
         SR    R1,R0                                                            
         STH   R1,APPARM+6                                                      
*                                                                               
VALSELX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GET NEXT LIST/SELECT RECORD                                         *         
***********************************************************************         
         SPACE 1                                                                
GETSEL   LA    R2,IOKEY                                                         
         MVC   GXKEY,APRECKEY                                                   
         CLI   GXKMAJ,X'FF'        TEST FIRST TIME FLAG                         
         BNE   GETSEL2                                                          
         MVI   GXKMAJ,0                                                         
         B     GETSEL6             READ HIGH                                    
GETSEL2  TM    APINDS,2            TEST SEQUENCE BROKEN                         
         BZ    GETSEL4                                                          
         GOTO1 AIO,IOGENDIR+IORD+IO1                                            
         BE    GETSEL8                                                          
         B     GETSELN                                                          
GETSEL4  TM    APINDS,1            TEST READ OR READ HIGH                       
         BNZ   GETSEL8                                                          
GETSEL6  LA    R1,IOGENDIR+IOHI+IO1                                             
         B     *+8                                                              
GETSEL8  LA    R1,IOGENDIR+IOSQ+IO1                                             
         GOTO1 AIO                                                              
         BNE   GETSELN                                                          
         LA    R2,IOKEY                                                         
         CLI   GXKREC,GXRKRECQ     CHECK STILL XLOG RECORD                      
         BNE   GETSELN                                                          
         SPACE 1                                                                
*                                                                               
         GOTO1 AIO,IOGENFIL+IOGET+IO1                                           
         BE    *+6                                                              
         DC    H'0'                RECORD MUST EXIST                            
         L     R2,AIOAREA1                                                      
         XC    APELEM,APELEM                                                    
*                                                                               
GETSELY  MVC   APRECKEY(L'GXKEY),GXKEY                                          
         MVC   APRECDA,IODA        SAVE DISK ADDRESS                            
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     GETSELX                                                          
*                                                                               
GETSELN  MVI   APMODE,APMEOFS      SET NO MORE RECORDS TO COME                  
*                                                                               
GETSELX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY LIST/SELECT LINE                                            *         
***********************************************************************         
         SPACE 1                                                                
DISSEL   L     R2,AIOAREA1                                                      
         L     R4,APPARM                                                        
         USING LISTD,R4            R4=A(LIST/SELECT LINE)                       
*                                  GET DATA FROM RECORD KEY                     
         GOTO1 ADISSE,GXRKSEN                                                   
         MVC   LISTSEN,APWORK                                                   
*                                  GET DATA FROM ELEMENTS                       
         LA    R3,GXFIRST(R2)                                                   
DSLP1    CLI   0(R3),0             E-O-R                                        
         BE    DISSELX                                                          
         CLI   0(R3),GXRSELQ                                                    
         BE    DSRSL                                                            
*                                                                               
DSLP1A   SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     DSLP1                                                            
*                                                                               
         USING GXRSEL,R3                                                        
DSRSL    EQU   *                                                                
         XC    APWORK,APWORK                                                    
         GOTO1 VDATCON,APPARM,(2,GXRSDAT),(X'51',APWORK)                        
         MVC   LISTDAT,APWORK                                                   
         GOTO1 TIMEOUT,APPARM,GXRSTIM,APWORK                                    
         MVC   LISTTIM,APWORK                                                   
         GOTO1 VHEXOUT,APPARM,GXRSFDA,APWORK,4,=C'TOG'                          
         MVC   LISTFDA,APWORK                                                   
         GOTO1 VHEXOUT,APPARM,GXRSTDA,APWORK,4,=C'TOG'                          
         MVC   LISTTDA,APWORK                                                   
         TM    GXRSFFL,X'01'                                                    
         BNO   *+8                                                              
         MVI   LISTFFL,C'*'                                                     
         TM    GXRSTFL,X'01'                                                    
         BNO   *+8                                                              
         MVI   LISTTFL,C'*'                                                     
         B     DSLP1A                                                           
*                                                                               
DISSELX  B     EXIT                                                             
         DROP  R3                                                               
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO HANDLE LAST FOR SCREEN (ENABLE PROGRAM FUNCTION KEYS)    *         
***********************************************************************         
         SPACE 1                                                                
LSTSCR   MVI   APMODE,APMPFKS                                                   
LSTSCRX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE REPORT REQUEST SCREEN                           *         
***********************************************************************         
         SPACE 1                                                                
VALREQ   L     R9,AREP                                                          
         USING REPD,R9             R9=A(REPORT WORK AREA)                       
         XC    SELKEY,SELKEY       SELECTION CRITERION                          
         XC    APRECKEY,APRECKEY                                                
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
*                                                                               
VRQ50    LA    R2,APRECKEY         SET UP INITIAL KEY                           
         MVI   GXKREC,GXAKRECQ                                                  
         MVC   REPDESC,REPDESCL    SET REPORT DESCRIPTION                       
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
* ROUTINE TO GENERATE MESSAGE REPORT                                  *         
***********************************************************************         
         SPACE 1                                                                
PRTREP   L     R9,AREP                                                          
         L     R2,AIOAREA1                                                      
         MVC   IOKEY,APRECKEY      SET INITIAL KEY VALUE                        
*                                                                               
PR010    LA    R1,IOHI+IOGENDIR+IO1                                             
         B     *+8                                                              
PR020    LA    R1,IOSQ+IOGENDIR+IO1                                             
         GOTO1 AIO                                                              
         BNE   PRTREPX                                                          
*                                                                               
         LA    R2,IOKEY                                                         
         CLI   GXKREC,GXAKRECQ     TEST STILL A MESSAGE RECORD                  
         BNE   PRTREPX                                                          
*                                                                               
PR040    GOTO1 VREPORT,REPD                                                     
         B     PR020                                                            
*                                                                               
PRTREPX  MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
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
***********************************************************************         
* CONVERT TIME                                                        *         
***********************************************************************         
         SPACE 1                                                                
TIMEOUT  NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         L     R1,0(R2)                                                         
         SRL   R1,28                                                            
         STC   R1,0(R3)                                                         
         OI    0(R3),X'F0'                                                      
         L     R1,0(R2)                                                         
         SRL   R1,24                                                            
         STC   R1,1(R3)                                                         
         OI    1(R3),X'F0'                                                      
         L     R1,0(R2)                                                         
         SRL   R1,20                                                            
         STC   R1,2(R3)                                                         
         OI    2(R3),X'F0'                                                      
         L     R1,0(R2)                                                         
         SRL   R1,16                                                            
         STC   R1,3(R3)                                                         
         OI    3(R3),X'F0'                                                      
         L     R1,0(R2)                                                         
         SRL   R1,12                                                            
         STC   R1,4(R3)                                                         
         OI    4(R3),X'F0'                                                      
         L     R1,0(R2)                                                         
         SRL   R1,8                                                             
         STC   R1,5(R3)                                                         
         OI    5(R3),X'F0'                                                      
         L     R1,0(R2)                                                         
         SRL   R1,4                                                             
         STC   R1,6(R3)                                                         
         OI    6(R3),X'F0'                                                      
         L     R1,0(R2)                                                         
         STC   R1,7(R3)                                                         
         OI    7(R3),X'F0'                                                      
         B     TOUTOK                                                           
*                                                                               
TOUTNO   B     NO                                                               
TOUTOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE TIME IN FVIFLD, HHMMSSHT                        *         
* NTRY: R1=A(FIELD HEADER)                                            *         
* EXIT: CC .EQ. IF OK ELSE .NE., APFULL TIME IN MVS PWOS FORMAT       *         
***********************************************************************         
         SPACE 1                                                                
VALTIM   NTR1                                                                   
         MVI   FVMAXL,8                                                         
         GOTO1 AFVAL                                                            
         BNE   VTIMNO                                                           
*                                                                               
         MVC   APWORK,=CL8'00000000'                                            
         ZIC   RF,FVXLEN                                                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   APWORK(0),FVIFLD                                                 
         PACK  APDUB(5),APWORK(8)                                               
         ICM   RE,15,APDUB                                                      
         ICM   RF,15,APDUB+4                                                    
         SLDL  RE,4                                                             
         ST    RE,APFULL                                                        
         B     VTIMOK                                                           
*                                                                               
VTIMNO   B     NO                                                               
VTIMOK   EQU   *                                                                
         B     YES                                                              
         EJECT                                                                  
*                                  ERROR EXITS                                  
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
ESYS     MVC   FVMSGNO,=AL2(FVFESYS)                                            
         B     NO                  SYSTEM NAME ERROR                            
EFTB     MVC   FVMSGNO,=AL2(CE#FVMAX)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  FIELD VALUE EXCEEDS MAXIMUM                  
*                                                                               
YES      SR    RC,RC               RETURN CC EQUAL                              
NO       LTR   RC,RC               RETURN CC NOT EQUAL                          
         B     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
SPACES   DC    CL80' '                                                          
FFS      DC    32X'FF'                                                          
         SPACE 1                                                                
         EJECT                                                                  
REPDESCL DC    C'MESSAGE LIST'                                                  
         SPACE 1                                                                
REPSPEC  DS    0X                                                               
         SPEC  H1,1,RUN                                                         
         SPEC  H1,57,C'MESSAGE RECORD LIST'                                     
         SPEC  H2,57,C'-------------------'                                     
         SPEC  M1,24,C'MESSAGE'                                                 
         SPEC  M2,1,C'SYSTEM  LANGUAGE      REFERENCE MESSAGE TEXT'             
         SPEC  M3,1,C'------  --------      --------- ------------'             
         SPEC  H1,100,AGYNAME                                                   
         SPEC  H2,100,AGYADD                                                    
         SPEC  H3,100,REQUESTOR                                                 
         SPEC  H3,120,PAGE                                                      
         SPEC  H4,100,REPORT                                                    
         SPEC  END                                                              
         EJECT                                                                  
* CTGENWRK                                                                      
       ++INCLUDE CTGENWRK                                                       
         EJECT                                                                  
* GEGENXTR                                                                      
       ++INCLUDE GEGENXTR                                                       
         EJECT                                                                  
TWAD     DSECT                                                                  
         ORG   GENTABH                                                          
       ++INCLUDE CTGENE0D                                                       
         ORG   GENTABH                                                          
       ++INCLUDE CTGENC2D                                                       
         ORG   GENTABH                                                          
       ++INCLUDE CTGENA2D                                                       
*                                  WORKING STORAGE SAVED IN TWA                 
         ORG   SAVOVER                                                          
SAVSYS   DS    XL1                 SAVE CURRENT DISPLAYED SYSTEM                
         EJECT                                                                  
LISTD    DSECT                     ** LIST/SELECT LINE LAYOUT **                
LISTACTH DS    XL8                                                              
LISTACT  DS    CL3                 ACTION FIELD                                 
LISTLINH DS    CL8                                                              
LISTLIN  DS    0CL(L'LSTLIN1)                                                   
LISTSEN  DS    CL7                                                              
         DS    CL1                                                              
LISTDAT  DS    CL8                                                              
         DS    CL1                                                              
LISTTIM  DS    CL8                                                              
         DS    CL1                                                              
LISTFDA  DS    CL8                                                              
         DS    CL1                                                              
LISTFFL  DS    CL1                                                              
         DS    CL1                                                              
LISTTDA  DS    CL8                                                              
         DS    CL1                                                              
LISTTFL  DS    CL1                                                              
         DS    CL1                                                              
         ORG   LISTLIN+L'LISTLIN                                                
         EJECT                                                                  
TSKTD    DSECT                     ** TASK TABLE DSECT **                       
TSKTACTH DS    XL8                                                              
TSKTACT  DS    CL3                 ACTION FIELD                                 
TSKTLINH DS    CL8                                                              
TSKTLIN  DS    0CL(L'XLGLIN1)                                                   
TSKTDA1  DS    CL8                                                              
         DS    CL4                                                              
TSKTDA2  DS    CL8                                                              
         DS    CL4                                                              
TSKTDA3  DS    CL8                                                              
         DS    CL4                                                              
TSKTDA4  DS    CL8                                                              
         DS    CL4                                                              
TSKTDA5  DS    CL8                                                              
         DS    CL4                                                              
TSKTDA6  DS    CL8                                                              
         DS    CL4                                                              
TSKTFMAX EQU   (*-TSKTDA1)/(TSKTDA2-TSKTDA1)                                    
         ORG   TSKTLIN+L'TSKTLIN                                                
         SPACE 2                                                                
REPD     DSECT                     ** PRINT LINE LAYOUT **                      
         ORG   REPP1                                                            
PRTLIN   DS    0CL(L'REPP1)                                                     
PRTSYS   DS    CL7                                                              
         DS    CL1                                                              
PRTLANG  DS    CL13                                                             
         DS    CL1                                                              
PRTREF   DS    CL8                                                              
         DS    CL2                                                              
PRTMSG   DS    CL(L'REPP1-(PRTMSG-REPP1))                                       
         EJECT                                                                  
LOCALD   DSECT                     ** DSECT TO COVER LOCAL W/S **               
DUB      DS    D                                                                
WORK     DS    XL64                                                             
ASE      DS    A                                                                
APGM     DS    A                                                                
*                                                                               
LASTSYS  DS    XL1                 CONTROL TOF ON CHANGE OF SYSTEM              
SYSTEM   DS    CL1                 SYSTEM SE NUMBER SAVE                        
SYCNT    DS    CL1                                                              
ASYSNAMS DS    A                                                                
SYSNAMS  DS    CL78                                                             
SYSNAMSL DS    XL1                                                              
SYSNUMS  DS    XL1                                                              
*                                                                               
TSKTSTA  DS    A                                                                
TSKTLEN  DS    H                                                                
TSKTCNT  DS    XL1                                                              
*                                                                               
SELKEY   DS    0XL32                                                            
SELSYS   DS    XL1                 MESSAGE SYSTEM                               
SELTYPE  DS    XL1                 MESSAGE TYPE                                 
SELLNG   DS    XL1                 MESSAGE LANGUAGE                             
SELMSG   DS    XL2                 MESSAGE NUMBER TO START                      
         ORG   SELKEY+L'SELKEY                                                  
*                                                                               
SYSEL    DS    XL(L'APELEM)                                                     
*                                                                               
LOCALX   EQU   *                                                                
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003CTGEN1D   08/22/00'                                      
         END                                                                    
