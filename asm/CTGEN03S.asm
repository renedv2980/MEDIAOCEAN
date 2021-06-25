*          DATA SET CTGEN03S   AT LEVEL 006 AS OF 05/01/02                      
*PHASE TA0B03A                                                                  
*                                                                               
* LEVEL 6 (RTUR) 'SELLANG' IN LOCAL W/S CHANGED TO 'SELLNG'.THIS TO             
*                 AVOID ASSEMBLY ERRORS AS 'SELLANG' IS NOW DEFINED             
*                 IN GEGENWRK. BOOK NOT RE-PANALNKED                            
         TITLE 'CTGEN03 - FILE MAINTENANCE - MESSAGE RECORDS'                   
GEN03    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**GEN3**,RA,RR=RE                                              
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         USING TWAD,R5             R5=A(TWA)                                    
         USING SAVAREA,R6          R6=A(GLOBAL SAVE AREA)                       
         LA    R2,IOKEY                                                         
         USING GMSGD,R2            R2=A(RECORD KEY)                             
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
* ROUTINE TO VALIDATE KEY OF MESSAGE RECORD                           *         
***********************************************************************         
         SPACE 1                                                                
VALKEY   LA    R2,IOKEY                                                         
         USING GMSGD,R2            R2=A(MESSAGE RECORD KEY)                     
         XC    GMKEY,GMKEY                                                      
         MVI   GMKREC,GMKRECQ      RECORD TYPE                                  
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,MSGSYSH       VALIDATE MESSAGE SYSTEM                      
         BNE   VALKEYX                                                          
         TM    FVIIND,FVINUM                                                    
         BO    VK010                                                            
         GOTO1 AVALSYS,MSGSYSH     VALIDATE SYSTEM NAME                         
         BNE   VALKEYX                                                          
         MVC   GMKSYS,APWORK                                                    
         B     VK020                                                            
*                                                                               
VK010    ZIC   R1,FVXLEN           EX LENTH OF INPUT                            
         EX    R1,*+8              CONVERT                                      
         B     *+10                                                             
         PACK  APDUB,MSGSYS(0)                                                  
         CVB   R1,APDUB                                                         
         STC   R1,GMKSYS                                                        
         GOTO1 ADISSYS,GMKSYS      DISPLAY SYSTEM NAME                          
         GOTO1 DISPFLD,MSGSYSH     REDISPLAY                                    
*                                                                               
VK020    MVI   FVMINL,1                                                         
         GOTO1 AFVAL,MSGTYPEH      MESSAGE TYPE                                 
         BNE   VALKEYX                                                          
         LA    R1,MTYPTAB          VALIDATE MESSAGE TYPE                        
         ZIC   RE,FVXLEN                                                        
VK030    CLI   0(R1),EOT                                                        
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALKEYX                                                          
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),0(R1)                                                  
         BE    *+12                                                             
         LA    R1,L'MTYPTAB(R1)                                                 
         B     VK030                                                            
         MVC   GMKTYP,FVIFLD                                                    
*                                                                               
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,MSGNUMH       GET MESSAGE NUMBER                           
         BNE   VALKEYX                                                          
         TM    FVIIND,FVINUM       MUST BE NUMERIC                              
         BO    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     VALKEYX                                                          
         OC    SCFULL(2),SCFULL                                                 
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALKEYX                                                          
         MVC   GMKMSG,SCFULL+2                                                  
*                                                                               
VK050    MVI   FVMINL,1                                                         
         GOTO1 AFVAL,MSGLANGH      LANGUAGE CODE                                
         BNE   VALKEYX                                                          
         GOTO1 AVALLNG,MSGLANGH    VALIDATE LANGUAGE NAME                       
         BNE   VALKEYX                                                          
         MVC   GMKLANG,APWORK                                                   
*                                                                               
         L     R1,APPARM           A(LANGUAGE TABLE ENTRY)                      
         MVI   APWORK,C' '                                                      
         MVC   APWORK+1(L'MSGLANG-1),APWORK                                     
         MVC   APWORK(L'LANGFUL),LANGFUL-LANGTABD(R1)                           
         GOTO1 DISPFLD,MSGLANGH    DISPLAY FULL NAME                            
*                                                                               
VK070    XI    GMKLANG,X'FF'       INVERT LANGUAGE                              
         MVC   APRECKEY(GMKEYL),GMKEY                                           
         LA    R1,IORDD+IOGENDIR+IO1                                            
         CLI   APACTN,ACTDIS                                                    
         BE    *+8                                                              
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO                                                              
         BL    VALKEYX             I/O ERROR EXIT                               
         BNE   *+12                                                             
         MVI   APINDS,APIOKDIS+APIOKCHA+APIOKDEL                                
         B     VK080                                                            
         TM    IOERR,IOEDEL        TEST RECORD IS DELETED                       
         BNZ   *+12                                                             
         MVI   APINDS,APIOKADD     NOT DEL THEREFORE NO REC                     
         B     VALKEYY                                                          
         MVI   APINDS,APIOKDIS+APIOKRES                                         
*                                                                               
VK080    LA    R1,IOGET+IOGENFIL+IO1                                            
         CLI   APACTN,ACTDIS                                                    
         BE    *+8                                                              
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO                 GET RECORD                                   
         TM    IOERR,IOERRS-IOEDEL DELETED IS ONLY SAFE ERR                     
         BZ    *+6                                                              
         DC    H'0'                ERROR ON GET OF D/A RECORD                   
         MVC   APRECDA,IODA        SAVE DISK ADDRESS                            
*                                                                               
VALKEYY  MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALKEYX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD OR CHANGE A MESSAGE RECORD                           *         
***********************************************************************         
         SPACE 1                                                                
VALREC   L     R2,AIOAREA1                                                      
         MVC   GMKEY,APRECKEY                                                   
         CLI   APACTN,ACTADD                                                    
         BE    VR010               NO ELEMENT TO REMOVE ON ADD                  
         MVI   APELEM,GMSGELC                                                   
         MVI   APELEM+1,0                                                       
         GOTO1 ADELELS,GMSGD       DELETE EXISTING MESSAGE ELEMENT              
         MVI   APELEM,GMPRGELC                                                  
         GOTO1 ADELELS,GMSGD       DELETE EXISTING PROGRAM ELEMENT              
         NI    GMFELEM,X'FF'-GMPRGQ                                             
         NI    GMFELEM,X'FF'-GMCOREQ                                            
         LA    R2,IOKEY                                                         
         NI    GMDELEM,X'FF'-GMPRGQ    CLR PRG ELEMENT FLAGS                    
         NI    GMDELEM,X'FF'-GMCOREQ   CLR CORE BUFFER FLAGS                    
         L     R2,AIOAREA1                                                      
*                                                                               
VR010    LA    R3,APELEM                                                        
         USING GMSGEL,R3           R3=A(MESSAGE ELEMENT)                        
         XC    APELEM,APELEM                                                    
         MVI   GMSGEL,GMSGELC                                                   
         MVI   GMSGSEV,C' '                                                     
         GOTO1 AFVAL,MSGSEVH       SEVERITY CODE                                
         BNE   *+10                                                             
         MVC   GMSGSEV,FVIFLD                                                   
*                                                                               
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,MSGMSGH       MESSAGE TEXT                                 
         BNE   VALRECX                                                          
         CLI   GMKTYP,GMKTDIC      IF DATA DICT TYPE                            
         BNE   VR010B                                                           
         CLI   FVIFLD,C'>'           AND COMPLEX FORMAT                         
         BNE   VR010B                                                           
*                                  CONVERT TO INTERNAL FORMAT                   
         GOTO1 FORMIC,APPARM,GMSGEL                                             
         BE    VR010C              LENGTH-1 LONGEST STRING IN APHALF            
         MVC   FVXTRA(L'FICNOMSG),FICNOMSG                                      
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALRECX                                                          
*                                                                               
VR010B   ZIC   R1,FVXLEN                                                        
         STH   R1,APHALF           SAVE MSG LENGTH FOR CHECK                    
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   GMSGTXT(0),FVIFLD                                                
         LA    R1,GMSGFXDL+1(R1)   ADD FIXED LEN +1 FOR EXECUTABLE              
         STC   R1,GMSGELL                                                       
*                                                                               
VR010C   ZIC   R1,GMSGELL                                                       
         LA    R1,GMFIRST(R1)      DEFINE INITIAL REC LENGTH                    
         STCM  R1,3,GMFLEN                                                      
         GOTO1 AADDELS,GMSGD                                                    
         DROP  R3                                                               
*                                                                               
         LA    R3,APELEM           R3=A(PROGRAM ELEMENT)                        
         USING GMPRGD,R3                                                        
         XC    APELEM,APELEM                                                    
         MVI   GMPRGPRG,C' '       CLR PRG AND SUB TO SPACES                    
         MVC   GMPRGPRG+1(L'GMPRGPRG-1),GMPRGPRG                                
         MVC   GMPRGSUB,GMPRGPRG                                                
         GOTO1 AFVAL,MSGPRGH       PROGRAM FIELD                                
         BNE   VR012                                                            
         MVI   GMPRGEL,GMPRGELC                                                 
         GOTO1 AVALPGM,APPARM,(GMKSYS,MSGPRGH)                                  
         BE    VR011                                                            
         LA    R1,PROGTAB          IF PROGRAM NAME                              
         ZIC   RF,FVXLEN           NOT IN SYSTEM LIST                           
VR010A   EX    RF,*+8              TRY TABLE FOR                                
         B     *+10                ANOTHER MATCH                                
         CLC   FVIFLD(0),0(R1)                                                  
         BE    VR011A                                                           
         LA    R1,8(R1)            NEXT ENTRY                                   
         CLI   0(R1),0                                                          
         BNE   VR010A                                                           
         MVC   FVMSGNO,=AL2(FVFEPGM)                                            
         B     VALRECX             NOT FOUND IN EITHER                          
VR011    L     R1,0(R1)                                                         
VR011A   MVC   GMPRGPRG(7),0(R1)   MOVE PGM NAME TO ELEMENT                     
         MVC   APWORK(7),0(R1)                                                  
         GOTO1 DISPFLD,MSGPRGH     MOVE PGM NAME TO FIELD                       
*                                                                               
VR012    GOTO1 AFVAL,MSGSUBH       SUB ID FIELD                                 
         BNE   VR014                                                            
         MVI   GMPRGEL,GMPRGELC                                                 
         ZIC   R1,FVXLEN                                                        
         EX    R1,*+8              MOVE SUB ID TO ELEMENT                       
         B     *+10                                                             
         MVC   GMPRGSUB(0),FVIFLD                                               
*                                                                               
VR014    GOTO1 AFVAL,MSGCOREH      CORE BUFFERED MSG                            
         BNE   VR015                                                            
         CLI   FVIFLD,C'N'                                                      
         BE    VR015                                                            
         CLI   FVIFLD,C'Y'                                                      
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALRECX                                                          
         OI    GMFELEM,GMCOREQ                                                  
         LA    R2,IOKEY                                                         
         OI    GMDELEM,GMCOREQ     SET DIRECTORY FLAGS                          
         L     R2,AIOAREA1                                                      
*                                                                               
VR015    GOTO1 AFVAL,MSGLMTH       MAX LENGTH                                   
         BNE   VR016                                                            
         MVI   GMPRGEL,GMPRGELC                                                 
         TM    FVIIND,FVINUM       CHECK NUMERIC                                
         BO    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     VALRECX                                                          
         OC    SCFULL,SCFULL       CHECK NOT ZERO                               
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALRECX                                                          
         MVC   GMPRGLMT,SCFULL+3   MOVE TO ELEMENT                              
         L     R1,SCFULL                                                        
         SH    R1,APHALF                                                        
         STH   R1,APHALF           APHALF=MAXLEN-(L'MSG-1)                      
*                                                                               
VR016    CLI   GMPRGEL,GMPRGELC    CHECK IF ANY PROG ELEMENTS INPUT             
         BNE   VR018                                                            
         MVI   GMPRGELL,GMPRGLNQ                                                
         OI    GMFELEM,GMPRGQ      SET PRG ELEMENT FLAGS                        
         LA    R2,IOKEY                                                         
         OI    GMDELEM,GMPRGQ      SET DIRECTORY FLAGS                          
         L     R2,AIOAREA1                                                      
         GOTO1 AADDELS,GMSGD                                                    
         DROP  R3                                                               
*                                                                               
VR018    MVI   APELEM,GMTXTELC     EXPANDED TEXT ELEMENTS                       
         MVI   APELEM+1,0                                                       
         GOTO1 ADELELS,GMSGD       REMOVE OLD ELEMENTS                          
         LA    R4,MSGTXT1H         1ST EXPANDED TEXT LINE                       
         LA    R3,APELEM                                                        
         USING GMTXTD,R3                                                        
         XC    APELEM,APELEM       INITIALISE ELEMENT                           
         MVI   GMTXTEL,GMTXTELC                                                 
         MVI   GMTXTLNO,1          RELATIVE LINE NUMBER                         
*                                                                               
VR020    GOTO1 AFVAL,(R4)                                                       
         BNE   VR030               IGNORE BLANK LINES                           
         ZIC   R1,FVXLEN                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   GMTXTLIN(0),FVIFLD                                               
         AH    R1,=Y(GMTXTFXD+1)   FULL ELEMENT LENGTH                          
         STC   R1,GMTXTELL                                                      
         GOTO1 AADDELS,GMSGD                                                    
         XC    GMTXTLIN(L'MSGTXT1),GMTXTLIN                                     
*                                                                               
VR030    CLI   GMTXTLNO,MAXTXTNO   CHECK FOR MORE LINES                         
         BNL   VR040                                                            
         ZIC   R1,GMTXTLNO         REDEFINE ELEMENT LINE NUMBER                 
         LA    R1,1(R1)                                                         
         STC   R1,GMTXTLNO                                                      
         AH    R4,TXTLEN           POINT TO NEXT SCREEN FIELD                   
         B     VR020                                                            
         DROP  R3                                                               
*                                                                               
VR040    GOTO1 ASETACT,GMSGD       DEFINE ACTIVITY ELEMENT                      
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
         BE    VR050                                                            
         LA    R2,IOKEY                                                         
         GOTO1 AIO,IOWRITE+IOGENDIR                                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VR050    XC    APPARM,APPARM       SET UP GETTXT BLOCK                          
         LA    R1,APPARM                                                        
         MVI   8(R1),C'W'          WARNING                                      
         MVI   3(R1),1             RECORD ADDED BUT TOO LONG                    
         CLI   APACTN,ACTADD                                                    
         BE    *+8                                                              
         MVI   3(R1),2             RECORD CHANGED BUT TOO LONG                  
*                                                                               
         ICM   R1,3,APHALF         CHECK MSG LENGTH                             
         BP    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFGTSET)                                           
         B     *+10                                                             
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         LA    R1,GENACTH                                                       
         ST    R1,APCURSOR         FORCE CURSOR TO ACTION FIELD                 
         CR    R1,R1                                                            
*                                                                               
VALRECX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY KEY OF MESSAGE RECORD                            *         
***********************************************************************         
         SPACE 1                                                                
DISKEY   LA    R2,APRECKEY                                                      
         GOTO1 ADISSYS,GMKSYS      GET MESSAGE SYSTEM                           
         GOTO1 DISPFLD,MSGSYSH                                                  
         MVC   MSGTYPE,GMKTYP                                                   
         EDIT  (B2,GMKMSG),(6,MSGNUM),WRK=APWORK,DUB=APDUB,ALIGN=LEFT           
         OI    MSGNUM,C'0'         FOR MESSAGE ZERO                             
*                                                                               
         MVC   APWORK(1),GMKLANG                                                
         XI    APWORK,X'FF'        INVERT LANGUAGE                              
         GOTO1 ADISLNG,APWORK      GET LANGUAGE TABLE ENTRY                     
*                                                                               
         L     R1,APPARM           A(LANGUAGE TABLE ENTRY)                      
         MVI   APWORK,C' '                                                      
         MVC   APWORK+1(L'MSGLANG-1),APWORK                                     
         MVC   APWORK(L'LANGFUL),LANGFUL-LANGTABD(R1)                           
         GOTO1 DISPFLD,MSGLANGH    DISPLAY FULL NAME                            
*                                                                               
DISKEYX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY MESSAGE RECORD                                   *         
***********************************************************************         
         SPACE 1                                                                
DISREC   L     R2,AIOAREA1                                                      
         TWAXC MSGPRGH                                                          
         MVC   MSGSEV,GMSGSEV      SEVERITY CODE                                
*                                                                               
         CLI   GMSGTXT,C' '        IF DATA DICT COMPLEX FORMAT                  
         BNL   DREC010               CONVERT FOR DISPLAY                        
         GOTO1 DISPIC,APPARM,GMSGTXT,MSGMSG,L'MSGMSG-1                          
         B     DREC020                                                          
*                                                                               
DREC010  ZIC   R1,GMSGELL          NOTE MSG ELEM ALWAYS FIXED POSN              
         SH    R1,=Y(GMSGFXDL+1)                                                
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   MSGMSG(0),GMSGTXT                                                
*                                                                               
DREC020  XC    APELEM,APELEM                                                    
         MVI   APELEM,GMPRGELC     LOOK PROGRAM ELEMENT                         
         GOTO1 AGETELS,GMSGD                                                    
         ICM   R3,15,APPARM        R3=A(PROGRAM ELEMENT)                        
         USING GMPRGD,R3                                                        
         BZ    DREC04                                                           
         MVC   MSGPRG,GMPRGPRG                                                  
         MVC   MSGSUB,GMPRGSUB                                                  
         EDIT  (B1,GMPRGLMT),(2,MSGLMT),WRK=APWORK,DUB=APDUB,ALIGN=LEFT         
*                                                                               
DREC04   MVI   MSGCORE,C'N'                                                     
         TM    GMFELEM,GMCOREQ                                                  
         BZ    DREC05                                                           
         MVI   MSGCORE,C'Y'                                                     
*                                                                               
DREC05   XC    APELEM,APELEM                                                    
         MVI   APELEM,GMTXTELC     LOOK FOR EXPANDED TEXT                       
         GOTO1 AGETELS,GMSGD                                                    
         ICM   R3,15,APPARM        R3=A(TEXT ELEMENT)                           
         USING GMTXTD,R3                                                        
         BZ    DISRECX                                                          
*                                                                               
DREC10   MVI   APWORK,C' '                                                      
         MVC   APWORK+1(L'MSGTXT1-1),APWORK                                     
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
         LA    R0,MSGTXT1H                                                      
         AR    R1,R0               R1=A(TEXT HEADER)                            
         GOTO1 DISPFLD             DISPLAY THE FIELD                            
         BAS   RE,NEXTEL           GET NEXT ELEMENT R3=A(CURRENT EL)            
         BE    DREC10                                                           
         DROP  R3                                                               
*                                                                               
*                                                                               
DISRECX  GOTO1 ADISACT,GMSGD       DISPLAY ACTIVITY DATE                        
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DELETE AN MESSAGE RECORD                                 *         
***********************************************************************         
         SPACE 1                                                                
DELREC   LA    R2,IOKEY                                                         
         OI    GMDSTAT,X'80'       SET DELETE FLAG IN DIRECTORY                 
         GOTO1 AIO,IOWRITE+IOGENDIR                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOAREA1                                                      
         GOTO1 ASETACT,GMSGD                                                    
         OI    GMFSTAT,X'80'       SET DELETE FLAG IN RECORD                    
         GOTO1 AIO,IOPUT+IOGENFIL+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
DELRECX  B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO RESTORE A DELETED MESSAGE RECORD                         *         
***********************************************************************         
         SPACE 1                                                                
RESREC   LA    R2,IOKEY                                                         
         NI    GMDSTAT,X'FF'-X'80' UNSET DELETE                                 
         GOTO1 AIO,IOWRITE+IOGENDIR                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOAREA1                                                      
         GOTO1 ASETACT,GMSGD                                                    
         NI    GMFSTAT,X'FF'-X'80' UNSET DELETE                                 
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
         XC    GMKEY,GMKEY                                                      
         MVI   GMKMAJ,X'FF'        FLAG FOR FIRST PASS                          
         MVI   GMKREC,GMKRECQ                                                   
         XC    SELKEY,SELKEY                                                    
*                                                                               
         GOTO1 AFVAL,LSTSYSH       VALIDATE SYSTEM (IF INPUT)                   
         BNE   VS020                                                            
         TM    FVIIND,FVINUM                                                    
         BO    VS010                                                            
         GOTO1 AVALSYS,LSTSYSH     VALIDATE SYSTEM NAME                         
         BNE   VALSELX                                                          
         MVC   SELSYS,APWORK                                                    
         CLI   SELSYS,0            GENERAL - SYSTEM WIDE MESSAGES               
         BNE   *+8                                                              
         MVI   SELSYS,X'FF'        FLAG TO DETECT FILTER ON ZERO                
         B     VS020                                                            
*                                                                               
VS010    ZIC   R1,FVXLEN           EX LENTH OF INPUT                            
         EX    R1,*+8              CONVERT                                      
         B     *+10                                                             
         PACK  APDUB,LSTSYS(0)                                                  
         CVB   R1,APDUB                                                         
         CH    R1,=H'255'                                                       
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALSELX                                                          
         STC   R1,SELSYS                                                        
         GOTO1 ADISSYS,SELSYS      GET SYSTEM NAME                              
         CLI   SELSYS,0            GENERAL SYSTEM WIDE MESSGES                  
         BNE   *+8                                                              
         MVI   SELSYS,X'FF'        FLAG TO DETECT FILTER ON ZERO                
         GOTO1 DISPFLD,LSTSYSH     REDISPLAY                                    
*                                                                               
VS020    CLI   OPTPROG,0           SEE IF 'PROG=' ENVOKED                       
         BE    VS025                                                            
         CLI   SELSYS,X'00'        CAN'T USE PROG= FOR ALL SYS                  
         BE    *+12                                                             
         CLI   SELSYS,X'FF'        CAN'T USE PROG= FOR GEN SYS                  
         BNE   VS021                                                            
         MVC   FVMSGNO,=AL2(FVFGTSET)                                           
         LA    R1,MSGSYSH                                                       
         ST    R1,APCURSOR                                                      
         XC    APPARM,APPARM                                                    
         GOTO1 ,APPARM,150,(C'E',0)                                             
         B     VALSELX                                                          
VS021    GOTO1 AVALPGM,APPARM,(SELSYS,OPTPROG)                                  
         BE    VS022                                                            
         LA    R1,PROGTAB          TRY PROGTAB NEXT                             
VS021A   ZIC   RF,OPTPROG+5        GET LENGTH                                   
         BCTR  RF,0                                                             
         EX    RF,*+8              COMPARE                                      
         B     *+10                                                             
         CLC   OPTPROG+8(0),0(R1)                                               
         BE    VS022A                                                           
         LA    R1,8(R1)            NEXT ENTRY                                   
         CLI   0(R1),0                                                          
         BNE   VS021A                                                           
         MVC   FVMSGNO,=AL2(FVFGTSET)                                           
         LA    R1,GENOPTH                                                       
         ST    R1,APCURSOR                                                      
         XC    APPARM,APPARM                                                    
         GOTO1 ,APPARM,151,(C'E',0)                                             
         B     VALSELX                                                          
VS022    L     R1,0(R1)                                                         
VS022A   MVC   OPTPROG(8),0(R1)    MOVE PROGRAM FULL NAME                       
VS025    GOTO1 AFVAL,LSTTYPEH      VALIDATE MESSAGE TYPE (IF INPUT)             
         BNE   VS040                                                            
         LA    R1,MTYPTAB          VALIDATE MESSAGE TYPE                        
         ZIC   RE,FVXLEN                                                        
*                                                                               
VS030    CLI   0(R1),EOT                                                        
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALSELX                                                          
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),0(R1)                                                  
         BE    *+12                                                             
         LA    R1,L'MTYPTAB(R1)                                                 
         B     VS030                                                            
         MVC   SELTYPE,FVIFLD                                                   
*                                                                               
VS040    GOTO1 AFVAL,LSTLANGH      LANGUAGE CODE (IF INPUT)                     
         BNE   VS050                                                            
         GOTO1 AVALLNG,LSTLANGH    VALIDATE LANGUAGE NAME                       
         BNE   VALSELX                                                          
         MVC   SELLNG,APWORK                                                    
         XI    SELLNG,X'FF'        INVERT LANGUAGE CODE                         
         L     R1,APPARM           A(LANGUAGE TABLE ENTRY)                      
         MVI   APWORK,C' '                                                      
         MVC   APWORK+1(L'LSTLANG-1),APWORK                                     
         MVC   APWORK(L'LANGFUL),LANGFUL-LANGTABD(R1)                           
         GOTO1 DISPFLD,LSTLANGH    DISPLAY FULL NAME                            
*                                                                               
VS050    GOTO1 AFVAL,LSTMSGH       GET MESSAGE NUMBER (IF ANY)                  
         BNE   VS070                                                            
         OC    SELSYS,SELSYS       MUST HAVE SYSTEM+TYPE DEFINED                
         BZ    *+12                                                             
         CLI   SELTYPE,0                                                        
         BNE   VS060                                                            
         MVC   FVMSGNO,=AL2(CTINVST)         INV SYSTEM AND TYPE                
         B     VALSELX                                                          
*                                                                               
VS060    TM    FVIIND,FVINUM       MUST BE NUMERIC                              
         BO    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     VALSELX                                                          
         OC    SCFULL(2),SCFULL                                                 
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALSELX                                                          
         OC    SCFULL(4),SCFULL                                                 
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALSELX                                                          
         MVC   SELMSG,SCFULL+2                                                  
*                                                                               
VS070    OC    SELSYS,SELSYS       BUILD AN INITIAL KEY                         
         BZ    VALSELY                                                          
         CLI   SELSYS,X'FF'        FILTER ON SYSTEM ZERO (GENERAL)              
         BE    *+10                                                             
         MVC   GMKSYS,SELSYS                                                    
         CLI   SELTYPE,0                                                        
         BE    VALSELY                                                          
         MVC   GMKTYP,SELTYPE                                                   
         OC    SELMSG,SELMSG                                                    
         BZ    VALSELY                                                          
         MVC   GMKMSG,SELMSG                                                    
         MVC   GMKLANG,SELLNG                                                   
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
         MVC   GMKEY,APRECKEY                                                   
         CLI   GMKMAJ,X'FF'        TEST FIRST TIME FLAG                         
         BNE   GETSEL2                                                          
         MVI   GMKMAJ,0                                                         
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
         CLI   GMKREC,GMKRECQ      CHECK STILL MESSAGE RECORD                   
         BNE   GETSELN                                                          
         SPACE 1                                                                
*                                  FILTER ON SELECTION CRITERION                
         CLI   SELSYS,0            TEST IF SYSTEM DEFINED                       
         BE    GETSEL10                                                         
*                                                                               
         CLC   SELSYS,GMKSYS       TEST SYSTEM                                  
         BE    GETSEL10                                                         
         CLI   GMKSYS,0            FILTER ON ZERO - SELSYS X'FF'                
         BNE   GETSELN                                                          
*                                                                               
GETSEL10 CLI   SELTYPE,0           TEST IF TYPE DEFINED                         
         BE    GETSEL12                                                         
         CLC   SELTYPE,GMKTYP      TEST MESSAGE TYPE                            
         BE    GETSEL12                                                         
         BH    GETSEL11            NOT REACHED MESSAGE TYPE YET                 
         CLI   GMKSYS,X'FF'                                                     
         BE    GETSELN             FINISHED                                     
         ZIC   R1,GMKSYS           REDEFINE IOKEY FOR READ HIGH                 
         LA    R1,1(R1)            BUMP SYSTEM NUMBER                           
         STC   R1,GMKSYS                                                        
GETSEL11 MVC   GMKTYP,SELTYPE                                                   
         MVC   GMKMSG,SELMSG                                                    
         MVC   GMKLANG,SELLNG                                                   
         B     GETSEL6             READ HIGH                                    
*                                                                               
GETSEL12 CLI   SELLNG,0            TEST IF LANGUAGE DEFINED                     
         BE    GETSEL13                                                         
         CLC   SELLNG,GMKLANG      TEST LANGUAGE                                
         BNE   GETSEL8             SEQ READ FOR NEXT MSG                        
*                                                                               
GETSEL13 CLI   OPTPROG,0           CHECK IF PROG OR SUB                         
         BNE   GETSELP                                                          
         CLI   OPTSUB,0            FILTERS INVOKED                              
         BE    GETSEL14                                                         
*                                                                               
GETSELP  TM    GMDELEM,GMPRGQ      HAS RECORD GOT A PRG ELEMENT                 
         BNO   GETSEL8             NO SO DONT BOTHER                            
*                                                                               
GETSEL14 GOTO1 AIO,IOGENFIL+IOGET+IO1                                           
         BE    *+6                                                              
         DC    H'0'                RECORD MUST EXIST                            
         L     R2,AIOAREA1                                                      
         XC    APELEM,APELEM                                                    
         MVI   APELEM,GMPRGELC                                                  
         GOTO1 AGETELS,GMSGD                                                    
         ICM   R3,15,APPARM                                                     
         USING GMPRGD,R3                                                        
         BZ    GETSEL15            SKIP IF NO ELEMENT                           
*                                                                               
         CLI   OPTPROG,0                                                        
         BE    GETSELP1                                                         
         CLC   GMPRGPRG(7),OPTPROG   CHECK PROGRAM NAME                         
         BNE   GETSEL8                                                          
*                                                                               
GETSELP1 CLI   OPTSUB,0                                                         
         BE    GETSEL15                                                         
         OC    GMPRGSUB,SPACES     CONVERT TO UPPER CASE                        
         OC    OPTSUB(8),SPACES                                                 
         CLC   GMPRGSUB,OPTSUB     CHECK SUB FIELD                              
         BNE   GETSEL8                                                          
*                                                                               
GETSEL15 CLI   OPTSCAN,0           SEE IF 'SCAN=' ENVOKED                       
         BE    GETSELY                                                          
*                                  TEST IF TEXT CONTAINS SCAN (S)               
         ZIC   R1,GMSGELL          L'MESSAGE ELEMENT                            
         SH    R1,=Y(GMSGFXDL+1)                                                
         MVI   APWORK,X'40'        PRESET WORK TO SPACES                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   APWORK+1(0),APWORK                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         OC    APWORK(0),GMSGTXT   MOVE IN MESSAGE AND CONVERT TO U/C           
         LA    R4,1(R1)            L'TEXT                                       
         LA    R3,OPTSCAN          A'SCAN TEXTS                                 
*                                                                               
GETSEL16 CLI   0(R3),0                                                          
         BE    GETSELY                                                          
*                                                                               
         ZIC   R1,0(R3)            GET L'SCAN STRING                            
         CR    R4,R1               CHECK L'TEXT NOT LESS THAN L'SCAN            
         BL    GETSEL8                                                          
         BCTR  R1,0                                                             
         LR    RF,R4                                                            
         SR    RF,R1               NUMBER OF COMPARES REQUIRED                  
         LA    RE,APWORK                                                        
GETSEL18 EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),1(R3)                                                    
         BE    GETSEL20                                                         
         LA    RE,1(RE)                                                         
         BCT   RF,GETSEL18                                                      
         B     GETSEL8             GET NEXT SEQUENTIAL RECORD                   
GETSEL20 LA    R3,2(R1,R3)         GET NEXT TEXT STRING                         
         B     GETSEL16                                                         
*                                                                               
GETSELY  MVC   APRECKEY(L'GMKEY),GMKEY                                          
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
*                                                                               
         GOTO1 ADISSYS,GMKSYS      GET SYSTEM SHORT NME                         
         ICM   R1,15,APPARM        R1=A(SYSLST ENTRY)                           
         BNZ   *+14                                                             
         MVC   LISTSYS,APWORK      UNKNOWN SYSTEM                               
         B     *+10                                                             
         MVC   LISTSYS,SYSLSHRT-SYSLSTD(R1)                                     
*                                                                               
         MVC   APWORK(1),GMKLANG                                                
         XI    APWORK,X'FF'        INVERT LANGUAGE                              
         GOTO1 ADISLNG,APWORK      GET LANGUAGE NAME                            
         L     R1,APPARM           A(LANGUAGE TABLE ENTRY)                      
         MVC   LISTLANG,LANGSHR-LANGTABD(R1)                                    
*                                                                               
         MVC   LISTMNO+0(1),GMKTYP BUILD MESSAGE REFERENCE                      
         ZIC   R1,GMKSYS                                                        
         LA    R1,SYSTAB(R1)                                                    
         MVC   LISTMNO+1(1),0(R1)                                               
         SR    R0,R0                                                            
         ICM   R0,3,GMKMSG                                                      
         CVD   R0,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  LISTMNO+2(5),APDUB                                               
         CLI   LISTMNO+2,C'0'                                                   
         BNE   *+8                                                              
         MVI   LISTMNO+2,C'/'                                                   
         MVC   LISTMNO+7(1),GMSGSEV                                             
*                                                                               
         CLI   GMSGTXT,C' '        IF DATA DICT COMPLEX FORMAT                  
         BNL   DISSEL1               CONVERT FOR DISPLAY                        
         GOTO1 DISPIC,APPARM,GMSGTXT,LISTMSG,L'LISTMSG-1                        
         B     DISSEL3                                                          
*                                                                               
DISSEL1  ZIC   R1,GMSGELL          ELEMENT LENGTH                               
         SH    R1,=Y(GMSGFXDL+1)   EX LENGTH OF TEXT STRING                     
         CH    R1,=Y(L'LISTMSG-1)  ENOUGH ROOM FOR FULL MESSAGE                 
         BNH   DISSEL2                                                          
         LH    R1,=Y(L'LISTMSG-1)  JUST MOVE AS MUCH AS FIT'S                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LISTMSG(0),GMSGTXT                                               
         LA    R1,LISTMSG(R1)                                                   
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         MVI   0(R1),C'>'          INDICATE SOME NOT DISPLAYED                  
         B     DISSEL3                                                          
*                                                                               
DISSEL2  EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LISTMSG(0),GMSGTXT                                               
*                                                                               
DISSEL3  MVI   LISTXTND,C' '       SET UP EXTENDED MSG IND                      
         XC    APELEM,APELEM                                                    
         MVI   APELEM,GMTXTELC     EXTENDED TEXT ELEMENT                        
         GOTO1 AGETELS,GMSGD                                                    
         OC    APPARM(4),APPARM    WAS EL FOUND                                 
         BZ    *+8                                                              
         MVI   LISTXTND,C'Y'                                                    
*                                                                               
DISSELX  B     EXIT                                                             
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
         GOTO1 AFVAL,REPSYSH       VALIDATE SYSTEM (IF INPUT)                   
         BNE   VRQ20                                                            
         TM    FVIIND,FVINUM                                                    
         BO    VRQ10                                                            
         GOTO1 AVALSYS,REPSYSH     VALIDATE SYSTEM NAME                         
         BNE   VALREQX                                                          
         MVC   SELSYS,APWORK                                                    
         B     VRQ20                                                            
*                                                                               
VRQ10    ZIC   R1,FVXLEN           SYSTEM (NUMERIC)                             
         EX    R1,*+8              CONVERT                                      
         B     *+10                                                             
         PACK  APDUB,REPSYS(0)                                                  
         CVB   R1,APDUB                                                         
         CH    R1,=H'255'                                                       
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALREQX                                                          
         STC   R1,SELSYS                                                        
         GOTO1 ADISSYS,SELSYS      GET SYSTEM NAME                              
         GOTO1 DISPFLD,REPSYSH     REDISPLAY                                    
*                                                                               
VRQ20    GOTO1 AFVAL,REPTYPH       VALIDATE MESSAGE TYPE (IF INPUT)             
         BNE   VRQ40                                                            
         LA    R1,MTYPTAB          VALIDATE MESSAGE TYPE                        
         ZIC   RE,FVXLEN                                                        
*                                                                               
VRQ30    CLI   0(R1),EOT                                                        
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALREQX                                                          
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),0(R1)                                                  
         BE    *+12                                                             
         LA    R1,L'MTYPTAB(R1)                                                 
         B     VRQ30                                                            
         MVC   SELTYPE,FVIFLD                                                   
*                                                                               
VRQ40    GOTO1 AFVAL,REPLNGSH      LANGUAGE CODE (IF INPUT)                     
         BNE   VRQ50                                                            
         GOTO1 AVALLNG,REPLNGSH    VALIDATE LANGUAGE NAME                       
         BNE   VALREQX                                                          
         MVC   SELLNG,APWORK                                                    
         XI    SELLNG,X'FF'        INVERT LANGUAGE CODE                         
         L     R1,APPARM           A(LANGUAGE TABLE ENTRY)                      
         MVI   APWORK,C' '                                                      
         MVC   APWORK+1(L'REPLNGS-1),APWORK                                     
         MVC   APWORK(L'LANGSHR),LANGSHR-LANGTABD(R1)                           
         GOTO1 DISPFLD,REPLNGSH    DISPLAY SHORT NAME                           
         MVI   APWORK,C' '                                                      
         MVC   APWORK+1(L'REPLNGF-1),APWORK                                     
         MVC   APWORK(L'LANGFUL),LANGFUL-LANGTABD(R1)                           
         GOTO1 DISPFLD,REPLNGFH    DISPLAY FULL NAME                            
*                                                                               
VRQ50    LA    R2,APRECKEY         SET UP INITIAL KEY                           
         MVI   GMKREC,GMKRECQ                                                   
         MVC   GMKSYS,SELSYS                                                    
         MVC   GMKTYP,SELTYPE                                                   
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
         MVI   LASTSYS,0           CLEAR LAST SYSTEM VALUE                      
*                                                                               
PR010    LA    R1,IOHI+IOGENDIR+IO1                                             
         B     *+8                                                              
PR020    LA    R1,IOSQ+IOGENDIR+IO1                                             
         GOTO1 AIO                                                              
         BNE   PRTREPX                                                          
*                                                                               
         LA    R2,IOKEY                                                         
         CLI   GMKREC,GMKRECQ      TEST STILL A MESSAGE RECORD                  
         BNE   PRTREPX                                                          
         CLI   SELSYS,0            TEST IF FILTER ON SYSTEM                     
         BE    *+14                                                             
         CLC   GMKSYS,SELSYS                                                    
         BNE   PRTREPX                                                          
*                                                                               
         CLC   LASTSYS,GMKSYS      SAME SYSTEM (OR FIRST PASS)                  
         BE    *+14                                                             
         OI    REPHEADI,REPHFRCE   FORCE NEW PAGE                               
         MVC   LASTSYS,GMKSYS                                                   
*                                                                               
         CLI   SELTYPE,0           TEST IF FILTER ON TYPE                       
         BE    PR030                                                            
         CLC   SELTYPE,GMKTYP                                                   
         BE    PR030                                                            
         BH    PR025               NOT REACHED MESSAGE TYPE YET                 
         ZIC   R1,GMKSYS           BUMP SYSTEM NUMBER                           
         LA    R1,1(R1)                                                         
         STC   R1,GMKSYS                                                        
*                                                                               
PR025    MVC   GMKTYP,SELTYPE      RESET TO REQUIRED MSG TYPE                   
         XC    GMKMSG(L'GMKMSG+L'GMKLANG),GMKMSG                                
         B     PR010               READ HIGH FOR NEXT SYSTEM/TYPE               
*                                                                               
PR030    CLI   SELLNG,0            TEST IF FILTER ON LANGUAGE                   
         BE    *+14                                                             
         CLC   GMKLANG,SELLNG                                                   
         BNE   PR020               READ SEQUENTIAL FOR NEXT MESSAGE             
*                                                                               
         GOTO1 AIO,IOGENFIL+IOGET+IO1                                           
         BE    *+6                                                              
         DC    H'0'                RECORD MUST EXIST                            
         L     R2,AIOAREA1                                                      
*                                                                               
         GOTO1 ADISSYS,GMKSYS      GET SYSTEM NAME                              
         MVC   PRTSYS,APWORK                                                    
*                                                                               
         MVC   APBYTE,GMKLANG      LANGUAGE CODE                                
         XI    APBYTE,X'FF'        INVERT                                       
         GOTO1 ADISLNG,APBYTE                                                   
         L     R1,APPARM           A(LANGUAGE TABLE ENTRY)                      
         MVC   PRTLANG,LANGFUL-LANGTABD(R1)                                     
*                                                                               
         MVC   PRTREF+0(1),GMKTYP  BUILD MESSAGE REFERENCE                      
         ZIC   R1,GMKSYS                                                        
         LA    R1,SYSTAB(R1)                                                    
         MVC   PRTREF+1(1),0(R1)                                                
         SR    R0,R0                                                            
         ICM   R0,3,GMKMSG                                                      
         CVD   R0,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  PRTREF+2(5),APDUB                                                
         CLI   PRTREF+2,C'0'       OVERWRITE LEADING ZERO                       
         BNE   *+8                                                              
         MVI   PRTREF+2,C'/'                                                    
         MVC   PRTREF+7(1),GMSGSEV                                              
*                                                                               
         CLI   GMSGTXT,C' '        IF FOR DATA DICT COMPLEX FORMAT              
         BNL   PR035                 CONVERT FOR DISPLAY                        
         GOTO1 DISPIC,APPARM,GMSGTXT,PRTMSG,L'PRTMSG-1                          
         B     PR040                                                            
*                                                                               
PR035    ZIC   R1,GMSGELL          GET MESSAGE TEXT                             
         SH    R1,=Y(GMSGFXDL+1)   EX LENGTH OF TEXT STRING                     
         CH    R1,=Y(L'PRTMSG-1)   ENOUGH ROOM FOR FULL MESSAGE                 
         BH    *+12                                                             
         EX    R1,PRMVC            MVC PRTMSG(0),GMSGTXT                        
         B     PR040                                                            
*                                                                               
         LH    R1,=Y(L'PRTMSG-1)   JUST MOVE AS MUCH AS FIT'S                   
         EX    R1,*+8                                                           
         B     *+10                                                             
PRMVC    MVC   PRTMSG(0),GMSGTXT                                                
         LA    R1,PRTMSG(R1)       FIND END OF TXT                              
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         MVI   0(R1),C'>'          INDICATE SOME NOT DISPLAYED                  
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
**********************************************************************          
* CONVERT DATA DICTIONARY COMPLEX MESSAGE DATA TO INTERNAL FORMAT    *          
* STRINGS DELIMITED BY '>' WHICH CAN BE IMBEDDED IN STRING AS, '>>'  *          
* STRING CAN ALSO INCLUDE SPECIAL SPLITTER CHARACTER ':'.            *          
* ON ENTRY: 0(R1) = A(MESSAGE ELEMENT.)  INPUT DEFINED IN FVIHDR ETC.*          
* ON EXIT CONVERTED FORMAT OF DATA IN ELEMENT IS:                    *          
* <STRING COUNT><LIST(<STRING LENGTH>)><LIST(<STRING TEXT>)>         *          
* ALSO: UPDATE ELEMNT LENGTH IN HEADER AND SAVE VALUE OF             *          
* (MAX STRINGLEN-1) IN APHALF FOR LATER COMPARISON.                  *          
**********************************************************************          
         SPACE 1                                                                
FORMIC   NTR1                                                                   
         L     R3,0(R1)            GET A(ELEMENT) ON PARAMETER LIST             
         USING GMSGEL,R3                                                        
         MVI   GMSGTXT,0           INITIALISE ELEMENT STRING COUNT              
         XC    APWORK,APWORK       CLEAR STRING SAVE BUFFER                     
         LA    R8,APWORK                                                        
         ZIC   R0,FVILEN           SAVE INPUT FIELD LENGTH                      
         LA    R4,FVIFLD             AND POINT TO IT                            
*                                  CONVERT EACH STRING IN INPUT                 
FICLP1   SR    R9,R9               LENGTH OF STRING                             
         LA    R4,1(R4)                                                         
         BCT   R0,*+8              IF END INPUT                                 
         B     FICLP1X               EXIT                                       
         MVI   APFLAG,0            INITIALISE NO SPLITTER FLAG                  
*                                  EXTRACT AND CONVERT STRING                   
FICLP2   CLI   0(R4),X'00'         NULL STRING                                  
         BE    FICLP1X                                                          
         CLC   0(2,R4),=C'>>'      DELIMITER CHARACTER IN STRING                
         BE    FICLP2A                                                          
         CLI   0(R4),C'>'          STRING DELIMITER CHARACTER                   
         BE    FICLP2B                                                          
         CLI   0(R4),C':'          STRING SPLITTER CHARACTER                    
         BNE   *+8                                                              
         OI    APFLAG,X'FF'        INDICATE SPLITTER PRESENCE                   
*                                                                               
         MVC   0(1,R8),0(R4)       PUT CHARACTER IN OUTPUT BUFFER               
         LA    R9,1(R9)            BUMP STRING LENGTH                           
         LA    R4,1(R4)              INPUT BUFFER POINTER                       
         LA    R8,1(R8)              OUTPUT BUFFER POINTER                      
         BCT   R0,FICLP2           GET NEXT CHARACTER                           
         B     FICNO                                                            
*                                                                               
FICLP2A  MVC   0(1,R8),0(R4)       EXTRACT DELIMITER IN STRING                  
         LA    R9,1(R9)                                                         
         LA    R4,2(R4)                                                         
         LA    R8,1(R8)                                                         
         BCTR  R0,0                                                             
         BCT   R0,FICLP2           GET NEXT CHARACTER                           
         B     FICNO                                                            
*                                                                               
FICLP2B  CH    R9,=H'01'           CHECK AT LEAST ONE CHARACTER FOUND           
         BL    FICNO                                                            
         CLI   APFLAG,0            IF SPLITTER                                  
         BE    FICLP2E                                                          
         MVI   APFLAG,0              CONVERT SPLIT STRING                       
         LA    RE,X'01'            TEST ODD OR EVEN                             
         NR    RE,R9               TEST DIVIDER NOT IN EXACT HALFWAY            
         BNZ   FICLP2C                                                          
         B     FICNO                                                            
*                                                                               
FICLP2C  BCTR  R9,0                CALCULATE SPLIT HALF LENGTH                  
         LR    RE,R9                                                            
         LA    RF,X'80'                                                         
         OR    R9,RF               MAKE IT SPECIAL LENGTH                       
         SRL   RE,1                DIVIDE BY 2                                  
         LR    RF,R4                                                            
         SR    RF,RE                                                            
         BCTR  RF,0                                                             
         CLI   0(RF),C':'                                                       
         BE    FICLP2D                                                          
         B     FICNO                                                            
*                                                                               
FICLP2D  LR    RF,R8               PATCH STRING BY REMOVING SPLITTER            
         SR    RF,RE                                                            
         BCTR  RF,0                                                             
         MVC   0(1,RF),1(RF)                                                    
         LA    RF,1(RF)                                                         
         CR    RF,R8                                                            
         BL    *-12                                                             
         BCTR  R8,0                SPLITTER PROCESSING COMPLETE                 
*                                  HERE AT END OF STRING                        
FICLP2E  LA    RF,GMSGTXT          POINT TO CONVERTED DATA BUFFER               
         ZIC   R1,0(RF)            CURRENT STRING COUNT                         
         AR    RF,R1               OFFSET TO LAST STRING LENGTH                 
         CLM   R9,1,0(RF)          TEST LENGTH IN ASCENDING ORDER               
         BH    FICLP2X                                                          
         B     FICNO                                                            
*                                                                               
FICLP2X  STC   R9,1(RF)            SAVE NEW STRING LENGTH                       
         BCTR  R9,0                                                             
         STH   R9,APHALF             AND SAVE-1 FOR MAX LEN COMPARE             
         NI    APHALF+1,X'FF'-X'80'  CLEARING ANY SPLITTER FLAG                 
         LA    R1,1(R1)            BUMP STRING COUNT                            
         STC   R1,GMSGTXT            AND SAVE IT                                
         B     FICLP1                                                           
*                                                                               
FICLP1X  EQU   *                                                                
         CLI   GMSGTXT,0           TEST IF ANY STRINGS FOUND                    
         BH    FICYES                                                           
         B     FICNO                                                            
*                                  EXIT WITH ERROR                              
FICNO    B     NO                                                               
*                                  EXIT OK                                      
FICYES   LA    RF,GMSGTXT          POINT TO CONVERTED DATA BUFFER               
         ZIC   R1,0(RF)             AND MOVE IN SAVED STRINGS                   
         AR    RF,R1                AFTER HEADER INFO                           
         LA    RF,1(RF)                                                         
         LA    R1,APWORK                                                        
FICY01   CLI   0(R1),0             TEST END OF DATA                             
         BE    FICY02                                                           
         MVC   0(1,RF),0(R1)       MOVE IN BYTE OF DATA                         
         LA    RF,1(RF)                                                         
         LA    R1,1(R1)                                                         
         B     FICY01                                                           
FICY02   LA    R1,GMSGTXT                                                       
         SR    RF,R1               FINAL LENGTH OF DATA                         
         LA    RF,GMSGFXDL(RF)       STORED IN ELEMENT HEADER                   
         STC   RF,GMSGELL                                                       
         B     YES                                                              
         XIT1                                                                   
         DROP  R3                                                               
*                                                                               
YES      SR    RC,RC               RETURN CC EQUAL                              
NO       LTR   RC,RC               RETURN CC NOT EQUAL                          
XIT      XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* CONVERT DATA DICTIONARY TYPE COMPLEX MESSAGE DATA FOR DISPLAY      *          
* ON ENTRY FORMAT OF DATA IN ELEMENT IS:                             *          
* <STRING COUNT><LIST(<STRING LENGTH>)><LIST(<STRING TEXT>)>         *          
* AND VALUES ON PARAMETER LIST ARE:                                  *          
* 0(R1)=A(ELEMENT DATA) 4(R1)=A(DISPLAY FIELD) 8(R1)=MAX FIELD LENGTH*          
* ALSO DEAL WITH IMBEDDED DELIMITER CHARACTER AND SPLIT STRINGS      *          
* AND MARK TRUNCATED STRINGS                                         *          
**********************************************************************          
         SPACE 1                                                                
DISPIC   NTR1                                                                   
         LM    R3,R5,0(R1)         GET VALUES FROM PARAMETER LIST               
         MVI   0(R4),C'>'          POINTER TO DISPLAY OUTPUT FIELD              
         LA    R4,1(R4)                                                         
         ZIC   R0,0(R3)            SAVE STRING COUNT                            
         LA    R3,1(R3)            POINT TO STRING LENGTH LIST                  
         LR    R1,R3                                                            
         AR    R1,R0               POINT TO FIRST STRING TEXT                   
*                                  PROCESS EACH STRING IN LIST                  
DICLP1   MVC   APFLAG,0(R3)        EXTRACT STRING LENGTH                        
         ZIC   RF,APFLAG                                                        
         SR    RE,RE               INDICATE NOT SPLITTER FORMAT                 
         TM    APFLAG,X'80'        CHECK IF SPLITTER FORMAT                     
         BZ    DICLP2                                                           
         NI    APFLAG,X'FF'-X'80'                                               
         ZIC   RF,APFLAG                                                        
         SRL   RF,1                                                             
         LR    RE,RF               SAVE SPLITTER HALF LENGTH                    
*                                  PROCESS EACH CHARACTER IN STRING             
DICLP2   MVC   0(1,R4),0(R1)                                                    
         CLI   0(R4),C'>'          CHECK IF DELIMITER CHARCATER                 
         BNE   DICLP2A                                                          
         BCT   R5,*+8              CHECK MAX LENGTH NOT EXCEEDED                
         B     DICLP1A                                                          
         LA    R4,1(R4)                                                         
         MVI   0(R4),C'>'          SHOW TWICE AS '>>'                           
DICLP2A  BCT   R5,*+8                                                           
         B     DICLP1A                                                          
         LA    R4,1(R4)            BUMP POINTERS                                
         LA    R1,1(R1)                                                         
         BCT   RF,DICLP2           DO NWEXT CHARACTER                           
         LTR   RE,RE               CHECK IF SPLITTER STRING                     
         BZ    DICLP2X                                                          
         LR    RF,RE               IF SO ONLY HALF DONE SO FAR                  
         SR    RE,RE               CLEAR INDICATOR AND DO REST                  
         MVI   0(R4),C':'            OUTPUTING SPLITTER CHARACTER               
         BCT   R5,*+8                                                           
         B     DICLP1A                                                          
         LA    R4,1(R4)                                                         
         B     DICLP2                                                           
*                                                                               
DICLP2X  MVI   0(R4),C'>'          OUTPUT DELIMITER AT END OF STRING            
         BCT   R5,*+8                                                           
         B     DICLP1A                                                          
         LA    R4,1(R4)            BUMP POINTERS                                
         LA    R3,1(R3)                                                         
         BCT   R0,DICLP1           DO NEXT STRING TO END OF FIELD               
         B     DICLP1X                                                          
*                                                                               
DICLP1A  MVI   0(R4),C'>'          MARK TRUNCATED STRING AT FIELD END           
         B     DICLP1X                                                          
*                                                                               
DICLP1X  XIT1  EXIT ROUTINE                                                     
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
FICNOMSG DC    C'- Invalid Data Dictionary syntax'                              
MSGINST  DC    C'ERROR - Must define SYSTEM && TYPE first'                      
SPACES   DC    CL80' '                                                          
         SPACE 1                                                                
PROGTAB  EQU   *                   EXTRA VALID PROGRAM ENTRIES                  
         DC    CL8'OFFLINE'                                                     
         DC    CL8'ALL'                                                         
         DC    H'0'                                                             
         EJECT                                                                  
SYSTAB   EQU   *                                                                
*&&UK                                                                           
*                0123456789ABCDEF                                               
         DC    C'GD23MPA789CZ??I?' 00-0F                                        
         DC    C'?????P??????????' 10-1F                                        
         DC    C'????????????????' 20-2F                                        
         DC    C'????????????????' 30-3F                                        
         DC    C'????????????????' 40-4F                                        
         DC    C'????????????????' 50-5F                                        
         DC    C'????????????????' 60-6F                                        
         DC    C'????????????????' 70-7F                                        
         DC    C'????????????????' 80-8F                                        
         DC    C'????????????????' 90-9F                                        
         DC    C'????????????????' A0-AF                                        
         DC    C'????????????????' B0-BF                                        
         DC    C'????????????????' C0-CF                                        
         DC    C'????????????????' D0-DF                                        
         DC    C'????????????????' E0-EF                                        
         DC    C'????????????????' F0-FF                                        
*&&                                                                             
*&&US                                                                           
*                0123456789ABCDEF                                               
         DC    C'GDSNPLANR9CZWENS' 00-0F                                        
         DC    C'P????LLSNP??????' 10-1F                                        
         DC    C'????????????????' 20-2F                                        
         DC    C'????????????????' 30-3F                                        
         DC    C'??????T?????????' 40-4F                                        
         DC    C'????????????????' 50-5F                                        
         DC    C'????????????????' 60-6F                                        
         DC    C'????????????????' 70-7F                                        
         DC    C'????????????????' 80-8F                                        
         DC    C'????????????????' 90-9F                                        
         DC    C'????????????????' A0-AF                                        
         DC    C'????????????????' B0-BF                                        
         DC    C'????????????????' C0-CF                                        
         DC    C'????????????????' D0-DF                                        
         DC    C'????????????????' E0-EF                                        
         DC    C'????????????????' F0-FF                                        
*&&                                                                             
         EJECT                                                                  
REPDESCL DC    C'MESSAGE LIST'                                                  
         SPACE 1                                                                
MTYPTAB  DS    0CL1                                                             
         DC    AL1(GMKTDIC)                                                     
         DC    AL1(GMKTERR)                                                     
         DC    AL1(GMKTINF)                                                     
         DC    AL1(GMKTSCR)                                                     
         DC    AL1(GMKTTXT)                                                     
         DC    AL1(GMKTWRN)                                                     
         DC    AL1(GMKTREP)                                                     
         DC    AL1(GMKTFAC)                                                     
         DC    AL1(EOT)                                                         
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
TXTLEN   DC    Y(MSGTXT2-MSGTXT1)                      L LINE                   
MAXTXTNO EQU   (MSGTXTL-MSGTXT1)/(MSGTXT2-MSGTXT1)+1   N LINES                  
         EJECT                                                                  
* CTGENWRK                                                                      
       ++INCLUDE CTGENWRK                                                       
         EJECT                                                                  
* GEGENMSG                                                                      
       ++INCLUDE GEGENMSG                                                       
         EJECT                                                                  
TWAD     DSECT                                                                  
         ORG   GENTABH                                                          
       ++INCLUDE CTGENFCD                                              0        
         ORG   GENTABH                                                          
       ++INCLUDE CTGENDCD                                                       
         ORG   GENTABH                                                          
       ++INCLUDE CTGENBCD                                                       
         ORG                                                                    
         EJECT                                                                  
LISTD    DSECT                     ** LIST/SELECT LINE LAYOUT **                
LISTACTH DS    XL8                                                              
LISTACT  DS    CL3                 ACTION FIELD                                 
LISTLINH DS    CL8                                                              
LISTLIN  DS    0CL(L'LSTLIN1)                                                   
LISTSYS  DS    CL3                                                              
         DS    CL1                                                              
LISTLANG DS    CL3                 LANGUAGE NAME                                
         DS    CL1                                                              
LISTMNO  DS    CL8                 MESSAGE REFERENCE                            
         DS    CL2                                                              
LISTMSG  DS    CL(L'LSTLIN1-(LISTMSG-LISTLIN)-L'LISTXTND)                       
LISTXTND DS    CL1                 EXTENDED MSG Y/N                             
         ORG   LISTLIN+L'LISTLIN                                                
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
SELKEY   DS    0XL32                                                            
SELSYS   DS    XL1                 MESSAGE SYSTEM                               
SELTYPE  DS    XL1                 MESSAGE TYPE                                 
SELLNG   DS    XL1                 MESSAGE LANGUAGE                             
SELMSG   DS    XL2                 MESSAGE NUMBER TO START                      
         ORG   SELKEY+L'SELKEY                                                  
LASTSYS  DS    XL1                 CONTROL TOF ON CHANGE OF SYSTEM              
LOCALX   EQU   *                                                                
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006CTGEN03S  05/01/02'                                      
         END                                                                    
