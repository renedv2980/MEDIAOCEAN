*          DATA SET GEPAK02    AT LEVEL 005 AS OF 12/08/09                      
*PHASE TF3602A,*                                                                
**NOTE: PHASE=TF35 IN UK, PHASE=TF36 IN US                                      
*                                                                               
*&&      SET   NOP=N               COMMENTED OUT CODE                           
         TITLE 'GEPAK02 - PCPAK DATA FILE UPLOAD'                               
         SPACE 1                                                                
*WHO  LVL DATE    CHANGE                                                        
         EJECT                                                                  
PAK02    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**PK02**,R9,RR=RE                                              
         L     RC,0(R1)            RC=A(GENCON GLOBAL STORAGE)                  
         USING GEND,RC                                                          
         L     R7,ASYSD                                                         
         USING SYSWORKD,R7                                                      
         L     RA,ATWA                                                          
         USING PAKFFD,RA                                                        
         L     R3,S#ATSBLK         R3=A(TSAR BUFFER)                            
         USING TSARD,R3                                                         
         ST    RE,SYOVRELO                                                      
         MVI   SYFLID,GENDIRQ                                                   
         OI    SYOVCALL,SYOVYESQ                                                
         EJECT                                                                  
***********************************************************************         
* DETERMINE MODE                                                      *         
***********************************************************************         
         SPACE 1                                                                
MODECHK  CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VKEY                                                             
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VREC                                                             
         SPACE 2                                                                
* GENERAL EXIT POINTS                                                           
*                                                                               
EXITBAD  LTR   RB,RB                                                            
         B     *+6                                                              
EXITOK   CR    RB,RB                                                            
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* VALIDATE KEY                                                        *         
***********************************************************************         
         SPACE 1                                                                
VKEY     CLI   TWASYSID,NULLQ      SKIP 1ST TIME IN OR SAME KEY AS LAST         
         BE    VKEYX                                                            
         TM    SCRKEYH+FHIID,FHIITH                                             
         BZ    VKEYX                                                            
         LA    R2,SCRKEYH                                                       
         GOTO1 ANY                                                              
         XC    SVKEY,SVKEY         SET INITIAL KEY                              
         LA    R4,SVKEY                                                         
         USING GPAKD,R4                                                         
         MVI   GPKREC,GPKRECQ                                                   
         MVC   GPKSYSLX,TWASYSLX   SYSTEM/LANGUAGE                              
         MVC   GPKFILE,WORK                                                     
         DROP  R4                                                               
*                                                                               
         SR    R1,R1               BUMP N'FILES PROCESSED                       
         ICM   R1,3,TWANFLS                                                     
         AHI   R1,1                                                             
         STCM  R1,3,TWANFLS                                                     
*                                                                               
         XC    TWALFLDS,TWALFLDS   CLEAR LINE RELATED FIELDS                    
         L     R0,S#ASVP2          CLEAR LINE BUFFER                            
         LHI   R1,TWAXTLNQ                                                      
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         SPACE 2                                                                
*                                  INITIALISE TSAR BUFFER                       
VK200    MVC   TSACOM,S#ACFACS     SET A(COMFACS)                               
         MVI   TSKEYL,TXRKEYLQ     SET KEY LENGTH                               
         MVC   TSRECL,=Y(TXRMAXLQ) SET RECORD LENGTH                            
         MVC   TSPAGN,=AL1(TSNMAX*2)   ASK FOR MAX PAGES (2 BUFFERS)            
         MVI   TSRECI,TSRVAR       VARIABLE LENGTH                              
         MVI   TSIND2,TSI2BIGN     USE 2 BUFFERS AS A BIG ONE                   
         MVI   TSACTN,TSAINI       SET INITIALISE                               
         MVI   TSINDS,TSIALLOC     SET TO ALLOCATE FROM TEMPEST                 
         OI    TSINDS,TSIXTTWA     SET TO ALLOCATE FROM TEMPEST                 
         CLI   TWAPAGL,0                                                        
         BE    VK210               VERY FIRST TIME IN                           
         OI    TSINDS,TSIREUSE     SET TO REUSE C/I                             
         MVC   TSPAGL,TWAPAGL      SET LOW PAGE NUMBER                          
         MVC   TSPAGN,TWAPAGN      SET NUMBER OF PAGES ALLOCATED                
VK210    GOTO1 VTSAR,TSARD                                                      
         BE    VK220                                                            
         CLI   TSERRS,TSEEOF                                                    
         BE    VKERR02                                                          
         DC    H'0'                (SEE TSERRS FOR DETAILS)                     
VK220    MVC   TWAPAGL,TSPAGL      SAVE LOW PAGE NUMBER                         
         MVC   TWAPAGN,TSPAGN      SAVE NUMBER OF PAGES ALLOCATED               
         SPACE 2                                                                
VKEYX    B     EXIT                                                             
         SPACE 1                                                                
VKERR01  MVI   ERROR,INVSYSQ       INVALID SYSTEM                               
         B     TRAPERR                                                          
VKERR02  MVI   ERROR,ALLOCQ        TSAR ALLOCATION FAILURE                      
         B     TRAPERR                                                          
         EJECT                                                                  
***********************************************************************         
* VALIDATE RECORD                                                     *         
***********************************************************************         
         SPACE 1                                                                
VREC     CLI   TWASYSID,NULLQ      DEFINE SYSTEM/LANG 1ST PASS                  
         BNE   VR020                                                            
         SPACE 1                                                                
* KEY CONTAINS UPLOAD/SYS/LANG. MAY HAVE /REBUILD OR OVERRIDE TAGGED ON         
*                                                                               
         XC    TWAFLDS,TWAFLDS                                                  
         LA    R2,SCRKEYH                                                       
         LA    R0,SCANWRK          CLEAR SCANNER BLOCK                          
         LHI   R1,SCANWKLQ                                                      
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         GOTO1 SCANNER,DMCB,('SCXTRA2Q',(R2)),SCANWRK,C',=/='                   
         CLI   4(R1),3                                                          
         BE    VR002                                                            
         CLI   4(R1),4                                                          
         BNE   VRERR01A           KEY FIELD ERROR 1                             
         MVI   TWAPARM4,REBUILDQ  4TH PARAM IS PRESENT (ASSUME REBUILD)         
         SPACE 1                                                                
* GET SYSTEM NAME                                                               
*                                                                               
VR002    LA    R2,SCANWRK+SCBLKLQ+SCXTRAQ    SKIP 1ST ENTRY                     
         USING SCANBLKD,R2                                                      
         CLI   SC2NDLEN,NULLQ                                                   
         BNE   VRERR01B                                                         
         CLI   SC1STLEN,2                                                       
         BL    VRERR01C                                                         
         SR    R1,R1                                                            
         IC    R1,SC1STLEN                                                      
         BCTR  R1,0                                                             
         L     RE,S#ASYSL                                                       
         LA    RE,6(RE)            RE=A(SYSTEM LIST)                            
         USING SYSLSTD,RE                                                       
*                                                                               
VR004    CLI   0(RE),NULLQ         TEST EOT                                     
         BE    VRERR02             INVALID SYSTEM                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SYSLNAME(0),SC1STFLD                                             
         BE    VR006                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SYSLSHRT(0),SC1STFLD                                             
         BE    VR006                                                            
         LA    RE,SYSLLEN(RE)                                                   
         B     VR004                                                            
VR006    MVC   TWASYSID,SYSLNUM                                                 
*&&US                                                                           
         CLI   TWASYSID,X'0D'      SYSTEM=STR                                   
         BNE   *+8                 NO                                           
         MVI   TWASYSID,X'02'      YES - OVERRIDE STR W/ SPT                    
*&&                                                                             
         DROP  RE                                                               
         SPACE 1                                                                
* GET LANGUAGE EXTENSION                                                        
*                                                                               
         LA    R2,SCBLKLQ+SCXTRAQ(R2)        NEXT ENTRY                         
         CLI   SC2NDLEN,NULLQ                                                   
         BNE   VRERR01D                                                         
         CLI   SC1STLEN,2                                                       
         BL    VRERR01E                                                         
         SR    R1,R1                                                            
         IC    R1,SC1STLEN                                                      
         BCTR  R1,0                                                             
         L     RE,VLANGEXT                                                      
         USING LANGEXTD,RE                                                      
*                                                                               
VR008    CLI   LANGCODE,EOTQ                                                    
         BE    VRERR03             INVALID LANGUAGE                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   LANGEXT(0),SC1STFLD                                              
         BE    VR010                                                            
         LA    RE,LANGELQ(RE)                                                   
         B     VR008                                                            
*                                                                               
VR010    MVC   TWALEXT,LANGCODE                                                 
         DROP  RE                                                               
         SPACE 1                                                                
* TEST FOR REBUILD/OVERRIDE REQUEST                                             
*                                                                               
         CLI   TWAPARM4,REBUILDQ   I.E. 4 FIELDS SCANNED                        
         BNE   VR014                                                            
         LA    R2,SCBLKLQ+SCXTRAQ(R2)        NEXT ENTRY                         
         CLI   SC2NDLEN,NULLQ                                                   
         BNE   VRERR01F                                                         
         CLI   SC1STLEN,REBTXTLQ                                                
         BNE   VR012                                                            
         CLC   SC1STFLD(REBTXTLQ),REBTXT                                        
         BNE   VRERR01G                                                         
         B     VR014                                                            
         SPACE 1                                                                
VR012    CLI   SC1STLEN,OVRTXTLQ                                                
         BNE   VRERR01H                                                         
         CLC   SC1STFLD(OVRTXTLQ),OVRTXT                                        
         BNE   VRERR01I                                                         
         MVI   TWAPARM4,OVERRIDQ                                                
         SPACE 1                                                                
*                                  SET DATE/TIME FOR VERSION STAMP              
*                                                                               
VR014    GOTO1 DATCON,DMCB,(5,0),(2,TWADATE)                                    
*                                                                               
         THMS                      GET TIME 0HHMMSS+                            
         ST    R1,FULL                                                          
         ZAP   DUB,FULL                                                         
         SRP   DUB,64-2,0          REMOVE SECONDS                               
         CVB   R1,DUB                                                           
         STCM  R1,3,TWATIME                                                     
         SPACE 1                                                                
         BAS   RE,LOCKSYS          LOCK RELEVANT SYSTEM                         
         BNE   VRERR04                                                          
*                                                                               
         CLI   TWAPARM4,REBUILDQ                                                
         BNE   VR016                                                            
         BAS   RE,DELSYS           DELETE ALL FILES FOR SYSTEM                  
*                                                                               
VR016    MVC   DSPWORK,SPACEPAD                                                 
         GOTO1 GENDSP,SCRKEYH                                                   
         B     VRECX                                                            
         SPACE 2                                                                
VR020    TM    SYINPIND,PFKQ       END OF UPLOAD ON PF4                         
         BZ    VR022                                                            
         CLI   S#PFKEY,PF04Q                                                    
         BE    VR100                                                            
         SPACE 1                                                                
* BUILD TSAR BUFFER OF PC FILE LINES                                            
*                                                                               
VR022    L     R4,AIO              SET A(IO AREA) - MAY SPAN ALL 3              
         USING TXRECD,R4                                                        
         MVC   TSAREC,AIO          NB TSAR REC MAY BE UP TO 4K                  
         OC    TWALFLDS,TWALFLDS   SKIP IF INITIAL CALL FOR NEW FILE            
         BZ    VR030                                                            
*                                  RESTORE TSAR BUFFER                          
         MVC   TSACOM,S#ACFACS     SET A(COMFACS)                               
         MVI   TSKEYL,TXRKEYLQ     SET KEY LENGTH                               
         MVC   TSRECL,=Y(TXRMAXLQ) SET RECORD LENGTH                            
         MVI   TSRECI,TSRVAR       VARIABLE LENGTH                              
         MVI   TSIND2,TSI2BIGN     USE 2 BUFFERS AS A BIG ONE                   
         MVI   TSACTN,TSARES       SET RESTORE                                  
         MVI   TSINDS,TSIALLOC     SET TO ALLOCATE FROM TEMPEST                 
         OI    TSINDS,TSIXTTWA     SET TO ALLOCATE FROM TEMPEST                 
         MVC   TSPAGL,TWAPAGL      SET LOW PAGE NUMBER                          
         MVC   TSPAGN,TWAPAGN      SET NUMBER OF PAGES ALLOCATED                
         GOTO1 VTSAR,TSARD                                                      
         BE    VR030                                                            
         CLI   TSERRS,TSEEOF                                                    
         BE    VRERR05                                                          
         DC    H'0'                (SEE TSERRS FOR DETAILS)                     
         SPACE 1                                                                
*                                  EXTRACT DATA LINES FROM SCREEN               
VR030    SR    R1,R1                                                            
         ICM   R1,3,TWASCRS        BUMP N'SCREENS                               
         AHI   R1,1                                                             
         STCM  R1,3,TWASCRS                                                     
*                                                                               
         LA    R2,SCRL03H                                                       
         LA    R0,(SCRL24H-SCRL03H)/(SCRL04H-SCRL03H)+1                         
*                                                                               
         L     R6,S#ASVP2          CURRENT LINE BUILT IN SVPAGE2                
         AH    R6,TWALDISP                                                      
*                                                                               
VR040    CLI   FHILD(R2),NULLQ                                                  
         BE    VR050                                                            
         SR    R1,R1                                                            
         IC    R1,FHILD(R2)                                                     
         SRL   R1,1                PROCESSING 2 CHRS AT A TIME                  
         LA    R5,FHDAD(R2)                                                     
*                                                                               
VR044    CLC   0(2,R5),EOLTXT      TEST FOR END OF PC FILE LINE                 
         BE    VR046                                                            
         MVC   0(2,R6),0(R5)                                                    
         LA    R5,2(R5)                                                         
         LA    R6,2(R6)                                                         
         BCT   R1,VR044                                                         
         B     VR050               END OF CURRENT SCREEN LINE                   
*                                                                               
VR046    ST    R1,SAVER1                                                        
         L     R1,S#ASVP2                                                       
         SR    R6,R1               R6=L'CURRENT LINE                            
         BNZ   *+6                                                              
         DC    H'0'                SHOULDN'T BE GETTING NULL LINES              
         GOTO1 HEXIN,DMCB,S#ASVP2,TXRDATA,(R6),0                                
         ICM   R1,15,12(R1)                                                     
         BZ    VRERR06                                                          
         AHI   R1,TXRFXDLQ                                                      
         STCM  R1,3,TXRLEN                                                      
         ICM   R1,3,TWALNO                                                      
         LA    R1,1(R1)                                                         
         STCM  R1,3,TWALNO                                                      
         STCM  R1,3,TXRLNUM                                                     
*                                                                               
         MVI   TSACTN,TSAADD                                                    
         GOTO1 VTSAR,TSARD                                                      
         CLI   TSERRS,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VR048    XC    TWALDISP,TWALDISP                                                
         L     R6,S#ASVP2                                                       
         LA    R5,2(R5)            BUMP OVER DELIM                              
         L     R1,SAVER1                                                        
         BCT   R1,VR044                                                         
*                                                                               
VR050    LA    R2,(SCRL04H-SCRL03H)(R2)                                         
         BCT   R0,VR040                                                         
*                                                                               
         S     R6,S#ASVP2          SAVE DISPLACEMENT INTO CURRENT LINE          
         STCM  R6,3,TWALDISP                                                    
*                                                                               
         MVI   TSACTN,TSASAV       SET SAVE                                     
         MVC   TSPAGL,TWAPAGL      SET LOW TSAR PAGE NUMBER                     
         MVC   TSPAGN,TWAPAGN      SET NUMBER OF PAGES ALLOCATED                
         GOTO1 VTSAR,TSARD                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R4                                                               
         SPACE 2                                                                
         TM    SYINPIND,PFKQ       END OF FILE ON PF3                           
         BZ    VRECX                                                            
         CLI   S#PFKEY,PF03Q                                                    
         BNE   VRECX                                                            
         SPACE 1                                                                
         OC    TWALNO,TWALNO       ENSURE FILE NOT EMPTY                        
         BZ    VRERR07                                                          
         SPACE 1                                                                
         MVC   HALF,=AL2(IFILUNCQ) ASSUME FILE UNCHANGED                        
*                                                                               
         BAS   RE,COMPARE          COMPARE PC FILE WITH CURRENT VERSION         
         BE    VRECOKX                                                          
         SPACE 1                                                                
         BAS   RE,REPLACE          REPLACE FILE                                 
*                                                                               
         MVC   HALF,=AL2(IFILUPDQ) SET CHANGED/ADDED MESSAGE                    
         CLI   UPDFLAG,NEWFILEQ                                                 
         BNE   VRECOKX                                                          
         MVC   HALF,=AL2(IFILNEWQ) FILE ADDED                                   
         B     VRECOKX                                                          
         SPACE 2                                                                
* END OF UPLOAD ON PF4                                                          
*                                                                               
VR100    BAS   RE,UNLKSYS          UNLOCK SYSTEM                                
*                                  RETURN VERSION (MAY BE UNCHANGED)            
         MVC   DSPWORK(VERTXTLQ),VERTXT                                         
         LA    RF,DSPWORK+VERTXTLQ                                              
         GOTO1 DATCON,DMCB,(2,TWADATE),(20,(RF))                                
         LA    RF,DSPWORK+VERTXTLQ+8                                            
         SR    R1,R1                                                            
         ICM   R1,3,TWATIME                                                     
         CVD   R1,DUB                                                           
         UNPK  0(4,RF),DUB                                                      
         OI    3(RF),ZEROQ                                                      
         GOTO1 GENDSP,SCRKEYH                                                   
         MVC   HALF,=AL2(IUPLENDQ) END OF UPLOAD                                
         B     VRECOKX                                                          
         SPACE 2                                                                
VRECOKX  XC    GETTXTCB,GETTXTCB   SET OK MESSAGE                               
         LA    R1,GETTXTCB                                                      
         USING GETTXTD,R1                                                       
         MVC   GTMSGNO,HALF                                                     
         MVI   GTMSYS,GTGENSYS                                                  
         MVI   GTMTYP,GTMINF                                                    
         DROP  R1                                                               
         OI    GENSTAT2,USMYOK+USGETTXT                                         
         SPACE 1                                                                
VRECX    TWAXC SCRL03H,SCRL24H                                                  
         B     EXIT                                                             
         SPACE 2                                                                
VRERRORS EQU   *                                                                
VRERR01A MVI   SYFLDIX,(*-VRERRORS)/8+1      ERR01A-Y = KEY ERRORS 1-25         
         B     VRERR01Z                                                         
VRERR01B MVI   SYFLDIX,(*-VRERRORS)/8+1                                         
         B     VRERR01Z                                                         
VRERR01C MVI   SYFLDIX,(*-VRERRORS)/8+1                                         
         B     VRERR01Z                                                         
VRERR01D MVI   SYFLDIX,(*-VRERRORS)/8+1                                         
         B     VRERR01Z                                                         
VRERR01E MVI   SYFLDIX,(*-VRERRORS)/8+1                                         
         B     VRERR01Z                                                         
VRERR01F MVI   SYFLDIX,(*-VRERRORS)/8+1                                         
         B     VRERR01Z                                                         
VRERR01G MVI   SYFLDIX,(*-VRERRORS)/8+1                                         
         B     VRERR01Z                                                         
VRERR01H MVI   SYFLDIX,(*-VRERRORS)/8+1                                         
         B     VRERR01Z                                                         
VRERR01I MVI   SYFLDIX,(*-VRERRORS)/8+1                                         
         B     VRERR01Z                                                         
         SPACE 1                                                                
VRERR01Z MVI   ERROR,KEYERRQ       KEY FIELD ERROR &I                           
         MVI   TWASYSID,NULLQ      FORCE REVALIDATION                           
         B     TRAPERR                                                          
*                                                                               
VRERR02  MVI   ERROR,INVSYSQ       INVALID SYSTEM                               
         B     TRAPERR                                                          
VRERR03  MVI   ERROR,INVLANGQ      INVALID LANGUAGE                             
         MVI   TWASYSID,NULLQ      FORCE REVALIDATION                           
         B     TRAPERR                                                          
VRERR04  MVI   ERROR,SYSLOCKQ      SYSTEM LOCKED FOR UPDATE                     
         MVI   TWASYSID,NULLQ      FORCE REVALIDATION                           
         B     TRAPERR                                                          
VRERR05  MVI   ERROR,ALLOCQ        TSAR RESTORE FAILED                          
         B     TRAPERR                                                          
VRERR06  MVI   ERROR,NOTHEX        INVALID CHARACTERS                           
         B     TRAPERR                                                          
VRERR07  MVI   ERROR,NODATAQ       PC FILE IS EMPTY (S/B TRAPPED ON PC)         
         B     TRAPERR                                                          
         EJECT                                                                  
***********************************************************************         
* GET CONTROL RECORD AND SET RELEVANT SYSTEM TO LOCKED                *         
***********************************************************************         
         SPACE 1                                                                
LOCKSYS  NTR1  ,                                                                
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING GPAKD,R4                                                         
         MVI   GPKREC,GPKRECQ                                                   
         OI    SYERRFLG,RETNERRQ                                                
         OI    SYIOFLG,RETDELQ                                                  
         MVI   BYTE,NULLQ                                                       
         GOTO1 VREAD                                                            
         BE    LCK010                                                           
         SPACE 1                                                                
* THIS SHOULD BE A FATAL ERROR ONCE PROGRAM IS LIVE                             
*                                                                               
         MVI   BYTE,NOQ            FLAG ADDREC REQUIRED                         
         MVC   KEY,KEYSAVE         RESTORE CONTROL REC KEY                      
         L     R4,AIO                                                           
         MVC   GPKEY,KEY                                                        
         XC    GPFST,GPFST         CLEAR ENTIRE STATUS AREA                     
         MVC   GPFLEN,=Y(GPFIRSTQ+1)        DEFINE INITIAL LENGTH               
         LA    R0,GPFIRSTQ(R4)     CLEAR ANY EXISTING DATA                      
         LHI   R1,(IOLNQ-GPFIRSTQ)                                              
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         B     LCK030                                                           
         SPACE 1                                                                
LCK010   OI    SYIOFLG,RETDELQ                                                  
         GOTO1 VGETREC                                                          
         XC    SYAELEM,SYAELEM     GET RELEVANT CONTROL ELEM                    
         MVI   ELCODE,GPKCELQ                                                   
*                                                                               
LCK020   GOTO1 VGETEL,AIO                                                       
         BNE   LCK030                                                           
         ICM   R1,15,SYAELEM       UPDATE CONTROL ELEMENT                       
         USING GPKCD,R1                                                         
         CLC   TWASYSLX,GPKCSYS    TEST SYSTEM/LANGEXT                          
         BNE   LCK020                                                           
         SPACE 1                                                                
         CLI   TWAPARM4,OVERRIDQ                                                
         BE    *+12                                                             
         TM    GPKCFLAG,GPKCLCKQ   TEST IF ALREADY LOCKED FOR UPDATE            
         BNZ   LCKERRX                                                          
         OI    GPKCFLAG,GPKCLCKQ   LOCK CONTROL ELEMENT                         
         B     LCK040                                                           
         SPACE 1                                                                
LCK030   XC    ELEM,ELEM           CREATE NEW CONTROL ELEMENT                   
         LA    R1,ELEM                                                          
         MVI   GPKCEL,GPKCELQ                                                   
         MVI   GPKCELL,GPKCLNQ                                                  
         MVC   GPKCSYS(L'GPKSYSLX),TWASYSLX                                     
         OI    GPKCFLAG,GPKCLCKQ                                                
         MVC   GPKCVER,TWAVER                                                   
         GOTO1 ADDELEM                                                          
*                                                                               
LCK040   CLI   BYTE,NULLQ                                                       
         BE    LCK042                                                           
         GOTO1 VADDREC                                                          
         B     LCKOKX                                                           
*                                                                               
LCK042   L     R1,AIO                                                           
         NI    GPFSTAT-GPAKD(R1),X'FF'-GPKDELQ                                  
         GOTO1 VPUTREC                                                          
         TM    KEY+(GPDSTAT-GPAKD),GPKDELQ                                      
         BZ    LCKOKX                                                           
         NI    KEY+(GPDSTAT-GPAKD),X'FF'-GPKDELQ                                
         GOTO1 VPUTDIR                                                          
         SPACE 1                                                                
LCKOKX   CR    RB,RB                                                            
         B     *+6                                                              
LCKERRX  LTR   RB,RB                                                            
*                                                                               
LCKEXIT  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GET CONTROL RECORD AND SET RELEVANT SYSTEM TO UNLOCKED              *         
***********************************************************************         
         SPACE 1                                                                
UNLKSYS  NTR1  ,                                                                
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING GPAKD,R4                                                         
         MVI   GPKREC,GPKRECQ                                                   
         OI    SYERRFLG,RETNERRQ                                                
         MVI   BYTE,NULLQ                                                       
         GOTO1 VREAD                                                            
         BE    *+6                 MUST EXIST                                   
         DC    H'0'                                                             
         SPACE 1                                                                
         GOTO1 VGETREC                                                          
         XC    SYAELEM,SYAELEM     GET RELEVANT CONTROL ELEM                    
         MVI   ELCODE,GPKCELQ                                                   
*                                                                               
UNLK020  GOTO1 VGETEL,AIO                                                       
         BE    *+6                                                              
         DC    H'0'                MUST EXIST                                   
         ICM   R1,15,SYAELEM       UPDATE CONTROL ELEMENT                       
         USING GPKCD,R1                                                         
         CLC   TWASYSLX,GPKCSYS    TEST SYSTEM/LANGEXT                          
         BNE   UNLK020                                                          
         SPACE 1                                                                
         NI    GPKCFLAG,X'FF'-GPKCLCKQ       UNLOCK SYSTEM                      
         OC    TWANADD(L'TWANADD+L'TWANCHA),TWANADD                             
         BZ    *+14                                                             
         MVC   GPKCVER,TWAVER      UPDATE VERSION IF ANY CHANGES                
         B     *+10                                                             
         MVC   TWAVER,GPKCVER      RETURN CURRENT VERSION NO                    
         GOTO1 VPUTREC                                                          
         SPACE 1                                                                
UNLKSYSX B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DELETE ALL RECORDS FOR SYSTEM/LANGUAGE                              *         
***********************************************************************         
*                                                                               
DELSYS   NTR1  ,                                                                
         GOTO1 GETFACT,DMCB,(X'80',DUB),F#MIOST        UNSET MAXIO TRAP         
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING GPAKD,R4                                                         
         MVI   GPKREC,GPKRECQ                                                   
         MVC   GPKSYSLX,TWASYSLX   SYSTEM/LANGUAGE                              
         OI    SYERRFLG,RETNERRQ                                                
         OI    SYIOFLG,RETDELQ                                                  
         GOTO1 HIGH                                                             
         B     DEL030                                                           
         SPACE 1                                                                
DEL020   OI    SYERRFLG,RETNERRQ                                                
         OI    SYIOFLG,RETDELQ                                                  
         GOTO1 VSEQ                                                             
*                                                                               
DEL030   CLC   GPKEY((GPKLEXT-GPKEY)+L'GPKLEXT),KEYSAVE                         
         BNE   DEL040              END OF SYSTEM/LANGUAGE                       
         TM    GPDSTAT,GPKDELQ                                                  
         BNZ   DEL020                                                           
         OI    GPDSTAT,GPKDELQ                                                  
         GOTO1 VPUTDIR                                                          
         B     DEL020                                                           
         SPACE 1                                                                
DEL040   XC    KEY,KEY             DELETE ALL PASSIVES                          
         USING GPVRD,R4                                                         
         MVI   GPVRREC,GPVRRECQ                                                 
         MVC   GPVRSYS(L'GPKSYSLX),TWASYSLX   SYSTEM/LANGUAGE                   
         OI    SYERRFLG,RETNERRQ                                                
         OI    SYIOFLG,RETDELQ                                                  
         GOTO1 HIGH                                                             
         B     DEL060                                                           
         SPACE 1                                                                
DEL050   OI    SYERRFLG,RETNERRQ                                                
         OI    SYIOFLG,RETDELQ                                                  
         GOTO1 VSEQ                                                             
*                                                                               
DEL060   CLC   GPVRKEY((GPVRLEXT-GPVRKEY)+L'GPVRLEXT),KEYSAVE                   
         BNE   DELSYSX             END OF SYSTEM/LANGUAGE                       
         TM    GPVDSTAT,GPKDELQ                                                 
         BNZ   DEL050                                                           
         OI    GPVDSTAT,GPKDELQ                                                 
         GOTO1 VPUTDIR                                                          
         B     DEL050                                                           
         SPACE 1                                                                
DELSYSX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* COMPARE FILE IN TSAR BUFFER WITH CURRENT FILE                       *         
***********************************************************************         
*                                                                               
COMPARE  NTR1  ,                                                                
         CLI   TWAPARM4,REBUILDQ   REBUILD MEANS ALL FILES ARE NEW              
         BE    CMP200                                                           
*                                                                               
         L     R6,S#ASVP2          GET PC LINES BACK HERE                       
         STCM  R6,15,TSAREC                                                     
         MVC   TSRNUM,=AL2(1)                                                   
         MVI   TSACTN,TSAGET                                                    
         USING TXRECD,R6                                                        
*                                                                               
         MVI   UPDFLAG,NULLQ                                                    
         XC    OLDVEREL,OLDVEREL                                                
         L     R5,S#ATIA           BUILD CURRENT MAINFRAME LINE HERE            
*                                                                               
         LA    R4,KEY              SET INITIAL KEY                              
         USING GPAKD,R4                                                         
         MVC   KEY,SVKEY                                                        
         OI    SYERRFLG,RETNERRQ                                                
         GOTO1 VREAD                                                            
         BNE   CMP200              DOESN'T EXIST (OR IS DELETED)                
         B     CMP020                                                           
         SPACE 1                                                                
CMP010   GOTO1 VSEQ                                                             
         CLC   GPKEY((GPKFILE-GPKEY)+L'GPKFILE),KEYSAVE                         
         BNE   CMP100              END OF CURRENT FILE                          
         SPACE 1                                                                
CMP020   GOTO1 VGETREC                                                          
*                                                                               
         CLI   GPKSEQN,NULLQ       GET VERSION INFO ELEM FROM FIRST REC         
         BNE   CMP024                                                           
         XC    SYAELEM,SYAELEM                                                  
         MVI   ELCODE,GPKVELQ                                                   
*                                                                               
         GOTO1 VGETEL,AIO                                                       
         BNE   CMP024                                                           
         L     R1,SYAELEM                                                       
         MVC   OLDVEREL,0(R1)     SAVE OLD VERSION ELEMENT                      
         SPACE 1                                                                
CMP024   MVI   ELCODE,GPKTELQ                                                   
         XC    SYAELEM,SYAELEM                                                  
*                                                                               
CMP030   GOTO1 VGETEL,AIO                                                       
         BNE   CMP010                                                           
         L     R1,SYAELEM                                                       
         USING GPKTD,R1                                                         
CMP040   SR    RE,RE                                                            
         IC    RE,GPKTELL                                                       
         SHI   RE,GPKTFXLQ+1                                                    
         BNM   *+6                                                              
         DC    H'0'                INVALID ELEM LENGTH                          
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),GPKTEXT                                                  
         LA    R5,1(RE,R5)                                                      
         CLI   GPKTLSUB,GPKTFINQ   TEST FINAL ELEMENT FOR LINE                  
         BNE   CMP030                                                           
         DROP  R1                                                               
         SPACE 1                                                                
         GOTO1 VTSAR,TSARD         GET LINE FROM TSAR BUFFER                    
         TM    TSERRS,TSEEOF                                                    
         BNZ   CMP210              NEW FILE HAS LESS LINES THAN CURRENT         
         CLI   TSERRS,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   TSACTN,TSANXT       SET GET NEXT FOR SUBSEQUENT LINES            
         SR    R1,R1                                                            
         ICM   R1,3,TXRLEN                                                      
         SHI   R1,TXRFXDLQ         R1=L'LINE FROM TSAR BUFFER                   
         LA    R0,TXRDATA                                                       
*                                                                               
         LR    RF,R5                                                            
         L     RE,S#ATIA                                                        
         SR    RF,RE               RF=L'CURRENT MAINFRAME LINE                  
         CR    R1,RF               TEST SAME LENGTH                             
         BNE   CMP210                                                           
         CLCL  R0,RE                                                            
         BNE   CMP210                                                           
         L     R5,S#ATIA                                                        
         B     CMP030              BUILD NEXT LINE                              
         SPACE 1                                                                
CMP100   C     R5,S#ATIA           CAN'T HAVE PART BUILT LINE                   
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 VTSAR,TSARD         CHECK END OF TSAR BUFFER                     
         TM    TSERRS,TSEEOF                                                    
         BNZ   COMPAREX            NEWFILE=CURRENT FILE                         
         CLI   TSERRS,0                                                         
         BE    CMP210              NEW FILE HAS MORE LINES THAN CURRENT         
         DC    H'0'                                                             
         SPACE 2                                                                
CMP200   MVI   UPDFLAG,NEWFILEQ    TSAR BUFFER CONTAINS NEW FILE                
         SR    R1,R1               BUMP N' FILES ADDED                          
         ICM   R1,3,TWANADD                                                     
         AHI   R1,1                                                             
         STCM  R1,3,TWANADD                                                     
         B     COMPAREX                                                         
*                                                                               
CMP210   MVI   UPDFLAG,CHAFILEQ    EXISTING FILE HAS CHANGED                    
         SR    R1,R1               BUMP N' FILES CHANGED                        
         ICM   R1,3,TWANCHA                                                     
         AHI   R1,1                                                             
         STCM  R1,3,TWANCHA                                                     
         B     COMPAREX                                                         
         SPACE 1                                                                
COMPAREX CLI   UPDFLAG,NULLQ                                                    
         B     EXIT                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
* REPLACE CURRENT FILE WITH FILE IN TSAR BUFFER                       *         
***********************************************************************         
*                                                                               
REPLACE  NTR1  ,                                                                
         L     R6,S#ASVP2                                                       
         STCM  R6,15,TSAREC                                                     
         MVC   TSRNUM,=AL2(1)                                                   
         MVI   TSACTN,TSAGET                                                    
         USING TXRECD,R6                                                        
         LA    R4,KEY                                                           
*                                                                               
         CLI   TWAPARM4,REBUILDQ   ALREADY DELETED IF REBUILD                   
         BE    REP040                                                           
*                                                                               
         CLI   OLDVEREL,NULLQ      DELETE VERSION STAMP PASSIVE                 
         BE    REP010              SKIP IF NO ELEM                              
         XC    KEY,KEY                                                          
         USING GPVRD,R4                                                         
         LA    R1,SVKEY                                                         
         USING GPAKD,R1                                                         
         MVI   GPVRREC,GPVRRECQ                                                 
         MVC   GPVRSYS(L'GPKSYSLX),GPKSYSLX                                     
         MVC   GPVRFILE,GPKFILE                                                 
         LA    R1,OLDVEREL                                                      
         USING GPKVD,R1                                                         
         MVC   GPVRVER,GPKVVER                                                  
         DROP  R1                                                               
         OI    SYERRFLG,RETNERRQ                                                
         GOTO1 VREAD                                                            
         BNE   REP010                                                           
         OI    GPVDSTAT,GPKDELQ                                                 
         GOTO1 VPUTDIR                                                          
         SPACE 1                                                                
         USING GPAKD,R4                                                         
REP010   MVC   KEY,SVKEY           DELETE EXISTING 'FILE'                       
         OI    SYERRFLG,RETNERRQ                                                
         OI    SYIOFLG,RETDELQ                                                  
         GOTO1 HIGH                                                             
         B     REP030                                                           
         SPACE 1                                                                
REP020   OI    SYERRFLG,RETNERRQ                                                
         OI    SYIOFLG,RETDELQ                                                  
         GOTO1 VSEQ                                                             
*                                                                               
REP030   CLC   GPKEY((GPKFILE-GPKEY)+L'GPKFILE),KEYSAVE                         
         BNE   REP040              END OF CURRENT FILE                          
         TM    GPDSTAT,GPKDELQ                                                  
         BNZ   REP020                                                           
         OI    GPDSTAT,GPKDELQ                                                  
         GOTO1 VPUTDIR                                                          
         B     REP020                                                           
         SPACE 1                                                                
REP040   MVC   KEY,SVKEY           RESET INITIAL KEY                            
         BAS   RE,CHKREC           GET EXISTING REC (IF ANY)                    
         LA    R1,ELEM             DEFINE VERSION ELEMENT                       
         USING GPKVD,R1                                                         
         MVI   GPKVEL,GPKVELQ                                                   
         MVI   GPKVELL,GPKVLNQ                                                  
         MVC   GPKVVER,TWAVER                                                   
         MVC   GPKVSCRS,TWASCRS                                                 
         MVC   GPKVPID,TWAPERID                                                 
         DROP  R1                                                               
         GOTO1 ADDELEM                                                          
         SPACE 1                                                                
         XC    ELEM,ELEM           DEFINE SKELETAL TEXT ELEM                    
         LA    R5,ELEM                                                          
         USING GPKTD,R5                                                         
         MVI   GPKTEL,GPKTELQ                                                   
*                                                                               
REP070   GOTO1 VTSAR,TSARD         GET NEW FILE FROM TSAR BUFFER                
         TM    TSERRS,TSEEOF                                                    
         BNZ   REP200                                                           
         CLI   TSERRS,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   TSACTN,TSANXT       SET GET NEXT FOR SUBSEQUENT LINES            
*                                                                               
         SR    R1,R1               BUMP LINE COUNT                              
         ICM   R1,3,GPKTLIN                                                     
         AHI   R1,1                                                             
         STCM  R1,3,GPKTLIN                                                     
         MVI   GPKTLSUB,NULLQ      SEQUENCE ZERO                                
*                                                                               
         L     R4,AIO                                                           
         LA    R2,TXRDATA                                                       
         SR    R1,R1                                                            
         ICM   R1,3,TXRLEN                                                      
         SHI   R1,TXRFXDLQ         R1=L'LINE FROM TSAR BUFFER                   
         STC   R1,FINALLEN         SAVE INCASE SINGLE ELEM                      
         CHI   R1,GPKTMXLQ         TEST IF FITS SINGLE ELEM                     
         BNH   REP100              ONLY ELEM FOR LINE                           
         SR    R0,R0               CALC N'ELEMS                                 
         D     R0,=A(GPKTMXLQ)                                                  
         LTR   R0,R0               ENSURE A FINAL ELEM EXISTS                   
         BNZ   REP080                                                           
         BCTR  R1,0                LINE IS EXACT MULTIPLE OF GPKTMXLQ           
         LHI   R0,GPKTMXLQ                                                      
*                                                                               
REP080   STC   R0,FINALLEN                                                      
         LR    R0,R1               R0=N'ELEMS FOR LINE (-1)                     
*                                                                               
REP090   MVC   GPKTEXT(GPKTMXLQ),0(R2)                                          
         MVI   GPKTELL,GPKTFXLQ+GPKTMXLQ                                        
*                                                                               
         BAS   RE,ADDTXT           ADD ELEM - MAY CAUSE ADDREC/PUTREC           
*                                                                               
         IC    R1,GPKTLSUB         BUMP SUB LINE NO                             
         AHI   R1,1                                                             
         STC   R1,GPKTLSUB                                                      
         LA    R2,GPKTMXLQ(R2)                                                  
         BCT   R0,REP090                                                        
         SPACE 1                                                                
* HANDLE FINAL (PERHAPS ONLY) ELEM FOR LINE                                     
*                                                                               
REP100   SR    R1,R1                                                            
         IC    R1,FINALLEN                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   GPKTEXT(0),0(R2)                                                 
         AHI   R1,GPKTFXLQ+1                                                    
         STC   R1,GPKTELL                                                       
         MVI   GPKTLSUB,GPKTFINQ   FLAG AS FINAL ELEMENT                        
*                                                                               
         BAS   RE,ADDTXT                                                        
         B     REP070              BACK FOR NEXT LINE                           
         SPACE 2                                                                
* PUT FINAL RECORD FOR FILE                                                     
*                                                                               
REP200   CLI   RECFLAG,EXISTSQ                                                  
         BE    REP210                                                           
         GOTO1 VADDREC                                                          
         MVC   KEY(GPKEYLQ),GPKEY  RESTORE KEY                                  
         B     REP220                                                           
*                                                                               
REP210   GOTO1 VPUTREC                                                          
         GOTO1 VPUTDIR                                                          
*                                                                               
REP220   CLI   GPKSEQN,NULLQ       TEST IF FIRST (ONLY) REC FOR FILE            
         BNE   *+10                                                             
         MVC   SEQN0DA,DMDSKADD    SAVE DISK ADDRESS FOR PASSIVES               
         SPACE 1                                                                
         XC    KEY,KEY             ADD NEW VERSION PASSIVE                      
         LA    R4,KEY                                                           
         USING GPVRD,R4                                                         
         LA    R1,SVKEY                                                         
         USING GPAKD,R1                                                         
         MVI   GPVRREC,GPVRRECQ                                                 
         MVC   GPVRSYS(L'GPKSYSLX),GPKSYSLX                                     
         MVC   GPVRFILE,GPKFILE                                                 
         MVC   GPVRVER,TWAVER                                                   
         MVC   GPVDSCRS,TWASCRS                                                 
         MVC   GPVDDA,SEQN0DA                                                   
         DROP  R1                                                               
         OI    SYERRFLG,RETNERRQ   SHOULD NOT EXIST                             
         OI    SYIOFLG,RETDELQ                                                  
         GOTO1 VREAD               MIGHT IF SAME MINUTE AS PREV UPD             
         BNE   REP230                                                           
         MVC   GPVDDA,SEQN0DA      THIS SHOULD BE THE SAME ANYWAY               
         NI    GPVDSTAT,X'FF'-GPKDELQ                                           
         GOTO1 VPUTDIR                                                          
         B     REPLACEX                                                         
*                                                                               
REP230   MVC   KEY,KEYSAVE         RESTORE PASSIVE POINTER                      
         GOTO1 VADDDIR                                                          
*                                                                               
REPLACEX B     EXIT                                                             
         DROP  R4,R5,R6                                                         
         EJECT                                                                  
**********************************************************************          
* ADD NEW TEXT ELEMENT TO RECORD. ADD RECORD IF IT GETS FULL         *          
**********************************************************************          
         SPACE 1                                                                
         USING GPAKD,R4                                                         
         USING GPKTD,R5                                                         
ADDTXT   NTR1  ,                                                                
         SR    RF,RF               ENSURE ROOM FOR NEW ELEM                     
         ICM   RF,3,GPFLEN                                                      
         SR    RE,RE                                                            
         IC    RE,GPKTELL                                                       
         AR    RF,RE                                                            
         CHI   RF,IOLNQ                                                         
         BNH   ADDT40                                                           
         CLI   RECFLAG,EXISTSQ                                                  
         BE    ADDT10                                                           
         GOTO1 VADDREC                                                          
         MVC   KEY(GPKEYLQ),GPKEY  RESTORE KEY                                  
         B     ADDT20                                                           
*                                                                               
ADDT10   GOTO1 VPUTREC                                                          
         GOTO1 VPUTDIR                                                          
*                                                                               
ADDT20   LA    R4,KEY              UPDATE KEY SEQUENCE NO                       
         SR    R1,R1                                                            
         ICM   R1,1,GPKSEQN        TEST IF FIRST REC FOR FILE                   
         BNZ   ADDT30                                                           
         MVC   SEQN0DA,DMDSKADD    SAVE DISK ADDRESS FOR PASSIVES               
*                                                                               
ADDT30   AHI   R1,1                                                             
         STC   R1,GPKSEQN                                                       
         BAS   RE,CHKREC                                                        
         SPACE 1                                                                
ADDT40   GOTO1 ADDELEM                                                          
*                                                                               
ADDTXTX  B     EXIT                                                             
         SPACE 1                                                                
**********************************************************************          
* CHECK FOR EXISTING RECORD AND CLEAR DOWN IOAREA                    *          
**********************************************************************          
         SPACE 1                                                                
CHKREC   NTR1  ,                                                                
         OI    SYERRFLG,RETNERRQ                                                
         OI    SYIOFLG,RETDELQ                                                  
         MVI   RECFLAG,NULLQ       ASSUME NOT FOUND                             
         L     R4,AIO                                                           
         USING GPAKD,R4                                                         
         GOTO1 VREAD               GET EXISTING REC (IF ANY)                    
         BNE   CHKR010                                                          
         XC    KEY+(GPDST-GPAKD)(L'GPDST),KEY+(GPDST-GPAKD)                     
         MVI   RECFLAG,EXISTSQ                                                  
         OI    SYIOFLG,RETDELQ                                                  
         GOTO1 VGETREC                                                          
         B     CHKR020                                                          
*                                                                               
CHKR010  MVC   KEY,KEYSAVE         RESTORE REQUESTED KEY                        
         MVC   GPKEY,KEY           SET KEY FOR NEW REC                          
*                                                                               
CHKR020  XC    GPFST,GPFST         CLEAR ENTIRE STATUS AREA                     
         MVC   GPFLEN,=Y(GPFIRSTQ+1)        DEFINE INITIAL LENGTH               
         LA    R0,GPFIRSTQ(R4)     CLEAR ANY EXISTING DATA                      
         LHI   R1,(IOLNQ-GPFIRSTQ)                                              
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
CHKRECX  B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* VARIOUS SHORT ROUTINES                                             *          
**********************************************************************          
         SPACE 1                                                                
**********************************************************************          
* MOVE FROM DSPWORK TO TWA AND XMIT IF REQUIRED - CLEARING DSPWORK   *          
**********************************************************************          
         SPACE 1                                                                
GENDSP   ST    RE,SAVERE                                                        
         SR    RE,RE                                                            
         IC    RE,FHLND(R1)                                                     
         SH    RE,=Y(FHDAD+1)                                                   
         TM    FHATD(R1),FHATXH    TEST FOR EXTENDED HEADER                     
         BZ    *+8                                                              
         SH    RE,=Y(FHDAD)                                                     
         EX    RE,GENCLC           IS DATA ON SCREEN ALREADY                    
         BE    GENDSP10            YES- RETURN (CC=)                            
         EX    RE,GENMVC           NO - SO MOVE IT THERE                        
         OI    FHOID(R1),FHOITR                                                 
GENDSP10 EX    RE,GENCLR                                                        
         L     RE,SAVERE                                                        
         BR    RE                  RETURN (CC NOT=)                             
         SPACE 1                                                                
GENCLC   CLC   FHDAD(0,R1),DSPWORK                                              
GENMVC   MVC   FHDAD(0,R1),DSPWORK                                              
GENCLR   MVC   DSPWORK(0),SPACEPAD                                              
         SPACE 2                                                                
TRAPERR  NI    GENSTAT2,X'FF'-USGETTXT                                          
*                                                                               
TRAPERR1 GOTO1 VERRXIT                                                          
         EJECT                                                                  
***********************************************************************         
* LITERAL POOL                                                        *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE GEPAK02TC                                                      
         EJECT                                                                  
***********************************************************************         
* INCLUDED DSECTS                                                     *         
***********************************************************************         
         SPACE 1                                                                
       ++INCLUDE GEPAKDS                                                        
         EJECT                                                                  
PAKFFD   DSECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE GEPAKFED                                                       
         EJECT                                                                  
***********************************************************************         
* LOCALLY DEFINED DSECTS                                              *         
***********************************************************************         
         SPACE 1                                                                
TXRECD   DSECT                     TSAR RECORD (LINE OF TEXT)                   
TXRLEN   DS    XL2                 'RECORD' LENGTH                              
TXRKEY   DS    0XL(TXRKEYLQ)                                                    
TXRLNUM  DS    XL2                 'KEY' (LINE NUMBER)                          
TXRKEYLQ EQU   *-TXRLNUM                                                        
TXRFXDLQ EQU   *-TXRECD                                                         
TXRDATA  DS    XL(4096-TXRFXDLQ)                                                
TXRMAXLQ EQU   *-TXRECD                                                         
         EJECT                                                                  
***********************************************************************         
* REDEFINED OVERLAY WORK AREA                                         *         
***********************************************************************         
         SPACE 1                                                                
SYSWORKD DSECT                                                                  
         ORG   SYOVWORK                                                         
UPDFLAG  DS    XL1                                                              
NEWFILEQ EQU   X'80'                                                            
CHAFILEQ EQU   X'40'                                                            
RECFLAG  DS    XL1                                                              
EXISTSQ  EQU   X'80'                                                            
SYSFLAG  DS    XL1                                                              
OLDVEREL DS    XL(GPKVLNQ)                                                      
FINALLEN DS    XL1                                                              
SEQN0DA  DS    XL(L'GPDDA)                                                      
*                                                                               
         SPACE 1                                                                
***********************************************************************         
* REDEFINED TWA0 SPARE AREA                                           *         
***********************************************************************         
         SPACE 1                                                                
PAKFFD   DSECT                                                                  
         ORG   SCRWORK+128                                                      
         DS    0A                                                               
TWAFLDS  DS    0XL(TWAFLDLQ)                                                    
TWAFSTRT EQU   *                                                                
TWACTL   DS    XL1                 CONTROL FLAG (TESTING)                       
BUILDQ   EQU   X'80'               BUILD BUFFER FOR FILE                        
READQ    EQU   X'40'               READ BUFFER                                  
TWASYSLX DS    0XL(L'GPKSYSLX)                                                  
TWASYSID DS    XL1                 EQUATED SYSTEM NUMBER                        
TWALEXT  DS    XL1                 LANGUAGE FILE EXTENSION CODE                 
         DS    XL1                 N/D (WORD ALIGN)                             
TWAVER   DS    0XL(GPKCVLQ)        VERSION STAMP FOR CURRENT UPDATE             
TWADATE  DS    XL(L'GPKCDATE)                                                   
TWATIME  DS    XL(L'GPKCTIME)                                                   
*                                                                               
TWALFLDS DS    0XL(TWALFLQ)                                                     
TWALNO   DS    XL2                 CURRENT LINE NUMBER                          
TWALDISP DS    XL2                 DISP INTO CURRENT LINE IN SV2PAGE            
TWASCRS  DS    XL2                 N'SCREENS FOR FILE                           
TWALFLQ  EQU   *-TWALNO                                                         
*                                                                               
TWANFLS  DS    XL2                 N'FILES PROCESSED                            
TWANADD  DS    XL2                 N'FILES CHANGED                              
TWANCHA  DS    XL2                 N'FILES ADDED                                
*                                                                               
TWAPARM4 DS    XL1                 PARAMETER 4                                  
REBUILDQ EQU   C'R'                CLEAR EVERYTHING DOWN AND REBUILD            
OVERRIDQ EQU   C'O'                IGNORE LOCKED FOR UPDATE                     
TWAFLDLQ EQU   *-TWAFSTRT                                                       
TWAUSEQ  EQU   *-PAKFFD                                                         
         ORG   PAKFFD+(TWAUSRLQ-TWAUSEQ) ASM ERR IF TWA USAGE > MAX             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005GEPAK02   12/08/09'                                      
         END                                                                    
