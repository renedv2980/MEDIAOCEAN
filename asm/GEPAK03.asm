*          DATA SET GEPAK03    AT LEVEL 005 AS OF 12/08/09                      
*PHASE TF3603A,*                                                                
*NOTE: PHASE=TF35 IN UK, PHASE=TF36 IN US                                       
*                                                                               
*&&      SET   NOP=N               COMMENTED OUT CODE                           
         TITLE 'GEPAK03 - PCPAK DATA FILE DOWNLOAD'                             
         SPACE 1                                                                
*WHO  LVL DATE    CHANGE                                                        
*DCHA 002 06OCT04 OUTPUT VERSION INFO AT START OF DOWNLOAD                      
         EJECT                                                                  
PAK03    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**PK03**,R9,RR=RE                                              
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
         BAS   RE,CHKLOCK          ENSURE SYSTEM STILL NOT LOCKED               
         BNE   VKERR01             SYSTEM LOCKED SINCE LAST TRN                 
*                                                                               
         TM    SCRKEYH+FHIID,FHIITH                                             
         BZ    VKEYX                                                            
         LA    R2,SCRKEYH          KEY IS 0000/FILENAME                         
         GOTO1 ANY                                                              
         XC    SVKEY,SVKEY         SET KEY FOR REQUESTED FILE                   
         LA    R4,SVKEY                                                         
         USING GPAKD,R4                                                         
         MVI   GPKREC,GPKRECQ                                                   
         MVC   GPKSYSLX,TWASYSLX   SYSTEM/LANGUAGE                              
         MVC   GPKFILE,WORK+5      SKIP OVER 0000/                              
         DROP  R4                                                               
*                                                                               
         SR    R1,R1               BUMP N'FILES PROCESSED                       
         ICM   R1,3,TWANFLS                                                     
         AHI   R1,1                                                             
         STCM  R1,3,TWANFLS                                                     
         SPACE 1                                                                
         XC    TWALNOS,TWALNOS     CLEAR LINE COUNTS                            
         SPACE 1                                                                
         BAS   RE,BLDBUFF          BUILD BUFFER FOR THIS FILE                   
         BNE   TRAPERR                                                          
         SPACE 2                                                                
VKEYX    B     EXIT                                                             
         SPACE 1                                                                
VKERR01  MVI   ERROR,SYSLOCKQ      SYSTEM LOCKED FOR UPDATE                     
         B     TRAPERR                                                          
         EJECT                                                                  
***********************************************************************         
* VALIDATE RECORD                                                     *         
***********************************************************************         
         SPACE 1                                                                
VREC     CLI   TWASYSID,NULLQ      1ST PASS GET VERSION ETC                     
         BNE   VR100                                                            
         SPACE 1                                                                
* KEY CONTAINS DOWNLOAD/SYS/LANG/VERSION=CCYYMMDDHHMM                           
*                                                                               
         LA    R2,SCRKEYH                                                       
         LA    R0,SCANWRK          CLEAR SCANNER BLOCK                          
         LHI   R1,SCANWKLQ                                                      
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         GOTO1 SCANNER,DMCB,('SCXTRA2Q',(R2)),SCANWRK,C',=/='                   
         CLI   4(R1),4                                                          
         BNE   VRERR01A           KEY FIELD ERROR 1                             
         SPACE 1                                                                
* GET SYSTEM NAME                                                               
*                                                                               
         LA    R2,SCANWRK+SCBLKLQ+SCXTRAQ    SKIP 1ST ENTRY                     
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
VR010    CLI   0(RE),NULLQ         TEST EOT                                     
         BE    VRERR02             INVALID SYSTEM                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SYSLNAME(0),SC1STFLD                                             
         BE    VR020                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SYSLSHRT(0),SC1STFLD                                             
         BE    VR020                                                            
         LA    RE,SYSLLEN(RE)                                                   
         B     VR010                                                            
VR020    MVC   TWASYSID,SYSLNUM                                                 
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
VR030    CLI   LANGCODE,EOTQ                                                    
         BE    VRERR03             INVALID LANGUAGE                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   LANGEXT(0),SC1STFLD                                              
         BE    VR040                                                            
         LA    RE,LANGELQ(RE)                                                   
         B     VR030                                                            
*                                                                               
VR040    MVC   TWALEXT,LANGCODE                                                 
         SPACE 1                                                                
* GET USERS VERSION INFO                                                        
*                                                                               
         LA    R2,SCBLKLQ+SCXTRAQ(R2)        NEXT ENTRY                         
         CLI   SC1STLEN,VERTXTLQ                                                
         BNE   VRERR01F                                                         
         CLC   SC1STFLD(VERTXTLQ),VERTXT                                        
         BNE   VRERR01G                                                         
         CLI   SC2NDLEN,VERLENQ                                                 
         BNE   VRERR01H                                                         
         SPACE 1                                                                
         MVI   WORK,ZEROQ                                                       
         MVC   WORK+1(VERLENQ*2-1),WORK                                         
         MVZ   WORK(VERLENQ),SC2NDFLD                                           
         CLC   WORK(VERLENQ),WORK+VERLENQ                                       
         BNE   VRERR01I                                                         
*                                                                               
         GOTO1 DATCON,DMCB,(9,SC2NDFLD),(2,TWAUDATE)                            
*                                                                               
         PACK  DUB,SC2NDFLD+VERTMDQ(VERTMLQ)                                    
         CVB   R1,DUB                                                           
         STCM  R1,3,TWAUTIME                                                    
         SR    R0,R0                                                            
         D     R0,=F'100'                                                       
         CHI   R0,59                                                            
         BH    VRERR01J                                                         
         CHI   R1,24                                                            
         BH    VRERR01K                                                         
*                                                                               
         BAS   RE,CHKLOCK          ENSURE SYSTEM AVAILABLE                      
         BL    VRERR04             VESRION CONTROL NOT YET IMPLEMENTED          
         BH    VRERR05             SYSTEM LOCKED                                
*                                                                               
         ICM   R1,15,SYAELEM       GET A(VERSION CONTROL ELEM)                  
         USING GPKCD,R1                                                         
         CLC   GPKCVER,TWAUCURR                                                 
         BE    VR900               USERS VERSION IS OK                          
         BL    VRERR06             USERS VERSION IS HIGHER!                     
         MVC   TWAVER,GPKCVER                                                   
         DROP  R1                                                               
*                                                                               
         BAS   RE,BLDLIST          BUILD LIST OF FILES TO UPDATE                
         BNE   VRERR07             FAILED TO FIND ANY NEWER VERSIONS            
         SPACE 1                                                                
         BAS   RE,SETKEY           BUILD KEY FIELD                              
         SR    R1,R1                                                            
         ICM   R1,3,TWATFLS        O/P TOTAL FILES TO DOWNLOAD                  
         CVD   R1,DUB                                                           
         UNPK  DSPWORK(5),DUB                                                   
         OI    DSPWORK+4,ZEROQ                                                  
         MVC   DSPWORK+5(L'FILSTXT),FILSTXT                                     
         GOTO1 GENDSP,SCRL03H                                                   
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,TWATSCRS       O/P TOTAL SCREENS TO DOWNLOAD                
         CVD   R1,DUB                                                           
         UNPK  DSPWORK(5),DUB                                                   
         OI    DSPWORK+4,ZEROQ                                                  
         MVC   DSPWORK+5(L'SCRSTXT),SCRSTXT                                     
         GOTO1 GENDSP,SCRL04H                                                   
*                                                                               
         MVC   DSPWORK(L'VERTXT),VERTXT      O/P VERSION INFO                   
         LA    RF,DSPWORK+L'VERTXT                                              
         GOTO1 DATCON,DMCB,(2,TWADATE),(20,(RF))                                
         LA    RF,DSPWORK+L'VERTXT+8                                            
         SR    R1,R1                                                            
         ICM   R1,3,TWATIME                                                     
         CVD   R1,DUB                                                           
         UNPK  0(4,RF),DUB                                                      
         OI    3(RF),ZEROQ                                                      
         GOTO1 GENDSP,SCRL05H                                                   
*                                                                               
         MVC   HALF,=AL2(IDLFLSQ)  DOWNLOAD OF FILES REQUIRED                   
         B     VRECOKX                                                          
         SPACE 2                                                                
* DOWNLOAD FIRST/NEXT SCREEN FOR THIS FILE                                      
*                                                                               
VR100    OC    TWALNO,TWALNO       SKIP IF INITIAL CALL FOR NEW FILE            
         BZ    VR110                                                            
*                                  RESTORE TSAR BUFFER                          
         MVC   TSACOM,S#ACFACS     SET A(COMFACS)                               
         LA    R1,LNREC            SET A(REC)                                   
         STCM  R1,15,TSAREC                                                     
         MVI   TSKEYL,LNRKEYLQ     SET KEY LENGTH                               
         MVC   TSRECL,=Y(LNRECLQ)  SET RECORD LENGTH                            
         MVI   TSIND2,TSI2BIGN     USE 2 BUFFERS AS A BIG ONE                   
         MVI   TSACTN,TSARES       SET RESTORE                                  
         MVI   TSINDS,TSIALLOC     SET TO ALLOCATE FROM TEMPEST                 
         OI    TSINDS,TSIXTTWA     SET TO ALLOCATE FROM TEMPEST                 
         MVC   TSPAGL,TWAPAGL      SET LOW PAGE NUMBER                          
         MVC   TSPAGN,TWAPAGN      SET NUMBER OF PAGES ALLOCATED                
         GOTO1 VTSAR,TSARD                                                      
         BE    VR110                                                            
         CLI   TSERRS,TSEEOF                                                    
         BE    VRERR08             RESTORE FAILED                               
         DC    H'0'                (SEE TSERRS FOR DETAILS)                     
         SPACE 1                                                                
*                                                                               
VR110    LA    R2,SCRL03H                                                       
         LA    R0,(SCRL24H-SCRL03H)/(SCRL04H-SCRL03H)+1                         
         SR    R1,R1                                                            
         ICM   R1,3,TWALNO                                                      
         AHI   R1,1                                                             
         STCM  R1,3,TWALNO                                                      
         STCM  R1,3,TSRNUM                                                      
         MVI   TSACTN,TSAGET                                                    
         SPACE 1                                                                
VR120    GOTO1 VTSAR,TSARD                                                      
         CLI   TSERRS,0            SHOULD NOT GET TO EOF                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   TSACTN,TSANXT                                                    
         MVC   DSPWORK(L'LNRDATA),LNRDATA                                       
         GOTO1 GENDSP,(R2)                                                      
         LA    R2,(SCRL04H-SCRL03H)(R2)                                         
         CLC   LNRLNUM,TWALNS                                                   
         BNL   VR130                                                            
         BCT   R0,VR120                                                         
         MVC   TWALNO,LNRLNUM                                                   
         MVC   HALF,=AL2(IFLCONTQ)                                              
         B     VRECOKX                                                          
         SPACE 1                                                                
* END OF FILE - FINAL PAGE IS ON SCREEN                                         
*                                                                               
VR130    LTR   R0,R0               TEST IF UNUSED LINES ON SCREEN               
         BZ    VR132                                                            
         TWAXC (R2),SCRL24H                                                     
*                                                                               
VR132    CLC   TWANFLS,TWATFLS                                                  
         BNL   VR800                                                            
         BAS   RE,SETKEY                                                        
         MVC   HALF,=AL2(IFLDONEQ)                                              
         B     VRECOKX                                                          
         SPACE 2                                                                
* END OF DOWNLOAD                                                               
*                                  RETURN CURRENT VERSION                       
VR800    MVC   DSPWORK(L'VERTXT),VERTXT                                         
         LA    RF,DSPWORK+L'VERTXT                                              
         GOTO1 DATCON,DMCB,(2,TWADATE),(20,(RF))                                
         LA    RF,DSPWORK+L'VERTXT+8                                            
         SR    R1,R1                                                            
         ICM   R1,3,TWATIME                                                     
         CVD   R1,DUB                                                           
         UNPK  0(4,RF),DUB                                                      
         OI    3(RF),ZEROQ                                                      
         GOTO1 GENDSP,SCRKEYH                                                   
         MVC   HALF,=AL2(IDWNENDQ) END OF DOWNLOAD                              
         MVI   TWASYSID,NULLQ      GENERATE ERROR IF PC HITS ENTER              
         B     VRECOKX                                                          
         SPACE 1                                                                
VR900    MVC   HALF,=AL2(IVEROKQ)  USER HAS CURRENT VERSION                     
         MVI   TWASYSID,NULLQ      GENERATE ERROR IF PC HITS ENTER              
         B     VRECOKX                                                          
         SPACE 2                                                                
VRECOKX  XC    GETTXTCB,GETTXTCB   SET OK MESSAGE                               
         LA    R1,GETTXTCB                                                      
         USING GETTXTD,R1                                                       
         MVC   GTMSGNO,HALF                                                     
         MVI   GTMSYS,GTGENSYS                                                  
         MVI   GTMTYP,GTMINF                                                    
         OI    GT1INDS,GT1REF                                                   
         DROP  R1                                                               
         OI    GENSTAT2,USMYOK+USGETTXT                                         
         SPACE 1                                                                
VRECX    B     EXIT                                                             
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
VRERR01J MVI   SYFLDIX,(*-VRERRORS)/8+1                                         
         B     VRERR01Z                                                         
VRERR01K MVI   SYFLDIX,(*-VRERRORS)/8+1                                         
         B     VRERR01Z                                                         
VRERR01Z MVI   ERROR,KEYERRQ       KEY FIELD ERROR &I                           
         MVI   TWASYSID,NULLQ      FORCE REVALIDATION                           
         B     TRAPERR                                                          
*                                                                               
VRERR02  MVI   ERROR,INVSYSQ       INVALID SYSTEM                               
         B     TRAPERR                                                          
VRERR03  MVI   ERROR,INVLANGQ      INVALID LANGUAGE                             
         MVI   TWASYSID,NULLQ      FORCE REVALIDATION                           
         B     TRAPERR                                                          
VRERR04  MVI   ERROR,NOVRCTLQ      VERSION CONTROL NOT YET IMPLEMENTED          
         B     TRAPERR                                                          
VRERR05  MVI   ERROR,SYSLOCKQ      SYSTEM LOCKED FOR UPDATE                     
         B     TRAPERR                                                          
VRERR06  MVI   ERROR,USERHIQ       USERS VERSION HIGHER THAN CURRENT            
         B     TRAPERR                                                          
VRERR07  MVI   ERROR,NOFILESQ      FAILED TO FIND ANY NEWER VERSIONS            
         B     TRAPERR                                                          
VRERR08  MVI   ERROR,ALLOCQ        TSAR RESTORE FAILED                          
         B     TRAPERR                                                          
         EJECT                                                                  
***********************************************************************         
* GET CONTROL RECORD AND ENSURE SYSTEM NOT LOCKED                     *         
***********************************************************************         
         SPACE 1                                                                
CHKLOCK  NTR1  ,                                                                
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING GPAKD,R4                                                         
         OC    GPDDA,TWACTLDA      SKIP DIRECTORY READ IF D/A KNOWN             
         BNZ   CHLK010                                                          
*                                                                               
         MVI   GPKREC,GPKRECQ                                                   
         OI    SYERRFLG,RETNERRQ                                                
         MVI   BYTE,NULLQ                                                       
         GOTO1 VREAD                                                            
         BNE   CHLKERR1            NOT YET IMPLEMENTED                          
         MVC   TWACTLDA,DMDSKADD   SAVE FOR SUBSEQUENT TRNS                     
         SPACE 1                                                                
CHLK010  GOTO1 VGETREC                                                          
         XC    SYAELEM,SYAELEM     GET RELEVANT CONTROL ELEM                    
         MVI   ELCODE,GPKCELQ                                                   
*                                                                               
CHLK020  GOTO1 VGETEL,AIO                                                       
         BNE   CHLKERR1            NOT YET IMPLEMENTED FOR SYS/LANG             
         ICM   R1,15,SYAELEM                                                    
         USING GPKCD,R1                                                         
         CLC   TWASYSLX,GPKCSYS    TEST SYSTEM/LANGEXT                          
         BNE   CHLK020                                                          
         SPACE 1                                                                
         TM    GPKCFLAG,GPKCLCKQ   TEST IF SYSTEM LOCKED                        
         BNZ   CHLKERR2                                                         
         CR    RB,RB                                                            
         B     CHLKSYSX                                                         
         SPACE 1                                                                
CHLKERR1 CR    RB,R9               FORCE CC LOW (NO CTL REC/ELEM)               
         B     *+6                                                              
CHLKERR2 CR    R9,RB               FORCE CC HIGH (LOCKED)                       
         SPACE 1                                                                
CHLKSYSX B     EXIT                                                             
         DROP  R1,R4                                                            
         EJECT                                                                  
**********************************************************************          
* BUILD LIST OF FILES TO DOWNLOAD                                    *          
**********************************************************************          
         EJECT                                                                  
BLDLIST  NTR1  ,                                                                
         L     R2,S#ASVP2          BUILD LIST IN SVPAGE2                        
         USING FLISTD,R2                                                        
*                                                                               
         LR    R0,R2                                                            
         LHI   R1,TWAXTLNQ                                                      
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         XC    TWATFLDS,TWATFLDS   CLEAR CONTROL INFO                           
*                                                                               
         XC    KEY,KEY             BUILD VERSION PASSIVE KEY                    
         LA    R4,KEY                                                           
         USING GPVRD,R4                                                         
         MVI   GPVRREC,GPVRRECQ                                                 
         MVC   GPVRSYS(L'GPKSYSLX),TWASYSLX                                     
         MVC   GPVRDATE,TWAUDATE                                                
         ICM   R1,3,TWAUTIME       SET TIME TO MIN+1                            
         AHI   R1,1                                                             
         STCM  R1,3,GPVRTIME                                                    
         GOTO1 VHIGH                                                            
         B     BLLS020                                                          
         SPACE 1                                                                
BLLS010  GOTO1 VSEQ                                                             
*                                                                               
BLLS020  CLC   GPVRKEY((GPVRLEXT-GPVRKEY)+L'GPVRLEXT),KEYSAVE                   
         BNE   BLDLISTX                                                         
         MVC   FNAME,GPVRFILE                                                   
         ICM   R1,3,GPVDSCRS                                                    
         STCM  R1,3,FNSCRS                                                      
         LA    R2,FLISTLQ(R2)                                                   
*                                                                               
         AH    R1,TWATSCRS         ADD TO TOTAL SCREENS                         
         STCM  R1,3,TWATSCRS                                                    
         ICM   R1,3,TWATFLS        BUMP FILE COUNT                              
         AHI   R1,1                                                             
         STCM  R1,3,TWATFLS                                                     
*                                                                               
         B     BLLS010                                                          
         SPACE 2                                                                
BLDLISTX OC    TWATFLS,TWATFLS     ENSURE SOME FILES FOUND                      
         BNZ   EXITOK                                                           
         B     EXITBAD             THIS SHOULD BE A FATAL ERROR                 
         EJECT                                                                  
**********************************************************************          
* BUILD BUFFER FOR CURRENT FILE                                      *          
**********************************************************************          
         EJECT                                                                  
BLDBUFF  NTR1  ,                                                                
*                                  INITIALISE TSAR BUFFER                       
         MVC   TSACOM,S#ACFACS     SET A(COMFACS)                               
         MVI   TSKEYL,LNRKEYLQ     SET KEY LENGTH                               
         XC    LNREC,LNREC                                                      
         LA    R1,LNREC            SET A(TSAR RECORD)                           
         STCM  R1,15,TSAREC                                                     
*                                                                               
         MVC   TSRECL,=Y(LNRECLQ)  SET RECORD LENGTH                            
         MVC   TSPAGN,=AL1(TSNMAX*2)   ASK FOR MAX PAGES (2 BUFFERS)            
         MVI   TSIND2,TSI2BIGN     USE 2 BUFFERS AS A BIG ONE                   
         MVI   TSACTN,TSAINI       SET INITIALISE                               
         MVI   TSINDS,TSIALLOC     SET TO ALLOCATE FROM TEMPEST                 
         OI    TSINDS,TSIXTTWA     SET TO ALLOCATE FROM TEMPEST                 
         CLI   TWAPAGL,0                                                        
         BE    BLBF010             VERY FIRST TIME IN                           
         OI    TSINDS,TSIREUSE     SET TO REUSE C/I                             
         MVC   TSPAGL,TWAPAGL      SET LOW PAGE NUMBER                          
         MVC   TSPAGN,TWAPAGN      SET NUMBER OF PAGES ALLOCATED                
BLBF010  GOTO1 VTSAR,TSARD                                                      
         BE    BLBF020                                                          
         CLI   TSERRS,TSEEOF                                                    
         BE    BLBFERR1                                                         
         DC    H'0'                (SEE TSERRS FOR DETAILS)                     
BLBF020  MVC   TWAPAGL,TSPAGL      SAVE LOW PAGE NUMBER                         
         MVC   TWAPAGN,TSPAGN      SAVE NUMBER OF PAGES ALLOCATED               
         SPACE 2                                                                
         MVC   KEY,SVKEY                                                        
         LA    R4,KEY                                                           
         USING GPAKD,R4                                                         
         L     R5,S#ATIA           BUILD CURRENT MAINFRAME LINE HERE            
         LR    R1,R5               SET A(OUTPUT RECORD) AFTER EOL               
         AHI   R1,4096             ACTUAL LINE MAX IS 4092                      
         ST    R1,AOUTREC                                                       
         MVI   SYERRFLG,RETNERRQ                                                
         GOTO1 VREAD                                                            
         BE    BLBF110                                                          
         DC    H'0'                SEQ ZERO REC NOT FOUND                       
         SPACE 1                                                                
BLBF100  GOTO1 VSEQ                                                             
         CLC   GPKEY((GPKFILE-GPKEY)+L'GPKFILE),KEYSAVE                         
         BNE   BLBF200                                                          
*                                                                               
BLBF110  GOTO1 VGETREC                                                          
         SPACE 1                                                                
         MVI   ELCODE,GPKTELQ                                                   
         XC    SYAELEM,SYAELEM                                                  
*                                                                               
BLBF120  GOTO1 VGETEL,AIO                                                       
         BNE   BLBF100                                                          
         MVI   PARTLINE,YESQ       POTENTIALLY HAVE AN INCOMPLETE LINE          
         L     R1,SYAELEM                                                       
         USING GPKTD,R1                                                         
         SR    RE,RE                                                            
         IC    RE,GPKTELL                                                       
         SHI   RE,GPKTFXLQ+1                                                    
         BNM   *+6                                                              
         DC    H'0'                INVALID ELEM LENGTH                          
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),GPKTEXT                                                  
         LA    R5,1(RE,R5)                                                      
         CLI   GPKTLSUB,GPKTFINQ   TEST FINAL ELEMENT FOR LINE                  
         BNE   BLBF120                                                          
         MVI   PARTLINE,NOQ        HAVE A COMPLETE LINE                         
         DROP  R1                                                               
         SPACE 1                                                                
* CONVERT LINE TO DISPLAYABLE FORMAT AND PUT TO TSAR BUFFER                     
* CURRRLEN IS THE L'EXISTING LINE WHICH HASN'T BEEN PUT TO TSAR BUFFER          
* IT COULD BE THE END OF A LONG LINE OR A COLLECTION OF SHORT LINES             
*                                                                               
         LR    R2,R5                                                            
         L     RE,S#ATIA                                                        
         SR    R2,RE               R2=L'CURRENT MAINFRAME LINE                  
*                                                                               
         SR    RF,RF               SET A(OUTPUT) BEYOND ANY PREV LINE           
         L     RF,AOUTREC                                                       
         AH    RF,CURRRLEN                                                      
*                                                                               
         GOTO1 HEXOUT,DMCB,S#ATIA,(RF),(R2),0                                   
*                                                                               
         SLL   R2,1                R2=L'OUTPUT LINE                             
         AH    R2,CURRRLEN         PLUS END OF PREVIOUS LINE(S)                 
         ICM   R1,15,AOUTREC       APPEND XX DELIM                              
         AR    R1,R2                                                            
         MVC   0(L'EOLTXT,R1),EOLTXT                                            
         AHI   R2,L'EOLTXT                                                      
         MVC   L'EOLTXT(L'LNRDATA,R1),SPACEPAD  SPACE PAD FINAL LINE            
*                                                                               
         SR    R0,R0               CALC N'SCREEN LINES                          
         LR    R1,R2                                                            
         ICM   R2,15,AOUTREC                                                    
         D     R0,=A(L'LNRDATA)                                                 
         STCM  R0,3,CURRRLEN       L' TEXT THAT DOENT'T FILL LNRDATA            
         LTR   R0,R1               TEST AT LEAST 1 FULL LINE                    
         BZ    BLBF140                                                          
*                                                                               
BLBF130  SR    R1,R1                                                            
         ICM   R1,3,LNRLNUM                                                     
         AHI   R1,1                                                             
         STCM  R1,3,LNRLNUM                                                     
         MVC   LNRDATA,0(R2)                                                    
*                                                                               
         MVI   TSACTN,TSAADD                                                    
         GOTO1 VTSAR,TSARD                                                      
         CLI   TSERRS,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R2,L'LNRDATA(R2)                                                 
         BCT   R0,BLBF130                                                       
*                                                                               
         L     RE,AOUTREC          MOVE ANY REMAINDER TO START OF REC           
         MVC   0(L'LNRDATA,RE),0(R2)         NB SPACE PADDED                    
         SPACE 1                                                                
BLBF140  L     R5,S#ATIA                                                        
         B     BLBF120                                                          
         SPACE 2                                                                
* MAY BE AN INCOMPLETE LINE STILL TO PUT TO BUFFER                              
*                                                                               
BLBF200  CLI   PARTLINE,YESQ       ENSURE FILE COMPLETE                         
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OC    CURRRLEN,CURRRLEN                                                
         BZ    BLBF210                                                          
         L     R2,AOUTREC                                                       
         SR    R1,R1                                                            
         ICM   R1,3,LNRLNUM                                                     
         AHI   R1,1                                                             
         STCM  R1,3,LNRLNUM                                                     
         MVC   LNRDATA,0(R2)                                                    
*                                                                               
         MVI   TSACTN,TSAADD                                                    
         GOTO1 VTSAR,TSARD                                                      
         CLI   TSERRS,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 2                                                                
* SAVE TSAR BUFFER ONCE BUILT                                                   
*                                                                               
BLBF210  MVC   TWALNS,LNRLNUM      N'LINES IN BUFFER                            
         MVI   TSACTN,TSASAV       SAVE BUFFER                                  
         MVC   TSPAGL,TWAPAGL      SET LOW TSAR PAGE NUMBER                     
         MVC   TSPAGN,TWAPAGN      SET NUMBER OF PAGES ALLOCATED                
         GOTO1 VTSAR,TSARD                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
BLDBUFFX B     EXITOK                                                           
         SPACE 1                                                                
BLBFERR1 MVI   ERROR,ALLOCQ        TSAR ALLOCATION FAILURE                      
         B     EXITBAD                                                          
         EJECT                                                                  
**********************************************************************          
* SET KEY FIELD                                                      *          
**********************************************************************          
         SPACE 1                                                                
SETKEY   NTR1  ,                                                                
         L     R2,S#ASVP2                                                       
         AH    R2,TWAAFIL                                                       
         SR    R1,R1                                                            
         ICM   R1,3,FNSCRS                                                      
         CVD   R1,DUB                                                           
         UNPK  DSPWORK(4),DUB                                                   
         OI    DSPWORK+3,ZEROQ                                                  
         MVI   DSPWORK+4,C'/'                                                   
         MVC   DSPWORK+5(L'FNAME),FNAME                                         
         LA    R2,FLISTLQ(R2)                                                   
         S     R2,S#ASVP2                                                       
         STCM  R2,3,TWAAFIL                                                     
         GOTO1 GENDSP,SCRKEYH                                                   
         OI    SCRKEYH+FHOID,FHOIMO+FHOITR                                      
*                                                                               
SETKEYX  B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* VARIOUS SHORT ROUTINES                                             *          
**********************************************************************          
         SPACE 1                                                                
         EJECT                                                                  
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
       ++INCLUDE GEPAK03TC                                                      
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
FLISTD   DSECT                     FILE LIST IN SVPAGE2                         
FNAME    DS    CL(L'GPKFILE)                                                    
FNSCRS   DS    XL(L'GPVDSCRS)                                                   
FLISTLQ  EQU   *-FLISTD                                                         
         EJECT                                                                  
***********************************************************************         
* REDEFINED OVERLAY WORK AREA                                         *         
***********************************************************************         
         SPACE 1                                                                
SYSWORKD DSECT                                                                  
         ORG   SYOVWORK                                                         
AOUTREC  DS    A                   A(CURRENT LINE(S)) IN HEXOUT FMT             
CURRRLEN DS    XL2                 L'STILL TO OUTPUT IN OUTREC                  
*                                                                               
LNREC    DS    0XL(LNRECLQ)        TSAR RECORD (SCREEN LINE OF TEXT)            
LNRKEY   DS    0XL(LNRKEYLQ)                                                    
LNRLNUM  DS    XL2                 'KEY' (LINE NUMBER)                          
LNRKEYLQ EQU   *-LNRLNUM                                                        
LNRDATA  DS    XL(L'SCRL03)                                                     
LNRECLQ  EQU   *-LNRKEY                                                         
         SPACE 1                                                                
PARTLINE DS    XL1                 FLAG TO INDICATE LAST LINE COMPLETE          
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
TWASYSLX DS    0XL(L'GPKSYSLX)                                                  
TWASYSID DS    XL1                 EQUATED SYSTEM NUMBER                        
TWALEXT  DS    XL1                 LANGUAGE FILE EXTENSION CODE                 
         DS    XL2                 N/D (WORD ALIGN)                             
TWAVER   DS    0XL(GPKCVLQ)        CURRENT VERSION STAMP                        
TWADATE  DS    XL(L'GPKCDATE)                                                   
TWATIME  DS    XL(L'GPKCTIME)                                                   
*                                                                               
TWAUCURR DS    0XL(GPKCVLQ)        USERS VERSION STAMP                          
TWAUDATE DS    XL(L'GPKCDATE)                                                   
TWAUTIME DS    XL(L'GPKCTIME)                                                   
*                                                                               
TWALNOS  DS    0XL4                LINE COUNTS FOR CURRENT FILE                 
TWALNO   DS    XL2                 CURRENT TSAR LINE NUMBER                     
TWALNS   DS    XL2                 N'LINES IN BUFFER                            
*                                                                               
TWATFLDS DS    0XL(TWATFLQ)        FIELDS FOR ENTIRE DOWNLOAD                   
TWATFLS  DS    XL2                 TOTAL N'FILES TO DOWNLOAD                    
TWATSCRS DS    XL2                 TOTAL NUMBER OF SCREENS TO DOWNLOAD          
TWANFLS  DS    XL2                 N'FILES PROCESSED SO FAR                     
TWAAFIL  DS    AL2                 DISP TO CURRENT FILE IN SVPAGE2              
TWATFLQ  EQU   *-TWATFLS                                                        
*                                                                               
TWACTLDA DS    XL(L'GPDDA)         DA OF CONTROL REC                            
*                                                                               
TWAFLDLQ EQU   *-TWAFSTRT                                                       
TWAUSEQ  EQU   *-PAKFFD                                                         
         ORG   PAKFFD+(TWAUSRLQ-TWAUSEQ) ASM ERR IF TWA USAGE > MAX             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005GEPAK03   12/08/09'                                      
         END                                                                    
