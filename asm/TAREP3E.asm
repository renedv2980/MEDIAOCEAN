*          DATA SET TAREP3E    AT LEVEL 003 AS OF 03/26/13                      
*PHASE T7033EE,*                                                                
*INCLUDE DLFLD                                                                  
         TITLE 'T7035D - NEW YORK MOBILITY TAX DOWNLOAD'                        
T7033E   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T7035D,R6                                                      
         L     RC,0(R1)            RC=A(CONTROLLER W/S)                         
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(SCREEN)                                 
         USING T703FFD,RA                                                       
         L     R9,ASUBSYSD         R9=A(SYSTEM W/S)                             
         USING SUBSYSD,R9                                                       
         L     R8,ASPOOLD          R8=A(SPOOL DSECT)                            
         USING SPOOLD,R8                                                        
         LA    R7,BUFF                                                          
         LA    R7,8(R7)                                                         
         USING MYD,R7                                                           
                                                                                
***********************************************************************         
*        MODE CONTROLLED ROUTINES                                     *         
***********************************************************************         
                                                                                
         GOTO1 INITIAL,DMCB,0      INITIALIZE                                   
                                                                                
         CLI   MODE,VALKEY                                                      
         BNE   *+8                                                              
         BRAS  RE,VK               VALIDATE KEY                                 
                                                                                
         CLI   MODE,PRINTREP                                                    
         BNE   *+8                                                              
         BRAS  RE,PREP             PRINT REPORT                                 
                                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
                                                                                
         GETEL R4,DATADISP,ELCODE                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE KEY                                                 *         
***********************************************************************         
                                                                                
VK       NTR1  BASE=*,LABEL=*                                                   
         XC    MYD(MYDLNQ),MYD     CLEAR WORKING STORAGE                        
                                                                                
         USING PERVALD,R3                                                       
         LA    R2,NMTPDH           PERIOD FILTER                                
         LA    R3,BLOCK                                                         
         GOTO1 PDVAL,DMCB,(R3)                                                  
         MVC   FLTPST,PVALPSTA                                                  
         XC    FLTPST,=3X'FF'                                                   
         MVC   FLTPEN,PVALPEND                                                  
         XC    FLTPEN,=3X'FF'                                                   
         DROP  R3                                                               
                                                                                
         GOTO1 RECVAL,DMCB,TLEMCDQ,(X'08',NMTEMPH),NMTEMPNH                     
                                                                                
         CLI   NMTOPTH+5,0                                                      
         JE    XIT                                                              
                                                                                
         USING SCAND,R3                                                         
         LA    R2,NMTOPTH                                                       
         GOTO1 SCANNER,DMCB,(R2),(R3),0                                         
         CLI   4(R1),0                                                          
         JE    ERRINV                                                           
         ZIC   R0,4(R1)                                                         
                                                                                
VK10     CLC   =C'GOV',SCDATA1                                                  
         JNE   ERRINV                                                           
         OI    OPTIONS,OPTGOV                                                   
         CLI   SCLEN2,0                                                         
         JE    VK20                                                             
         CLI   SCDATA2,C'Y'                                                     
         JNE   ERRINV                                                           
                                                                                
VK20     LA    R3,SCANNEXT                                                      
         BCT   R0,VK10                                                          
         J     XIT                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        ERROR MESSAGES                                               *         
***********************************************************************         
                                                                                
ERRINV   MVI   ERROR,INVALID                                                    
         GOTO1 ERREX                                                            
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        PRINT REPORT                                                 *         
***********************************************************************         
                                                                                
PREP     NTR1  BASE=*,LABEL=*                                                   
         ZAP   TOTWAG,=P'0'                                                     
                                                                                
         USING TLW4D,R3                                                         
         LA    R3,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   TLW4CD,TLW4CDQ                                                   
         GOTO1 HIGH                                                             
         J     PREP20                                                           
PREP10   GOTO1 SEQ                                                              
PREP20   CLI   TLW4CD,TLW4CDQ                                                   
         JNE   PREP140                                                          
         DROP  R3                                                               
                                                                                
         GOTO1 GETREC                                                           
                                                                                
         USING TAA2D,R4                                                         
         L     R4,AIO1                                                          
         MVI   ELCODE,TAA2ELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   PREP10                                                           
                                                                                
         CLC   TAA2ST,=C'NY'                                                    
         JNE   PREP10                                                           
                                                                                
         LA    R2,ZIPTAB                                                        
PREP30   CLC   0(L'ZIPTAB,R2),TAA2ZIP                                           
         JE    PREP40                                                           
         CLI   L'ZIPTAB(R2),X'FF'                                               
         JE    PREP10                                                           
         LA    R2,L'ZIPTAB(R2)                                                  
         J     PREP30                                                           
                                                                                
PREP40   ST    R4,ATAA2EL                                                       
         DROP  R4                                                               
                                                                                
         USING TAW4D,R4                                                         
         L     R4,AIO1                                                          
         MVI   ELCODE,TAW4ELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   PREP10                                                           
                                                                                
         CLI   TAW4TYPE,TAW4TYIN                                                
         JE    PREP50                                                           
         CLI   TAW4TYPE,TAW4TYFO                                                
         JNE   PREP10                                                           
         DROP  R4                                                               
                                                                                
PREP50   MVC   SVW4KEY,KEY                                                      
                                                                                
         MVC   SYSDIR,=CL8'CHKDIR'                                              
         MVC   SYSFIL,=CL8'CHKFIL'                                              
         MVC   AIO,AIO2                                                         
         XR    R0,R0                                                            
                                                                                
         USING TLCKPD,R3                                                        
         XC    KEY,KEY                                                          
         MVI   TLCKPCD,TLCKECDQ                                                 
         MVC   TLCKESSN,SVW4KEY+TLW4SSN-TLW4D                                   
         MVI   TLCKECUR,C'U'                                                    
         MVC   TLCKEEMP,TGEMP                                                   
         MVC   TLCKEDTE,FLTPEN                                                  
         GOTO1 HIGH                                                             
         J     PREP70                                                           
PREP60   GOTO1 SEQ                                                              
PREP70   CLC   KEY(TLCKEDTE-TLCKPD),KEYSAVE                                     
         JNE   PREP90                                                           
         CLC   TLCKEDTE,FLTPST                                                  
         JH    PREP90                                                           
         DROP  R3                                                               
                                                                                
         GOTO1 GETREC                                                           
                                                                                
         USING TACAD,R4                                                         
         L     R4,AIO2                                                          
         MVI   ELCODE,TACAELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         TM    TACASTA3,TACASPPL   PAYROLL PLUS?                                
         JO    PREP82                                                           
         CLC   TACAUNIT,=CL3'NY'                                                
         JE    PREP80                                                           
         CLC   TACAUNIT,=CL3'NYC'                                               
         JE    PREP80                                                           
         CLC   TACAUNIT,=CL3'YON'                                               
         JNE   PREP60                                                           
         DROP  R4                                                               
                                                                                
         USING TACDD,R4                                                         
PREP80   L     R4,AIO2                                                          
         MVI   ELCODE,TACDELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         A     R0,TACDEARN                                                      
         J     PREP60                                                           
         DROP  R4                                                               
                                                                                
PREP82   MVC   AIO,AIO2                                                         
         MVI   ELCODE,TATUELQ                                                   
         GOTO1 GETL,DMCB,(3,=C'NY ')                                            
         JE    PREP84                                                           
         GOTO1 GETL,DMCB,(3,=C'NYC')                                            
         JE    PREP84                                                           
         GOTO1 GETL,DMCB,(3,=C'YON')                                            
         JNE   PREP60                                                           
                                                                                
         USING TATUD,R4                                                         
PREP84   MVC   AIO,AIO2                                                         
         MVI   ELCODE,TATUELQ                                                   
         GOTO1 GETL,DMCB,(3,=C'NY ')                                            
         JNE   PREP60                                                           
         L     R4,TGELEM                                                        
         A     R0,TATUWAGE                                                      
         A     R0,TATUTNWA                                                      
         DROP  R4                                                               
                                                                                
PREP90   MVC   AIO,AIO1                                                         
                                                                                
         LTR   R0,R0                                                            
         JZ    PREP130                                                          
                                                                                
         USING SORTD,R2                                                         
         LA    R2,SRTREC                                                        
                                                                                
         USING TLW4D,R4                                                         
         TM    OPTIONS,OPTGOV                                                   
         JZ    PREP100                                                          
         L     R4,AIO                                                           
         MVC   SORTSSN,TLW4SSN                                                  
         DROP  R4                                                               
                                                                                
         USING TAA2D,R4                                                         
PREP100  L     R4,ATAA2EL                                                       
         MVC   SORTZIP,TAA2ZIP                                                  
         DROP  R4                                                               
                                                                                
         USING TANUD,R4                                                         
         TM    OPTIONS,OPTGOV                                                   
         JO    PREP110                                                          
         MVI   ELCODE,TANUELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TANUPIDN))                                     
         JE    *+6                                                              
         DC    H'00'                                                            
         L     R4,TGELEM                                                        
         MVC   SORTSSN,SPACES                                                   
         MVC   SORTPID,TANUMBER                                                 
         DROP  R4                                                               
                                                                                
PREP110  STCM  R0,15,SORTWAG                                                    
         MVC   SORTDA,SVW4KEY+TLDRDA-TLDRD                                      
                                                                                
         TM    SRTSTAT,SRTPROG                                                  
         JO    PREP120                                                          
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD                                     
         OI    SRTSTAT,SRTPROG                                                  
                                                                                
PREP120  GOTO1 SORTER,DMCB,=C'PUT',(R2)                                         
         DROP  R2                                                               
                                                                                
PREP130  MVC   SYSDIR,=CL8'TALDIR'                                              
         MVC   SYSFIL,=CL8'TALFIL'                                              
         MVC   KEY,SVW4KEY                                                      
         GOTO1 HIGH                                                             
         J     PREP10                                                           
                                                                                
***********************************************************************         
                                                                                
PREP140  TM    SRTSTAT,SRTPROG                                                  
         JZ    XIT                                                              
                                                                                
         USING DLCBD,R5                                                         
         LA    R5,DLCB                                                          
         BAS   RE,INITDOWN         INITIALIZE DOWNLOAD                          
                                                                                
         GOTO1 OUTPDOWN,DMCB,(C'T',ZIPHEAD),L'ZIPHEAD                           
                                                                                
         LA    RF,PIDHEAD                                                       
         TM    OPTIONS,OPTGOV                                                   
         JZ    PREP150                                                          
         LA    RF,SSNHEAD                                                       
PREP150  GOTO1 OUTPDOWN,DMCB,(C'T',(RF)),L'PIDHEAD                              
                                                                                
         GOTO1 OUTPDOWN,DMCB,(C'T',NAMHEAD),L'NAMHEAD                           
         GOTO1 OUTPDOWN,DMCB,(C'T',ADDHEAD),L'ADDHEAD                           
         GOTO1 OUTPDOWN,DMCB,(C'T',CTYHEAD),L'CTYHEAD                           
         GOTO1 OUTPDOWN,DMCB,(C'T',WAGHEAD),L'WAGHEAD                           
         GOTO1 OUTPDOWN,DMCB,(C'T',TAXHEAD),L'TAXHEAD                           
         BAS   RE,EOLDOWN                                                       
                                                                                
         USING SORTD,R2                                                         
         LA    R2,SRTREC                                                        
PREP160  GOTO1 SORTER,DMCB,=C'GET'                                              
         ICM   RF,15,4(R1)                                                      
         JZ    PREP190                                                          
         MVC   SRTREC,0(RF)                                                     
                                                                                
         BAS   RE,OUTZIP                                                        
                                                                                
         GOTO1 OUTPDOWN,DMCB,(C'T',SORTZIP),L'SORTZIP                           
                                                                                
         GOTO1 OUTPDOWN,DMCB,(C'T',SORTSSN),L'SORTSSN                           
                                                                                
         MVC   KEY+TLDRDA-TLDRD(L'TLDRDA),SORTDA                                
         GOTO1 GETREC                                                           
                                                                                
         USING TAW4D,R4                                                         
         L     R4,AIO1                                                          
         MVI   ELCODE,TAW4ELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   BLOCK(L'TAW4NAM1),TAW4NAM1                                       
         MVC   BLOCK+L'TAW4NAM1(L'TAW4NAM2),TAW4NAM2                            
         GOTO1 SQUASHER,DMCB,BLOCK,L'TAW4NAM1+L'TAW4NAM2                        
         GOTO1 OUTPDOWN,DMCB,(C'T',BLOCK),L'TAW4NAM1+L'TAW4NAM2                 
         DROP  R4                                                               
                                                                                
         USING TAA2D,R4                                                         
         L     R4,AIO1                                                          
         MVI   ELCODE,TAA2ELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         GOTO1 SQUASHER,DMCB,TAA2ADDR,L'TAA2ADDR                                
         GOTO1 OUTPDOWN,DMCB,(C'T',TAA2ADDR),L'TAA2ADDR                         
         GOTO1 OUTPDOWN,DMCB,(C'T',TAA2CITY),L'TAA2CITY                         
         DROP  R4                                                               
                                                                                
         EDIT  SORTWAG,(12,BLOCK),2,MINUS=YES                                   
         GOTO1 OUTPDOWN,DMCB,(C'T',BLOCK),12                                    
                                                                                
         ZICM  RE,SORTWAG,4                                                     
         A     RE,ZIPWAG                                                        
         ST    RE,ZIPWAG                                                        
                                                                                
         XR    R0,R0                                                            
         ZICM  R1,SORTWAG,4                                                     
                                                                                
         MVI   TGBYTE,C'N'                                                      
         LTR   R1,R1                                                            
         JNM   PREP170                                                          
         LCR   R1,R1                                                            
         MVI   TGBYTE,C'Y'                                                      
                                                                                
PREP170  MHI   R1,34                                                            
         D     R0,=F'10000'                                                     
         CHI   R0,5000                                                          
         JL    *+8                                                              
         AHI   R1,1                                                             
                                                                                
         LR    R4,R1                                                            
         CLI   TGBYTE,C'Y'                                                      
         JNE   PREP180                                                          
         LCR   R4,R1                                                            
                                                                                
PREP180  EDIT  (R4),(12,BLOCK),2,MINUS=YES                                      
         GOTO1 OUTPDOWN,DMCB,(C'T',BLOCK),12                                    
                                                                                
         L     RE,ZIPTAX                                                        
         AR    RE,R4                                                            
         ST    RE,ZIPTAX                                                        
                                                                                
         BAS   RE,EOLDOWN                                                       
         J     PREP160                                                          
                                                                                
PREP190  MVI   LASTZIP,X'FF'                                                    
         BAS   RE,OUTZIP                                                        
                                                                                
         GOTO1 OUTPDOWN,DMCB,(C'T',SPACES),1                                    
         GOTO1 OUTPDOWN,DMCB,(C'T',SPACES),1                                    
         GOTO1 OUTPDOWN,DMCB,(C'T',SPACES),1                                    
         GOTO1 OUTPDOWN,DMCB,(C'T',SPACES),1                                    
         GOTO1 OUTPDOWN,DMCB,(C'T',SPACES),1                                    
         EDIT  (P10,TOTWAG),(12,BLOCK),2,MINUS=YES                              
         GOTO1 OUTPDOWN,DMCB,(C'T',BLOCK),12                                    
         EDIT  TOTTAX,(12,BLOCK),2,MINUS=YES                                    
         GOTO1 OUTPDOWN,DMCB,(C'T',BLOCK),12                                    
         BAS   RE,EOLDOWN                                                       
                                                                                
         BAS   RE,ENDDOWN                                                       
         J     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
*        OUTPUT TOTALS FOR LAST ZIP CODE                              *         
*        ON ENTRY ... R2=A(SORT RECORD)                               *         
*                     R5=A(DOWNLOAD BLOCK)                            *         
***********************************************************************         
                                                                                
         USING SORTD,R2                                                         
OUTZIP   NTR1                                                                   
         OC    LASTZIP,LASTZIP                                                  
         JZ    OZIP10                                                           
         CLC   LASTZIP,SORTZIP                                                  
         JE    XIT                                                              
         GOTO1 OUTPDOWN,DMCB,(C'T',SPACES),1                                    
         GOTO1 OUTPDOWN,DMCB,(C'T',SPACES),1                                    
         GOTO1 OUTPDOWN,DMCB,(C'T',SPACES),1                                    
         GOTO1 OUTPDOWN,DMCB,(C'T',SPACES),1                                    
         GOTO1 OUTPDOWN,DMCB,(C'T',SPACES),1                                    
                                                                                
         EDIT  ZIPWAG,(12,BLOCK),2,MINUS=YES                                    
         GOTO1 OUTPDOWN,DMCB,(C'T',BLOCK),12                                    
                                                                                
         L     RF,ZIPWAG                                                        
         CVD   RF,DUB                                                           
         AP    TOTWAG,DUB                                                       
                                                                                
         EDIT  ZIPTAX,(12,BLOCK),2,MINUS=YES                                    
         GOTO1 OUTPDOWN,DMCB,(C'T',BLOCK),12                                    
         L     RE,TOTTAX                                                        
         A     RE,ZIPTAX                                                        
         ST    RE,TOTTAX                                                        
                                                                                
         BAS   RE,EOLDOWN                                                       
                                                                                
OZIP10   MVC   LASTZIP,SORTZIP                                                  
         XC    ZIPWAG,ZIPWAG                                                    
         XC    ZIPTAX,ZIPTAX                                                    
         J     XIT                                                              
         DROP  R2                                                               
                                                                                
***********************************************************************         
*        INITIALIZE DOWNLOAD SETTINGS                                 *         
*        ON ENTRY ... R5=A(DOWNLOAD BLOCK)                            *         
***********************************************************************         
                                                                                
INITDOWN NTR1                                                                   
         XC    DLCBD(DLCBXLX),DLCBD                                             
         MVI   DLCBACT,DLCBINIT    INITIALIZE FOR DOWNLOAD                      
         LA    R1,PRTDOWN          A(HOOK ROUTINE FOR PRINTING)                 
         ST    R1,DLCBAPR                                                       
         LA    R1,P                A(PRINT LINE)                                
         MVC   P,SPACES                                                         
         ST    R1,DLCBAPL                                                       
         MVC   DLCBAED,EDITOR                                                   
         MVC   DLCXMAXL,=Y(L'P)    MAXIMUM LENGTH OF PRINT LINE                 
         MVI   DLCXDELC,C' '       DELIMITER                                    
         MVI   DLCXEOTC,C'"'       TEXT DELIMITER                               
         MVI   DLCXEOTA,C''''      TEXT DELIMITER ALTERNATE                     
         MVI   DLCXEOLC,X'5E'      SEMI-COLON FOR END OF LINE                   
         MVI   DLCXEORC,C':'       END OF REPORT                                
         OI    DLCBFLG1,DLCBFXTN                                                
         GOTO1 =V(DLFLD),DLCBD                                                  
         J     XIT                                                              
                                                                                
***********************************************************************         
*        USER SUPPLIED PRINT ROUTINE                                  *         
***********************************************************************         
                                                                                
PRTDOWN  NTR1                                                                   
         MVI   LINE,1              PREVENT PAGE BREAK                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
         J     XIT                                                              
*                                                                               
***********************************************************************         
*        OUTPUT DOWNLOAD ENTRY                                        *         
*        ON ENTRY ... P1, B0    = TYPE TO PASS                        *         
*                         B1-B3 = ADDRESS OF DATA                     *         
*                     P2        = LENGTH                              *         
***********************************************************************         
                                                                                
OUTPDOWN NTR1                                                                   
         MVI   DLCBACT,DLCBPUT                                                  
         MVC   DLCBTYP,0(R1)                                                    
         OI    DLCBFLG1,DLCBFXFL                                                
                                                                                
         L     RF,0(R1)            RF=A(DOWNLOAD FIELD)                         
         L     RE,4(R1)            RE=L'DOWNLOAD FIELD                          
                                                                                
         LR    R1,RF                                                            
         AR    R1,RE                                                            
OPD10    SHI   R1,1                                                             
         CLI   0(R1),C' '                                                       
         JNE   OPD20                                                            
         BCT   RE,OPD10                                                         
         LHI   RE,1                                                             
OPD20    STC   RE,DLCBLEN                                                       
                                                                                
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   DLCBFLX(0),0(RF)                                                 
         GOTO1 =V(DLFLD),DLCBD                                                  
         J     XIT                                                              
                                                                                
***********************************************************************         
*        FINISH LINE OF DOWNLOAD OUPUT                                *         
***********************************************************************         
                                                                                
EOLDOWN  NTR1                                                                   
         MVI   DLCBACT,DLCBEOL      END OF LINE                                 
         GOTO1 =V(DLFLD),DLCBD      (DON'T USE RF, BASR RE,RF BELOW)            
         J     XIT                                                              
                                                                                
***********************************************************************         
*        END DOWNLOAD                                                 *         
***********************************************************************         
                                                                                
ENDDOWN  NTR1                                                                   
         MVI   DLCBACT,DLCBEOR      END OF REPORT                               
         GOTO1 =V(DLFLD),DLCBD      LAST FOR REPORT                             
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
ZIPHEAD  DC    C'ZIP CODE'                                                      
SSNHEAD  DC    C'SSN'                                                           
PIDHEAD  DC    C'PID'                                                           
NAMHEAD  DC    C'NAME'                                                          
ADDHEAD  DC    C'ADDRESS'                                                       
CTYHEAD  DC    C'CITY'                                                          
WAGHEAD  DC    C'WAGES'                                                         
TAXHEAD  DC    C'TAX'                                                           
                                                                                
SORTCARD DC    CL80'SORT FIELDS=(1,93,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=135'                                   
                                                                                
ZIPTAB   DC    0CL3                                                             
         DC    C'005'                                                           
         DC    C'063'                                                           
         DC    C'100'                                                           
         DC    C'101'                                                           
         DC    C'102'                                                           
         DC    C'103'                                                           
         DC    C'104'                                                           
         DC    C'105'                                                           
         DC    C'106'                                                           
         DC    C'107'                                                           
         DC    C'108'                                                           
         DC    C'109'                                                           
         DC    C'110'                                                           
         DC    C'111'                                                           
         DC    C'112'                                                           
         DC    C'113'                                                           
         DC    C'114'                                                           
         DC    C'115'                                                           
         DC    C'116'                                                           
         DC    C'117'                                                           
         DC    C'118'                                                           
         DC    C'119'                                                           
         DC    C'125'                                                           
         DC    C'126'                                                           
         DC    C'127'                                                           
         DC    X'FF'                                                            
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        WORKING STORAGE                                                        
***********************************************************************         
                                                                                
MYD      DSECT                                                                  
FILTERS  DS    0C                                                               
FLTPST   DS    XL3                                                              
FLTPEN   DS    XL3                                                              
FPDLNQ   EQU   *-FILTERS                                                        
                                                                                
OPTIONS  DS    X                                                                
OPTGOV   EQU   X'80'                                                            
                                                                                
SRTSTAT  DS    X                                                                
SRTPROG  EQU   X'80'                                                            
SRTREC   DS    XL(SORTLNQ)                                                      
                                                                                
SVW4KEY  DS    XL(L'KEY)                                                        
ATAA2EL  DS    A                                                                
                                                                                
LASTZIP  DS    CL5                                                              
ZIPWAG   DS    F                                                                
ZIPTAX   DS    F                                                                
                                                                                
TOTWAG   DS    PL10                                                             
TOTTAX   DS    F                                                                
MYDLNQ   EQU   *-MYD                                                            
                                                                                
DLCB     DS    CL(DLCBXLX)                                                      
         EJECT                                                                  
       ++INCLUDE TAREPFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TAREPC9D                                                       
         EJECT                                                                  
*DDGENTWA  (MUST FOLLOW LAST SCREEN)                                            
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*DDBIGBOX                                                                       
*TAGENFILE                                                                      
*DDPERVALD                                                                      
*TASYSDSECT                                                                     
*TASYSEQUS                                                                      
*DDDLCB                                                                         
*DDTWADCONS                                                                     
*TAREPWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE DDDLCB                                                         
TWADCOND DSECT                                                                  
       ++INCLUDE DDTWADCONS                                                     
       ++INCLUDE TAREPWORKD                                                     
         PRINT ON                                                               
***********************************************************************         
*        SORT RECORD DSECT                                                      
***********************************************************************         
                                                                                
SORTD    DSECT                                                                  
SORTZIP  DS    CL(L'TAA2ZIP)                                                    
SORTPID  DS    CL(L'TGPID)                                                      
         ORG   SORTPID                                                          
SORTSSN  DS    CL(L'TGSSN)                                                      
SORTWAG  DS    XL4                                                              
SORTDA   DS    XL4                                                              
SORTLNQ  EQU   *-SORTD                                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003TAREP3E   03/26/13'                                      
         END                                                                    
