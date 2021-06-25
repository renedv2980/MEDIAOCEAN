*          DATA SET TAREP73    AT LEVEL 002 AS OF 09/12/14                      
*PHASE T70373A,*                                                                
*INCLUDE DLFLD                                                                  
         TITLE 'T70373 - EVTIME DOWNLOAD'                                       
T70373   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 PTLNQ,T70373,R6                                                  
         LR    RE,RC                                                            
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
         ST    RE,APIDTBL                                                       
         EJECT                                                                  
***********************************************************************         
*        MODE CONTROLLED ROUTINES                                     *         
***********************************************************************         
                                                                                
         GOTO1 INITIAL,DMCB,0      INITIALIZE                                   
                                                                                
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         JNE   *+8                                                              
         BRAS  RE,VK                                                            
                                                                                
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         JNE   *+8                                                              
         BRAS  RE,PR                                                            
                                                                                
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
         XC    FILTS(FILTLNQ),FILTS                                             
         BAS   RE,VKEV                                                          
         BAS   RE,VKCS                                                          
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ERRORS                                                       *         
***********************************************************************         
                                                                                
ERINV    MVI   ERROR,INVALID                                                    
         J     EREND                                                            
                                                                                
ERMIS    MVI   ERROR,MISSING                                                    
         J     EREND                                                            
                                                                                
EREND    GOTO1 ERREX                                                            
         EJECT                                                                  
***********************************************************************         
*        VALIDATE KEY FOR EVTIME/DOWN                                 *         
***********************************************************************         
                                                                                
VKEV     NTR1                                                                   
         CLI   RECNUM,EV                                                        
         JNE   XIT                                                              
                                                                                
         CLI   EVTAGYH+5,0                                                      
         JE    VKEV10                                                           
         GOTO1 RECVAL,DMCB,TLAYCDQ,EVTAGYH                                      
         MVC   FAGY,TGAGY                                                       
                                                                                
VKEV10   CLI   EVTCLIH+5,0                                                      
         JE    VKEV20                                                           
         OC    FAGY,FAGY                                                        
         JZ    ERAGYMIS                                                         
         GOTO1 RECVAL,DMCB,TLCLCDQ,EVTCLIH                                      
         MVC   FCLI,TGCLI                                                       
                                                                                
VKEV20   CLI   EVTPRDH+5,0                                                      
         JE    VKEV30                                                           
         OC    FCLI,FCLI                                                        
         JZ    ERCLIMIS                                                         
         GOTO1 RECVAL,DMCB,TLPRCDQ,EVTPRDH                                      
         MVC   FPRD,TGPRD                                                       
                                                                                
VKEV30   CLI   EVTEVTH+5,0                                                      
         JE    VKEV40                                                           
         OC    FAGY,FAGY                                                        
         JZ    ERAGYMIS                                                         
         GOTO1 RECVAL,DMCB,TLCOICDQ,EVTEVTH                                     
         MVC   FEVT,TGCID                                                       
                                                                                
VKEV40   CLI   EVTINVH+5,0                                                      
         JE    VKEV50                                                           
         OC    FAGY,FAGY                                                        
         JZ    ERAGYMIS                                                         
         GOTO1 TINVCON,DMCB,EVTINV,TGINV,DATCON                                 
         CLI   0(R1),X'FF'                                                      
         JE    ERINVINV                                                         
         XC    TGINV,=6X'FF'                                                    
         GOTO1 RECVAL,DMCB,TLINCDQ,(X'20',0)                                    
         MVC   FINV,TGINV                                                       
                                                                                
         USING PERVALD,R3                                                       
VKEV50   CLI   EVTPDH+5,0                                                       
         JE    VKEV60                                                           
         LA    R2,EVTPDH                                                        
         LA    R3,BLOCK                                                         
         GOTO1 PDVAL,DMCB,(R3)                                                  
         MVC   FPPS,PVALPSTA                                                    
         MVC   FPPE,PVALPEND                                                    
         CLI   TWAWHEN,2                                                        
         JNE   VKEV70                                                           
         GOTO1 DATCON,DMCB,(1,PVALPSTA),WORK                                    
         GOTO1 ADDAY,DMCB,(C'Y',WORK),WORK+6,1                                  
         GOTO1 DATCON,DMCB,WORK+6,(1,WORK)                                      
         CLC   FPPE,WORK                                                        
         JH    ERPPEINV                                                         
         J     VKEV70                                                           
         DROP  R3                                                               
                                                                                
VKEV60   CLI   EVTBDRH+5,0                                                      
         JNE   VKEV70                                                           
         CLI   EVTCDRH+5,0                                                      
         JE    ERPDMIS                                                          
                                                                                
VKEV70   CLI   EVTPAIDH+5,0                                                     
         JE    VKEV80                                                           
         MVC   FPAID,EVTPAID                                                    
         CLI   EVTPAID,C'Y'                                                     
         JE    VKEV80                                                           
         CLI   EVTPAID,C'N'                                                     
         JNE   ERPAINV                                                          
                                                                                
         USING PERVALD,R3                                                       
VKEV80   CLI   EVTBDRH+5,0                                                      
         JE    VKEV90                                                           
         LA    R2,EVTBDRH                                                       
         LA    R3,BLOCK                                                         
         GOTO1 PDVAL,DMCB,(R3)                                                  
         MVC   FBDS,PVALPSTA                                                    
         MVC   FBDE,PVALPEND                                                    
         CLI   TWAWHEN,2                                                        
         JNE   VKEV90                                                           
         GOTO1 DATCON,DMCB,(1,PVALPSTA),WORK                                    
         GOTO1 ADDAY,DMCB,(C'Y',WORK),WORK+6,1                                  
         GOTO1 DATCON,DMCB,WORK+6,(1,WORK)                                      
         CLC   FBDE,WORK                                                        
         JH    ERBDRINV                                                         
         DROP  R3                                                               
                                                                                
         USING PERVALD,R3                                                       
VKEV90   CLI   EVTCDRH+5,0                                                      
         JE    VKEV100                                                          
         LA    R2,EVTCDRH                                                       
         LA    R3,BLOCK                                                         
         GOTO1 PDVAL,DMCB,(R3)                                                  
         MVC   FCDS,PVALPSTA                                                    
         MVC   FCDE,PVALPEND                                                    
         CLI   TWAWHEN,2                                                        
         JNE   VKEV100                                                          
         GOTO1 DATCON,DMCB,(1,PVALPSTA),WORK                                    
         GOTO1 ADDAY,DMCB,(C'Y',WORK),WORK+6,1                                  
         GOTO1 DATCON,DMCB,WORK+6,(1,WORK)                                      
         CLC   FCDE,WORK                                                        
         JH    ERCDRINV                                                         
         DROP  R3                                                               
                                                                                
VKEV100  CLI   EVTPIDH+5,0                                                      
         JE    VKEV120                                                          
         CLI   EVTPIDH+5,6                                                      
         JNE   VKEV110                                                          
         GOTO1 SSNUNPK,DMCB,EVTPID,TGSSN                                        
         JNE   VKEV110                                                          
         MVC   EVTPID,TGSSN                                                     
         MVI   EVTPIDH+5,9                                                      
VKEV110  GOTO1 RECVAL,DMCB,TLW4CDQ,(X'80',EVTPID)                               
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVC   EVTPID,SPACES                                                    
         MVC   EVTPID(L'TGPID),TGPID                                            
         MVI   EVTPIDH+5,6                                                      
         OI    EVTPIDH+6,X'80'                                                  
         MVC   FSSN,TGSSN                                                       
                                                                                
VKEV120  CLI   EVTWKAH+5,0                                                      
         JE    VKEV130                                                          
         MVC   FWKAREA,EVTWKA                                                   
         MVC   TGTHREE,SPACES                                                   
         MVC   TGTHREE(2),EVTWKA                                                
         GOTO1 TAXVAL,DMCB,(3,TGTHREE)                                          
         JE    VKEV130                                                          
         MVC   TGCTRY,=C'CA'                                                    
         GOTO1 TAXVAL,DMCB,(X'FF',TGTHREE)                                      
         JNE   ERWKAINV                                                         
                                                                                
VKEV130  CLI   EVTWKCH+5,0                                                      
         JE    VKEV140                                                          
         MVC   FWKCITY,EVTWKC                                                   
         MVC   TGTHREE,EVTWKC                                                   
         GOTO1 TAXVAL,DMCB,(3,TGTHREE)                                          
         JE    VKEV140                                                          
         MVC   TGCTRY,=C'CA'                                                    
         GOTO1 TAXVAL,DMCB,(X'FF',TGTHREE)                                      
         JNE   ERWKCINV                                                         
                                                                                
VKEV140  CLI   EVTTXAH+5,0                                                      
         JE    VKEV150                                                          
         MVC   FTXAREA,EVTTXA                                                   
         MVC   TGTHREE,SPACES                                                   
         MVC   TGTHREE(2),EVTTXA                                                
         GOTO1 TAXVAL,DMCB,(3,TGTHREE)                                          
         JE    VKEV150                                                          
         MVC   TGCTRY,=C'CA'                                                    
         GOTO1 TAXVAL,DMCB,(X'FF',TGTHREE)                                      
         JNE   ERTXAINV                                                         
                                                                                
VKEV150  CLI   EVTTASKH+5,0                                                     
         JE    VKEV160                                                          
         GOTO1 RECVAL,DMCB,TLTKCDQ,EVTTASKH,('TLTKSCDQ',0)                      
         MVC   FTASK,TGTASK                                                     
                                                                                
VKEV160  CLI   EVTPTYPH+5,0                                                     
         JE    XIT                                                              
         GOTO1 RECVAL,DMCB,TLPMCDQ,EVTPTYPH,('TLPMSCDQ',0)                      
         MVC   FPTYP,TGPTYP                                                     
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ERRORS                                                       *         
***********************************************************************         
                                                                                
ERAGYMIS LA    R2,EVTAGYH                                                       
         J     ERMIS                                                            
                                                                                
ERBDRINV LA    R2,EVTBDRH                                                       
         J     ERINV                                                            
                                                                                
ERCDRINV LA    R2,EVTCDRH                                                       
         J     ERINV                                                            
                                                                                
ERCLIMIS LA    R2,EVTCLIH                                                       
         J     ERMIS                                                            
                                                                                
ERPAINV  LA    R2,EVTPAIDH                                                      
         J     ERINV                                                            
                                                                                
ERINVINV LA    R2,EVTINVH                                                       
         J     ERINV                                                            
                                                                                
ERPDMIS  LA    R2,EVTPDH                                                        
         J     ERMIS                                                            
                                                                                
ERPPEINV LA    R2,EVTPDH                                                        
         J     ERINV                                                            
                                                                                
ERTXAINV LA    R2,EVTTXAH                                                       
         J     ERINV                                                            
                                                                                
ERWKAINV LA    R2,EVTWKAH                                                       
         J     ERINV                                                            
                                                                                
ERWKCINV LA    R2,EVTWKCH                                                       
         J     ERINV                                                            
                                                                                
***********************************************************************         
*        VALIDATE KEY FOR CAPS/DOWN                                   *         
***********************************************************************         
                                                                                
VKCS     NTR1                                                                   
         CLI   RECNUM,CS                                                        
         JNE   XIT                                                              
                                                                                
         USING PERVALD,R3                                                       
         LA    R2,SPLPERH                                                       
         CLI   5(R2),0                                                          
         JE    ERMIS                                                            
         LA    R3,BLOCK                                                         
         GOTO1 PDVAL,DMCB,(R3)                                                  
         MVC   FCDS,PVALPSTA                                                    
         MVC   FCDE,PVALPEND                                                    
         CLI   TWAWHEN,2                                                        
         JNE   VKCS10                                                           
         GOTO1 DATCON,DMCB,(1,PVALPSTA),WORK                                    
         GOTO1 ADDAY,DMCB,(C'Y',WORK),WORK+6,1                                  
         GOTO1 DATCON,DMCB,WORK+6,(1,WORK)                                      
         CLC   FCDE,WORK                                                        
         JH    ERINV                                                            
         DROP  R3                                                               
                                                                                
VKCS10   CLI   SPLEMPH+5,0                                                      
         JE    VKCS20                                                           
         OC    SPLEMP,SPACES                                                    
         CLC   SPLEMP,=C'P+ '                                                   
         JNE   EREMPINV                                                         
                                                                                
VKCS20   CLI   SPLUNITH+5,0                                                     
         JE    VKCS30                                                           
         OC    SPLUNIT,SPACES                                                   
         MVC   TGCTRY,=C'CA'                                                    
         GOTO1 TAXVAL,DMCB,(X'FF',SPLUNIT)                                      
         JNE   ERUNTINV                                                         
         MVC   FTXAREA,SPLUNIT                                                  
                                                                                
VKCS30   CLI   SPLOPTH+5,0                                                      
         JNE   EROPINV                                                          
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ERRORS                                                       *         
***********************************************************************         
                                                                                
EREMPINV LA    R2,SPLEMPH                                                       
         J     ERINV                                                            
                                                                                
ERUNTINV LA    R2,SPLUNITH                                                      
         J     ERINV                                                            
                                                                                
EROPINV  LA    R2,SPLOPTH                                                       
         J     ERINV                                                            
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        PRINT REPORT                                                 *         
***********************************************************************         
                                                                                
PR       NTR1  BASE=*,LABEL=*                                                   
         USING DLCBD,R5                                                         
         LA    R5,DLCB                                                          
         BRAS  RE,INITDOWN                                                      
                                                                                
         LA    R3,KEY                                                           
         BAS   RE,PREV                                                          
         BAS   RE,PRCS                                                          
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        PRINT REPORT FOR EVTIME/DOWN                                 *         
*        ON ENTRY ... R3 = A(KEY)                                     *         
*                     R5 = A(DOWNLOAD BLOCK)                          *         
***********************************************************************         
                                                                                
PREV     NTR1                                                                   
         CLI   RECNUM,EV                                                        
         JNE   XIT                                                              
                                                                                
         GOTO1 OUTPDOWN,DMCB,(C'T',AGYHEAD),L'AGYHEAD                           
         GOTO1 (RF),(R1),(C'T',CLIHEAD),L'CLIHEAD                               
         GOTO1 (RF),(R1),(C'T',PRDHEAD),L'PRDHEAD                               
         GOTO1 (RF),(R1),(C'T',EVTHEAD),L'EVTHEAD                               
         GOTO1 (RF),(R1),(C'T',INVHEAD),L'INVHEAD                               
         GOTO1 (RF),(R1),(C'T',PSTHEAD),L'PSTHEAD                               
         GOTO1 (RF),(R1),(C'T',PENHEAD),L'PENHEAD                               
         GOTO1 (RF),(R1),(C'T',PDTHEAD),L'PDTHEAD                               
         GOTO1 (RF),(R1),(C'T',BDTHEAD),L'BDTHEAD                               
         GOTO1 (RF),(R1),(C'T',CDTHEAD),L'CDTHEAD                               
         GOTO1 (RF),(R1),(C'T',APOHEAD),L'APOHEAD                               
         GOTO1 (RF),(R1),(C'T',ESTHEAD),L'ESTHEAD                               
         GOTO1 (RF),(R1),(C'T',PIDHEAD),L'PIDHEAD                               
         GOTO1 (RF),(R1),(C'T',LNMHEAD),L'LNMHEAD                               
         GOTO1 (RF),(R1),(C'T',FNMHEAD),L'FNMHEAD                               
         GOTO1 (RF),(R1),(C'T',CPNHEAD),L'CPNHEAD                               
         GOTO1 (RF),(R1),(C'T',WSTHEAD),L'WSTHEAD                               
         GOTO1 (RF),(R1),(C'T',WCYHEAD),L'WCYHEAD                               
         GOTO1 (RF),(R1),(C'T',TSTHEAD),L'TSTHEAD                               
         GOTO1 (RF),(R1),(C'T',TSKHEAD),L'TSKHEAD                               
         GOTO1 (RF),(R1),(C'T',PTYHEAD),L'PTYHEAD                               
         GOTO1 (RF),(R1),(C'T',UNTHEAD),L'UNTHEAD                               
         GOTO1 (RF),(R1),(C'T',RATHEAD),L'RATHEAD                               
         GOTO1 (RF),(R1),(C'T',AMTHEAD),L'AMTHEAD                               
         BRAS  RE,EOLDOWN                                                       
                                                                                
         USING TLTMD,R3                                                         
         XC    KEY,KEY                                                          
         MVI   TLTMCD,TLTMCDQ                                                   
         MVI   TLTMSTA,TLTMSPPL                                                 
         GOTO1 HIGH                                                             
         J     PREV20                                                           
PREV10   GOTO1 SEQ                                                              
PREV20   CLC   KEY(TLTMCOM-TLTMD),KEYSAVE                                       
         JNE   XIT                                                              
                                                                                
         OC    FINV,FINV                                                        
         JZ    *+14                                                             
         CLC   TLTMINV,FINV                                                     
         JNE   PREV10                                                           
                                                                                
         OC    FSSN,FSSN                                                        
         JZ    *+14                                                             
         CLC   TLTMSSN,FSSN                                                     
         JNE   PREV10                                                           
                                                                                
         GOTO1 GETREC                                                           
                                                                                
         L     R4,AIO                                                           
         MVI   ELCODE,TATDELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   PREV10                                                           
                                                                                
         MVC   SVTMKEY,KEY                                                      
         DROP  R3                                                               
                                                                                
         USING TLCOPD,R3                                                        
         XC    KEY,KEY                                                          
         MVI   TLCOPCD,TLCOCCDQ                                                 
         MVC   TLCOCCOM,SVTMKEY+TLTMCOM-TLTMD                                   
         GOTO1 HIGH                                                             
         CLC   TLCOPKEY,KEYSAVE                                                 
         JE    *+6                                                              
         DC    H'00'                                                            
         DROP  R3                                                               
                                                                                
         GOTO1 GETREC                                                           
                                                                                
         USING TLCOD,R4                                                         
         L     R4,AIO                                                           
                                                                                
         OC    FAGY,FAGY                                                        
         JZ    *+14                                                             
         CLC   TLCOAGY,FAGY                                                     
         JNE   PREV170                                                          
                                                                                
         OC    FCLI,FCLI                                                        
         JZ    *+14                                                             
         CLC   TLCOCLI,FCLI                                                     
         JNE   PREV170                                                          
                                                                                
         OC    FPRD,FPRD                                                        
         JZ    *+14                                                             
         CLC   TLCOPRD,FPRD                                                     
         JNE   PREV170                                                          
                                                                                
         MVC   TGAGY,TLCOAGY                                                    
         MVC   TGCLI,TLCOCLI                                                    
         MVC   TGPRD,TLCOPRD                                                    
         OC    TGPRD,SPACES                                                     
         DROP  R4                                                               
                                                                                
         USING TACOD,R4                                                         
         MVI   ELCODE,TACOELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         OC    FEVT,FEVT                                                        
         JZ    *+14                                                             
         CLC   TACOCID,FEVT                                                     
         JNE   PREV170                                                          
                                                                                
         MVC   TGCID,TACOCID                                                    
         DROP  R4                                                               
                                                                                
         USING TLIND,R3                                                         
         XC    KEY,KEY                                                          
         MVI   TLINCD,TLINCDQ                                                   
         MVC   TLINAGY,TGAGY                                                    
         MVC   TLININV,SVTMKEY+TLTMINV-TLTMD                                    
         GOTO1 HIGH                                                             
         CLC   TLINKEY,KEYSAVE                                                  
         JE    *+6                                                              
         DC    H'00'                                                            
         DROP  R3                                                               
                                                                                
         GOTO1 GETREC                                                           
                                                                                
         USING TAIND,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAINELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         TM    TAINSTAT,TAINSCAN                                                
         JO    PREV170                                                          
                                                                                
         OC    FPPS,FPPS                                                        
         JZ    PREV30                                                           
         CLC   FPPS,TAINPTPE                                                    
         JH    PREV170                                                          
         CLC   FPPE,TAINPTPS                                                    
         JL    PREV170                                                          
                                                                                
PREV30   CLI   FPAID,C'Y'                                                       
         JNE   *+14                                                             
         OC    TAINPDTE,TAINPDTE                                                
         JZ    PREV170                                                          
                                                                                
         CLI   FPAID,C'N'                                                       
         JNE   *+14                                                             
         OC    TAINPDTE,TAINPDTE                                                
         JNZ   PREV170                                                          
                                                                                
         OC    FBDS,FBDS                                                        
         JZ    PREV40                                                           
         CLC   FBDS,TAINBDTE                                                    
         JH    PREV170                                                          
         CLC   FBDE,TAINBDTE                                                    
         JL    PREV170                                                          
                                                                                
PREV40   OC    FCDS,FCDS                                                        
         JZ    PREV50                                                           
         CLC   FCDS,TAINCDTE                                                    
         JH    PREV170                                                          
         CLC   FCDE,TAINCDTE                                                    
         JL    PREV170                                                          
                                                                                
PREV50   MVC   SVINPDTE,TAINPDTE                                                
         MVC   SVINBDTE,TAINBDTE                                                
         MVC   SVINCDTE,TAINCDTE                                                
         MVC   SVINPTPD,TAINPTPD                                                
         DROP  R4                                                               
                                                                                
         MVI   SVPDPST1,0                                                       
         XC    SVATUNIT,SVATUNIT                                                
         MVC   SVNUTAUT,SPACES                                                  
         MVC   SVNUTEST,SPACES                                                  
         OC    SVINPDTE,SVINPDTE                                                
         JZ    PREV100                                                          
                                                                                
         USING TAPDD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAPDELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   SVPDPST1,TAPDPST1                                                
         DROP  R4                                                               
                                                                                
         USING TANUD,R4                                                         
         MVI   ELCODE,TANUELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TANUTAUT))                                     
         JNE   PREV54                                                           
         L     R4,TGELEM                                                        
         ZIC   RF,TANULEN                                                       
         SHI   RF,4                                                             
         EX    RF,*+8                                                           
         J     *+10                                                             
         MVC   SVNUTAUT(0),TANUMBER                                             
         DROP  R4                                                               
                                                                                
         USING TANUD,R4                                                         
PREV54   GOTO1 GETL,DMCB,(1,=AL1(TANUTEST))                                     
         JNE   PREV56                                                           
         L     R4,TGELEM                                                        
         ZIC   RF,TANULEN                                                       
         SHI   RF,4                                                             
         EX    RF,*+8                                                           
         J     *+10                                                             
         MVC   SVNUTEST(0),TANUMBER                                             
         DROP  R4                                                               
                                                                                
PREV56   BRAS  RE,SETCHK                                                        
                                                                                
         USING TLCKD,R3                                                         
         XC    KEY,KEY                                                          
         MVI   TLCKCD,TLCKCDQ                                                   
         MVC   TLCKAGY,TGAGY                                                    
         MVC   TLCKINV,SVTMKEY+TLTMINV-TLTMD                                    
         XC    TLCKINV,=6X'FF'                                                  
         GOTO1 HIGH                                                             
         J     PREV70                                                           
PREV60   GOTO1 SEQ                                                              
PREV70   CLC   KEY(TLCKSORT-TLCKD),KEYSAVE                                      
         JE    *+6                                                              
         DC    H'00'                                                            
         CLC   TLCKSORT+4(2),SVTMKEY+TLTMSORT+4-TLTMD                           
         JNE   PREV60                                                           
         DROP  R3                                                               
                                                                                
         GOTO1 GETREC                                                           
                                                                                
         USING TAATD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAATELQ                                                   
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
PREV80   BRAS  RE,NEXTEL                                                        
         JNE   PREV90                                                           
         CLC   TAATUNIT,=C'CN '                                                 
         JE    PREV80                                                           
         MVC   SVATUNIT,TAATUNIT                                                
         DROP  R4                                                               
                                                                                
PREV90   BRAS  RE,SETTAL                                                        
                                                                                
PREV100  MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'A4',SVTMKEY+TLTMSSN-TLTMD)                
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   AIO,AIO1                                                         
                                                                                
         MVC   KEY,SVTMKEY                                                      
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
                                                                                
         USING TLTMD,R4                                                         
         L     R4,AIO                                                           
         GOTO1 SSNPACK,DMCB,TLTMSSN,TGPID                                       
         DROP  R4                                                               
                                                                                
         USING TAW4D,R4                                                         
         L     R4,AIO2                                                          
         MVI   ELCODE,TAW4ELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         CLI   TAW4TYPE,TAW4TYCO                                                
         JE    PREV104                                                          
         CLI   TAW4TYPE,TAW4TYES                                                
         JE    PREV104                                                          
         CLI   TAW4TYPE,TAW4TYTR                                                
         JE    PREV104                                                          
         MVC   SVW4NAM2,TAW4NAM2                                                
         MVC   SVW4NAM1,TAW4NAM1                                                
         MVC   SVW4CRPN,SPACES                                                  
         J     PREV106                                                          
                                                                                
PREV104  MVC   SVW4NAM2,SPACES                                                  
         MVC   SVW4NAM1,SPACES                                                  
         MVC   SVW4CRPN,TAW4CRPN                                                
         DROP  R4                                                               
                                                                                
         USING TATDD,R4                                                         
PREV106  L     R4,AIO                                                           
         MVI   ELCODE,TATDELQ                                                   
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
PREV110  BRAS  RE,NEXTEL                                                        
         JNE   PREV10                                                           
                                                                                
         MVC   WKAREA,TATDUNIT                                                  
         CLC   TATDUNIT,=C'DET'                                                 
         JNE   *+10                                                             
         MVC   WKAREA,=C'MI '                                                   
         CLC   TATDUNIT,=C'NYC'                                                 
         JNE   *+10                                                             
         MVC   WKAREA,=C'NY '                                                   
         CLC   TATDUNIT,=C'CIN'                                                 
         JNE   *+10                                                             
         MVC   WKAREA,=C'OH '                                                   
         CLC   TATDUNIT,=C'CLV'                                                 
         JNE   *+10                                                             
         MVC   WKAREA,=C'OH '                                                   
         CLC   TATDUNIT,=C'PHL'                                                 
         JNE   *+10                                                             
         MVC   WKAREA,=C'PA '                                                   
                                                                                
         OC    FWKAREA,FWKAREA                                                  
         JZ    *+14                                                             
         CLC   FWKAREA,WKAREA                                                   
         JNE   PREV110                                                          
                                                                                
         MVC   WKCITY,SPACES                                                    
         CLI   TATDUNIT+2,C' '                                                  
         JE    *+10                                                             
         MVC   WKCITY,TATDUNIT                                                  
                                                                                
         OC    FWKCITY,FWKCITY                                                  
         JZ    *+14                                                             
         CLC   FWKCITY,WKCITY                                                   
         JNE   PREV110                                                          
                                                                                
         MVC   TXAREA,WKAREA                                                    
         OC    SVATUNIT,SVATUNIT                                                
         JZ    *+10                                                             
         MVC   TXAREA,SVATUNIT                                                  
                                                                                
         OC    FTXAREA,FTXAREA                                                  
         JZ    *+14                                                             
         CLC   FTXAREA,TXAREA                                                   
         JNE   PREV110                                                          
                                                                                
         OC    FTASK,FTASK                                                      
         JZ    *+14                                                             
         CLC   FTASK,TATDTASK                                                   
         JNE   PREV110                                                          
                                                                                
         OC    FPTYP,FPTYP                                                      
         JZ    *+14                                                             
         CLC   FPTYP,TATDPMTY                                                   
         JNE   PREV110                                                          
                                                                                
         GOTO1 OUTPDOWN,DMCB,(C'T',TGAGY),L'TGAGY                               
         GOTO1 (RF),(R1),(C'T',TGCLI),L'TGCLI                                   
         GOTO1 (RF),(R1),(C'T',TGPRD),L'TGPRD                                   
         GOTO1 (RF),(R1),(C'T',TGCID),L'TGCID                                   
                                                                                
         MVC   TGINV,SVTMKEY+TLTMINV-TLTMD                                      
         XC    TGINV,=6X'FF'                                                    
         GOTO1 TINVCON,DMCB,TGINV,PARAS,DATCON                                  
         GOTO1 OUTPDOWN,DMCB,(C'T',PARAS),L'TGINV                               
                                                                                
         GOTO1 DATCON,DMCB,(1,SVINPTPD),(20,PARAS)                              
         GOTO1 OUTPDOWN,DMCB,(C'T',PARAS),8                                     
                                                                                
         GOTO1 DATCON,DMCB,(1,SVINPTPD+3),(20,PARAS)                            
         GOTO1 OUTPDOWN,DMCB,(C'T',PARAS),8                                     
                                                                                
         GOTO1 DATCON,DMCB,(1,SVINPDTE),(20,PARAS)                              
         GOTO1 OUTPDOWN,DMCB,(C'T',PARAS),8                                     
                                                                                
         GOTO1 DATCON,DMCB,(1,SVINBDTE),(20,PARAS)                              
         GOTO1 OUTPDOWN,DMCB,(C'T',PARAS),8                                     
                                                                                
         GOTO1 DATCON,DMCB,(1,SVINCDTE),(20,PARAS)                              
         GOTO1 OUTPDOWN,DMCB,(C'T',PARAS),8                                     
                                                                                
         GOTO1 (RF),(R1),(C'T',SVNUTAUT),L'SVNUTAUT                             
         GOTO1 (RF),(R1),(C'T',SVNUTEST),L'SVNUTEST                             
                                                                                
         GOTO1 (RF),(R1),(C'T',TGPID),L'TGPID                                   
         GOTO1 (RF),(R1),(C'T',SVW4NAM2),L'SVW4NAM2                             
         GOTO1 (RF),(R1),(C'T',SVW4NAM1),L'SVW4NAM1                             
         GOTO1 (RF),(R1),(C'T',SVW4CRPN),L'SVW4CRPN                             
                                                                                
         GOTO1 (RF),(R1),(C'T',WKAREA),L'WKAREA                                 
         GOTO1 (RF),(R1),(C'T',WKCITY),L'WKCITY                                 
         GOTO1 (RF),(R1),(C'T',TXAREA),L'TXAREA                                 
                                                                                
         GOTO1 (RF),(R1),(C'T',TATDTASK),L'TATDTASK                             
         GOTO1 (RF),(R1),(C'T',TATDPMTY),L'TATDPMTY                             
                                                                                
         MVC   PARAS(12),SPACES                                                 
         ICM   R2,15,TATDUNTS                                                   
         JZ    PREV130                                                          
         TM    SVPDPST1,TAPDPCRD                                                
         JZ    PREV120                                                          
         LNR   R2,R2                                                            
PREV120  EDIT  (R2),(12,PARAS),2,FLOAT=-,ALIGN=LEFT                             
PREV130  GOTO1 OUTPDOWN,DMCB,(C'T',PARAS),12                                    
                                                                                
         MVC   PARAS(12),SPACES                                                 
         ICM   R2,15,TATDRATE                                                   
         JZ    PREV150                                                          
         TM    SVPDPST1,TAPDPCRD                                                
         JZ    PREV140                                                          
         LNR   R2,R2                                                            
PREV140  EDIT  (R2),(12,PARAS),2,FLOAT=-,ALIGN=LEFT                             
PREV150  GOTO1 OUTPDOWN,DMCB,(C'T',PARAS),12                                    
                                                                                
         ICM   R2,15,TATDAMNT                                                   
         TM    SVPDPST1,TAPDPCRD                                                
         JZ    PREV160                                                          
         LNR   R2,R2                                                            
PREV160  EDIT  (R2),(12,PARAS),2,FLOAT=-,ALIGN=LEFT                             
         GOTO1 OUTPDOWN,DMCB,(C'T',PARAS),12                                    
         BRAS  RE,EOLDOWN                                                       
         J     PREV110                                                          
         DROP  R4                                                               
                                                                                
PREV170  MVC   KEY,SVTMKEY                                                      
         GOTO1 HIGH                                                             
         J     PREV10                                                           
         EJECT                                                                  
***********************************************************************         
*        PRINT REPORT FOR CAPS/DOWN                                   *         
*        ON ENTRY ... R3 = A(KEY)                                     *         
*                     R5 = A(DOWNLOAD BLOCK)                          *         
***********************************************************************         
                                                                                
PRCS     NTR1                                                                   
         CLI   RECNUM,CS                                                        
         JNE   XIT                                                              
                                                                                
         GOTO1 OUTPDOWN,DMCB,(C'T',LNMHEAD),L'LNMHEAD                           
         GOTO1 (RF),(R1),(C'T',FNMHEAD),L'FNMHEAD                               
         GOTO1 (RF),(R1),(C'T',SSNHEAD),L'SSNHEAD                               
         GOTO1 (RF),(R1),(C'T',PIDHEAD),L'PIDHEAD                               
         GOTO1 (RF),(R1),(C'T',RPRHEAD),L'RPRHEAD                               
         GOTO1 (RF),(R1),(C'T',GRSHEAD),L'GRSHEAD                               
         GOTO1 (RF),(R1),(C'T',PDRHEAD),L'PDRHEAD                               
         GOTO1 (RF),(R1),(C'T',DUEHEAD),L'DUEHEAD                               
         GOTO1 (RF),(R1),(C'T',TWGHEAD),L'TWGHEAD                               
         GOTO1 (RF),(R1),(C'T',FDWHEAD),L'FDWHEAD                               
         GOTO1 (RF),(R1),(C'T',PRWHEAD),L'PRWHEAD                               
         GOTO1 (RF),(R1),(C'T',QCWHEAD),L'QCWHEAD                               
         GOTO1 (RF),(R1),(C'T',ECPPHEAD),L'ECPPHEAD                             
         GOTO1 (RF),(R1),(C'T',EPEIHEAD),L'EPEIHEAD                             
         GOTO1 (RF),(R1),(C'T',EQPPHEAD),L'EQPPHEAD                             
         GOTO1 (RF),(R1),(C'T',EQEIHEAD),L'EQEIHEAD                             
         GOTO1 (RF),(R1),(C'T',EQPIHEAD),L'EQPIHEAD                             
         GOTO1 (RF),(R1),(C'T',TEEHEAD),L'TEEHEAD                               
         GOTO1 (RF),(R1),(C'T',SUBHEAD),L'SUBHEAD                               
         GOTO1 (RF),(R1),(C'T',NETHEAD),L'NETHEAD                               
         GOTO1 (RF),(R1),(C'T',RCPPHEAD),L'RCPPHEAD                             
         GOTO1 (RF),(R1),(C'T',RPEIHEAD),L'RPEIHEAD                             
         GOTO1 (RF),(R1),(C'T',RQPPHEAD),L'RQPPHEAD                             
         GOTO1 (RF),(R1),(C'T',RQEIHEAD),L'RQEIHEAD                             
         GOTO1 (RF),(R1),(C'T',RQPIHEAD),L'RQPIHEAD                             
         GOTO1 (RF),(R1),(C'T',RQHSHEAD),L'RQHSHEAD                             
         GOTO1 (RF),(R1),(C'T',TERHEAD),L'TERHEAD                               
         GOTO1 (RF),(R1),(C'T',TTRHEAD),L'TTRHEAD                               
         BRAS  RE,EOLDOWN                                                       
                                                                                
***********************************************************************         
                                                                                
         L     RE,APIDTBL                                                       
         MVI   0(RE),X'FF'                                                      
                                                                                
         USING TLINPD,R3                                                        
         XC    KEY,KEY                                                          
         MVI   TLINPCD,TLINKCDQ                                                 
         MVC   TLINKDTE,FCDS                                                    
         GOTO1 HIGH                                                             
         J     PRCS20                                                           
PRCS10   GOTO1 SEQ                                                              
PRCS20   CLI   TLINPCD,TLINKCDQ                                                 
         JNE   XIT                                                              
         CLC   TLINKEMP,=C'P+ '                                                 
         JNE   PRCS10                                                           
         CLC   TLINKDTE,FCDE                                                    
         JH    XIT                                                              
         MVC   SVINKDTE,TLINKDTE                                                
         MVC   SVINPKEY,KEY                                                     
         DROP  R3                                                               
                                                                                
***********************************************************************         
                                                                                
         BRAS  RE,SETCHK                                                        
                                                                                
         USING TLCKD,R3                                                         
         XC    KEY,KEY                                                          
         MVI   TLCKCD,TLCKCDQ                                                   
         MVC   TLCKAGY,SVINPKEY+TLINKAGY-TLINPD                                 
         MVC   TLCKINV,SVINPKEY+TLINKINV-TLINPD                                 
         GOTO1 HIGH                                                             
         J     PRCS40                                                           
PRCS30   GOTO1 SEQ                                                              
PRCS40   CLC   KEY(TLCKSORT-TLCKD),KEYSAVE                                      
         JNE   PRCS200                                                          
                                                                                
         GOTO1 SSNPACK,DMCB,TLCKSSN,TGPID                                       
         BRAS  RE,CKPIDTBL                                                      
         JE    PRCS30                                                           
         DROP  R3                                                               
                                                                                
         GOTO1 GETREC                                                           
                                                                                
         USING TAPDD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAPDELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         TM    TAPDPST1,TAPDPCRD                                                
         JO    PRCS30                                                           
         DROP  R4                                                               
                                                                                
         USING TAATD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAATELQ                                                   
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
PRCS50   BRAS  RE,NEXTEL                                                        
         JNE   PRCS30                                                           
         OC    FTXAREA,FTXAREA                                                  
         JZ    PRCS60                                                           
         CLC   FTXAREA,TAATUNIT                                                 
         JNE   PRCS50                                                           
         DROP  R4                                                               
                                                                                
PRCS60   BRAS  RE,SETTAL                                                        
         MVC   SVCKKEY,KEY                                                      
                                                                                
         USING TLW4D,R3                                                         
         XC    KEY,KEY                                                          
         MVI   TLW4CD,TLW4CDQ                                                   
         MVC   TLW4SSN,SVCKKEY+TLCKSSN-TLCKD                                    
         GOTO1 HIGH                                                             
         CLC   TLW4KEY,KEYSAVE                                                  
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
                                                                                
         BRAS  RE,SETCHK                                                        
                                                                                
***********************************************************************         
                                                                                
         XC    TOTS(TOTSLNQ),TOTS                                               
                                                                                
         USING TLCKPD,R3                                                        
         XC    KEY,KEY                                                          
         MVI   TLCKPCD,TLCKECDQ                                                 
         MVC   TLCKESSN,SVCKKEY+TLCKSSN-TLCKD                                   
         MVI   TLCKECUR,C'U'                                                    
         MVC   TLCKEEMP,=C'P+ '                                                 
         MVC   TLCKEDTE,FCDE                                                    
         XC    TLCKEDTE,=3X'FF'                                                 
         GOTO1 HIGH                                                             
         J     PRCS80                                                           
PRCS70   GOTO1 SEQ                                                              
PRCS80   CLC   KEY(TLCKEDTE-TLCKPD),KEYSAVE                                     
         JNE   PRCS160                                                          
         MVC   WORK(3),TLCKEDTE                                                 
         XC    WORK(3),=3X'FF'                                                  
         CLC   WORK(3),FCDS                                                     
         JL    PRCS160                                                          
         CLC   WORK(3),FCDE                                                     
         JH    PRCS160                                                          
         DROP  R3                                                               
                                                                                
         GOTO1 GETREC                                                           
                                                                                
         USING TAATD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAATELQ                                                   
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
PRCS90   BRAS  RE,NEXTEL                                                        
         JNE   PRCS70                                                           
         OC    FTXAREA,FTXAREA                                                  
         JZ    PRCS100                                                          
         CLC   FTXAREA,TAATUNIT                                                 
         JNE   PRCS90                                                           
         DROP  R4                                                               
                                                                                
         USING TAPDD,R2                                                         
PRCS100  L     R4,AIO                                                           
         MVI   ELCODE,TAPDELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         LR    R2,R4                                                            
         TM    TAPDPST1,TAPDPCRD                                                
         JO    PRCS70                                                           
                                                                                
         L     RE,GRS                                                           
         L     RF,TAPDGRS                                                       
         AR    RE,RF                                                            
         L     RF,TAPDREXP                                                      
         AR    RE,RF                                                            
         ST    RE,GRS                                                           
                                                                                
         USING TATUD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TATUELQ                                                   
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
PRCS110  BRAS  RE,NEXTEL                                                        
         JNE   PRCS120                                                          
         CLC   TATUUNIT,=C'CN '                                                 
         JNE   PRCS110                                                          
                                                                                
         ICM   RF,15,TATUNNWA                                                   
         ICM   RE,15,TATUSTRE                                                   
         SR    RF,RE                                                            
         LR    R1,RF                                                            
         L     RE,PDR                                                           
         AR    RE,RF                                                            
         ST    RE,PDR                                                           
                                                                                
         L     RE,TAPDGRS                                                       
         ICM   RF,15,TATUWAAD                                                   
         SR    RE,RF                                                            
         ST    RE,DUE                                                           
                                                                                
         ICM   RE,15,TATUWAAD                                                   
         L     RF,TAPDREXP                                                      
         AR    RE,RF                                                            
         SR    RE,R1                                                            
         ST    RE,THISWG                                                        
                                                                                
         L     RF,TWG                                                           
         AR    RF,RE                                                            
         ST    RF,TWG                                                           
         DROP  R2,R4                                                            
                                                                                
         USING TAATD,R4                                                         
PRCS120  L     R4,AIO                                                           
         MVI   ELCODE,TAATELQ                                                   
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
PRCS130  BRAS  RE,NEXTEL                                                        
         JNE   PRCS70                                                           
                                                                                
         CLC   TAATUNIT,=C'CN '                                                 
         JNE   PRCS140                                                          
         L     RE,FDW                                                           
         L     RF,TAATTAX                                                       
         AR    RE,RF                                                            
         ST    RE,FDW                                                           
         J     PRCS130                                                          
                                                                                
PRCS140  CLC   TAATUNIT,=C'QC '                                                 
         JNE   PRCS150                                                          
                                                                                
         L     RE,QCTWG                                                         
         L     RF,THISWG                                                        
         AR    RE,RF                                                            
         ST    RE,QCTWG                                                         
                                                                                
         L     RE,QCW                                                           
         L     RF,TAATTAX                                                       
         AR    RE,RF                                                            
         ST    RE,QCW                                                           
                                                                                
         L     RE,EQPP                                                          
         L     RF,TAATPP                                                        
         AR    RE,RF                                                            
         ST    RE,EQPP                                                          
                                                                                
         L     RE,EQEI                                                          
         L     RF,TAATEI                                                        
         AR    RE,RF                                                            
         ST    RE,EQEI                                                          
                                                                                
         L     RE,EQPI                                                          
         L     RF,TAATPIP                                                       
         AR    RE,RF                                                            
         ST    RE,EQPI                                                          
         J     PRCS130                                                          
                                                                                
PRCS150  L     RE,OTTWG                                                         
         L     RF,THISWG                                                        
         AR    RE,RF                                                            
         ST    RE,OTTWG                                                         
                                                                                
         L     RE,PRW                                                           
         L     RF,TAATTAX                                                       
         AR    RE,RF                                                            
         ST    RE,PRW                                                           
                                                                                
         L     RE,ECPP                                                          
         L     RF,TAATPP                                                        
         AR    RE,RF                                                            
         ST    RE,ECPP                                                          
                                                                                
         L     RE,EPEI                                                          
         L     RF,TAATEI                                                        
         AR    RE,RF                                                            
         ST    RE,EPEI                                                          
         J     PRCS130                                                          
         DROP  R4                                                               
                                                                                
PRCS160  OC    GRS,GRS                                                          
         JZ    PRCS170                                                          
                                                                                
         USING TAW4D,R4                                                         
         L     R4,AIO3                                                          
         MVI   ELCODE,TAW4ELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         GOTO1 OUTPDOWN,DMCB,(C'T',TAW4NAM2),L'TAW4NAM2                         
         GOTO1 (RF),(R1),(C'T',TAW4NAM1),L'TAW4NAM1                             
         DROP  R4                                                               
                                                                                
         GOTO1 (RF),(R1),(C'T',SVCKKEY+TLCKSSN-TLCKD),L'TLCKSSN                 
         GOTO1 (RF),(R1),(C'T',TGPID),L'TGPID                                   
                                                                                
         USING TAA2D,R4                                                         
         L     R4,AIO3                                                          
         MVI   ELCODE,TAA2ELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         GOTO1 OUTPDOWN,DMCB,(C'T',TAA2ST),L'TAA2ST                             
         DROP  R4                                                               
                                                                                
         GOTO1 OUTAMT,DMCB,GRS     GROSS WAGES                                  
         GOTO1 (RF),(R1),PDR       PER DIEM/REIMB                               
         GOTO1 (RF),(R1),DUE       DUE COMPANY COLLECTED                        
         GOTO1 (RF),(R1),TWG       TAXABLE WAGES                                
         GOTO1 (RF),(R1),FDW       FEDERAL W/H                                  
         GOTO1 (RF),(R1),PRW       PROV W/H                                     
         GOTO1 (RF),(R1),QCW       QUEBEC W/H                                   
         GOTO1 (RF),(R1),ECPP      EE-CPP                                       
         GOTO1 (RF),(R1),EPEI      EE-EI                                        
         GOTO1 (RF),(R1),EQPP      EE-QPP                                       
         GOTO1 (RF),(R1),EQEI      EE-QEI                                       
         GOTO1 (RF),(R1),EQPI      EE-QPIP                                      
                                                                                
         L     RE,FDW              TOTAL EE TAXES                               
         L     RF,PRW                                                           
         AR    RE,RF                                                            
         L     RF,QCW                                                           
         AR    RE,RF                                                            
         L     RF,ECPP                                                          
         AR    RE,RF                                                            
         L     RF,EPEI                                                          
         AR    RE,RF                                                            
         L     RF,EQPP                                                          
         AR    RE,RF                                                            
         L     RF,EQEI                                                          
         AR    RE,RF                                                            
         L     RF,EQPI                                                          
         AR    RE,RF                                                            
         ST    RE,TEE                                                           
         GOTO1 OUTAMT,DMCB,TEE                                                  
                                                                                
         L     RE,TWG              SUBTOTAL                                     
         L     RF,TEE                                                           
         SR    RE,RF                                                            
         ST    RE,TGDUB                                                         
         GOTO1 OUTAMT,DMCB,TGDUB                                                
                                                                                
         L     RE,GRS              NET PAY                                      
         L     RF,TEE                                                           
         SR    RE,RF                                                            
         L     RF,DUE                                                           
         SR    RE,RF                                                            
         ST    RE,TGDUB                                                         
         GOTO1 OUTAMT,DMCB,TGDUB                                                
                                                                                
         GOTO1 (RF),(R1),ECPP            ER-CPP                                 
         MVC   TER,ECPP                                                         
                                                                                
         GOTO1 CALC,DMCB,OTTWG,=F'26300' ER-EI                                  
                                                                                
         GOTO1 OUTAMT,DMCB,EQPP          ER-QPP                                 
         L     RE,TER                                                           
         L     RF,EQPP                                                          
         AR    RE,RF                                                            
         ST    RE,TER                                                           
                                                                                
         GOTO1 CALC,DMCB,QCTWG,=F'21300' ER-QEI                                 
         GOTO1 (RF),(R1),QCTWG,=F'7820'  ER-QPIP                                
         GOTO1 (RF),(R1),QCTWG,=F'42600' ER-QHSF                                
                                                                                
         GOTO1 OUTAMT,DMCB,TER     TOTAL ER TAXES                               
                                                                                
         L     RE,TEE              TOTAL TAX REMITTANCE                         
         L     RF,TER                                                           
         AR    RE,RF                                                            
         ST    RE,TGFULL                                                        
         GOTO1 OUTAMT,DMCB,TGFULL                                               
                                                                                
         BRAS  RE,EOLDOWN                                                       
                                                                                
PRCS170  MVC   KEY,SVCKKEY                                                      
         GOTO1 HIGH                                                             
         J     PRCS30                                                           
                                                                                
***********************************************************************         
                                                                                
PRCS200  BRAS  RE,SETTAL                                                        
                                                                                
         MVC   KEY,SVINPKEY                                                     
         GOTO1 HIGH                                                             
         J     PRCS10                                                           
                                                                                
***********************************************************************         
*        ROUTINE CALCULATES TAX, SENDS IT OFF TO OUTPDOWN AND         *         
*        ADDS IT TO EMPLOYER TAXES                                              
*        ON ENTRY ... P1 = A(TOTAL TO CALCULATE PCT OFF OF            *         
*                     P2 = A(PERCENTAGE)                              *         
***********************************************************************         
                                                                                
CALC     NTR1                                                                   
         XR    R3,R3                                                            
         L     RE,0(R1)                                                         
         L     RE,0(RE)                                                         
         LTR   RE,RE                                                            
         JZ    CALC10                                                           
                                                                                
         XR    R2,R2                                                            
         L     R3,4(R1)            P2                                           
         L     R3,0(R3)                                                         
                                                                                
         MR    R2,RE               R2,R3 = RE * R3                              
         L     RF,=F'500000'                                                    
         DR    R2,RF                                                            
         LTR   R3,R3               R3 = QUOTIENT                                
         BM    *+8                                                              
         AH    R3,=H'1'            ROUND                                        
         SRA   R3,1                                                             
         ST    R3,0(R1)            RETURN CALC FOR 1                            
                                                                                
CALC10   ST    R3,TGFULL                                                        
         GOTO1 OUTAMT,DMCB,TGFULL                                               
                                                                                
         L     RE,TER                                                           
         AR    RE,R3                                                            
         ST    RE,TER                                                           
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE CONVERTS PROVIDED TOTAL AND SENDS IT OFF TO OUTPDOWN *         
*        ON ENTRY ... P1 = A(TOTAL TO OUTPUT)                         *         
***********************************************************************         
                                                                                
OUTAMT   NTR1                                                                   
         L     R2,0(R1)                                                         
         EDIT  (4,0(R2)),(12,PARAS),2,ZERO=NOBLANK,FLOAT=-,ALIGN=LEFT           
         GOTO1 OUTPDOWN,DMCB,(C'N',PARAS),12                                    
         J     XIT                                                              
         EJECT                                                                  
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
         CLI   DLCBTYP,DLCBNUM     DATA TYPE IS NUMERIC                         
         JE    OPD30                                                            
                                                                                
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   DLCBFLX(0),0(RF)                                                 
         J     OPD50                                                            
                                                                                
OPD30    EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   DLCBFLD(0),0(RF)                                                 
                                                                                
OPD50    GOTO1 =V(DLFLD),DLCBD                                                  
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
AGYHEAD  DC    C'AGENCY'                                                        
CLIHEAD  DC    C'CLIENT'                                                        
PRDHEAD  DC    C'PRODUCT'                                                       
EVTHEAD  DC    C'EVENT ID'                                                      
INVHEAD  DC    C'INVOICE'                                                       
PSTHEAD  DC    C'PAY PERIOD START'                                              
PENHEAD  DC    C'PAY PERIOD END'                                                
PDTHEAD  DC    C'PAY DATE'                                                      
BDTHEAD  DC    C'BILL DATE'                                                     
CDTHEAD  DC    C'CHECK DATE'                                                    
APOHEAD  DC    C'AUTH/PO'                                                       
ESTHEAD  DC    C'EST#'                                                          
SSNHEAD  DC    C'SS#'                                                           
PIDHEAD  DC    C'PID'                                                           
LNMHEAD  DC    C'LAST NAME'                                                     
FNMHEAD  DC    C'FIRST NAME'                                                    
CPNHEAD  DC    C'CORPORATION NAME'                                              
WSTHEAD  DC    C'WORK STATE'                                                    
WCYHEAD  DC    C'WORK CITY'                                                     
TSTHEAD  DC    C'TAX STATE'                                                     
TSKHEAD  DC    C'TASK'                                                          
PTYHEAD  DC    C'PAY TYPE'                                                      
UNTHEAD  DC    C'UNITS'                                                         
RATHEAD  DC    C'RATE'                                                          
AMTHEAD  DC    C'AMOUNT'                                                        
                                                                                
RPRHEAD  DC    C'RESIDENT PROVINCE'                                             
GRSHEAD  DC    C'GROSS WAGES'                                                   
PDRHEAD  DC    C'PER DIEM/REIMB'                                                
DUEHEAD  DC    C'DUE COMPANY COLLECTED'                                         
TWGHEAD  DC    C'TAXABLE WAGES'                                                 
FDWHEAD  DC    C'FEDERAL W/H'                                                   
PRWHEAD  DC    C'PROV W/H'                                                      
QCWHEAD  DC    C'QUEBEC W/H'                                                    
ECPPHEAD DC    C'EE-CPP'                                                        
EPEIHEAD DC    C'EE-EI'                                                         
EQPPHEAD DC    C'EE-QPP'                                                        
EQEIHEAD DC    C'EE-QEI'                                                        
EQPIHEAD DC    C'EE-QPIP'                                                       
TEEHEAD  DC    C'TOTAL EE TAXES'                                                
SUBHEAD  DC    C'SUBTOTAL'                                                      
NETHEAD  DC    C'NET PAY'                                                       
RCPPHEAD DC    C'ER-CPP'                                                        
RPEIHEAD DC    C'ER-EI'                                                         
RQPPHEAD DC    C'ER-QPP'                                                        
RQEIHEAD DC    C'ER-QEI'                                                        
RQPIHEAD DC    C'ER-QPIP'                                                       
RQHSHEAD DC    C'ER-QHSF'                                                       
TERHEAD  DC    C'TOTAL ER TAXES'                                                
TTRHEAD  DC    C'TOTAL TAX REMITTANCE'                                          
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        INITIALIZE DOWNLOAD SETTINGS                                 *         
*        ON ENTRY ... R5=A(DOWNLOAD BLOCK)                            *         
***********************************************************************         
                                                                                
INITDOWN NTR1  BASE=*,LABEL=*                                                   
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
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        FINISH LINE OF DOWNLOAD OUPUT                                *         
***********************************************************************         
                                                                                
EOLDOWN  NTR1  BASE=*,LABEL=*                                                   
         MVI   DLCBACT,DLCBEOL      END OF LINE                                 
         GOTO1 =V(DLFLD),DLCBD      (DON'T USE RF, BASR RE,RF BELOW)            
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        END DOWNLOAD                                                 *         
***********************************************************************         
                                                                                
ENDDOWN  NTR1  BASE=*,LABEL=*                                                   
         MVI   DLCBACT,DLCBEOR      END OF REPORT                               
         GOTO1 =V(DLFLD),DLCBD      LAST FOR REPORT                             
         J     XIT                                                              
         DROP  R5                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE RETURNS POSTIVE CONDITION CODE IF TGPID IS ALREADY   *         
*        IN PID TABLE ... NEGATIVE CONDITION CODE IF IT'S NOT         *         
***********************************************************************         
                                                                                
CKPIDTBL NTR1  BASE=*,LABEL=*                                                   
         L     R2,APIDTBL                                                       
CPT10    CLI   0(R2),X'FF'                                                      
         JE    CPT20                                                            
         CLC   TGPID,0(R2)                                                      
         JE    YES                                                              
         LA    R2,L'TGPID(R2)                                                   
         J     CPT10                                                            
                                                                                
CPT20    MVC   0(L'TGPID,R2),TGPID                                              
         MVI   L'TGPID(R2),X'FF'                                                
         J     NO                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS TO READ FROM TALENT DIRECTORY/FILE              *         
***********************************************************************         
                                                                                
SETTAL   NTR1  BASE=*,LABEL=*                                                   
         MVC   SYSDIR,=CL8'TALDIR'                                              
         MVC   SYSFIL,=CL8'TALFIL'                                              
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS TO READ FROM CHECK DIRECTORY/FILE               *         
***********************************************************************         
                                                                                
SETCHK   NTR1  BASE=*,LABEL=*                                                   
         MVC   SYSDIR,=CL8'CHKDIR'                                              
         MVC   SYSFIL,=CL8'CHKFIL'                                              
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        WORKING STORAGE                                                        
***********************************************************************         
                                                                                
MYD      DSECT                                                                  
FILTS    DS    0X                                                               
FAGY     DS    CL(L'TGAGY)                                                      
FCLI     DS    CL(L'TGCLI)                                                      
FPRD     DS    CL(L'TGPRD)                                                      
FEVT     DS    CL(L'TGCID)                                                      
FINV     DS    XL(L'TGINV)                                                      
FPPS     DS    XL3                                                              
FPPE     DS    XL3                                                              
FPAID    DS    CL1                                                              
FBDS     DS    XL3                                                              
FBDE     DS    XL3                                                              
FCDS     DS    XL3                                                              
FCDE     DS    XL3                                                              
FSSN     DS    CL(L'TGSSN)                                                      
FWKAREA  DS    CL2                                                              
FWKCITY  DS    CL(L'TATDUNIT)                                                   
FTXAREA  DS    CL2                                                              
FTASK    DS    CL(L'TGTASK)                                                     
FPTYP    DS    CL(L'TGPTYP)                                                     
FILTLNQ  EQU   *-FILTS                                                          
                                                                                
TOTS     DS    0F                                                               
GRS      DS    F                                                                
PDR      DS    F                                                                
DUE      DS    F                                                                
TWG      DS    F                                                                
OTTWG    DS    F                                                                
QCTWG    DS    F                                                                
FDW      DS    F                                                                
PRW      DS    F                                                                
QCW      DS    F                                                                
TEE      DS    F                                                                
ECPP     DS    F                                                                
EPEI     DS    F                                                                
EQPP     DS    F                                                                
EQEI     DS    F                                                                
EQPI     DS    F                                                                
TER      DS    F                                                                
TTR      DS    F                                                                
FD       DS    F                                                                
QC       DS    F                                                                
TOTSLNQ  EQU   *-TOTS                                                           
                                                                                
THISWG   DS    F                                                                
SVTMKEY  DS    XL(L'KEY)                                                        
SVINPKEY DS    XL(L'KEY)                                                        
SVCKKEY  DS    XL(L'KEY)                                                        
SVINPDTE DS    XL(L'TAINPDTE)                                                   
SVINBDTE DS    XL(L'TAINBDTE)                                                   
SVINCDTE DS    XL(L'TAINCDTE)                                                   
SVINPTPD DS    XL(L'TAINPTPD)                                                   
SVATUNIT DS    CL(L'TAATUNIT)                                                   
SVINKDTE DS    XL(L'TLINKDTE)                                                   
SVW4NAM2 DS    CL(L'TAW4NAM2)                                                   
SVW4NAM1 DS    CL(L'TAW4NAM1)                                                   
SVW4CRPN DS    CL(L'TAW4CRPN)                                                   
WKAREA   DS    CL(L'TATDUNIT)                                                   
WKCITY   DS    CL(L'TATDUNIT)                                                   
TXAREA   DS    CL(L'TATDUNIT)                                                   
SVPDPST1 DS    XL(L'TAPDPST1)                                                   
SVNUTAUT DS    CL16                                                             
SVNUTEST DS    CL16                                                             
                                                                                
DLCB     DS    CL(DLCBXLX)         DOWNLOAD BLOCK                               
                                                                                
APIDTBL  DS    A                                                                
PTLNQ    EQU   (L'TGPID*5000)+1                                                 
         EJECT                                                                  
       ++INCLUDE TAREPFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TAREPEBD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TAREPCDD                                                       
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
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002TAREP73   09/12/14'                                      
         END                                                                    
