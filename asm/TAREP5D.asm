*          DATA SET TAREP5D    AT LEVEL 002 AS OF 04/03/09                      
*PHASE T7035DA,*                                                                
*INCLUDE DLFLD                                                                  
         TITLE 'T7035D - COMMERCIAL DOWNLOAD'                                   
T7035D   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 TBLLNQ,T7035D,R6                                                 
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
         ST    RE,AHISTTAB         SAVE A(HISTORY TABLE)                        
         AHI   RE,SORTTAB-HISTTAB                                               
         ST    RE,ASORTTAB                                                      
         AHI   RE,CONTTAB-SORTTAB                                               
         ST    RE,ACONTTAB         SAVE A(CONTRACT TABLE)                       
         EJECT                                                                  
***********************************************************************         
*        MODE CONTROLLED ROUTINES                                     *         
***********************************************************************         
                                                                                
         GOTO1 INITIAL,DMCB,0      INITIALIZE                                   
*                                                                               
         CLI   MODE,VALKEY                                                      
         BNE   *+8                                                              
         BRAS  RE,VK               VALIDATE KEY                                 
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BNE   *+8                                                              
         BRAS  RE,PREP             PRINT REPORT                                 
*                                                                               
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE KEY                                                 *         
***********************************************************************         
                                                                                
VK       NTR1  BASE=*,LABEL=*                                                   
         XC    MYD(MYDLNQ),MYD     CLEAR WORKING STORAGE                        
         XC    TIFILTS,TIFILTS     AND SYSIO FILETERS                           
*                                                                               
         USING PERVALD,R3                                                       
         LA    R2,SCOPDH           PERIOD FILTER                                
         LA    R3,BLOCK                                                         
         GOTO1 PDVAL,DMCB,(R3)                                                  
         MVC   FLTPST,PVALPSTA                                                  
         MVC   FLTPEN,PVALPEND                                                  
         DROP  R3                                                               
*                                                                               
         GOTO1 RECVAL,DMCB,TLAYCDQ,(8,SCOAGYH),SCOAGYNH                         
         MVC   TIFAGY,TGAGY                                                     
*                                                                               
         CLI   SCOCLIH+5,0         CLIENT FILTER                                
         JE    VK10                                                             
         GOTO1 RECVAL,DMCB,TLCLCDQ,(8,SCOCLIH),SCOCLINH                         
         MVC   TIFCLI,TGCLI                                                     
*                                                                               
VK10     CLI   SCOPRDH+5,0         PRODUCT FILTER                               
         JE    VK20                                                             
         GOTO1 RECVAL,DMCB,TLPRCDQ,(8,SCOPRDH),SCOPRDNH                         
         MVC   TIFPRD,TGPRD                                                     
*                                                                               
         USING TLCOD,R4                                                         
VK20     CLI   SCOCIDH+5,0         COMMERCIAL ID FILTER                         
         JE    VK30                                                             
         LA    R2,SCOCIDH                                                       
         GOTO1 RECVAL,DMCB,TLCOICDQ,(8,(R2)),SCOCIDNH                           
         L     R4,AIO                                                           
         MVC   TIFCOM,TLCOCOM                                                   
*                                                                               
         OC    TIFCLI,TIFCLI                                                    
         JZ    *+14                                                             
         CLC   TIFCLI,TLCOCLI                                                   
         JNE   FLDINV                                                           
*                                                                               
         OC    TIFPRD,TIFPRD                                                    
         JZ    *+14                                                             
         CLC   TIFPRD,TLCOPRD                                                   
         JNE   FLDINV                                                           
         DROP  R4                                                               
*                                                                               
         USING TACOD,R4                                                         
         CLI   SCOMEDH+5,0                                                      
         JE    VK30                                                             
         MVI   ELCODE,TACOELQ                                                   
         BRAS  RE,GETEL                                                         
         LA    R2,SCOMEDH                                                       
         CLC   TACOMED,SCOMED                                                   
         JNE   FLDINV                                                           
         DROP  R4                                                               
*                                                                               
VK30     CLI   SCOMEDH+5,0         COMMERCIAL MEDIA FILTER                      
         JE    VK40                                                             
         LA    R2,SCOMEDH                                                       
         CLI   5(R2),1                                                          
         JNE   FLDINV                                                           
         GOTO1 MEDVAL,DMCB,8(R2)                                                
         JNE   FLDINV                                                           
         MVC   TIFMED,SCOMED                                                    
*                                                                               
VK40     CLI   SCOOPTH+5,0         OPTIONS                                      
         JE    XIT                                                              
         LA    R2,SCOOPTH                                                       
*                                                                               
         USING SCAND,R3                                                         
         LA    R3,BLOCK                                                         
         GOTO1 SCANNER,DMCB,(R2),(R3),0                                         
         CLI   4(R1),0                                                          
         JE    FLDINV                                                           
         ZIC   R0,4(R1)                                                         
*                                                                               
VK50     CLC   =C'GROUP',SCDATA1   GROUP OPTION                                 
         JNE   FLDINV                                                           
         CLI   SCDATA2,C'N'                                                     
         JE    VK60                                                             
         CLI   SCDATA2,C'Y'                                                     
         JNE   FLDINV                                                           
         OI    OPTIONS,OPTGROUP                                                 
         MVI   NAMEH,44                                                         
*                                                                               
VK60     LA    R3,SCANNEXT                                                      
         BCT   R0,VK50                                                          
         J     XIT                                                              
         DROP  R3                                                               
*                                                                               
FLDINV   MVI   ERROR,INVALID                                                    
         GOTO1 ERREX                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        PRINT REPORT                                                 *         
***********************************************************************         
                                                                                
PREP     NTR1  BASE=*,LABEL=*                                                   
         USING DLCBD,R5                                                         
         LA    R5,DLCB                                                          
         BAS   RE,INITDOWN         INITIALIZE DOWNLOAD                          
*                                                                               
         GOTO1 OUTPDOWN,DMCB,(C'T',CIDHEAD),L'CIDHEAD                           
         GOTO1 OUTPDOWN,DMCB,(C'T',TITHEAD),L'TITHEAD                           
         GOTO1 OUTPDOWN,DMCB,(C'T',CLNHEAD),L'CLNHEAD                           
         GOTO1 OUTPDOWN,DMCB,(C'T',LFTHEAD),L'LFTHEAD                           
         GOTO1 OUTPDOWN,DMCB,(C'T',LLNHEAD),L'LLNHEAD                           
         GOTO1 OUTPDOWN,DMCB,(C'T',EXPHEAD),L'EXPHEAD                           
         GOTO1 OUTPDOWN,DMCB,(C'T',PCYHEAD),L'PCYHEAD                           
         GOTO1 OUTPDOWN,DMCB,(C'T',PDTHEAD),L'PDTHEAD                           
         GOTO1 OUTPDOWN,DMCB,(C'T',CNTHEAD),L'CNTHEAD                           
         BAS   RE,EOLDOWN                                                       
*                                                                               
         MVC   TIACOMFC,ACOMFACS   SET UP SYSIO BLOCK                           
         MVC   TIACCS,TWAACCS      LIMIT ACCESS                                 
         MVC   TIAUTH,TWAAUTH      AUTHORIZATION                                
         MVC   TIUSERID,TWAORIG    REQUESTING ID                                
*                                                                               
         MVI   TIREAD,TLCOCDQ      READ COMML RECORDS                           
         MVC   TIHOOK,=A(IOHOOK)   A(I/O HOOK)                                  
*                                                                               
         GOTO1 TASYSIO,DMCB,TASYSIOD                                            
*                                                                               
         BAS   RE,ENDDOWN          CLOSE DOWNLOAD                               
         J     XIT                                                              
         EJECT                                                                  
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
*                                                                               
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
*                                                                               
         L     RF,0(R1)            RF=A(DOWNLOAD FIELD)                         
         L     RE,4(R1)            RE=L'DOWNLOAD FIELD                          
*                                                                               
         LR    R1,RF                                                            
         AR    R1,RE                                                            
OPD10    SHI   R1,1                                                             
         CLI   0(R1),C' '                                                       
         JNE   OPD20                                                            
         BCT   RE,OPD10                                                         
         LHI   RE,1                                                             
OPD20    STC   RE,DLCBLEN                                                       
*                                                                               
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   DLCBFLX(0),0(RF)                                                 
         GOTO1 =V(DLFLD),DLCBD                                                  
         J     XIT                                                              
*                                                                               
***********************************************************************         
*        FINISH LINE OF DOWNLOAD OUPUT                                *         
***********************************************************************         
                                                                                
EOLDOWN  NTR1                                                                   
         MVI   DLCBACT,DLCBEOL      END OF LINE                                 
         GOTO1 =V(DLFLD),DLCBD      (DON'T USE RF, BASR RE,RF BELOW)            
         J     XIT                                                              
*                                                                               
***********************************************************************         
*        END DOWNLOAD                                                 *         
***********************************************************************         
                                                                                
ENDDOWN  NTR1                                                                   
         MVI   DLCBACT,DLCBEOR      END OF REPORT                               
         GOTO1 =V(DLFLD),DLCBD      LAST FOR REPORT                             
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        PROCESS COMMERCIAL RECORDS FROM SYSIO                        *         
***********************************************************************         
                                                                                
IOHOOK   NTR1                                                                   
         CLI   TIMODE,PROCREC      IF COMMERCIAL RECORD FOUND                   
         JNE   XIT                                                              
*                                                                               
         OC    TIFCOM,TIFCOM       IF FILTERING ON COMMERCIAL                   
         JZ    IOH10                                                            
         CLC   TIFCOM,TICOM        FILTER NOW                                   
         JNE   XIT                                                              
*                                                                               
IOH10    MVC   TIQSKEY,TIKEY       SAVE COMMERCIAL KEY                          
         OC    TIPRD,SPACES                                                     
*                                                                               
         USING HISTTABD,R2                                                      
         L     R2,AHISTTAB         INITIALIZE HISTORY TABLE                     
         MVI   0(R2),X'FF'                                                      
*                                                                               
         USING CONTTABD,R3                                                      
         L     R3,ACONTTAB         INITIALIZE CONTRACT TABLE                    
         MVI   0(R3),X'FF'                                                      
*                                                                               
         USING TLINPD,R4                                                        
         LA    R4,KEY              READ ALL PAYMENTS FOR THIS                   
         XC    KEY,KEY             COMMERCIAL                                   
         MVI   TLINPCD,TLINHCDQ                                                 
         MVC   TLINHCOM,TICOM                                                   
         GOTO1 HIGH                                                             
         J     IOH30                                                            
IOH20    GOTO1 SEQ                                                              
IOH30    CLC   KEY(TLINHINV-TLINPD),KEYSAVE                                     
         JNE   IOH40                                                            
         GOTO1 GETREC                                                           
         DROP  R4                                                               
*                                                                               
         USING TAPDD,R4                                                         
         L     R4,AIO              REPORT CYCLE MUST BE INCLUSIVE               
         MVI   ELCODE,TAPDELQ      OF THIS PAYMENT CYCLE                        
         BRAS  RE,GETEL                                                         
         JNE   IOH20                                                            
         CLC   FLTPST,TAPDCYCE                                                  
         JH    IOH20                                                            
         CLC   FLTPEN,TAPDCYCS                                                  
         JL    IOH20                                                            
*                                                                               
         MVC   HISTCYCS,TAPDCYCS   SAVE PAYMENT CYCLE START DATE                
         MVC   HISTCYCE,TAPDCYCE   CYCLE END DATE                               
         BAS   RE,SETDESC          AND USE DETAILS                              
         GOTO1 SQUASHER,DMCB,HISTDETS,L'HISTDETS                                
         DROP  R4                                                               
*                                                                               
         LA    R2,HISTLNQ(R2)      BUMP TO NEXT SPOT IN HISTORY                 
         MVI   0(R2),X'FF'         TABLE AND GO READ NEXT PAYMENT               
         J     IOH20                                                            
*                                                                               
IOH40    L     R2,AHISTTAB                                                      
         CLI   0(R2),X'FF'         IF NO PAYMENTS FOUND FOR COMMERCIAL          
         JE    IOH200              GO READ NEXT COMMERCIAL                      
         DROP  R2                                                               
*                                                                               
         USING TACOD,R4                                                         
         L     R4,TIAREC                                                        
         MVI   ELCODE,TACOELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   EXPDATE,TACOEXP     SAVE COMMERCIAL EXPIRATION                   
         DROP  R4                                                               
*                                                                               
         MVC   LIFTID,SPACES       SAVE LIFT ID AND LENGTH                      
         MVC   LIFTSEC,SPACES                                                   
         USING TALFD,R4                                                         
         L     R4,TIAREC           SAVE LIFT ID AND LENGTH                      
         MVI   ELCODE,TALFELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   IOH50                                                            
         MVC   LIFTID,TALFLID                                                   
         EDIT  (B1,TALFSEC),(3,LIFTSEC),ALIGN=LEFT                              
         DROP  R4                                                               
*                                                                               
         USING TLCAD,R4                                                         
IOH50    LA    R4,KEY                                                           
         XC    KEY,KEY             READ ALL CAST RECORDS FOR                    
         MVI   TLCACD,TLCACDQ      THE COMMERCIAL                               
         MVC   TLCACOM,TICOM                                                    
         GOTO1 HIGH                                                             
         J     IOH70                                                            
IOH60    GOTO1 SEQ                                                              
IOH70    CLC   KEY(TLCASORT-TLCAD),KEYSAVE                                      
         JNE   IOH80                                                            
         GOTO1 GETREC                                                           
         DROP  R4                                                               
*                                                                               
         USING TACAD,R4                                                         
         L     R4,AIO              IF CAST EXPIRATION DATE IS                   
         MVI   ELCODE,TACAELQ      EARLIER THAN COMMERCIAL'S                    
         BRAS  RE,GETEL            SAVE NEW EARLIEST EXPIRATION DATE            
         JNE   IOH60                                                            
         OC    TACAEXP,TACAEXP                                                  
         JZ    IOH60                                                            
         CLC   TACAEXP,EXPDATE                                                  
         JNL   IOH60                                                            
         MVC   EXPDATE,TACAEXP                                                  
         J     IOH60                                                            
         DROP  R4                                                               
*                                                                               
IOH80    XC    LCNTKEY,LCNTKEY                                                  
*                                                                               
         USING TLCNPD,R4                                                        
         LA    R4,KEY              READ ALL CONTRACT RECORDS FOR                
         XC    KEY,KEY             THIS COMMERCIAL                              
         MVI   TLCNPCD,TLCNPCDQ                                                 
         MVC   TLCNPCOM,TICOM                                                   
         GOTO1 HIGH                                                             
         J     IOH100                                                           
IOH90    GOTO1 SEQ                                                              
IOH100   CLC   KEY(TLCNPAGY-TLCNPD),KEYSAVE                                     
         JNE   IOH150                                                           
         DROP  R4                                                               
*                                                                               
         CLC   LCNTKEY,KEY         ONLY REPORT ON EACH COMMERCIAL               
         JE    IOH90               ONCE (SKIP MULTIPLE VERSIONS)                
         MVC   LCNTKEY,KEY                                                      
*                                                                               
         GOTO1 GETREC              GET CONTRACT RECORD                          
*                                                                               
         USING TLCND,R4                                                         
         L     R4,AIO                                                           
*                                                                               
         CLC   TLCNTRME,EXPDATE    IF CONTRACT EXPIRATION DATE IS               
         JNL   IOH110              EARLIER THAN COMMERCIAL'S                    
         MVC   EXPDATE,TLCNTRME    SAVE NEW EARLIEST EXPIRATION DATE            
*                                                                               
IOH110   MVC   CONTENT,SPACES                                                   
         MVC   CONTENT(L'CNTID),CNTID                                           
         MVC   CONTENT+L'CNTID(L'TLCNCNID),TLCNCNID                             
         LA    R3,CONTLNQ(R3)                                                   
*                                                                               
         MVC   CONTENT,SPACES                                                   
         MVC   CONTENT(L'CNTTERM),CNTTERM                                       
         GOTO1 DATCON,DMCB,(1,TLCNTRMS),(8,CONTENT+L'CNTTERM)                   
         MVI   CONTENT+L'CNTTERM+8,C'-'                                         
         GOTO1 DATCON,DMCB,(1,TLCNTRME),(8,CONTENT+L'CNTTERM+9)                 
         LA    R3,CONTLNQ(R3)                                                   
         DROP  R4                                                               
*                                                                               
         USING TARDD,R4                                                         
         MVI   ELCODE,TARDELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         USING CTYTABD,RF                                                       
         LA    RF,CTYPETAB                                                      
IOH120   CLI   0(RF),X'FF'                                                      
         JNE   *+6                                                              
         DC    H'00'                                                            
         CLC   TARDTYP1,CTYTYPE                                                 
         JE    IOH130                                                           
         LA    RF,CTYLNQ(RF)                                                    
         J     IOH120                                                           
IOH130   MVC   CONTENT,SPACES                                                   
         MVC   CONTENT(L'CNTTYPE),CNTTYPE                                       
         MVC   CONTENT+L'CNTTYPE(L'CTYDESC),CTYDESC                             
         LA    R3,CONTLNQ(R3)                                                   
         DROP  R4,RF                                                            
*                                                                               
         USING TAFND,R4                                                         
         MVI   ELCODE,TAFNELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTCON))                                     
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   CONTENT,SPACES                                                   
         MVC   CONTENT(L'CNTDESC),CNTDESC                                       
         L     R4,TGELEM                                                        
         ZIC   RE,TAFNLEN                                                       
         SHI   RE,4                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   CONTENT+L'CNTDESC(0),TAFNNAME                                    
         LA    R3,CONTLNQ(R3)                                                   
         DROP  R4                                                               
*                                                                               
         MVC   CONTENT,SPACES                                                   
         MVC   CONTENT(L'CNTCOMT),CNTCOMT                                       
*                                                                               
         USING TACMD,R4                                                         
         MVI   ELCODE,TACMELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TACMTYPG))                                     
         JNE   IOH140                                                           
         L     R4,TGELEM                                                        
         ZIC   RE,TACMLEN                                                       
         SHI   RE,4                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   CONTENT+L'CNTCOMT(0),TACMCOMM                                    
         DROP  R4                                                               
*                                                                               
IOH140   LA    R3,CONTLNQ(R3)                                                   
         MVI   0(R3),X'FF'                                                      
         J     IOH90                                                            
         DROP  R3                                                               
*                                                                               
IOH150   GOTO1 DATCON,DMCB,(1,EXPDATE),(8,OUTEXP)                               
*                                                                               
         BAS   RE,SORTHIST         SORT HISTORY TABLE BY USE                    
*                                                                               
         USING HISTTABD,R2                                                      
         L     R2,ASORTTAB         R2=A(SORTED HISTORY TABLE)                   
*                                                                               
         USING CONTTABD,R3                                                      
         L     R3,ACONTTAB         R3=A(CONTRACT TABLE)                         
*                                                                               
IOH160   CLI   0(R2),X'FF'                                                      
         JNE   IOH170                                                           
         CLI   0(R3),X'FF'                                                      
         JE    IOH200                                                           
*                                                                               
IOH170   TM    OPTIONS,OPTGROUP    IF RUNNING WITH OPTION TO                    
         JZ    IOH180              GROUP BY CLIENT/PRODUCT                      
         CLC   LASTCLI,TICLI       AND WE HAVE NOT ALREADY REPORTED             
         JNE   IOH175              THIS CLIENT/PRODUCT COMBO                    
         CLC   LASTPRD,TIPRD                                                    
         JE    IOH180                                                           
*                                                                               
IOH175   MVC   LASTCLI,TICLI       REPORT THE CLIENT                            
         GOTO1 OUTPDOWN,DMCB,(C'T',TICLI),L'TICLI                               
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'A8',TICLI),NAMEH                          
         OC    NAME,SPACES                                                      
         GOTO1 OUTPDOWN,DMCB,(C'T',NAME),L'NAME                                 
         BAS   RE,EOLDOWN                                                       
*                                                                               
         MVC   LASTPRD,TIPRD                                                    
         OC    TIPRD,TIPRD         AND REPORT THE PRODUCT                       
         JZ    IOH180                                                           
         GOTO1 OUTPDOWN,DMCB,(C'T',TIPRD),L'TIPRD                               
         GOTO1 RECVAL,DMCB,TLPRCDQ,(X'A8',TIPRD),NAMEH                          
         OC    NAME,SPACES                                                      
         GOTO1 OUTPDOWN,DMCB,(C'T',NAME),L'NAME                                 
         BAS   RE,EOLDOWN                                                       
*                                                                               
IOH180   GOTO1 OUTPDOWN,DMCB,(C'T',TICID),L'TICID                               
         GOTO1 OUTPDOWN,DMCB,(C'T',TINAME),L'TINAME                             
         GOTO1 OUTPDOWN,DMCB,(C'T',TISEC),L'TISEC                               
         GOTO1 OUTPDOWN,DMCB,(C'T',LIFTID),L'LIFTID                             
         GOTO1 OUTPDOWN,DMCB,(C'T',LIFTSEC),L'LIFTSEC                           
         GOTO1 OUTPDOWN,DMCB,(C'T',OUTEXP),L'OUTEXP                             
         BAS   RE,OUTHIST                                                       
         BAS   RE,OUTCONT                                                       
         BAS   RE,EOLDOWN                                                       
*                                                                               
         CLI   0(R2),X'FF'                                                      
         JE    IOH190                                                           
         LA    R2,HISTLNQ(R2)                                                   
*                                                                               
IOH190   CLI   0(R3),X'FF'                                                      
         JE    IOH160                                                           
         LA    R3,CONTLNQ(R3)                                                   
         J     IOH160                                                           
*                                                                               
IOH200   XC    KEY,KEY                                                          
         MVC   KEY(L'TIQSKEY),TIQSKEY                                           
         GOTO1 HIGH                                                             
         J     XIT                                                              
*                                                                               
***********************************************************************         
*        SAVE USE DESCRIPTION INTO HISTORY TABLE                      *         
*        ON ENTRY ... R2=A(CURRENT ENTRY IN HISTORY TABLE)            *         
*                     R4=A(PAYMENT DETAILS ELEMENT)                   *         
***********************************************************************         
                                                                                
         USING HISTTABD,R2                                                      
         USING TAPDD,R4                                                         
SETDESC  NTR1                                                                   
         GOTO1 USEVAL,DMCB,(X'20',TAPDUSE),TAPDTYPE                             
*                                                                               
         MVC   HISTDET1,TGUSNAME      SET USE NAME                              
         MVC   HISTDET2,SPACES        AND CLEAR DETAILS                         
*                                                                               
         OC    TAPDAREA(6),TAPDAREA   IF THERE'S PRINT AREA AND USE             
         BZ    SDESC10                                                          
         MVC   HISTDET2(3),TAPDAREA   DISPLAY IT                                
         MVC   HISTDET2+4(3),TAPDPUSE                                           
         J     XIT                                                              
*                                                                               
SDESC10  TM    TGUSTYST,USES                                                    
         JO    SDESC90                      BRANCH IF USES REQUIRED             
         TM    TGUSTYST,MAJORS                                                  
         JO    SDESC20                      BRANCH IF MAJORS REQUIRED           
         TM    TGUSTYST,UNITS                                                   
         JO    SDESC30                      BRANCH IF UNITS REQUIRED            
         TM    TGUSTYST,INSERTS                                                 
         JO    SDESC40                      BRANCH IF INSERTS REQUIRED          
         CLI   TGUSEQU,UTAG                                                     
         JE    SDESC50                      BRANCH IF TAG PAYMENT               
         CLI   TGUSEQU,UDEM                                                     
         JE    SDESC60                      BRANCH IF DEMO PAYMENT              
         CLI   TGUSEQU,USNA                                                     
         JE    SDESC60                      BRANCH IF SPANISH DEMO PAY          
         CLI   TGUSEQU,UCDM                                                     
         JE    SDESC60                      BRANCH IF CANADIAN DEM PAY          
         CLI   TGUSEQU,UITN                                                     
         JE    SDESC70                      BRANCH IF ITN PAYMENT               
         J     XIT                          ELSE DONE                           
*                                                                               
SDESC20  GOTO1 MAJVAL,DMCB,(X'80',TAPDMAJ) VALIDATE MAJORS                      
         JNE   SDESC30                                                          
         MVC   HISTDET2(L'TGMACHAR),TGMACHAR                                    
*                                                                               
SDESC30  OC    TAPDUNIT,TAPDUNIT           IF THERE ARE UNITS                   
         JZ    XIT                                                              
         LA    RF,HISTDET2+L'TGMACHAR-1    FIND END OF MAJORS                   
         CLI   0(RF),X'40'                                                      
         JH    *+8                                                              
         BCT   RF,*-8                                                           
         EDIT  TAPDUNIT,(5,2(RF)),ALIGN=LEFT NUMBER OF UNITS                    
         J     XIT                                                              
*                                                                               
SDESC40  OC    TAPDINS,TAPDINS     IF THERE ARE INSERTS                         
         JZ    XIT                                                              
         LA    RF,HISTDET2                                                      
         EDIT  TAPDINS,(5,2(RF)),ALIGN=LEFT NUMBER OF INSERTS                   
         J     XIT                                                              
*                                                                               
SDESC50  OC    TAPDTAGS,TAPDTAGS   IF THERE ARE TAGS                            
         JZ    XIT                                                              
         LA    RF,HISTDET2                                                      
         EDIT  TAPDTAGS,(3,2(RF)),ALIGN=LEFT NUMBER OF TAGS                     
         J     XIT                                                              
*                                                                               
SDESC60  OC    TAPDDEMS,TAPDDEMS   IF THERE ARE DEMOS                           
         JZ    XIT                                                              
         LA    RF,HISTDET2                                                      
         EDIT  TAPDDEMS,(3,2(RF)),ALIGN=LEFT NUMBER OF DEMOS                    
         J     XIT                                                              
*                                                                               
         USING TAUHD,R4                                                         
SDESC70  L     R4,AIO              FOR ITN PAYMENT                              
         MVI   ELCODE,TAUHELQ      IF USAGE HISTORY ELEMENT                     
         BRAS  RE,GETEL            IS NOT PRESENT                               
         JE    SDESC80             MUST BE AUTO PAYMENT                         
         MVC   HISTDET2+3(4),=C'AUTO'                                           
         J     XIT                                                              
*                                                                               
SDESC80  LA    R5,HISTDET2+3        OTHERWISE PRINT OUT USE #'S                 
         MVC   0(10,R5),SPACES                                                  
         EDIT  TAUHIFUS,(3,(R5)),ZERO=NOBLANK,ALIGN=LEFT                        
         CLI   0(R5),C' '                                                       
         JE    *+12                                                             
         LA    R5,1(R5)                                                         
         J     *-12                                                             
         MVI   0(R5),C'-'                                                       
         LH    RE,TAUHIFUS                                                      
         AH    RE,TAUHINUS                                                      
         SHI   RE,1                                                             
         EDIT  (RE),(3,1(R5)),ZERO=NOBLANK,ALIGN=LEFT                           
         J     XIT                                                              
*                                                                               
         USING TANDD,R4                                                         
SDESC90  L     R4,AIO                                                           
         MVI   ELCODE,TANDELQ      GET NETWORK/CLASS A DETAILS ELEMENT          
         BRAS  RE,GETEL                                                         
         JNE   XIT                                                              
         SR    R3,R3                                                            
         ICM   R3,3,TANDSTUS       FIND TOTAL START USE NUMBER                  
         JNZ   *+8                                                              
         AHI   R3,1                + 1 IF 0 FROM CONVERSION                     
         SPACE                                                                  
         SR    R1,R1                                                            
         ICM   R1,3,TANDSTUL                                                    
         JNZ   *+8                                                              
         AHI   R1,1                                                             
         SPACE                                                                  
         AR    R3,R1                                                            
         BCTR  R3,0                R3=TOTAL START USE NUMBER                    
         LA    R1,HISTDET2                                                      
         EDIT  (R3),(5,(R1)),ALIGN=LEFT                                         
         LA    RF,HISTDET2+4       FIND END OF START USE NUMBER                 
         CLI   0(RF),X'40'                                                      
         JH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'-'                                                       
         AH    R3,TANDUSES         FIND TOTAL END USE NUMBER                    
         AH    R3,TANDUSEL                                                      
         BCTR  R3,0                                                             
         EDIT  (R3),(5,2(RF)),ALIGN=LEFT                                        
*                                                                               
         OC    TANDUSEL,TANDUSEL   IF THERE ARE LIFT USES                       
         JZ    XIT                                                              
         MVI   HISTDET2+10,C'L'    SHOW LIFT USES                               
         LA    R1,HISTDET2+11                                                   
         EDIT  (2,TANDSTUL),(5,(R1)),ALIGN=LEFT  LIFT START USE NUMBER          
         LA    RF,HISTDET2+15      FIND END OF LIFT START USE NUMBER            
         CLI   0(RF),X'40'                                                      
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'-'                                                       
         LH    R3,TANDSTUL         FIND LIFT END USE NUMBER                     
         AH    R3,TANDUSEL                                                      
         BCTR  R3,0                                                             
         EDIT  (R3),(5,2(RF)),ALIGN=LEFT                                        
         J     XIT                                                              
         DROP  R2,R4                                                            
*                                                                               
***********************************************************************         
*        ROUTINE TO SORT HISTORY TABLE ALPHABETICALLY BY USE          *         
***********************************************************************         
                                                                                
SORTHIST NTR1                                                                   
         L     R2,ASORTTAB                                                      
*                                                                               
         USING HISTTABD,RE                                                      
SHIST10  L     RE,AHISTTAB                                                      
*                                                                               
SHIST20  OC    0(HISTLNQ,RE),0(RE)                                              
         JNZ   SHIST30                                                          
         LA    RE,HISTLNQ(RE)                                                   
         J     SHIST20                                                          
*                                                                               
SHIST30  CLI   0(RE),X'FF'                                                      
         JE    XIT                                                              
         LR    RF,RE                                                            
*                                                                               
SHIST40  LA    RF,HISTLNQ(RF)                                                   
         OC    0(HISTLNQ,RF),0(RF)                                              
         JZ    SHIST40                                                          
*                                                                               
         CLI   0(RF),X'FF'                                                      
         JE    SHIST50                                                          
*                                                                               
         CLC   HISTDETS,HISTDETS-HISTTABD(RF)                                   
         JNH   SHIST40                                                          
         LR    RE,RF                                                            
         J     SHIST40                                                          
*                                                                               
SHIST50  MVC   0(HISTLNQ,R2),0(RE)                                              
         LA    R2,HISTLNQ(R2)                                                   
         MVI   0(R2),X'FF'                                                      
*                                                                               
         XC    0(HISTLNQ,RE),0(RE)                                              
         J     SHIST10                                                          
         DROP  RE                                                               
*                                                                               
***********************************************************************         
*        ROUTINE TO OUTPUT HISTORY DETAILS                            *         
*        ON ENTRY ... R2=A(CURRENT ENTRY IN HISTORY TABLE)            *         
***********************************************************************         
                                                                                
         USING HISTTABD,R2                                                      
OUTHIST  NTR1                                                                   
         CLI   0(R2),X'FF'                                                      
         JE    OHIST10                                                          
         GOTO1 DATCON,DMCB,(1,HISTCYCS),(8,BLOCK)                               
         MVI   BLOCK+8,C'-'                                                     
         GOTO1 DATCON,DMCB,(1,HISTCYCE),(8,BLOCK+9)                             
         GOTO1 OUTPDOWN,DMCB,(C'T',BLOCK),17                                    
*                                                                               
         GOTO1 OUTPDOWN,DMCB,(C'T',HISTDETS),L'HISTDETS                         
         J     XIT                                                              
*                                                                               
OHIST10  MVC   BLOCK(L'SPACES),SPACES                                           
         GOTO1 OUTPDOWN,DMCB,(C'T',BLOCK),17                                    
         GOTO1 OUTPDOWN,DMCB,(C'T',BLOCK),L'HISTDETS                            
         J     XIT                                                              
         DROP  R2                                                               
*                                                                               
***********************************************************************         
*        ROUTINE TO OUTPUT CONTRACT DETAILS                           *         
*        ON ENTRY ... R2=A(CURRENT ENTRY IN CONTRACT TABLE)           *         
***********************************************************************         
                                                                                
         USING CONTTABD,R3                                                      
OUTCONT  NTR1                                                                   
         CLI   0(R3),X'FF'                                                      
         JE    XIT                                                              
         GOTO1 OUTPDOWN,DMCB,(C'T',CONTENT),L'CONTENT                           
         J     XIT                                                              
         DROP  R3,R5                                                            
         EJECT                                                                  
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
CIDHEAD  DC    C'COMM ID'                                                       
TITHEAD  DC    C'TITLE'                                                         
CLNHEAD  DC    C'LEN'                                                           
LFTHEAD  DC    C'LFT ID'                                                        
LLNHEAD  DC    C'LFT LEN'                                                       
EXPHEAD  DC    C'1ST EXPIRY'                                                    
PCYHEAD  DC    C'CYCLE'                                                         
PDTHEAD  DC    C'USE DETAILS'                                                   
CNTHEAD  DC    C'CONTRACT DETAILS'                                              
*                                                                               
CNTID    DC    C'ID: '                                                          
CNTTERM  DC    C'TERM: '                                                        
CNTDESC  DC    C'DESCRIPTION: '                                                 
CNTTYPE  DC    C'TYPE: '                                                        
CNTCOMT  DC    C'COMMENT: '                                                     
*                                                                               
CTYPETAB DC    AL1(TARDTYCG),CL29'CELEBRITY/OVERSCALE GUARANTEE'                
         DC    AL1(TARDTYCL),CL29'CHARACTER/NAME/LIKENESS'                      
         DC    AL1(TARDTYML),CL29'MUSIC LICENSE'                                
         DC    AL1(TARDTYNM),CL29'NEEDLEDROP MUSIC'                             
         DC    AL1(TARDTYOT),CL29'OTHER'                                        
         DC    AL1(TARDTYPR),CL29'PROPERTY RELEASE'                             
         DC    AL1(TARDTYMR),CL29'MASTER RECORDING'                             
         DC    AL1(TARDTYSF),CL29'STOCK FOOTAGE AGREEMENT'                      
         DC    AL1(TARDTYNU),CL29'NON-UNION AGREEMENT'                          
         DC    X'FF'                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        WORKING STORAGE                                                        
***********************************************************************         
                                                                                
MYD      DSECT                                                                  
OPTIONS  DS    X                   OPTIONS STATUS                               
OPTGROUP EQU   X'80'               GROUP OPTION                                 
*                                                                               
FILTERS  DS    0C                  REPORT FILTERS                               
FLTPST   DS    CL(L'TAPDCYCS)      CYCLE START FILTER                           
FLTPEN   DS    CL(L'TAPDCYCE)      CYCLE END FILTER                             
FPDLNQ   EQU   *-FILTERS                                                        
*                                                                               
EXPDATE  DS    XL3                 EARLIEST EXPIRATION DATE                     
OUTEXP   DS    CL8                 EARLIEST EXPIRATION DATE                     
*                                                                               
LIFTID   DS    CL(L'TALFLID)       LIFT ID                                      
LIFTSEC  DS    CL3                 LIFT LENGTH                                  
*                                                                               
LASTCLI  DS    CL(L'TICLI)         LAST CLIENT REPORTED ON                      
LASTPRD  DS    CL(L'TIPRD)         LAST PRODUCT REPORTED ON                     
*                                                                               
NAMEH    DS    CL8                 NAME HEADER                                  
NAME     DS    CL36                NAME FIELD (CLIENT/PRODUCT)                  
*                                                                               
LCNTKEY  DS    XL(TLCNPVER-TLCNPD)                                              
MYDLNQ   EQU   *-MYD                                                            
*                                                                               
DLCB     DS    CL(DLCBXLX)         DOWNLOAD BLOCK                               
*                                                                               
AHISTTAB DS    A                   A(HISTORY TABLE)                             
ASORTTAB DS    A                   A(SORTED HISTORY TABLE)                      
ACONTTAB DS    A                   A(CONTRACT TABLE)                            
         EJECT                                                                  
       ++INCLUDE TAREPFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TAREPDED                                                       
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
         EJECT                                                                  
***********************************************************************         
*        DSECT TO COVER HISTORY TABLE                                 *         
***********************************************************************         
                                                                                
HISTTABD DSECT                                                                  
HISTCYCS DS    XL3                 CYCLE START DATE                             
HISTCYCE DS    XL3                 CYCLE END DATE                               
HISTDETS DS    0XL48                                                            
HISTDET1 DS    XL16                USE NAME                                     
HISTDET2 DS    CL32                USE DETAILS                                  
HISTLNQ  EQU   *-HISTTABD                                                       
*                                                                               
***********************************************************************         
*        DSECT TO COVER CONTRACT TABLE                                *         
***********************************************************************         
                                                                                
CONTTABD DSECT                                                                  
CONTENT  DS    CL73                CONTRACT ENTRY                               
CONTLNQ  EQU   *-CONTTABD                                                       
*                                                                               
***********************************************************************         
*        DSECT TO COVER CONTRACT TYPE TABLE                           *         
***********************************************************************         
                                                                                
CTYTABD  DSECT                                                                  
CTYTYPE  DS    AL1                 TYPE                                         
CTYDESC  DS    CL29                DESCRIPTION                                  
CTYLNQ   EQU   *-CTYTABD                                                        
*                                                                               
***********************************************************************         
*        DSECT TO COVER NMOD GRABBED STORAGE                          *         
***********************************************************************         
                                                                                
HISTTAB  DS    200XL(HISTLNQ)                                                   
SORTTAB  DS    200XL(HISTLNQ)                                                   
CONTTAB  DS    200XL(CONTLNQ)                                                   
TBLLNQ   EQU   *-HISTTAB                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002TAREP5D   04/03/09'                                      
         END                                                                    
