*          DATA SET TAGENFC    AT LEVEL 003 AS OF 04/16/14                      
*PHASE T702FCA,*                                                                
         TITLE 'T702FC - STAFF2 MAINTENANCE'                                    
T702FC   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 TBLLNQ,T702FC,R7                                                 
         LR    RE,RC                                                            
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         ST    RE,ACTBL            SAVE ADDR OF INITIAL AGY/CLI TABLE           
         AHI   RE,ACTBLNQ/2                                                     
         AHI   RE,ACTBLNQ/2                                                     
         ST    RE,ACTBLX                                                        
         AHI   RE,1                                                             
         ST    RE,AADDTBL          SAVE ADDR OF AGY/CLI ADD TABLE               
         AHI   RE,ACTBLNQ/2                                                     
         AHI   RE,ACTBLNQ/2                                                     
         ST    RE,AADDTBLX                                                      
*                                                                               
         GOTO1 INITIAL,DMCB,PFTAB  INITIALIZE OVERLAY                           
*                                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VK                                                               
         CLI   MODE,DISPREC        DISPLAY THE RECORD                           
         BE    DR                                                               
         CLI   MODE,VALREC         VALIDATE THE RECORD                          
         BE    VR                                                               
         CLI   MODE,XRECPUT        DISPLAY THE RECORD                           
         BE    DR                                                               
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*        DISPLAY THE KEY                                              *         
***********************************************************************         
                                                                                
         USING TLSTD,R4                                                         
DK       LA    R4,KEY              R4=A(STAFF KEY)                              
*                                                                               
         MVC   WKGSTAF(3),=C'ALL'  DISPLAY C'ALL'                               
         OC    TLSTSTAF,TLSTSTAF   IF STAFF CODE IS ZEROS                       
         BZ    DK10                                                             
         MVC   WKGSTAF,TLSTSTAF    ELSE DISPLAY STAFF CODE                      
DK10     NI    WKGSTAFH+4,X'FF'-X'20'                                           
         MVI   WKGSTAFH+5,L'WKGSTAF                                             
         OI    WKGSTAFH+6,X'80'                                                 
*                                                                               
         XC    WORK,WORK           SET UP WORK AREA FOR USERVAL CALL            
         MVC   WORK+8(2),TLSTUSER                                               
         GOTO1 USERVAL,DMCB,(X'E0',WORK),WKGUNAMH                               
         MVC   WKGUSER,TGUSERID    MOVE EBCDIC USER ID TO SCREEN                
         NI    WKGUSERH+4,X'FF'-X'20'                                           
         MVI   WKGUSERH+5,L'WKGUSER                                             
         OI    WKGUSERH+6,X'80'                                                 
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*        VALIDATE THE KEY                                             *         
***********************************************************************         
                                                                                
VK       GOTO1 FLDVAL,DMCB,(X'42',WKGUSERH),(X'80',WKGSTAFH)                    
         BE    VKX                                                              
*                                                                               
         GOTO1 FLDVAL,DMCB,(X'03',WKGUNAMH),1                                   
         GOTO1 FLDVAL,DMCB,(X'03',WKGCLGH),1                                    
         GOTO1 FLDVAL,DMCB,(X'03',WKGAAGYH),(X'80',WKGDCLGH)                    
         GOTO1 FLDVAL,DMCB,(X'03',WKGFRSTH),WKGLSTH                             
         XC    LASTDSP,LASTDSP                                                  
*                                                                               
         GOTO1 USERVAL,DMCB,(X'40',WKGUSERH),WKGUNAMH                           
*                                                                               
         GOTO1 RECVAL,DMCB,TLSTCDQ,(X'20',WKGSTAFH)                             
         MVC   SVKEY,KEY                                                        
*                                                                               
         CLC   TWAORIG,TGUSER      IF USER IS SAME AS RECORD THEN OK            
         BNE   VK10                                                             
         CLC   TGCTSTAF,TGSTAF                                                  
         BE    VKX                                                              
*                                                                               
         USING TASTD,R4                                                         
VK10     L     R4,AIO              ELSE POINT TO STAFF ELEMENT                  
         MVI   ELCODE,TASTELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 STAFVAL,DMCB,TASTTYPE,TGCTSTLV  TEST USER CAN ACCESS             
         BNE   ERRLOCK                         THIS STAFF TYPE                  
         DROP  R4                                                               
*                                                                               
VKX      GOTO1 FLDVAL,DMCB,(X'20',WKGUSERH),(X'80',WKGSTAFH)                    
         MVC   MYSTAF,TGSTAF       SAVE GLOBAL STAFF CODE                       
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        DISPLAY THE RECORD                                           *         
***********************************************************************         
                                                                                
DR       BAS   RE,GETSTAFF         GET STAFF RECORD                             
*                                                                               
         GOTO1 FLDVAL,DMCB,(X'03',WKGCLGH),1                                    
         GOTO1 FLDVAL,DMCB,(X'23',WKGAAGYH),(X'80',WKGDCLGH)                    
         GOTO1 FLDVAL,DMCB,(X'23',WKGFRSTH),WKGLSTH                             
*                                                                               
         USING TASTD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TASTELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   WKGCLG,TASTCLG      DISPLAY CLIENT GROUP ACCESS                  
         DROP  R4                                                               
*                                                                               
         LA    R2,WKGFRSTH         R2=A(FIRST DEFAULT/DELETE FIELD)             
         LA    R5,WKGLSTH          R5=A(LAST DEFAULT/DELETE FIELD)              
*                                                                               
         XC    FRSTDSP,FRSTDSP     INITIALIZE ALL VARIABLES                     
         XC    DEFAGY,DEFAGY                                                    
*                                                                               
         BAS   RE,BLDTBL           BUILD INITIAL AGENCY/CLIENT TABLE            
*                                                                               
         MVI   BS2LSSEQ,1          RECALL INITIAL AGENCY/CLIENT TABLE           
DR10     BAS   RE,GETTBL           VIA WSSVR                                    
*                                                                               
         USING ACTBLD,R3                                                        
         L     R3,ACTBL            R3=A(INITIAL AGENCY/CLIENT TABLE)            
DR20     CLI   0(R3),X'FF'         IF AT END OF TABLE, DONE                     
         BE    NOMORE                                                           
*                                                                               
         CR    R2,R5               IF PAST END OF SCREEN, DONE                  
         BH    MORE                                                             
*                                                                               
         C     R3,ACTBLX           IF REACHED LAST ENTRY IN WSSVR               
         BNL   DR10                BLOCK, GO GET NEXT WSSVR BLOCK               
*                                                                               
         CLC   LASTDSP,0(R3)       IF DISPLAYED LAST TIME, DON'T                
         BNL   DR60                DISPLAY AGAIN                                
*                                                                               
         OC    FRSTDSP,FRSTDSP     IF FIRST ENTRY DISPLAYED ON SCREEN,          
         BNZ   DR30                SAVE IT                                      
         MVC   FRSTDSP,0(R3)                                                    
         TM    ACSTAT,TAVANDEF     AND IF THIS IS DEFAULT AGENCY                
         BO    DR30                PUT * IN INPUT FIELD                         
         MVI   8(R2),C'*'                                                       
         MVI   5(R2),1                                                          
         MVC   DEFAGY,ACAGY                                                     
*                                                                               
DR30     ZIC   RE,0(R2)            PUT AGENCY CODE TO SCREEN                    
         AR    R2,RE                                                            
         MVC   8(L'ACAGY,R2),ACAGY                                              
         MVI   5(R2),L'ACAGY                                                    
*                                                                               
DR40     ZIC   RE,0(R2)            PUT CLIENT CODE TO SCREEN                    
         AR    R2,RE                                                            
         OC    ACCLI,ACCLI                                                      
         BZ    DR50                                                             
         MVC   8(L'ACCLI,R2),ACCLI                                              
         MVI   5(R2),L'ACCLI                                                    
*                                                                               
DR50     MVC   LASTDSP,0(R3)       SAVE LAST DISPLAYED ENTRY                    
*                                                                               
         ZIC   RE,0(R2)            BUMP TO NEXT INPUT FIELD                     
         AR    R2,RE                                                            
DR60     LA    R3,ACLNQ(R3)        BUMP TO NEXT AGY/CLI TABLE ENTRY             
         B     DR20                                                             
         DROP  R3                                                               
*                                                                               
***********************************************************************         
*        ROUTINE TO GET STAFF RECORD                                  *         
***********************************************************************         
                                                                                
GETSTAFF NTR1                                                                   
         MVC   KEY,SVKEY                                                        
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLSTKEY),KEYSAVE                                           
         BNE   NO                                                               
         GOTO1 GETREC                                                           
                                                                                
         USING TASTD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TASTELQ                                                   
         BAS   RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   TGSTEQU,TASTTYPE                                                 
         B     YES                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*        VALIDATE THE RECORD                                          *         
***********************************************************************         
                                                                                
VR       BAS   RE,GETSTAFF         GET STAFF RECORD                             
         BAS   RE,BLDTBL           BUILD INITIAL AGENCY/CLIENT TABLE            
         BAS   RE,CADDTBL          CLEAR ADDITIONS TABLE                        
*                                                                               
         XC    TGAGY,TGAGY         INITIALIZE VARIABLES                         
         XC    TGCLI,TGCLI                                                      
*                                                                               
         MVC   TGCLG,WKGCLG        SAVE INTIAL CLIENT GROUP SETTING             
*                                                                               
         CLI   WKGACLGH+5,0        IF ADDING A CLIENT GROUP                     
         BE    VR10                SAVE IN TGCLG                                
         OC    TGCLG,TGCLG         CLIENT GROUP CANNOT BE PREV SET              
         BNZ   ERRSTFCG                                                         
         LA    R2,WKGACLGH                                                      
         GOTO1 RECVAL,DMCB,TLCGCDQ,(R2)                                         
*                                                                               
VR10     CLI   WKGDCLGH+5,0        IF DELETING A CLIENT GROUP                   
         BE    VR20                                                             
         CLC   WKGDCLG,WKGCLG      MUST MATCH CLIENT GROUP THAT                 
         BNE   ERRDELCG            IS ALREADY ON RECORD                         
         XC    TGCLG,TGCLG                                                      
*                                                                               
VR20     GOTO1 FLDVAL,DMCB,(X'80',WKGAAGYH),(X'80',WKGACLIH)                    
         BE    VR50                                                             
*                                                                               
         CLI   WKGAAGYH+5,0        IF ADDING AN AGENCY                          
         BE    VR30                SAVE IT IN TGAGY                             
         LA    R2,WKGAAGYH                                                      
         GOTO1 RECVAL,DMCB,TLAYCDQ,(R2)                                         
*                                                                               
         OC    TGCLG,TGCLG         IF CLIENT GROUP IS DEFINED                   
         BZ    VR30                AGENCY MUST BE IN IT                         
         BAS   RE,RDCLIGRP                                                      
         CLC   KEY(TLCLGCLI-TLCLPD),KEYSAVE                                     
         BNE   ERRAGYCG                                                         
*                                                                               
VR30     CLI   WKGACLIH+5,0        IF ADDING A CLIENT                           
         BE    VR40                SAVE IT IN TGCLI                             
         CLI   WKGAAGYH+5,0                                                     
         BE    AGYMISS             AGENCY IS REQUIRED                           
         LA    R2,WKGACLIH                                                      
         GOTO1 RECVAL,DMCB,TLCLCDQ,(R2)                                         
*                                                                               
         OC    TGCLG,TGCLG         IF CLIENT GROUP IS DEFINED                   
         BZ    VR40                AGENCY/CLIENT MUST BE IN IT                  
         BAS   RE,RDCLIGRP                                                      
         CLC   KEY(L'TLCLPKEY),KEYSAVE                                          
         BNE   ERRCLICG                                                         
*                                                                               
VR40     BAS   RE,ALRDYON          AGY/CLI CANNOT ALREADY BE ON STAFF           
*                                                                               
         CLI   WKGACLGH+5,0        IF ADDING CLIENT GROUP                       
         BE    VR50                                                             
         OC    TGCLI,TGCLI         AND SPECIFIC CLIENT                          
         BZ    VR50                                                             
         XC    TGAGY,TGAGY         LET CLIENT GROUP ROUTINE HANDLE              
         XC    TGCLI,TGCLI         ADDING THE AGENCY AND CLIENT                 
*                                                                               
VR50     BAS   RE,PRODEL           PROCESS DELETED AGENCY/CLIENT(S)             
         BAS   RE,PRODEF           PROCESS DEFAULT AGENCY                       
         BAS   RE,PROADD           PROCESS ADDITIONS                            
         BAS   RE,PROCLG           PROCESS NEW CLIENT GROUP                     
*                                                                               
         BAS   RE,GETSTAFF          GET STAFF RECORD                            
         GOTO1 SAVPTRS,DMCB,SVSTPTR AND SAVE POINTERS                           
*                                                                               
         BAS   RE,UPDRECS          BUILD AND UPDATE STAFF RECORDS               
         MVI   IOOPT,C'Y'                                                       
         B     XIT                                                              
*                                                                               
***********************************************************************         
*        ROUTINE TO CLEAR ADDITIONS TABLE                             *         
***********************************************************************         
                                                                                
CADDTBL  NTR1                                                                   
         L     R3,AADDTBL          R3=(AGENCY/CLIENT ADD TABLE)                 
         LR    RE,R3                                                            
         L     RF,AADDTBLX                                                      
CAT10    XC    0(250,RE),0(RE)                                                  
         LA    RE,250(RE)                                                       
         CR    RE,RF                                                            
         BL    CAT10                                                            
         MVI   0(R3),X'FF'                                                      
         B     XIT                                                              
*                                                                               
***********************************************************************         
*        ROUTINE TO READ CLIENT GROUP KEY                             *         
***********************************************************************         
                                                                                
RDCLIGRP NTR1                                                                   
         USING TLCLPD,R4                                                        
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   TLCLPCD,TLCLGCDQ                                                 
         MVC   TLCLGCLG,TGCLG                                                   
         MVC   TLCLGAGY,TGAGY                                                   
         MVC   TLCLGCLI,TGCLI                                                   
*                                                                               
         OC    TLCLGCLG,SPACES                                                  
         OC    TLCLGAGY,SPACES                                                  
*                                                                               
         OC    TLCLGCLI,TLCLGCLI                                                
         BZ    *+10                                                             
         OC    TLCLGCLI,SPACES                                                  
*                                                                               
         GOTO1 HIGH                                                             
         B     XIT                                                              
         DROP  R4                                                               
*                                                                               
***********************************************************************         
*        ROUTINE TO CHECK IS AGENCY/CLIENT IS ALREADY ON STAFF RECORD *         
***********************************************************************         
                                                                                
ALRDYON  NTR1                                                                   
         MVI   BS2LSSEQ,1          RECALL INITIAL AGENCY/CLIENT TABLE           
AO10     BAS   RE,GETTBL           VIA WSSVR                                    
*                                                                               
         USING ACTBLD,R3                                                        
         L     R3,ACTBL            R3=A(INITIAL AGENCY/CLIENT TABLE)            
AO20     CLI   0(R3),X'FF'         IF AT END OF TABLE, DONE                     
         BE    XIT                                                              
*                                                                               
         C     R3,ACTBLX           IF REACHED LAST ENTRY IN WSSVR               
         BNL   AO10                BLOCK, GO GET NEXT WSSVR BLOCK               
*                                                                               
         CLC   TGAGY,ACAGY         IF AGENCY ALREADY IN TABLE                   
         BNE   AO30                                                             
         OC    TGCLI,TGCLI         AND NOT ADDING A CLIENT                      
         BZ    ERRAINV             RETURN ERROR                                 
*                                                                               
         CLC   TGCLI,ACCLI         IF AGENCY/CLIENT ALREADY IN TABLE            
         BE    ERRCINV             RETURN ERROR                                 
*                                                                               
AO30     LA    R3,ACLNQ(R3)        BUMP TO NEXT AGY/CLI TABLE ENTRY             
         B     AO20                                                             
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO DELETE AGENCY/CLIENT(S)                           *         
***********************************************************************         
                                                                                
PRODEL   NTR1                                                                   
         GOTO1 FLDVAL,DMCB,(X'40',WKGFRSTH),WKGLSTH                             
         BE    PDEL80                                                           
*                                                                               
         LA    R2,WKGFRSTH       R2=A(FIRST DEFAULT/DELETE FIELD)               
         LA    R5,WKGLSTH        R5=A(LAST DEFAULT/DELETE FIELD)                
*                                                                               
PDEL10   CR    R2,R5                                                            
         BNL   PDEL80                                                           
         CLI   8(R2),C'D'        IF ENTRY IS NOT SET FOR DELETE                 
         BE    PDEL20                                                           
         ZIC   RE,0(R2)          BUMP TO NEXT AGENCY FIELD                      
         AR    R2,RE                                                            
         ZIC   RE,0(R2)          BUMP TO NEXT CLIENT FIELD                      
         AR    R2,RE                                                            
         ZIC   RE,0(R2)          BUMP TO NEXT DEFAULT/DELETE FIELD              
         AR    R2,RE                                                            
         B     PDEL10                                                           
*                                                                               
PDEL20   ZIC   RE,0(R2)          IF ENTRY IS SET FOR DELETE                     
         AR    RE,R2                                                            
         CLI   5(RE),0           ENSURE AGENCY IS PRESENT                       
         BE    ERRINV                                                           
*                                                                               
PDEL30   MVI   8(R2),0           CLEAR DELETE INDICATOR FROM SCREEN             
         MVI   5(R2),0                                                          
*                                                                               
         LR    R1,RE             R1=A(AGENCY FIELD)                             
         OC    8(6,R1),SPACES    PAD IT WITH SPACES                             
*                                                                               
         ZIC   R2,0(R1)                                                         
         AR    R2,R1             R2=A(CLIENT FIELD)                             
         CLI   5(R2),0           IF CLIENT INPUT PRESENT                        
         BE    PDEL40                                                           
         OC    8(6,R2),SPACES    PAD IT WITH SPACES                             
*                                                                               
PDEL40   MVI   BS2LSSEQ,1        RECALL INITIAL AGENCY/CLIENT TABLE             
PDEL50   BAS   RE,GETTBL         VIA WSSVR                                      
*                                                                               
         USING ACTBLD,R3                                                        
         L     R3,ACTBL          R3=A(INITIAL AGY/CLI TABLE)                    
PDEL60   CLI   0(R3),X'FF'       MUST FIND AGENCY/CLIENT IN THE TABLE           
         BNE   *+6                                                              
         DC    H'00'                                                            
*                                                                               
         C     R3,ACTBLX         IF REACHED LAST ENTRY IN WSSVR                 
         BNL   PDEL50            BLOCK, GO GET NEXT WSSVR BLOCK                 
*                                                                               
         CLC   ACAGY,8(R1)       WHEN ENTRY TO DELETE IS FOUND                  
         BNE   PDEL70            ERASE IT                                       
         CLC   ACCLI,8(R2)                                                      
         BNE   PDEL70                                                           
         XC    0(ACLNQ,R3),0(R3)                                                
         BAS   RE,CHGTBL                                                        
*                                                                               
         ZIC   RE,0(R2)          BUMP TO NEXT DEFAULT/DELETE FIELD              
         AR    R2,RE                                                            
         B     PDEL10            NOW GO LOOK FOR MORE DELETE INPUT              
*                                                                               
PDEL70   LA    R3,ACLNQ(R3)                                                     
         B     PDEL60                                                           
         DROP  R3                                                               
*                                                                               
***********************************************************************         
                                                                                
PDEL80   CLI   WKGACLGH+5,0      IF ADDING A CLIENT GROUP                       
         BZ    XIT                                                              
*                                                                               
         MVI   BS2LSSEQ,1        RECALL INITIAL AGENCY/CLIENT TABLE             
         BAS   RE,GETTBL         VIA WSSVR                                      
*                                                                               
         USING ACTBLD,R3                                                        
         L     R3,ACTBL          R2=A(INITIAL AGENCY/CLIENT TABLE)              
         CLI   0(R3),X'FF'       EXIT IF AGENCY/CLIENT LIMITS WERE              
         BE    XIT               NOT ALREADY DEFINED                            
*                                                                               
         USING TLCLPD,R4                                                        
         LA    R4,KEY            R4=A(CLIENT GROUP PASSIVE KEY)                 
*                                                                               
PDEL90   CLI   ACSTAT,X'FF'      READ ALL EXISTING AGENCY/CLIENTS               
         BE    XIT                                                              
*                                                                               
         C     R3,ACTBLX         IF REACHED LAST ENTRY IN WSSVR                 
         BNE   PDEL100           BLOCK, GO GET NEXT WSSVR BLOCK                 
         BAS   RE,GETTBL                                                        
         L     R3,ACTBL                                                         
         B     PDEL90                                                           
*                                                                               
PDEL100  OC    0(ACLNQ,R3),0(R3) IGNORE DELETED ENTRIES                         
         BZ    PDEL120                                                          
*                                                                               
         OC    ACCLI,ACCLI       IF CLIENT IS NOT DEFINED FOR                   
         BZ    PDEL110           AGENCY, DELETE AGENCY                          
*                                                                               
         XC    KEY,KEY           IF CLIENT IS DEFINED                           
         MVI   TLCLPD,TLCLGCDQ   SEE IF IT FITS WITHIN NEW CLIENT               
         MVC   TLCLGCLG,TGCLG    GROUP                                          
         MVC   TLCLGAGY,ACAGY                                                   
         MVC   TLCLGCLI,ACCLI                                                   
         GOTO1 HIGH                                                             
         CLC   TLCLPKEY,KEYSAVE                                                 
         BE    PDEL120                                                          
PDEL110  XC    0(ACLNQ,R3),0(R3) IF IT DOESN'T, DELETE ENTRY FROM               
         BAS   RE,CHGTBL         INTIAL TABLE                                   
*                                                                               
PDEL120  LA    R3,ACLNQ(R3)                                                     
         B     PDEL90                                                           
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO PROCESS DEFAULT AGENCY                            *         
***********************************************************************         
                                                                                
PRODEF   NTR1                                                                   
         GOTO1 FLDVAL,DMCB,(X'40',WKGFRSTH),WKGLSTH                             
         BE    XIT                                                              
*                                                                               
         BAS   RE,SAVIDEF                                                       
         BAS   RE,SCNDINP        SCAN FOR DEFAULT AGENCY INPUT                  
         BAS   RE,SAVNDEF        SAVE NEW DEFAULT INTO ADDITIONS TABLE          
         BAS   RE,SAVODEF        SAVE OLD DEFAULT INTO ADDITIONS TABLE          
         B     XIT                                                              
*                                                                               
***********************************************************************         
*        ROUTINE TO SAVE INITIAL DEFAULT AGENCY INTO DEFAGY           *         
***********************************************************************         
                                                                                
SAVIDEF  NTR1                                                                   
         XC    DEFAGY,DEFAGY     SET DEFAGY AS ORIGINAL DEFAULT AGENCY          
*                                                                               
         MVI   BS2LSSEQ,1        RECALL INITIAL AGENCY/CLIENT TABLE             
SID10    BAS   RE,GETTBL         VIA WSSVR                                      
*                                                                               
         USING ACTBLD,R3                                                        
         L     R3,ACTBL          R3=A(INITIAL AGY/CLI TABLE)                    
*                                                                               
SID20    CLI   0(R3),X'FF'       IF AT END OF INTIAL AGENCY/CLIENT              
         BE    XIT               TABLE, DEFAULT AGENCY WAS NOT SET              
*                                                                               
         C     R3,ACTBLX         IF REACHED LAST ENTRY IN WSSVR                 
         BNL   SID10             BLOCK, GO GET NEXT WSSVR BLOCK                 
*                                                                               
         TM    ACSTAT,TAVANDEF   IF AGENCY NOT SET AS DEFAULT                   
         BZ    SID30                                                            
         LA    R3,ACLNQ(R3)      BUMP TO NEXT INITIAL ENTRY                     
         B     SID20                                                            
*                                                                               
SID30    MVC   DEFAGY,ACAGY      SAVE INITIAL DEFAULT AGENCY                    
         DROP  R3                                                               
*                                                                               
         CLC   DEFAGY,FRSTDSP+ACAGY-ACTBLD                                      
         BL    XIT                                                              
         XC    DEFAGY,DEFAGY                                                    
         B     XIT                                                              
*                                                                               
***********************************************************************         
*        ROUTINE TO SCAN FOR NEW DEFAULT AGENCY INPUT                 *         
***********************************************************************         
                                                                                
SCNDINP  NTR1                                                                   
         XR    R1,R1             R1=NUMBER OF ENTRIES MARKED DEFAULT            
*                                                                               
         LA    R2,WKGFRSTH       R2=A(FIRST DEFAULT/DELETE FIELD)               
         LA    R5,WKGLSTH        R5=A(LAST DEFAULT/DELETE FIELD)                
*                                                                               
SDI10    CR    R2,R5             CHECK ALL DEFAULT/DELETE FIELDS                
         BNL   XIT                                                              
*                                                                               
         LR    R3,R2             BUMP R3 TO AGENCY FIELD                        
         ZIC   RE,0(R3)                                                         
         AR    R3,RE                                                            
*                                                                               
         CLI   8(R2),C'*'        IF ENTRY IS SET FOR DEFAULT                    
         BNE   SDI20                                                            
         LTR   R1,R1             ENSURE DEFAULT NOT ALREADY SET                 
         BNE   ERRINV            ON THIS SCREEN                                 
         CLI   5(R3),0           ENSURE AGENCY INPUT IS PRESENT                 
         BE    ERRINV                                                           
         MVC   DEFAGY,8(R3)      SAVE NEW DEFAULT AGENCY                        
         OC    DEFAGY,SPACES                                                    
         LHI   R1,1                                                             
*                                                                               
SDI20    LR    R2,R3             BUMP TO CLIENT FIELD                           
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
*                                                                               
         ZIC   RE,0(R2)          BUMP TO NEXT DEFAULT/DELETE FIELD              
         AR    R2,RE                                                            
         B     SDI10                                                            
*                                                                               
***********************************************************************         
*        ROUTINE TO TRANSFER NEW DEFAULT AGENCY ENTRIES INTO THE      *         
*        ADDITIONS TABLE                                              *         
***********************************************************************         
                                                                                
SAVNDEF  NTR1                                                                   
         L     R5,AADDTBL        R5=A(ADDITIONS TABLE)                          
*                                                                               
         MVI   BS2LSSEQ,1        RECALL INITIAL AGENCY/CLIENT TABLE             
SND10    BAS   RE,GETTBL         VIA WSSVR                                      
*                                                                               
         USING ACTBLD,R3                                                        
         L     R3,ACTBL          R3=A(INITIAL AGY/CLI TABLE)                    
*                                                                               
SND20    CLI   0(R3),X'FF'       IF AT END OF INTIAL AGENCY/CLIENT              
         BE    XIT               TABLE, ALL DEFAULTS TRANSFERRED                
*                                                                               
         C     R3,ACTBLX         IF REACHED LAST ENTRY IN WSSVR                 
         BNL   SND10             BLOCK, GO GET NEXT WSSVR BLOCK                 
*                                                                               
         CLC   ACAGY,DEFAGY      IF AGENCY MATCHES THE DEFAULT AGENCY           
         BNE   SND40                                                            
         CLC   TGAGY,DEFAGY      AND ADDING ANOTHER CLIENT FOR THIS             
         BNE   SND30             DEFAULT AGENCY                                 
         CLC   ACCLI,TGCLI       ADD IT TO ADDITIONS TABLE                      
         BNH   SND30                                                            
         MVI   ACSTAT-ACTBLD(R5),0                                              
         MVC   ACAGY-ACTBLD(L'ACAGY,R5),TGAGY                                   
         MVC   ACCLI-ACTBLD(L'ACCLI,R5),TGCLI                                   
         XC    TGAGY,TGAGY                                                      
         XC    TGCLI,TGCLI                                                      
         LA    R5,ACLNQ(R5)                                                     
         MVI   0(R5),X'FF'                                                      
*                                                                               
SND30    MVC   0(ACLNQ,R5),0(R3) ADD AGY/CLI ENTRY TO ADD TABLE                 
         MVI   0(R5),0           AS DEFAULT AGENCY                              
         XC    0(ACLNQ,R3),0(R3) AND DELETE OLD ENTRY FROM INITIAL              
         BAS   RE,CHGTBL         TABLE                                          
*                                                                               
         LA    R5,ACLNQ(R5)      BUMP TO NEXT NEW ENTRY                         
         MVI   0(R5),X'FF'                                                      
*                                                                               
SND40    LA    R3,ACLNQ(R3)      BUMP TO NEXT INITIAL ENTRY                     
         B     SND20                                                            
         DROP  R3                                                               
*                                                                               
***********************************************************************         
*        ROUTINE TO TRANSFER OLD DEFAULT AGENCY ENTRIES INTO THE      *         
*        ADDITIONS TABLE                                              *         
***********************************************************************         
                                                                                
SAVODEF  NTR1                                                                   
         L     R5,AADDTBL        R5=A(ADDITIONS TABLE)                          
SOD10    CLI   0(R5),X'FF'       R5=A(FIRST EMPTY ENTRY IN ADDITIONS            
         BE    SOD20             TABLE)                                         
         LA    R5,ACLNQ(R5)                                                     
         B     SOD10                                                            
*                                                                               
SOD20    MVI   BS2LSSEQ,1        RECALL INITIAL AGENCY/CLIENT TABLE             
SOD30    BAS   RE,GETTBL         VIA WSSVR                                      
*                                                                               
         USING ACTBLD,R3                                                        
         L     R3,ACTBL          R3=A(INITIAL AGY/CLI TABLE)                    
*                                                                               
SOD40    CLI   0(R3),X'FF'       IF AT END OF INTIAL AGENCY/CLIENT              
         BE    XIT               TABLE, ALL OLD DEFAULTS TRANSFERRED            
*                                                                               
         C     R3,ACTBLX         IF REACHED LAST ENTRY IN WSSVR                 
         BNL   SOD30             BLOCK, GO GET NEXT WSSVR BLOCK                 
*                                                                               
         OC    0(ACLNQ,R3),0(R3) SKIP DELETED ENTRIES                           
         BZ    SOD110                                                           
*                                                                               
         TM    ACSTAT,TAVANDEF   IF AGENCY WAS NOT THE DEFAULT,                 
         BO    SOD110            SKIP IT                                        
*                                                                               
         OC    TGAGY,TGAGY       IF ADDING A NEW AGENCY/CLIENT                  
         BZ    SOD100            SIMULTANEOUSLY                                 
         CLC   TGAGY,ACAGY       AND NEW AGENCY IS LOWER THAN OLD               
         BL    SOD50             DEFAULT, GO ADD IT                             
         CLC   TGAGY,ACAGY       IF NEW AGENCY MATCHES THE OLD DEFAULT          
         BNE   SOD100            AND CLIENT IS LOWER THAN OLD CLIENT,           
         CLC   ACCLI,TGCLI       GO ADD IT                                      
         BNH   SOD100                                                           
         B     SOD90                                                            
*                                                                               
SOD50    OC    TGCLG,TGCLG       IF CLIENT GROUP IS ALREADY DEFINED             
         BZ    SOD90             OR BEING ADDED                                 
         OC    TGCLI,TGCLI       AND ALSO ADDING AN AGENCY RESTRICTION          
         BNZ   SOD90             WITHOUT A CLIENT RESTRICTION                   
*                                                                               
         USING TLCLPD,R4                                                        
         LA    R4,KEY            R4=A(CLIENT GROUP PASSIVE KEY)                 
         XC    KEY,KEY                                                          
         MVI   TLCLPCD,TLCLGCDQ  BUILD KEY TO READ ALL CLIENTS THAT             
         MVC   TLCLGCLG,TGCLG    FIT WITHIN CLIENT GROUP                        
         OC    TLCLGCLG,SPACES                                                  
         GOTO1 HIGH                                                             
         B     SOD70                                                            
SOD60    GOTO1 SEQ                                                              
SOD70    CLC   KEY(TLCLGAGY-TLCLPD),KEYSAVE                                     
         BNE   SOD80                                                            
*                                                                               
         OC    TLCLGAGY,TLCLGAGY SKIP GLOBAL CLIENTS                            
         BZ    SOD60                                                            
         CLC   TLCLGAGY,TGAGY    ONLY ADD CLIENTS FOR THAT AGENCY               
         BNE   SOD60                                                            
*                                                                               
         MVI   ACSTAT-ACTBLD(R5),TAVANDEF                                       
         MVC   ACAGY-ACTBLD(L'ACAGY,R5),TLCLGAGY                                
         MVC   ACCLI-ACTBLD(L'ACCLI,R5),TLCLGCLI                                
         LA    R5,ACLNQ(R5)                                                     
         B     SOD60                                                            
         DROP  R4                                                               
*                                                                               
SOD80    XC    TGAGY,TGAGY                                                      
         B     SOD100                                                           
*                                                                               
SOD90    MVI   ACSTAT-ACTBLD(R5),TAVANDEF                                       
         MVC   ACAGY-ACTBLD(L'ACAGY,R5),TGAGY                                   
         MVC   ACCLI-ACTBLD(L'ACCLI,R5),TGCLI                                   
         XC    TGAGY,TGAGY                                                      
         XC    TGCLI,TGCLI                                                      
         LA    R5,ACLNQ(R5)                                                     
*                                                                               
SOD100   MVC   0(ACLNQ,R5),0(R3) ADD AGY/CLI TO ADDITONS TABLE                  
         OI    0(R5),TAVANDEF    AS NON-DEFAULT AGENCY                          
         XC    0(ACLNQ,R3),0(R3) AND DELETE OLD ENTRY FROM INITIAL              
         BAS   RE,CHGTBL         TABLE                                          
*                                                                               
         LA    R5,ACLNQ(R5)      BUMP TO NEXT NEW ENTRY                         
         MVI   0(R5),X'FF'                                                      
*                                                                               
SOD110   LA    R3,ACLNQ(R3)      BUMP TO NEXT INITIAL ENTRY                     
         B     SOD40                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO PROCESS ADDITIONS                                 *         
***********************************************************************         
                                                                                
PROADD   NTR1                                                                   
         OC    TGAGY,TGAGY       IF ADDING AGENCY/CLIENT                        
         BZ    XIT               ADD IT TO ADDITIONS TABLE                      
*                                                                               
         USING ACTBLD,R3                                                        
         L     R3,AADDTBL        R3=A(ADDITIONS TABLE)                          
PADD10   CLI   ACSTAT,X'FF'                                                     
         BE    PADD20            FIND END OF TABLE                              
         LA    R3,ACLNQ(R3)                                                     
         B     PADD10                                                           
*                                                                               
PADD20   OC    TGCLG,TGCLG       IF CLIENT GROUP IS ALREADY SET                 
         BZ    PADD30                                                           
         OC    TGCLI,TGCLI       AND ONLY ADDING AN AGENCY RESTRICTION          
         BZ    PCLG10            GO ADD THE CLIENT IN THAT CGROUP               
*                                                                               
PADD30   MVC   ACAGY,TGAGY       ADD AGENCY                                     
         MVC   ACCLI,TGCLI       AND CLIENT                                     
         MVI   ACSTAT,TAVANDEF   SET DEFAULT STATUS                             
         CLC   DEFAGY,ACAGY                                                     
         BNE   PADD40                                                           
         MVI   ACSTAT,0                                                         
*                                                                               
PADD40   MVI   ACLNQ(R3),X'FF'   AND MARK NEW END OF ADDITIONS TABLE            
         B     XIT                                                              
*                                                                               
***********************************************************************         
*        ROUTINE TO PROCESS NEW CLIENT GROUP                          *         
***********************************************************************         
                                                                                
PROCLG   NTR1                                                                   
         CLI   WKGACLGH+5,0      IF ADDING A CLIENT GROUP                       
         BE    XIT                                                              
*                                                                               
         MVI   BS2LSSEQ,1        RECALL INITIAL AGENCY/CLIENT TABLE             
         BAS   RE,GETTBL         VIA WSSVR                                      
*                                                                               
         L     R2,ACTBL          R2=A(INITIAL AGENCY/CLIENT TABLE)              
         CLI   0(R2),X'FF'       EXIT IF AGENCY/CLIENT LIMITS                   
         BNE   XIT               ALREADY DEFINED FOR THIS STAFF RECORD          
*                                                                               
         USING ACTBLD,R3                                                        
         L     R3,AADDTBL        R3=A(ADDITIONS TABLE)                          
         MVI   0(R3),X'FF'                                                      
*                                                                               
         USING TLCLPD,R4                                                        
PCLG10   LA    R4,KEY            R4=A(CLIENT GROUP PASSIVE KEY)                 
         XC    KEY,KEY                                                          
         MVI   TLCLPCD,TLCLGCDQ  BUILD KEY TO READ ALL CLIENTS THAT             
         MVC   TLCLGCLG,TGCLG    FIT WITHIN CLIENT GROUP                        
         OC    TLCLGCLG,SPACES                                                  
         GOTO1 HIGH                                                             
         B     PCLG30                                                           
PCLG20   GOTO1 SEQ                                                              
PCLG30   CLC   KEY(TLCLGAGY-TLCLPD),KEYSAVE                                     
         BNE   XIT                                                              
*                                                                               
         OC    TLCLGAGY,TLCLGAGY SKIP GLOBAL CLIENTS                            
         BZ    PCLG20                                                           
*                                                                               
         OC    TGAGY,TGAGY       IF ADDING AN AGENCY RESTRICTION TOO            
         BZ    PCLG40                                                           
         CLC   TLCLGAGY,TGAGY    ONLY ADD CLIENTS FOR THAT AGENCY               
         BNE   PCLG20                                                           
*                                                                               
PCLG40   MVC   ACAGY,TLCLGAGY    ADD AGENCY                                     
         MVC   ACCLI,TLCLGCLI    ADD CLIENT                                     
         MVI   ACSTAT,TAVANDEF   AND SET DEFAULT STATUS                         
*                                                                               
         LA    R3,ACLNQ(R3)                                                     
         MVI   0(R3),X'FF'       MARK NEW END OF ADDITIONS TABLE                
         B     PCLG20                                                           
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
*        TASTLD                                                       *         
***********************************************************************         
                                                                                
       ++INCLUDE TASTBLD                                                        
         EJECT                                                                  
***********************************************************************         
*        EXIT MESSAGES                                                *         
***********************************************************************         
                                                                                
ERRSTFCG LA    R2,WKGACLGH         CLIENT GROUP ALREADY ON STAFF REC            
         MVC   MYMSGNO,=Y(ERRCGSTF)                                             
         B     ERREND                                                           
*                                                                               
ERRDELCG LA    R2,WKGDCLGH         CAN'T DELETE UNASSIGNED CLIENT GROUP         
         MVC   MYMSGNO,=Y(ERRCGDEL)                                             
         B     ERREND                                                           
*                                                                               
ERRAGYCG MVC   MYMSGNO,=Y(ERRCGAGY) CGROUP DOES NOT INCLUDE THIS AGENCY         
         B     ERREND                                                           
*                                                                               
AGYMISS  LA    R2,WKGAAGYH         MISSING AGENCY INPUT                         
         B     ERRMISS                                                          
*                                                                               
ERRCLICG MVC   MYMSGNO,=Y(ERRCGCLI) CGROUP DOES NOT INCLUDE THIS CLIENT         
         B     ERREND                                                           
*                                                                               
ERRAINV  LA    R2,WKGAAGYH         AGENCY ALREADY ON STAFF RECORD               
         MVC   MYMSGNO,=Y(ERRSTAGY)                                             
         B     ERREND                                                           
*                                                                               
ERRCINV  LA    R2,WKGACLIH         CLIENT ALREADY ON STAFF RECORD               
         MVC   MYMSGNO,=Y(ERRSTCLI)                                             
         B     ERREND                                                           
*                                                                               
ERRALOA  LA    R2,WKGAAGYH         CLIENTS REQUIRE AT LEAST ONE LIMIT           
         MVC   MYMSGNO,=Y(ERCROLAY)                                             
         B     ERREND                                                           
*                                                                               
ERRINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     MSGEXIT                                                          
*                                                                               
ERRMISS  MVI   ERROR,MISSING       MISSING INPUT FIELD                          
         B     MSGEXIT                                                          
*                                                                               
ERRNTFND MVI   ERROR,NOTFOUND      RECORD NOT FOUND                             
         B     MSGEXIT                                                          
*                                                                               
ERRLOCK  MVI   ERROR,SECLOCK       SECURITY LOCK OUT                            
         LA    R2,WKGUSERH                                                      
         B     MSGEXIT                                                          
*                                                                               
MORE     CLI   ACTNUM,ACTSEL                                                    
         BE    XIT                                                              
         MVC   MYMSGNO,=H'261'     MORE COMBINATIONS TO DISPLAY                 
         B     INFEND                                                           
*                                                                               
NOMORE   XC    LASTDSP,LASTDSP                                                  
         CLI   ACTNUM,ACTSEL                                                    
         BE    XIT                                                              
         MVC   MYMSGNO,=H'262'     NO MORE COMBINATIONS TO DISPLAY              
         B     INFEND                                                           
*                                                                               
INFEND   LA    R2,WKGFRSTH                                                      
         MVI   MYMTYP,GTMINF                                                    
         B     MSGEND                                                           
*                                                                               
ERREND   MVI   MYMTYP,GTMERR                                                    
         B     MSGEND                                                           
*                                                                               
MSGEND   MVI   BLOCK,0                                                          
         OI    GENSTAT2,USGETTXT                                                
         B     MSGEXIT                                                          
*                                                                               
MSGEXIT  GOTO1 EXIT,DMCB,0                                                      
         EJECT                                                                  
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
PFTAB    DS    0X                  PF KEY TABLE                                 
         DC    AL1(PF13X-*,13,0,0,0)                                            
         DC    CL3' ',CL8'STAFF2  ',CL8'LIST    '                               
PF13X    EQU   *                                                                
*                                                                               
         DC    AL1(PF14X-*,14,0,(PF14X-PF14)/KEYLNQ,0)                          
         DC    CL3' ',CL8'STAFF   ',CL8'DISPLAY '                               
PF14     DC    AL1(KEYTYTWA,L'WKGUSER-1),AL2(WKGUSER-T702FFD)                   
         DC    AL1(KEYTYTWA,L'WKGSTAF-1),AL2(WKGSTAF-T702FFD)                   
PF14X    EQU   *                                                                
         DC    X'FF'                                                            
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TASYSDSECT                                                                    
* TASYSEQUS                                                                     
* FAWSSVRD                                                                      
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE FAWSSVRD                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRFCD                                                       
         EJECT                                                                  
*              LOCAL VARIABLES                                                  
ACTBL    DS    A                                                                
ACTBLX   DS    A                                                                
*                                                                               
AADDTBL  DS    A                                                                
AADDTBLX DS    A                                                                
*                                                                               
PPBLOCK  DS    XL(2*38+1)          BLOCK FOR 2 DIRECTORY POINTERS               
*                                                                               
SVKEY    DS    XL(L'KEY)           SAVED STAFF RECORD KEY                       
LASTKEY  DS    XL(L'TLSTKEY)       LAST PROCESSES STAFF RECORD KEY              
*                                                                               
FRSTDSP  DS    XL(ACLNQ)           1ST  DISPLAYED AGY/CLI TABLE ENTRY           
LASTDSP  DS    XL(ACLNQ)           LAST DISPLAYED AGY/CLI TABLE ENTRY           
DISPDEF  DS    XL(ACLNQ)           LAST DISPLAYED DEFAULT TABLE ENTRY           
*                                                                               
DEFAGY   DS    CL(L'TGAGY)         DEFAULT AGENCY                               
*                                                                               
MYSTAF   DS    CL8                 BACK UP OF GLOBAL STAFF CODE                 
*                                                                               
MYELEM   DS    XL(L'ELEMENT)                                                    
*                                                                               
RECSTAT  DS    XL1                                                              
ADDINGR  EQU   C'A'                                                             
PUTTINGR EQU   C'P'                                                             
*                                                                               
BS2LSSEQ DS    XL1                 WSSVR BLOCK SEQUENCE NUMBER                  
BS2LWBLK DS    XL50                WSSVR BLOCK AREA                             
*                                                                               
SVSTPTR  DS    XL(6*38+1)          SAVED PASSIVE POINTER BLOCK                  
         EJECT                                                                  
*              DSECT FOR AGENCY/CLIENT TABLE                                    
ACTBLD   DSECT                                                                  
ACSTAT   DS    X                                                                
ACAGY    DS    CL6                                                              
ACCLI    DS    CL6                                                              
ACLNQ    EQU   *-ACTBLD                                                         
ACTBLNQ  EQU   (ACLNQ*1500)+1                                                   
TBLLNQ   EQU   ACTBLNQ*2                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003TAGENFC   04/16/14'                                      
         END                                                                    
