*          DATA SET PRSFM13E   AT LEVEL 009 AS OF 05/01/02                      
*PHASE T41C13A                                                                  
*INCLUDE PUBEDIT                                                                
*                                                                               
         TITLE 'PRSFM13 PINTPAK-EDR PUB CODE LINKS MAINT-2ND TRY'               
*                                                                               
T41C13   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T41C13*,R7,RR=RE                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
         ST    RE,WRKRELO          SAVE RELOCATION FACTOR                       
         ST    RC,WRKWORKA         SAVE WORKAREA ADDRESS                        
*                                                                               
         MVI   USEIO,C'Y'          INDICATE USER DOES IO                        
         MVI   IOOPT,C'Y'          OVERLAY DOES IO                              
         MVI   ACTELOPT,C'N'       NO ACTIVITY ELEMENTS                         
*                                                                               
         LA    RF,KEY              SET I/O AREA FOR KEYS                        
         ST    RF,AIO                                                           
*                                                                               
         CLI   ACTNUM,ACTLIST      INVALID ACTIONS                              
         BE    ACTERR                                                           
         CLI   ACTNUM,ACTADD       INVALID ACTIONS                              
         BE    ACTERR                                                           
         CLI   ACTNUM,ACTDEL       INVALID ACTIONS                              
         BE    ACTERR                                                           
         CLI   ACTNUM,ACTREST      INVALID ACTIONS                              
         BE    ACTERR                                                           
*                                                                               
MAIN10   CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VK                                                               
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    LR                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BNE   EXIT                   NON-USABLE MODE                           
*                                                                               
         MVI   WRKACTSW,0          INIT ACTIVITY SWITCH                         
*                                                                               
         CLC   WRKACTNM,ACTNUM     IF ACTION HAS CHANGED                        
         BE    *+14                                                             
         MVC   WRKACTNM,ACTNUM        SAVE ACTION NUMBER                        
         MVI   WRKACTSW,C'A'          INDICATE ACTIVITY ON SCREEN               
*                                                                               
         CLI   ACTNUM,ACTDIS       DISPLAY RECORD                               
         BE    DR                                                               
         CLI   ACTNUM,ACTCHA       VALIDATE RECORD                              
         BE    VR                                                               
         B     EXIT                                                             
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
EXIT     XIT1                                                                   
         EJECT                                                                  
*                                                                               
*        VALIDATE KEY                                                           
*                                                                               
VK       DS    0H                                                               
*                                                                               
*        OPEN CONTROL SYSTEM FILES                                              
*                                                                               
         CLI   OFFLINE,C'Y'        IF OFF-LINE OPEN CONTROL SYS FILES           
         BNE   VKOPCTLX                                                         
*                                                                               
         L     RF,ATWA             POINT TO TWA                                 
         L     RF,TWAMASTC-TWATASK(RF) POINT TO MASTC                           
         L     RF,MCUTL-MCBLOCK(RF)  POINT TO UTL                               
         ST    RF,WRKUTLA          SAVE ADDRESS                                 
         MVC   WRKSYS,4(RF)        SAVE CURRENT SYSTEM ID                       
         MVI   4(RF),X'0A'         SET FOR CONTROL SYSTEM                       
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMOPEN',=C'CONTROL',CONFILES,DMWORK              
*                                                                               
         L     RF,WRKUTLA          SWITCH BACK TO CURRENT SYSTEM                
         MVC   4(1,RF),WRKSYS                                                   
*                                                                               
         B     VKOPCTLX                                                         
*                                                                               
CONFILES DS    0D                  CONTROL SYSEM FILE LIST                      
         DC    CL8' GENDIR'                                                     
         DC    CL8' GENFIL'                                                     
         DC    CL8'X'              END OF LIST                                  
*                                                                               
VKOPCTLX DS    0H                                                               
         MVC   AIO,AIO3                                                         
*                                                                               
         GOTO1 =A(SWTCNTL),RR=WRKRELO SWITCH TO CONTROL SYSTEM                  
*                                                                               
*        FORMAT BASIC KEY                                                       
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY              ESTABLISH AS CTFILE EDR LINK RECORD          
         USING CEPREC,R4                                                        
*                                                                               
         MVC   CEPKTYP,=AL2(CEPKTYPQ)  SET RECORD TYPE                          
         MVC   CEPKPSRT,SPACES     INIT EDR PUB NAME                            
*                                                                               
         MVI   FLDOPT,C'Y'         OPTIONAL FIELD                               
*                                                                               
         LA    R2,EDRSPUBH         STARTING PUB NAME                            
*                                                                               
         TM    4(R2),X'80'         IF INPUT THIS TIME                           
         BNO   *+18                                                             
         XC    WRKLSTKY,WRKLSTKY      FORCE RE-DISPLAY OF FIRST PAGE            
         NI    4(R2),X'FF'-X'80'      TURN OFF INPUT IND                        
         OI    4(R2),X'60'            INDICATE VALID FIELD                      
*                                                                               
         GOTO1 VGETFLD             READ IN FIELD                                
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,FLDH+5         GET INPUT LENGTH                             
         BZ    *+10                                                             
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   CEPKPSRT(0),FLD     MOVE TO KEY                                  
*                                                                               
         LA    R2,EDRSEDNH         STARTING EDITION NAME                        
*                                                                               
         TM    4(R2),X'80'         IF INPUT THIS TIME                           
         BNO   *+18                                                             
         XC    WRKLSTKY,WRKLSTKY      FORCE RE-DISPLAY OF FIRST PAGE            
         NI    4(R2),X'FF'-X'80'      TURN OFF INPUT IND                        
         OI    4(R2),X'60'            INDICATE VALID FIELD                      
*                                                                               
         MVI   FLDOPT,C'Y'         OPTIONAL FIELD                               
*                                                                               
         GOTO1 VGETFLD             READ IN FIELD - REQUIRED                     
*                                                                               
         MVC   WRKSEDN,SPACES      INIT STARTING EDITION SAVEAREA               
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,FLDH+5         GET INPUT LENGTH                             
         BZ    *+10                NO INPUT                                     
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WRKSEDN(0),FLD      MOVE TO SAVEAREA                             
*                                                                               
         GOTO1 HIGH                FIND A RECORD ON THE FILE                    
*                                                                               
VKLOOP   DS    0H                                                               
*                                                                               
         LA    R4,KEY              RE-POINT TO KEY AREA                         
*                                                                               
         CLC   CEPKTYP,=AL2(CEPKTYPQ) DONE IF NEW RECORD TYPE                   
         BNE   VKDONE                                                           
*                                                                               
         CLC   CEPKPSRT,CEPKPSRT-CEPKEY+KEYSAVE                                 
         BH    VKDONE              DONE IF HIGHER ALPHA REACHED                 
*                                                                               
         L     R4,AIO              POINT TO FOUND RECORD                        
*                                                                               
*        FIND EDR ELEMENT                                                       
*                                                                               
         SR    RF,RF                                                            
         LA    R6,CEPDATA          POINT TO FIRST ELEMENT                       
         USING CEDRELD,R6          ESTABLISH AS EDR ELEMENT                     
*                                                                               
         CLI   CEDRELEM,0          CONTINUE IF END OF REC REACHED               
         BE    VKCONT                                                           
         CLI   CEDRELEM,CEDRELTQ   DONE IF ELEMENT FOUND                        
         BE    *+16                                                             
         IC    RF,CEDRLEN          ELEMENT LENGTH                               
         LA    R6,CEDRELD(R6)      POINT TO NEXT ELEMENT                        
         B     *-24                                                             
*                                                                               
         CLC   EDRSPUB,EDRSPUB     DONE IF GTEQ PUBNAME                         
         BL    VKCONT                                                           
*                                                                               
         CLC   CEDREDN,WRKSEDN     DONE IF GTEQ EDITION NAME                    
         BNL   VKDONE                                                           
*                                                                               
VKCONT   DS    0H                                                               
*                                                                               
         GOTO1 SEQ                                                              
*                                                                               
         B     VKLOOP                                                           
*                                                                               
VKDONE   DS    0H                                                               
*                                                                               
         MVC   WRKKEYSV,KEY        SAVE STARTING KEY                            
*                                                                               
         GOTO1 =A(SWTBACK),RR=WRKRELO   SWITCH BACK TO USER SYSTEM              
*                                                                               
VKX      DS    0H                                                               
*                                                                               
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        VALIDATE RECORD                                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VR       DS    0H                                                               
*                                                                               
         LA    R3,SAVKEYS          POINT TO SCREEN KEYS SAVEAREA                
         LA    R3,L'CEPKEY+L'PUBKEY(R3)  START WITH SECOND ENTRY                
*                                                                               
         OC    0(L'CEPKEY+L'PUBKEY,R3),0(R3)  IF NONE GO DO DISPLAY             
         BZ    DR                                                               
*                                                                               
         LA    R2,EDRENM1H         FIRST NAME/EDITION FIELD                     
         LA    R0,((EDRENMLH-EDRENM1H)/(EDRENM2H-EDRENM1H))+1 # OF LNS          
*                                                                               
         MVI   USEIO,C'Y'          INDICATE USER DOES IO                        
         MVI   IOOPT,C'Y'          OVERLAY DOES IO                              
*                                                                               
         GOTO1 =A(SWTCNTL),RR=WRKRELO SWITCH TO CONTROL SYSTEM                  
*                                                                               
VREDRLP  DS    0H                                                               
*                                                                               
         MVI   WRKPUBSW,0          INIT DELETE SWITCH                           
         MVC   WRKOLDPB,L'CEPKEY(R3) SAVE CURRENT PUB ASSIGNMENT                
*                                                                               
         MVC   AIO,AIO3            READ RECORD INTO I/OAREA3                    
*                                                                               
         LA    R4,KEY              ESTABLISH EDR LINKAGE RECORD                 
         USING CEPREC,R4                                                        
*                                                                               
         MVC   CEPKEY,0(R3)        COPY KEY OF DISPLAYED PUB                    
*                                                                               
         OC    CEPKEY,CEPKEY       CONTINUE IF THERE IS ONE                     
         BNZ   VREDR10                                                          
*                                                                               
VREDR05  DS    0H                                                               
*                                  SKIP REST OF LINE                            
         BAS   RE,BUMP             BUMP TO RATECARD FIELD                       
         BAS   RE,BUMP             BUMP TO PUBID    FIELD                       
         BAS   RE,BUMP             BUMP TO MEDIA    FIELD                       
         BAS   RE,BUMP             BUMP TO PUBNAME  FIELD                       
*                                                                               
         B     VREDRCN                                                          
*                                                                               
VREDR10  DS    0H                                                               
*                                                                               
         GOTO1 HIGH                READ RECORD                                  
*                                                                               
         CLC   CEPKEY,KEYSAVE      MUST FIND RECORD                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R4,AIO              POINT TO FOUND RECORD                        
*                                                                               
         SR    RF,RF                                                            
         LA    R6,CEPDATA          POINT TO FIRST ELEMENT                       
         USING CEDRELD,R6          ESTABLISH AS EDR ELEMENT                     
*                                                                               
         CLI   CEDRELEM,0          CONTINUE IF END OF REC REACHED               
         BE    VREDR05                                                          
         CLI   CEDRELEM,CEDRELTQ   DONE IF ELEMENT FOUND                        
         BE    *+16                                                             
         IC    RF,CEDRLEN          ELEMENT LENGTH                               
         LA    R6,CEDRELD(R6)      POINT TO NEXT ELEMENT                        
         B     *-24                                                             
*                                                                               
         GOTO1 =A(SWTBACK),RR=WRKRELO   SWITCH TO PRINTPAK FILES                
*                                                                               
         MVI   USEIO,C'N'          TURN OFF USER DOING IO                       
         MVI   IOOPT,C'N'                                                       
*                                                                               
         BAS   RE,BUMP             BUMP TO RATECARD FIELD                       
         BAS   RE,BUMP             BUMP TO PUBID    FIELD                       
*                                                                               
         ST    R2,FULL             SAVE PUBID FIELD ADDRESS                     
*                                                                               
*        VALIDATE MEDIA                                                         
*                                                                               
         BAS   RE,BUMP             BUMP TO MEDIA FIELD                          
*                                                                               
         TM    1(R2),X'20'         IF PROTECTED FIELD                           
         BNO   *+12                                                             
         BAS   RE,BUMP                BUMP TO START OF NEXT LINE                
         B     VREDRCN                                                          
*                                                                               
         GOTO1 VALIMED             VALIDATE PRINTPAK MEDIA                      
*                                                                               
*        VALIDATE PUB FIELD                                                     
*                                                                               
         L     R2,FULL             POINT BACK TO PUBID FIELD                    
*                                                                               
         CLI   5(R2),0             SKIP IF PUB ENTERED                          
         BNE   VRNOPUBX                                                         
*                                                                               
*        IF NO PUB ENTERED WE MAY HAVE TO COPY PRIOR LINE'S PUB                 
*                                                                               
         OC    WRKOLDPB,WRKOLDPB   DELETING IF OLD PUB PRESENT                  
         BNZ   VRDEL                                                            
*                                                                               
         CLI   PFAID,9             SKIP UNLESS PF9/21 HIT                       
         BE    *+8                                                              
         CLI   PFAID,21                                                         
         BNE   VRNOPUB0                                                         
*                                                                               
         LR    RF,R3               POINT TO LINE SAVE AREA                      
         SH    RF,=Y(L'CEPKEY+L'PUBKEY)   BACK UP TO PRIOR ENTRY                
*                                                                               
         OC    L'CEPKEY(L'PUBKEY,RF),L'CEPKEY(RF)  SKIP IF NO PRIOR PUB         
         BZ    *+14                                                             
         CLC   CEPKEY(CEPKESQN-CEPKEY),0(RF)  SKIP LINE IF NEW EDR PUB          
         BE    VRNOPUB1                                                         
*                                                                               
VRNOPUB0 DS    0H                                                               
*                                                                               
         BAS   RE,BUMP             BUMP TO MEDIA FIELD                          
         BAS   RE,BUMP             BUMP TO PUBNAME FIELD                        
         B     VREDRCN                                                          
*                                                                               
VRNOPUB1 DS    0H                                                               
*                                                                               
         MVC   L'CEPKEY(L'PUBKEY,R3),L'CEPKEY(RF)  COPY PRIOR PUB               
         MVC   WRKOLDPB,L'CEPKEY(RF)  COPY PRIOR PUB                            
*                                                                               
         MVC   QMED,PUBKMED-PUBKEY+L'CEPKEY(R3)  SAVE PRINT MEDIA               
*                                                                               
         LA    RF,PUBKPUB-PUBKEY+L'CEPKEY(R3)   PUB ID                          
*                                                                               
         GOTO1 =V(PUBEDIT),DMCB,(RF),8(R2),RR=WRKRELO  EXPAND PUB               
*                                                                               
         LA    RF,L'EDRPUB1        ELIMINATE TRAILING SPACES                    
         LA    R1,8-1(RF,R2)       LAST BYTE OF FIELD                           
*                                                                               
         CLI   0(R1),C' '                                                       
         BH    *+10                                                             
         BCTR  R1,0                                                             
         BCT   RF,*-10                                                          
*                                                                               
         STC   RF,5(R2)            SET INPUT LENGTH                             
*                                                                               
         OI    6(R2),X'80'         TRANSMIT FIELD                               
*                                                                               
         ST    R2,FULL             SAVE FIELD POINTER                           
*                                                                               
         BAS   RE,BUMP             BUMP TO MEDIA FIELD                          
*                                                                               
         CLI   QMED,0              IF WE HAVE PRINTPAK MEDIA                    
         BE    *+14                                                             
         MVC   8(1,R2),QMED           DISPLAY PRINTPAK MEDIA                    
         B     VRDISMDX                 FOUND                                   
*                                                                               
         CLI   CEDRMED,CEDRCONQ    IF CONSUMER MAGAZINE                         
         BNE   *+8                                                              
         MVI   8(R2),C'M'             USE MAGAZINE MEDIA                        
*                                                                               
         CLI   CEDRMED,CEDRBUSQ    IF BUSINESS  MAGAZINE                        
         BNE   *+8                                                              
         MVI   8(R2),C'T'             USE MAGAZINE MEDIA                        
*                                                                               
VRDISMDX DS    0H                                                               
*                                                                               
         MVI   5(R2),1             SET INPUT LENGTH                             
         OI    6(R2),X'80'         TRANSMIT FIELD                               
*                                                                               
         L     R2,FULL             RESTORE FIELD POINTER                        
*                                                                               
VRNOPUBX DS    0H                                                               
*                                                                               
         CLC   =C'DEL',8(R2)       IF DELETED                                   
         BNE   VRDELX                                                           
*                                                                               
VRDEL    DS    0H                                                               
*                                                                               
         MVC   BPUB,PUBKPUB-PUBKEY+WRKOLDPB   COPY OLD PUB KEY                  
         MVI   WRKPUBSW,C'D'          INDICATE DELETE MODE                      
         MVI   WRKACTSW,C'A'          INDICATE ACTIVITY ON SCREEN               
         MVC   8(L'EDRPUB1,R2),SPACES CLEAR OLD PUB ID                          
         OI    6(R2),X'80'            TRANSMIT FIELD                            
         MVI   5(R2),0                SET INPUT LENGTH TO ZERO                  
*                                                                               
         BAS   RE,BUMP             BUMP TO MEDIA FIELD                          
*                                                                               
         CLI   CEDRMED,CEDRCONQ    IF CONSUMER MAGAZINE                         
         BNE   *+8                                                              
         MVI   8(R2),C'M'             USE MAGAZINE MEDIA                        
*                                                                               
         CLI   CEDRMED,CEDRBUSQ    IF BUSINESS  MAGAZINE                        
         BNE   *+8                                                              
         MVI   8(R2),C'T'             USE MAGAZINE MEDIA                        
*                                                                               
         MVI   5(R2),1             SET INPUT LENGTH                             
         OI    6(R2),X'80'         TRANSMIT FIELD                               
*                                                                               
         L     R2,FULL             RESTORE FIELD POINTER                        
*                                                                               
         B     VRUPD                  GO UPDATE RECORD                          
*                                                                               
VRDELX   DS    0H                                                               
*                                                                               
         GOTO1 VALIPUB             VALIDATE PUB FIELD                           
*                                                                               
         MVI   USEIO,C'Y'          TURN ON USER DOING IO                        
         MVI   IOOPT,C'Y'                                                       
*                                                                               
*        EDR ASSIGNMENT MUST BE UNIQUE                                          
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY              ESTABLISH AS EDR/PUB PASSIVE POINTER         
         USING PE2PRECD,R4                                                      
*                                                                               
         MVC   PE2PKAGY,AGENCY     SET AGENCY                                   
         MVI   PE2PKMED,C'Z'       SET MEDIA                                    
         MVI   PE2PKRCD,PE2PKIDQ   SET RECORD ID                                
         MVC   PE2PKEDR,CEDRPID    SET EDR CODE                                 
*                                                                               
         OI    DMINBTS,X'08'       READ DELETED RECORDS                         
*                                                                               
         GOTO1 HIGH                FIND RECORD                                  
*                                                                               
VRE2PLP  DS    0H                                                               
*                                                                               
         CLC   PE2PKEY(PE2PKPMD-PE2PKEY),KEYSAVE OKAY IF NO MATCH               
         BNE   VRE2PDN                FOUND                                     
*                                                                               
         CLC   PE2PKPMD,PUBKMED-PUBKEY+WRKOLDPB  OKAY IF OLD MEDIA              
         BNE   *+10                                                             
         CLC   PE2PKPUB,PUBKPUB-PUBKEY+WRKOLDPB  OKAY IF OLD PUB ID             
         BE    VRE2PCN                                                          
*                                                                               
         TM    PE2PCNTL,X'80'         ELSE MUST BE DELETED                      
         BNO   VREDRER2                                                         
*                                                                               
VRE2PCN  DS    0H                                                               
*                                                                               
         GOTO1 SEQ                    READ NEXT POINTER                         
*                                                                               
         B     VRE2PLP                                                          
*                                                                               
VRE2PDN  DS    0H                                                               
*                                                                               
*        UPDATE PRINTPAK PASSIVE POINTERS                                       
*                                                                               
VRUPD    DS    0H                                                               
*                                                                               
         MVI   USEIO,C'Y'          TURN ON USER DOING IO                        
         MVI   IOOPT,C'Y'                                                       
*                                                                               
         OC    BPUB,BPUB           SKIP IF NO PUB ID ENTERED                    
         BZ    VRUPDPE2                                                         
*                                                                               
         LA    R4,KEY              ESTABLISH AS EDR/PUB PASSIVE POINTER         
         ST    R4,AIO              READ RECORD INTO HERE                        
*                                                                               
         XC    KEY,KEY                                                          
         USING PPE2RECD,R4         ESTABLISH AS EDR/PUB PASSIVE POINTER         
*                                                                               
         MVC   PPE2KAGY,AGENCY     SET AGENCY                                   
         MVI   PPE2KMED,C'Z'       SET MEDIA                                    
         MVI   PPE2KRCD,PPE2KIDQ   SET RECORD ID                                
         MVC   PPE2KPMD,QMED       SET PRINTPAK PUB'S MEDIA                     
         MVC   PPE2KPUB,BPUB       SET PUB CODE                                 
         MVC   PPE2KEDR,CEDRPID    SET EDR CODE                                 
*                                                                               
         OI    DMINBTS,X'08'       READ DELETED RECORDS                         
*                                                                               
         GOTO1 HIGH                READ DESIRED POINTER                         
*                                                                               
         CLC   PPE2KEY,KEYSAVE     IF NOT FOUND                                 
         BE    VRUPDPE1                                                         
*                                                                               
         CLI   WRKPUBSW,C'D'       SKIP IF DELETING A PUB                       
         BE    VRUPDPE2                                                         
*                                                                               
         MVC   KEY,KEYSAVE         RESTORE KEY                                  
         MVC   PPE2CNTL,=X'00FF'   INDICATE DIRECTORY ONLY RECORD               
*                                                                               
         MVI   WRKACTSW,C'A'       INDICATE THERE WAS ACTIVITY                  
*                                                                               
         GOTO1 ADD                    ADD RECORD                                
         BE    *+6                    NO ERRORS TOLERATED                       
         DC    H'0'                                                             
*                                                                               
         B     VRUPDPE2                                                         
*                                                                               
VRUPDPE1 DS    0H                                                               
*                                                                               
         TM    PPE2CNTL,X'80'      IF CURRENT PUB DELETED                       
         BNO   VRUPDPE2                                                         
*                                                                               
         CLI   WRKPUBSW,C'D'          DONE IF WE ARE DELETING                   
         BE    VRUPDPE2                                                         
*                                                                               
         NI    PPE2CNTL,X'FF'-X'80'   ELSE MUST RESTORE OLD POINTER             
*                                                                               
         MVI   WRKACTSW,C'A'          INDICATE THERE WAS ACTIVITY               
*                                                                               
         GOTO1 WRITE                  RE-WRITE RECORD                           
*                                                                               
VRUPDPE2 DS    0H                                                               
*                                                                               
         CLC   QMED,PUBKMED-PUBKEY+WRKOLDPB   DONE IF  MEDIA UNCHANGED          
         BNE   VRUPDPE3                                                         
         CLC   BPUB,PUBKPUB-PUBKEY+WRKOLDPB        AND PUB   UNCHANGED          
         BNE   VRUPDPE3                                                         
*                                                                               
         CLI   WRKPUBSW,C'D'                       AND NOT DELETING             
         BNE   VRUPDPEX                                                         
*                                                                               
VRUPDPE3 DS    0H                                                               
*                                                                               
*        MUST DELETE OLD POINTER                                                
*                                                                               
         OC    WRKOLDPB,WRKOLDPB   SKIP IF NO PRIOR PUB                         
         BZ    VRUPDPEX                                                         
*                                                                               
         LA    R4,KEY              ESTABLISH AS EDR/PUB PASSIVE POINTER         
         ST    R4,AIO              READ RECORD INTO HERE                        
*                                                                               
         XC    KEY,KEY                                                          
         USING PPE2RECD,R4         ESTABLISH AS EDR/PUB PASSIVE POINTER         
*                                                                               
         MVC   PPE2KAGY,AGENCY     SET AGENCY                                   
         MVI   PPE2KMED,C'Z'       SET MEDIA                                    
         MVI   PPE2KRCD,PPE2KIDQ   SET RECORD ID                                
         MVC   PPE2KPMD,PUBKMED-PUBKEY+WRKOLDPB    SET MEDIA                    
         MVC   PPE2KPUB,PUBKPUB-PUBKEY+WRKOLDPB    SET PUB CODE                 
         MVC   PPE2KEDR,CEDRPID    SET EDR CODE                                 
*                                                                               
         OI    DMINBTS,X'08'       READ DELETED RECORDS                         
*                                                                               
         GOTO1 HIGH                READ DESIRED POINTER                         
*                                                                               
         CLC   PPE2KEY,KEYSAVE     MUST FIND IT                                 
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OI    PPE2CNTL,X'80'      DELETE OLD POINTER                           
*                                                                               
         MVI   WRKACTSW,C'A'       INDICATE THERE WAS ACTIVITY                  
*                                                                               
         GOTO1 WRITE                  RE-WRITE RECORD                           
*                                                                               
VRUPDPEX DS    0H                                                               
*                                                                               
*         UPDATE EDR-PUB PASSIVE POINTER                                        
*                                                                               
         XC    KEY,KEY                                                          
         USING PE2PRECD,R4         ESTABLISH AS EDR/PUB PASSIVE POINTER         
*                                                                               
         MVC   PE2PKAGY,AGENCY     SET AGENCY                                   
         MVI   PE2PKMED,C'Z'       SET MEDIA                                    
         MVI   PE2PKRCD,PE2PKIDQ   SET RECORD ID                                
         MVC   PE2PKEDR,CEDRPID    SET EDR CODE                                 
         MVC   PE2PKPMD,QMED       SET MEDIA FOR PRINTPAK PUB                   
         MVC   PE2PKPUB,BPUB       SET PUB CODE                                 
*                                                                               
         OI    DMINBTS,X'08'       READ DELETED RECORDS                         
*                                                                               
         GOTO1 HIGH                FIND RECORD                                  
*                                                                               
         CLC   PE2PKEY,KEYSAVE     IF RECORD NOT FOUND                          
         BE    VRUPDEP1                                                         
*                                                                               
         CLI   WRKPUBSW,C'D'       SKIP IF DELETING A PUB                       
         BE    VRUPDEP2                                                         
*                                                                               
         MVC   KEY,KEYSAVE         RESTORE KEY                                  
         MVC   PE2PCNTL,=X'00FF'   INDICATE DIRECTORY ONLY RECORD               
*                                                                               
         MVI   WRKACTSW,C'A'       INDICATE THERE WAS ACTIVITY                  
*                                                                               
         GOTO1 ADD                 ADD RECORD                                   
         BE    *+6                 NO ERRORS TOLERATED                          
         DC    H'0'                                                             
*                                                                               
         B     VRUPDEP2                                                         
*                                                                               
VRUPDEP1 DS    0H                                                               
*                                                                               
         TM    PE2PCNTL,X'80'      IF CURRENT PUB DELETED                       
         BNO   VRUPDEP2                                                         
*                                                                               
         CLI   WRKPUBSW,C'D'          DONE IF WE ARE DELETING                   
         BE    VRUPDEP2                                                         
*                                                                               
         NI    PE2PCNTL,X'FF'-X'80'   ELSE MUST RESTORE OLD POINTER             
*                                                                               
         GOTO1 WRITE                  RE-WRITE RECORD                           
*                                                                               
         MVI   WRKACTSW,C'A'       INDICATE THERE WAS ACTIVITY                  
*                                                                               
VRUPDEP2 DS    0H                                                               
*                                                                               
         CLC   QMED,PUBKMED-PUBKEY+WRKOLDPB   DONE IF  MEDIA UNCHANGED          
         BNE   VRUPDEP3                                                         
         CLC   BPUB,PUBKPUB-PUBKEY+WRKOLDPB        AND PUB   UNCHANGED          
         BNE   VRUPDEP3                                                         
*                                                                               
         CLI   WRKPUBSW,C'D'                       AND NOT DELETING             
         BNE   VRUPDEPX                                                         
*                                                                               
VRUPDEP3 DS    0H                                                               
*                                                                               
*        MUST DELETE OLD POINTER                                                
*                                                                               
         OC    WRKOLDPB,WRKOLDPB   SKIP IF NO PRIOR PUB                         
         BZ    VRUPDEPX                                                         
*                                                                               
         LA    R4,KEY              ESTABLISH AS EDR/PUB PASSIVE POINTER         
         ST    R4,AIO              READ RECORD INTO HERE                        
*                                                                               
         XC    KEY,KEY                                                          
         USING PE2PRECD,R4         ESTABLISH AS EDR/PUB PASSIVE POINTER         
*                                                                               
         MVC   PE2PKAGY,AGENCY     SET AGENCY                                   
         MVI   PE2PKMED,C'Z'       SET MEDIA                                    
         MVI   PE2PKRCD,PE2PKIDQ   SET RECORD ID                                
         MVC   PE2PKEDR,CEDRPID    SET EDR CODE                                 
         MVC   PE2PKPMD,PUBKMED-PUBKEY+WRKOLDPB    SET MEDIA                    
         MVC   PE2PKPUB,PUBKPUB-PUBKEY+WRKOLDPB    SET PUB CODE                 
*                                                                               
         OI    DMINBTS,X'08'       READ DELETED RECORDS                         
*                                                                               
         GOTO1 HIGH                READ DESIRED POINTER                         
*                                                                               
         CLC   PE2PKEY,KEYSAVE     MUST FIND IT                                 
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OI    PE2PCNTL,X'80'         ELSE MUST DELETE OLD POINTER              
*                                                                               
         MVI   WRKACTSW,C'A'       INDICATE THERE WAS ACTIVITY                  
*                                                                               
         GOTO1 WRITE                  RE-WRITE RECORD                           
*                                                                               
VRUPDEPX DS    0H                                                               
*                                                                               
VRUPDX   DS    0H                                                               
*                                                                               
         BAS   RE,BUMP             BUMP TO PUB MEDIA FIELD                      
         BAS   RE,BUMP             BUMP TO PUB NAME  FIELD                      
*                                                                               
*        DISPLAY PUB NAME                                                       
*                                                                               
         XC    L'CEPKEY(L'PUBKEY,R3),L'CEPKEY(R3)  INIT PUBKEY SAVE             
*                                                                               
         OC    BPUB,BPUB           SKIP IF NO PUB                               
         BZ    VREDRPNX               ENTERED                                   
*                                                                               
         CLI   WRKPUBSW,C'D'       IF DELETING PUB                              
         BNE   VREDRPN1                                                         
*                                                                               
         MVC   8(L'EDRPBN1,R2),SPACES   CLEAR OLD NAME                          
         OI    6(R2),X'80'         TRANSMIT FIELD                               
         B     VREDRCN                                                          
*                                                                               
*        READ PUB RECORD                                                        
*                                                                               
VREDRPN1 DS    0H                                                               
*                                                                               
         MVI   USEIO,C'N'          TURN OFF USER DOES IO                        
         MVI   IOOPT,C'N'          TURN OFF OVERLAY DOES IO                     
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PUBRECD,R4                                                       
*                                                                               
         MVC   PUBKMED,QMED        SET MEDIA                                    
         MVC   PUBKPUB(6),BPUB     PUB NUMBER                                   
         MVC   PUBKAGY,AGENCY                                                   
         MVI   PUBKCOD,X'81'                                                    
*                                                                               
         L     RF,AIO1                                                          
         CLC   PUBKEY,0(RF)        DONE IF RECORD ALREADY IN CORE               
         BE    VREDRPN2                                                         
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
*                                                                               
         MVC   FILENAME,=CL8'PUBDIR'                                            
*                                                                               
         NI    DMINBTS,X'FF'-X'08'  IGNORE DELETED RECORDS                      
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         XC    FILENAME,FILENAME                                                
*                                                                               
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+6                 MUST FIND PUB                                
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO1             READ PUB RECORD                              
         ST    R6,AIO                                                           
*                                                                               
         MVC   FILENAME,=CL8'PUBFILE'                                           
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         XC    FILENAME,FILENAME                                                
*                                                                               
VREDRPN2 DS    0H                                                               
*                                                                               
         MVC   L'CEPKEY(L'PUBKEY,R3),0(R6) SAVE PUBKEY                          
*                                                                               
         L     R6,AIO1             POINT TO PUB RECORD                          
         MVI   ELCODE,X'10'                                                     
*                                                                               
         BAS   RE,GETEL                                                         
         BE    *+6                 MUST FIND NAME ELEMENT                       
         DC    H'0'                                                             
*                                                                               
         USING PUBNAMEL,R6                                                      
*                                                                               
         MVC   8(L'EDRPBN1,R2),PUBNAME  DISPLAY PUB NAME                        
*                                                                               
         OI    6(R2),X'80'         TRANSMIT FIELD                               
*                                                                               
         DROP  R6                                                               
*                                                                               
VREDRPNX DS    0H                                                               
*                                                                               
         MVC   AIO,AIO3            RESTORE I/OAREA POINTER                      
         MVI   USEIO,C'Y'          INDICATE USER DOES IO                        
         MVI   IOOPT,C'Y'          OVERLAY DOES IO                              
*                                                                               
         GOTO1 =A(SWTCNTL),RR=WRKRELO  RETURN TO CONTROL SYSTEM                 
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'CEPKEY),0(R3)  RESTORE CURRENT EDR LINK KEY                
*                                                                               
         GOTO1 HIGH                RESTORE FILE POINTER                         
*                                                                               
VREDRCN  DS    0H                                                               
*                                                                               
         MVI   USEIO,C'Y'          INDICATE USER DOES IO                        
         MVI   IOOPT,C'Y'          OVERLAY DOES IO                              
*                                                                               
         GOTO1 =A(SWTCNTL),RR=WRKRELO SWITCH TO CONTROL SYSTEM                  
*                                                                               
         BAS   RE,BUMP             BUMP TO START OF NEXT LINE                   
*                                                                               
         LA    R3,L'CEPKEY+L'PUBKEY(R3)  POINT TO NEXT SAVED KEY                
         BCT   R0,VREDRLP                                                       
*                                                                               
VREDRDN  DS    0H                                                               
*                                                                               
VRX      DS    0H                                                               
*                                                                               
         GOTO1 =A(SWTBACK),RR=WRKRELO SWITCH BACK TO USER SYSTEM                
*                                                                               
         CLI   WRKACTSW,C'A'       IF THERE IS NO ACTIVITY ON SCREEN            
         BNE   DR                     GO SEE IF WE NEED TO PAGE.                
*                                                                               
         B     EXIT                                                             
*                                                                               
VREDRER1 DS    0H                                                               
         MVI   ERROR,EDRDUPPB      DUPLICATE PUB CODE                           
*                                                                               
         B     VREDRERR                                                         
*                                                                               
VREDRER2 DS    0H                                                               
         MVI   ERROR,EDRDUP        DUPLICATE EDR CODE                           
*                                                                               
VREDRERR DS    0H                                                               
*                                                                               
         GOTO1 =A(SWTBACK),RR=WRKRELO SWITCH BACK TO USER SYSTEM                
*                                                                               
         B     ERRXIT                                                           
*                                                                               
EDRNOTV  EQU   177                 EDR MUST BE 9 DIGITS                         
EDRDUP   EQU   178                 EDR ALREADY ASSIGNED                         
EDRDUPPB EQU   181                 PUB ALREADY ASSIGNED                         
         TITLE 'PRWRI13 - DISPLAY RECORD - DR'                                  
***********************************************************************         
*                                                                     *         
*        DISPLAY RECORD                                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DR       DS    0H                                                               
*                                                                               
         MVI   USEIO,C'Y'          INDICATE USER DOES IO                        
         MVI   IOOPT,C'Y'          OVERLAY DOES IO                              
*                                                                               
         MVC   AIO,AIO3            SET IOAREA                                   
*                                                                               
         TWAXC EDRENM1H,EDRPBNLH,PROT=Y,TRNS=Y  CLEAR OLD SCREEN                
*                                                                               
         GOTO1 =A(SWTCNTL),RR=WRKRELO SWITCH TO CONTROL SYSTEM                  
*                                                                               
         MVC   KEY,WRKKEYSV        RESTORE STARTING KEY                         
*                                                                               
         OC    WRKLSTKY,WRKLSTKY IF THERE IS A LAST KY FROM PREV PAGE           
         BZ    *+10                                                             
         MVC   KEY,WRKLSTKY           USE IT FOR START                          
*                                                                               
         GOTO1 HIGH                READ IN START KEY                            
*                                                                               
         LA    R3,SAVKEYS          POINT TO SCREEN KEYS SAVEAREA                
         XC    0(L'CEPKEY+L'PUBKEY,R3),0(R3) INIT FIRST ENTRY                   
         LA    R3,L'CEPKEY+L'PUBKEY(R3)     START WITH SECOND ENTRY             
*                                                                               
         LA    R2,EDRENM1H         FIRST NAME/EDITION FIELD                     
         LA    R0,((EDRENMLH-EDRENM1H)/(EDRENM2H-EDRENM1H))+1 # OF LNS          
*                                                                               
DRDISLP  DS    0H                                                               
*                                                                               
         XC    0(L'CEPKEY+L'PUBKEY,R3),0(R3) INIT SAVEAREA                      
*                                                                               
         LA    R4,KEY              ESTABLISH EDR LINKAGE RECORD                 
         USING CEPREC,R4                                                        
*                                                                               
         CLC   CEPKTYP,=AL2(CEPKTYPQ) DONE IF NEW RECORD TYPE                   
         BNE   DRDISDN                                                          
*                                                                               
         MVC   0(L'CEPKEY,R3),KEY   SAVE LATEST KEY                             
*                                                                               
*        FIND EDR ELEMENT                                                       
*                                                                               
         L     R4,AIO              POINT TO FOUND RECORD                        
*                                                                               
         SR    RF,RF                                                            
         LA    R6,CEPDATA          POINT TO FIRST ELEMENT                       
         USING CEDRELD,R6          ESTABLISH AS EDR ELEMENT                     
*                                                                               
         CLI   CEDRELEM,0          CONTINUE IF END OF REC REACHED               
         BE    DRDISCN                                                          
         CLI   CEDRELEM,CEDRELTQ   DONE IF ELEMENT FOUND                        
         BE    *+16                                                             
         IC    RF,CEDRLEN          ELEMENT LENGTH                               
         LA    R6,CEDRELD(R6)      POINT TO NEXT ELEMENT                        
         B     *-24                                                             
*                                                                               
         LR    RF,R3                                                            
         SH    RF,=Y(L'CEPKEY+L'PUBKEY)     BACKUP A SAVED KEY                  
*                                                                               
         CLC   CEPKPSRT,CEPKPSRT-CEPKEY(RF) IF NEW PUB NAME                     
         BNE   DREDRNM               PRINT PUB NAME                             
         CLC   CEPKPSQN,CEPKPSQN-CEPKEY(RF) SAME IF NEW SEQ NUMBER              
         BNE   DREDRNM                                                          
*                                                                               
         B     DREDREDN            ELSE PRINT EDITION                           
*                                                                               
DREDRNM  DS    0H                                                               
*                                                                               
         MVC   8(L'EDRENM1,R2),CEDRNAME EDR PUBNAME                             
*                                                                               
         CLC   CEDREDN,SPACES      IF EDITION PRESENT                           
         BNH   DREDREDX                                                         
*                                                                               
*                                     COPY KEY TO NEXT LINE                     
         MVC   L'CEPKEY+L'PUBKEY(L'CEPKEY+L'PUBKEY,R3),0(R3)                    
         LA    R3,L'CEPKEY+L'PUBKEY(R3)  BUMP TO NEXT LINE'S KEYAREA            
*                                                                               
         BAS   RE,BUMP             SKIP TO PRINTPAK PUB FIELD                   
         BAS   RE,BUMP                                                          
         OI    1(R2),X'20'         PROTECT PUB FIELD                            
*                                                                               
         BAS   RE,BUMP                                                          
         OI    1(R2),X'20'         PROTECT MEDIA FIELD                          
*                                                                               
         BAS   RE,BUMP             BUMP TO EDR NAME FIELD ON NEXT LINE          
         BAS   RE,BUMP                                                          
*                                                                               
         BCT   R0,*+8              DECREMENT LINE COUNTER                       
         B     DRDISDN             NO MORE LINES ON SCREEN                      
*                                                                               
DREDREDN DS    0H                  DISPLAY EDITION                              
*                                                                               
         MVC   10(L'EDRENM1-2,R2),CEDREDN   EDITION NAME-INDENTED BY 2          
*                                                                               
DREDREDX DS    0H                  DISPLAY EDITION                              
*                                                                               
         BAS   RE,BUMP             BUMP TO RATECARD FIELD                       
*                                                                               
         MVC   8(L'EDRECT1,R2),CEDRCTYP  CARD TYPE                              
*                                                                               
         BAS   RE,BUMP             BUMP TO PUBCODE FIELD                        
*                                                                               
         TM    1(R2),X'20'         IF FIELD PROTECTED                           
         BNO   *+8                                                              
         NI    1(R2),X'FF'-X'20'      OPEN IT UP                                
*                                                                               
*        FIND PRINTPAK NUMBER FROM PASSIVE POINTERS                             
*                                                                               
         MVI   USEIO,C'N'          TURN OFF IO INDICATORS                       
         MVI   IOOPT,C'N'                                                       
*                                                                               
         GOTO1 =A(SWTBACK),RR=WRKRELO SWITCH BACK TO USER SYSTEM                
*                                                                               
         LA    RF,KEY              SET I/O AREA FOR KEYS                        
         ST    RF,AIO                                                           
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY              ESTABLISH AS EDR/PUB PASSIVE POINTER         
         USING PE2PRECD,R4                                                      
*                                                                               
         MVC   PE2PKAGY,AGENCY     SET AGENCY                                   
         MVI   PE2PKMED,C'Z'       SET MEDIA                                    
         MVI   PE2PKRCD,PE2PKIDQ   SET RECORD ID                                
         MVC   PE2PKEDR,CEDRPID    SET EDR CODE                                 
*                                                                               
         GOTO1 HIGH                FIND RECORD                                  
*                                                                               
DRDISPCL DS    0H                                                               
*                                                                               
         LA    RF,0                DEFAULT FIELD LENGTH                         
*                                                                               
         CLC   PE2PKEY(PE2PKPMD-PE2PKEY),KEYSAVE SKIP IF NOT                    
         BNE   DRDISPCX               FOUND                                     
*                                                                               
         TM    PE2PCNTL,X'80'      ACCEPT IF NOT DELETED                        
         BNO   DRDISPCD                                                         
*                                                                               
DRDISPCC DS    0H                                                               
*                                                                               
         GOTO1 SEQ                 READ NEXT ON FILE                            
*                                                                               
         B     DRDISPCL                                                         
*                                                                               
DRDISPCD DS    0H                                                               
*                                                                               
*        DISPLAY PUB NUMBER                                                     
*                                                                               
         GOTO1 =V(PUBEDIT),DMCB,PE2PKPUB,8(R2),RR=WRKRELO  EXPAND PUB           
*                                                                               
         LA    RF,L'EDRPUB1        ELIMINATE TRAILING SPACES                    
         LA    R1,8-1(RF,R2)       LAST BYTE OF FIELD                           
*                                                                               
         CLI   0(R1),C' '                                                       
         BH    *+10                                                             
         BCTR  R1,0                                                             
         BCT   RF,*-10                                                          
*                                                                               
DRDISPCX DS    0H                                                               
*                                                                               
         STC   RF,5(R2)            SET INPUT LENGTH                             
*                                                                               
         OI    6(R2),X'80'         TRANSMIT FIELD                               
*                                                                               
         BAS   RE,BUMP             BUMP TO MEDIA FIELD                          
*                                                                               
         TM    1(R2),X'20'         IF FIELD PROTECTED                           
         BNO   *+8                                                              
         NI    1(R2),X'FF'-X'20'      OPEN IT UP                                
*                                                                               
         CLC   PE2PKEY(PE2PKPMD-PE2PKEY),KEYSAVE IF POINTER FOUND               
         BNE   *+14                                                             
         MVC   8(1,R2),PE2PKPMD         DISPLAY PRINTPAK MEDIA                  
         B     DRDISMDX                 FOUND                                   
*                                                                               
         CLI   CEDRMED,CEDRCONQ    IF CONSUMER MAGAZINE                         
         BNE   *+8                                                              
         MVI   8(R2),C'M'             USE MAGAZINE MEDIA                        
*                                                                               
         CLI   CEDRMED,CEDRBUSQ    IF BUSINESS  MAGAZINE                        
         BNE   *+8                                                              
         MVI   8(R2),C'T'             USE MAGAZINE MEDIA                        
*                                                                               
DRDISMDX DS    0H                                                               
*                                                                               
         MVI   5(R2),1             SET INPUT LENGTH                             
         OI    6(R2),X'80'         TRANSMIT FIELD                               
*                                                                               
         BAS   RE,BUMP             BUMP TO PUB NAME FIELD                       
*                                                                               
*        DISPLAY PUB NAME                                                       
*                                                                               
         CLC   PE2PKEY(PE2PKPMD-PE2PKEY),KEYSAVE SKIP IF NOT                    
         BNE   DRDISPNX               FOUND                                     
*                                                                               
*        READ PUB RECORD                                                        
*                                                                               
         MVC   WRKKEY,KEY          SAVE PASSIVE POINTER                         
*                                                                               
         MVI   USEIO,C'N'          TURN OFF USER DOES IO                        
         MVI   IOOPT,C'N'          TURN OFF OVERLAY DOES IO                     
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PUBRECD,R4                                                       
*                                                                               
         MVC   PUBKMED,PE2PKPMD-PE2PKEY+WRKKEY     SET MEDIA                    
         MVC   PUBKPUB(6),PE2PKPUB-PE2PKEY+WRKKEY  PUB NUMBER                   
         MVC   PUBKAGY,AGENCY                                                   
         MVI   PUBKCOD,X'81'                                                    
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
*                                                                               
         MVC   FILENAME,=CL8'PUBDIR'                                            
*                                                                               
         NI    DMINBTS,X'FF'-X'08'  IGNORE DELETED RECORDS                      
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         XC    FILENAME,FILENAME                                                
*                                                                               
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+6                 MUST FIND PUB                                
         DC    H'0'                                                             
*                                                                               
         MVC   L'CEPKEY(L'PUBKEY,R3),KEY   SAVE PUBKEY                          
*                                                                               
         L     R6,AIO2             READ PUB RECORD                              
         ST    R6,AIO                                                           
*                                                                               
         MVC   FILENAME,=CL8'PUBFILE'                                           
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         XC    FILENAME,FILENAME                                                
*                                                                               
         MVI   ELCODE,X'10'                                                     
*                                                                               
         BAS   RE,GETEL                                                         
         BE    *+6                 MUST FIND NAME ELEMENT                       
         DC    H'0'                                                             
*                                                                               
         USING PUBNAMEL,R6                                                      
*                                                                               
         MVC   8(L'EDRPBN1,R2),PUBNAME  DISPLAY PUB NAME                        
*                                                                               
         DROP  R6                                                               
*                                                                               
DRDISPNX DS    0H                                                               
*                                                                               
         MVC   AIO,AIO3            RESTORE I/OAREA POINTER                      
         MVI   USEIO,C'Y'          INDICATE USER DOES IO                        
         MVI   IOOPT,C'Y'          OVERLAY DOES IO                              
*                                                                               
         GOTO1 =A(SWTCNTL),RR=WRKRELO  RETURN TO CONTROL SYSTEM                 
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'CEPKEY),0(R3)  RESTORE CURRENT EDR LINK KEY                
*                                                                               
         GOTO1 HIGH                RESTORE FILE POINTER                         
*                                                                               
DRDISCN  DS    0H                                                               
*                                                                               
         GOTO1 SEQ                 FOUND NEXT RECORD                            
*                                                                               
         BAS   RE,BUMP             BUMP TO START OF NEXT LINE                   
*                                                                               
         MVC   WRKLSTKY,0(R3)      SAVE AS LAST KEY SO FAR                      
*                                                                               
         LA    R3,L'CEPKEY+L'PUBKEY(R3)     POINT TO NEXT SAVED KEY             
         BCT   R0,DRDISLP                                                       
*                                                                               
DRDISDN  DS    0H                                                               
*                                                                               
DRX      DS    0H                                                               
*                                                                               
         GOTO1 =A(SWTBACK),RR=WRKRELO SWITCH BACK TO USER SYSTEM                
*                                                                               
         B     EXIT                                                             
*                                                                               
         DROP  R4                                                               
*                                                                               
         TITLE 'PRWRI13 - REPORT RECORDS - LR'                                  
***********************************************************************         
*                                                                     *         
*        REPORT RECORDS                                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LR       DS    0H                                                               
*                                                                               
*        INITIALIZATION                                                         
*                                                                               
         OI    GLSTSTAT,RETEXTRA   RETURN ONCE AFTER LIST END                   
*                                                                               
         MVI   USEIO,C'Y'          INDICATE USER DOES IO                        
         MVI   IOOPT,C'Y'          OVERLAY DOES IO                              
*                                                                               
         MVC   AIO,AIO3            SET IOAREA                                   
*                                                                               
         LA    R2,P                   USE OFF-LINE PRINT AREA                   
*                                                                               
         USING RPLINED,R2          ESTABLISH REPORT LINE                        
*                                                                               
         LA    R1,EDRSPECS         SET SPECS ADDRESS                            
         ST    R1,SPECS                                                         
*                                                                               
         GOTO1 =A(SWTCNTL),RR=WRKRELO SWITCH TO CONTROL SYSTEM                  
*                                                                               
         XC    KEY,KEY             SET TO FIND RECORD OF TYPE                   
         MVC   KEY(2),=AL2(CEPKTYPQ) RECORD TYPE                                
*                                                                               
         GOTO1 HIGH                READ IN START KEY                            
*                                                                               
LRDISLP  DS    0H                                                               
*                                                                               
         MVC   RPLINE,SPACES       INIT REPORT LINE                             
*                                                                               
         LA    R4,KEY              ESTABLISH EDR LINKAGE RECORD                 
         USING CEPREC,R4                                                        
*                                                                               
         CLC   CEPKTYP,=AL2(CEPKTYPQ) DONE IF NEW RECORD TYPE                   
         BNE   LRDISDN                                                          
*                                                                               
*        FIND EDR ELEMENT                                                       
*                                                                               
         L     R4,AIO              POINT TO FOUND RECORD                        
*                                                                               
         SR    RF,RF                                                            
         LA    R6,CEPDATA          POINT TO FIRST ELEMENT                       
         USING CEDRELD,R6          ESTABLISH AS EDR ELEMENT                     
*                                                                               
         CLI   CEDRELEM,0          CONTINUE IF END OF REC REACHED               
         BE    LRDISCN                                                          
         CLI   CEDRELEM,CEDRELTQ   DONE IF ELEMENT FOUND                        
         BE    *+16                                                             
         IC    RF,CEDRLEN          ELEMENT LENGTH                               
         LA    R6,CEDRELD(R6)      POINT TO NEXT ELEMENT                        
         B     *-24                                                             
*                                                                               
         MVC   RPEDRNAM,CEDRNAME   EDR PUBNAME                                  
*                                                                               
         MVC   RPEDREDN,CEDREDN    EDITION NAME                                 
*                                                                               
         MVC   RPEDRCRD,CEDRCTYP   CARD TYPE                                    
*                                                                               
         MVC   WRKKEYSV,KEY        SAVE CURRENT EDR KEY                         
*                                                                               
*        FIND PRINTPAK NUMBER FROM PASSIVE POINTERS                             
*                                                                               
         GOTO1 =A(SWTBACK),RR=WRKRELO SWITCH BACK TO USER SYSTEM                
*                                                                               
         LA    RF,KEY              SET I/O AREA FOR KEYS                        
         ST    RF,AIO                                                           
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY              ESTABLISH AS EDR/PUB PASSIVE POINTER         
         USING PE2PRECD,R4                                                      
*                                                                               
         MVC   PE2PKAGY,AGENCY     SET AGENCY                                   
         MVI   PE2PKMED,C'Z'       SET PRINTPAK MEDIA                           
         MVI   PE2PKRCD,PE2PKIDQ   SET RECORD ID                                
         MVC   PE2PKEDR,CEDRPID    SET EDR CODE                                 
*                                                                               
         GOTO1 HIGH                FIND RECORD                                  
*                                                                               
LRDISPCL DS    0H                                                               
*                                                                               
         CLC   PE2PKEY(PE2PKPMD-PE2PKEY),KEYSAVE SKIP IF NOT                    
         BNE   LRDISPCX               FOUND                                     
*                                                                               
         TM    PE2PCNTL,X'80'      ACCEPT IF NOT DELETED                        
         BNO   LRDISPCD                                                         
*                                                                               
LRDISPCC DS    0H                                                               
*                                                                               
         GOTO1 SEQ                 READ NEXT ON FILE                            
*                                                                               
         B     LRDISPCL                                                         
*                                                                               
LRDISPCD DS    0H                                                               
*                                                                               
         GOTO1 =V(PUBEDIT),DMCB,PE2PKPUB,RPPUBID,RR=WRKRELO EXPAND PUB          
*                                                                               
LRDISPCX DS    0H                                                               
*                                                                               
         CLC   PE2PKEY(PE2PKPMD-PE2PKEY),KEYSAVE IF POINTER FOUND               
         BNE   *+14                                                             
         MVC   RPPRTMED,PE2PKPMD        DISPLAY PRINTPAK MEDIA                  
         B     LRDISMDX                 FOUND                                   
*                                                                               
         CLI   CEDRMED,CEDRCONQ    IF CONSUMER MAGAZINE                         
         BNE   *+8                                                              
         MVI   RPPRTMED,C'M'           USE MAGAZINE MEDIA                       
*                                                                               
         CLI   CEDRMED,CEDRBUSQ    IF BUSINESS  MAGAZINE                        
         BNE   *+8                                                              
         MVI   RPPRTMED,C'T'          USE MAGAZINE MEDIA                        
*                                                                               
LRDISMDX DS    0H                                                               
*                                                                               
*        DISPLAY PUB NAME                                                       
*                                                                               
         CLC   PE2PKEY(PE2PKPMD-PE2PKEY),KEYSAVE SKIP IF NOT                    
         BNE   LRDISPNX               FOUND                                     
*                                                                               
*        READ PUB RECORD                                                        
*                                                                               
         MVC   WRKKEY,KEY          SAVE PASSIVE POINTER                         
*                                                                               
         MVI   USEIO,C'N'          TURN OFF USER DOES IO                        
         MVI   IOOPT,C'N'          TURN OFF OVERLAY DOES IO                     
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PUBRECD,R4                                                       
*                                                                               
         MVC   PUBKMED,PE2PKPMD-PE2PKEY+WRKKEY SET MEDIA                        
         MVC   PUBKPUB(6),PE2PKPUB-PE2PKEY+WRKKEY  PUB NUMBER                   
         MVC   PUBKAGY,AGENCY                                                   
         MVI   PUBKCOD,X'81'                                                    
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
*                                                                               
         MVC   FILENAME,=CL8'PUBDIR'                                            
*                                                                               
         NI    DMINBTS,X'FF'-X'08'  IGNORE DELETED RECORDS                      
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         XC    FILENAME,FILENAME                                                
*                                                                               
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+6                 MUST FIND PUB                                
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO2             READ PUB RECORD                              
         ST    R6,AIO                                                           
*                                                                               
         MVC   FILENAME,=CL8'PUBFILE'                                           
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         XC    FILENAME,FILENAME                                                
*                                                                               
         MVI   ELCODE,X'10'                                                     
*                                                                               
         BAS   RE,GETEL                                                         
         BE    *+6                 MUST FIND NAME ELEMENT                       
         DC    H'0'                                                             
*                                                                               
         USING PUBNAMEL,R6                                                      
*                                                                               
         MVC   RPPRNAME,PUBNAME    DISPLAY PUB NAME                             
         MVC   RPPRZONE,PUBZNAME   DISPLAY PUB ZONE NAME                        
*                                                                               
         DROP  R6                                                               
*                                                                               
LRDISPNX DS    0H                                                               
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)     PRINT A LINE                                 
*                                                                               
         MVC   AIO,AIO3            RESTORE I/OAREA POINTER                      
         MVI   USEIO,C'Y'          INDICATE USER DOES IO                        
         MVI   IOOPT,C'Y'          OVERLAY DOES IO                              
*                                                                               
         GOTO1 =A(SWTCNTL),RR=WRKRELO  RETURN TO CONTROL SYSTEM                 
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'CEPKEY),WRKKEYSV  RESTORE CURRENT EDR LINK KEY             
*                                                                               
         GOTO1 HIGH                RESTORE FILE POINTER                         
*                                                                               
LRDISCN  DS    0H                                                               
*                                                                               
         GOTO1 SEQ                 FIND NEXT RECORD                             
*                                                                               
         B     LRDISLP                                                          
*                                                                               
LRDISDN  DS    0H                                                               
*                                                                               
LRX      DS    0H                                                               
*                                                                               
         GOTO1 =A(SWTBACK),RR=WRKRELO SWITCH BACK TO USER SYSTEM                
*                                                                               
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
BUMP     ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         BR    RE                                                               
*                                                                               
TESTYN   NTR1                                                                   
         CLI   8(R2),C'N'          Y/N                                          
         BE    NO                                                               
         CLI   8(R2),C'Y'                                                       
         BE    YES                                                              
         B     INVERR                                                           
*                                                                               
MISSERR  MVI   ERROR,MISSING                                                    
         LR    R2,R4                                                            
         B     ERRXIT                                                           
*                                                                               
INVERR   MVI   ERROR,INVALID                                                    
         B     ERRXIT                                                           
*                                                                               
NUMBERR  MVI   ERROR,NOTNUM                                                     
         B     ERRXIT                                                           
*                                                                               
ACTERR   MVI   ERROR,INVACT                                                     
         B     ERRXIT                                                           
*                                                                               
ERRXIT   GOTO1 ERREX                                                            
         B     EXIT                                                             
*                                                                               
         TITLE 'PRWRI13 - REPORT SPECS - EDRSPECS'                              
***********************************************************************         
*                                                                     *         
*        REPORT SPECS                                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
*                                                                               
EDRSPECS DS    0H                                                               
         SSPEC H1,1,RUN                                                         
         SSPEC H1,100,REQUESTOR                                                 
         SSPEC H2,100,REPORT                                                    
         SSPEC H2,113,PAGE                                                      
         SPACE 1                                                                
         SSPEC H2,51,C'EDR/PRINTPAK LINKAGE'                                    
         SSPEC H3,51,C'--------------------'                                    
         SPACE 1                                                                
         SSPEC H5,1,C'EDR PUB NAME'                                             
         SSPEC H5,31,C'EDR EDITION'                                             
         SSPEC H5,61,C'EDR RATE CARD'                                           
         SSPEC H5,77,C'PRINT ID'                                                
         SSPEC H5,93,C'MED'                                                     
         SSPEC H5,98,C'PRINTPAK PUB NAME'                                       
         SSPEC H5,118,C'ZONE NAME'                                              
         SPACE 1                                                                
         SSPEC H6,1,C'------------'                                             
         SSPEC H6,31,C'-----------'                                             
         SSPEC H6,61,C'-------------'                                           
         SSPEC H6,77,C'--------'                                                
         SSPEC H6,93,C'---'                                                     
         SSPEC H6,98,C'-----------------'                                       
         SSPEC H6,118,C'---------'                                              
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
DASHS    DC    64C'-'              DASHES                                       
         DS    0F                                                               
         LTORG                                                                  
         TITLE 'PRWRI13 - SWITCH BACK TO USER SYSTEM - SWTBACK'                 
***********************************************************************         
*                                                                     *         
*        SWITCH BACK TO USER SYSTEM                                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
SWTBACK  NMOD1 0,**#SBK                                                         
*                                                                               
         L     RC,WRKWORKA         RESTORE WORKAREA ADDRESS                     
*                                                                               
         MVC   USEIO,SVUSEIO       SET USER I/O OPTION                          
         MVC   SYSDIR,SVSYSDIR     SET FILE NAMES                               
         MVC   SYSFIL,SVSYSFIL                                                  
         MVC   DATADISP,SVDATADI   SET DISPLACEMENT OF DATA IN RECORD           
         MVC   LKEY,SVLKEY         SET KEY LENGTH                               
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+1(3),=X'FFFFFF'                                             
         MVC   DMCB(1),SVSYS       SYSTEM ID                                    
         MVC   FILENAME,SVFILENM                                                
*                                                                               
         CLI   OFFLINE,C'Y'        IF OFF-LINE                                  
         BNE   SWTBACK1                                                         
*                                                                               
         L     RF,WRKUTLA             POINT TO UTL                              
         MVC   4(1,RF),WRKSYS         RESTORE CURENT SYSTEM ID                  
         B     SWTBACKX                                                         
*                                                                               
SWTBACK1 DS    0H                                                               
*                                                                               
         GOTO1 SWITCH,DMCB         SWITCH BACK TO USER SYSTEM                   
         CLI   DMCB+4,0            MUST WORK                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
SWTBACKX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PRWRI13 - SWITCH TO CONTROL SYSTEM - SWTCNTL'                   
***********************************************************************         
*                                                                     *         
*        SWITCH TO CONTROL SYSTEM                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
SWTCNTL  NMOD1 0,**#SCT                                                         
*                                                                               
         L     RC,WRKWORKA         RESTORE WORKAREA ADDRESS                     
*                                                                               
         MVI   USEIO,C'Y'          USER DOES I/O                                
         MVC   SYSDIR,=CL8'CTFILE' SET FILE NAMES                               
         MVC   SYSFIL,=CL8'CTFILE'                                              
         MVC   DATADISP,=H'28'     SET DISPLACEMENT OF DATA IN RECORD           
         MVC   LKEY,=H'25'         SET KEY LENGTH                               
*                                                                               
         CLI   OFFLINE,C'Y'        IF OFF-LINE                                  
         BNE   SWTCNTL1                                                         
*                                                                               
         L     RF,WRKUTLA             POINT TO UTL                              
         MVI   4(RF),X'0A'            SET TO CONTROL SYSTEM                     
         B     SWTCNTLX                                                         
*                                                                               
SWTCNTL1 DS    0H                                                               
*                                                                               
         GOTO1 SWITCH,DMCB,X'0AFFFFFF',0  SWITCH TO CONTROL SYSTEM              
         CLI   DMCB+4,0            MUST WORK                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
SWTCNTLX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
         TITLE 'PRSFM13 - REPORT DISPLAY LINE - RPLINE'                         
***********************************************************************         
*                                                                     *         
*        REPORT LINE LAYOUT                                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
RPLINED  DSECT                     REPORT LINE                                  
RPLINE   DS    0CL132              REPORT LINE                                  
RPEDRNAM DS    CL30                EDR PUB NAME                                 
RPEDREDN DS    CL30                EDR EDITION                                  
RPEDRCRD DS    CL15                EDR RATE CARD                                
         DS    CL1                 SPACING                                      
RPPUBID  DS    CL16                PRINTPAK PUB ID                              
         DS    CL1                 SPACING                                      
RPPRTMED DS    CL1                 PRINTPAK MEDIA CODE                          
         DS    CL1                 SPACING                                      
         DS    CL2                 SPACING                                      
RPPRNAME DS    CL20                PRINTPAK PUB NAME                            
RPPRZONE DS    CL15                PRINTPAK PUB ZONE NAME                       
         EJECT                                                                  
       ++INCLUDE PRSFMFFD                                                       
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFMD3D                                                       
         EJECT                                                                  
       ++INCLUDE PRSFMWORKD                                                     
         SPACE 1                                                                
         ORG   SYSSPARE                                                         
*                                                                               
WRKRELO  DS    A                   THIS MODULE'S RELO FACTOR                    
WRKWORKA DS    A                   COMMON WORKAREA ADDRESS                      
WRKUTLA  DS    A                   A(UTL)                                       
WRKFCTR  DS    PL4                 FIELDS ON SCREEN COUNTER                     
WRKPUB   DS    XL6                 PUB NUMBER SAVEAREA                          
WRKEDR   DS    XL8                 EDR NUMBER SAVEAREA                          
WRKLRPUB DS    XL6                 PUB NUMBER SAVEAREA FOR LIST                 
WRKKEY   DS    XL25                KEY SAVEAREA                                 
WRKKEYSV DS    XL(L'CEPKEY)        STARTING KEY SAVEAREA                        
WRKLSTKY DS    XL(L'CEPKEY)        LAST KEY ON PAGE                             
WRKOLDPB DS    XL(L'PUBKEY)        KEY OF OLD PUB FOR LINK                      
WRKSYS   DS    XL1                 CURRENT SYSTEM SE NUMBER                     
WRKACTNM DS    XL1                 LAST ACTION NUMBER                           
WRKPUBSW DS    XL1                 C'D' - DELETING PUB FROM LINKAGE             
WRKACTSW DS    XL1                 C'A' - SOME ACTIVITY ON SCREEN               
*                                    DON'T PAGE                                 
*                                                                               
WRKSPUB  DS    XL30                STARTING PUB NAME                            
WRKSEDN  DS    XL30                STARTING EDITION WORKAREA                    
*                                                                               
SAVKEYS  DS    20XL(L'CEPKEY+L'PUBKEY)  SAVE AREA FOR 2 KEYS PER LINE           
*                                                                               
         EJECT                                                                  
       ++INCLUDE CTEDRREC                                                       
         EJECT                                                                  
*       ++INCLUDE PRGENFILE                                                     
*       ++INCLUDE PRGLOBEQUS                                                    
*       ++INCLUDE DDSPLWORKD                                                    
*       ++INCLUDE DDSPOOLD                                                      
*       ++INCLUDE DDMASTD                                                       
         PRINT OFF                                                              
       ++INCLUDE PRGENFILE                                                      
       ++INCLUDE PRGLOBEQUS                                                     
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDMASTD                                                        
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009PRSFM13E  05/01/02'                                      
         END                                                                    
