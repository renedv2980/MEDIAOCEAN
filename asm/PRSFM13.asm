*          DATA SET PRSFM13    AT LEVEL 105 AS OF 07/17/18                      
*PHASE T41C13A                                                                  
*INCLUDE PUBEDIT                                                                
*                                                                               
         TITLE 'PRSFM13 PINTPAK-SRDS PUB CODE LINKS MAINT'                      
*                                                                               
T41C13   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T41C13*,R7,RR=RE     ** NOTE R7 AS 2ND BASE **                
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
         L     R0,=A(SWTBACK)                                                   
         A     R0,WRKRELO                                                       
         ST    R0,ASWTBK                                                        
         L     R0,=A(SWTCNTL)                                                   
         A     R0,WRKRELO                                                       
         ST    R0,ASWTCN                                                        
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
         GOTO1 ASWTCN            SWITCH TO CONTROL SYSTEM                       
*                                                                               
*        FORMAT BASIC KEY                                                       
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY              ESTABLISH AS GENFIL SRDS                     
         USING GPUBNKYD,R4         PASSIVE POINTER BY NAME                      
*                                                                               
         MVI   GPUBNREC,GPUBNRCQ   SET RECORD CODE                              
         MVI   GPUBNTYP,GPUBNTYQ   SET RECORD TYPE                              
         MVC   GPUBNPBT(GPUBNPUB-GPUBNTYP),SPACES    PUB TYPE & NAME            
*                                                                               
         MVI   FLDOPT,C'Y'         OPTIONAL FIELD                               
*                                                                               
         LA    R2,SRDSPUBH         STARTING PUB NAME                            
*                                                                               
         TM    4(R2),X'80'         IF INPUT THIS TIME                           
         BNO   *+18                                                             
         XC    WRKLSTKY,WRKLSTKY      FORCE RE-DISPLAY OF FIRST PAGE            
         NI    4(R2),X'FF'-X'80'      TURN OFF INPUT IND                        
         OI    4(R2),X'60'            INDICATE VALID FIELD                      
*                                                                               
         GOTO1 VGETFLD             READ IN FIELD                                
*                                                                               
         MVC   WRKSPUB,SPACES      INIT STARTING PUB NAME SAVEAREA              
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,FLDH+5         GET INPUT LENGTH                             
         BZ    *+10                                                             
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WRKSPUB(0),FLD      MOVE TO SAVEAREA                             
*                                                                               
         LA    R2,SRDSTYPH         STARTING PUB TYPE                            
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
         CLI   FLD,C' '            BLANK ?                                      
         BNH   VKTYP               YES - USE DEFAULT ("C")                      
         CLI   FLD,C'B'            BUSINESS TYPE PUB ?                          
         BE    VKTYP10             YES                                          
         CLI   FLD,C'C'            CONSUMER TYPE PUB ?                          
         BE    VKTYP10             YES                                          
         B     INVERR              INVALID FIELD                                
*                                                                               
VKTYP    DS    0H                                                               
         MVI   FLD,C'C'            CONSUMER (DEFAULT)                           
VKTYP10  MVC   GPUBNPBT,FLD        MOVE TO KEY                                  
         MVC   WRKSTYP,FLD         MOVE TO SAVEAREA                             
*                                                                               
         MVC   GPUBNNAM,WRKSPUB    STARTING PUB NAME                            
*                                                                               
         GOTO1 HIGH                FIND A RECORD ON THE FILE                    
*                                                                               
VKLOOP   DS    0H                                                               
*                                                                               
         LA    R4,KEY              RE-POINT TO KEY AREA                         
*                                                                               
         CLI   GPUBNREC,GPUBNRCQ   DONE IF NEW RECORD CODE                      
         BNE   VKDONE                                                           
*                                                                               
         CLI   GPUBNTYP,GPUBNTYQ   OR IF NEW RECORD TYPE CODE                   
         BNE   VKDONE                                                           
*                                                                               
         CLC   GPUBNPBT,WRKSTYP                                                 
         BNE   VKDONE              OR IF NEW SRDS PUB TYPE                      
*                                                                               
******* I THINK ONE RECORD "FOUND" IS ENOUGH ****** (12/22/98)                  
*                                                                               
*NOP*    GOTO1 SEQ                                                              
*                                                                               
*NOP*    B     VKLOOP                                                           
*                                                                               
VKDONE   DS    0H                                                               
*                                                                               
         MVC   WRKKEYSV,KEY        SAVE STARTING KEY                            
*NOP*    MVC   WRKKEYSV,KEYSAVE    SAVE STARTING KEY                            
*                                                                               
         GOTO1 ASWTBK                   SWITCH BACK TO USER SYSTEM              
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
         LA    R3,L'GPUBNKY+L'PUBKEY(R3) START WITH SECOND ENTRY                
*                                                                               
         OC    0(L'GPUBNKY+L'PUBKEY,R3),0(R3) IF NONE GO DO DISPLAY             
         BZ    DR                                                               
*                                                                               
         LA    R2,SRDSNM1H         FIRST NAME/EDITION FIELD                     
         LA    R0,((SRDSNMLH-SRDSNM1H)/(SRDSNM2H-SRDSNM1H))+1 # OF LNS          
*                                                                               
         MVI   USEIO,C'Y'          INDICATE USER DOES IO                        
         MVI   IOOPT,C'Y'          OVERLAY DOES IO                              
*                                                                               
         GOTO1 ASWTCN            SWITCH TO CONTROL SYSTEM                       
*                                                                               
*                                                                               
VREDRLP  DS    0H                                                               
*                                                                               
         MVI   WRKPUBSW,0          INIT DELETE SWITCH                           
         MVC   WRKOLDPB,L'GPUBNKY(R3) SAVE CURRENT PUB ASSIGNMENT               
*                                                                               
         MVC   AIO,AIO3            READ RECORD INTO I/OAREA3                    
*                                                                               
         LA    R4,KEY              ESTABLISH SRDS LINKAGE RECORD                
         USING GPUBNKYD,R4                                                      
*                                                                               
         MVC   GPUBNKY,0(R3)       COPY KEY OF DISPLAYED PUB                    
*                                                                               
         OC    GPUBNKY,GPUBNKY     CONTINUE IF THERE IS ONE                     
         BNZ   VREDR10                                                          
*                                                                               
VREDR05  DS    0H                                                               
*                                  SKIP REST OF LINE                            
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
         CLC   GPUBNKY,KEYSAVE     MUST FIND RECORD                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 ASWTBK        SWITCH TO PRINTPAK FILES                           
*                                                                               
         MVI   USEIO,C'N'          TURN OFF USER DOING IO                       
         MVI   IOOPT,C'N'                                                       
*                                                                               
         BAS   RE,BUMP             BUMP TO PUBID    FIELD                       
*                                                                               
         ST    R2,FULL             SAVE PUBID FIELD ADDRESS                     
*                                                                               
*        VALIDATE MEDIA                                                         
*                                                                               
         BAS   RE,BUMP             BUMP TO MEDIA FIELD                          
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
         OC    WRKOLDPB,WRKOLDPB   DELETING IF OLD PUB PRESENT                  
         BNZ   VRDEL                                                            
*                                                                               
VRNOPUB0 DS    0H                                                               
*                                                                               
         BAS   RE,BUMP             BUMP TO MEDIA FIELD                          
         BAS   RE,BUMP             BUMP TO PUBNAME FIELD                        
         B     VREDRCN                                                          
*                                                                               
VRNOPUBX DS    0H      ??????????????  SMYE 12/30/98  ???????????????           
*                                                                               
         CLC   =C'DEL',8(R2)       IF DELETED                                   
         BNE   VRDELX                                                           
*                                                                               
VRDEL    DS    0H                                                               
*                                                                               
         MVC   BPUB,PUBKPUB-PUBKEY+WRKOLDPB   COPY OLD PUB KEY                  
         MVI   WRKPUBSW,C'D'          INDICATE DELETE MODE                      
         MVI   WRKACTSW,C'A'          INDICATE ACTIVITY ON SCREEN               
         MVC   8(L'SRDPUB1,R2),SPACES CLEAR OLD PUB ID                          
         OI    6(R2),X'80'            TRANSMIT FIELD                            
         MVI   5(R2),0                SET INPUT LENGTH TO ZERO                  
*                                                                               
         BAS   RE,BUMP             BUMP TO MEDIA FIELD                          
*                                                                               
         MVI   8(R2),C'T'          SET FOR TRADE MEDIA                          
         CLI   WRKSTYP,C'B'        BUSINESS TYPE SRDS PUB ?                     
         BE    *+8                 YES                                          
         MVI   8(R2),C'M'          NO - USE MAGAZINE MEDIA                      
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
*  ONLY ONE PRINTPAK PUB CODE MAY BE ASSIGNED TO AN SRDS PUB                    
*                                                                               
*NOP*    L     R4,AIO3             CONTROL SYS I/O AREA                         
*                                                                               
         LA    R4,ALLMEDT          ALL MEDIA TABLE                              
*                                                                               
VRE2PLP  DS    0H                                                               
         CLI   0(R4),X'FF'         ALL MEDIA CHECKED ?                          
         BE    VRE2PDN             YES - CONTINUE                               
*                                                                               
         LA    R6,KEY            ESTABLISH AS PUB/SRDS PASSIVE POINTER          
         ST    R6,AIO              READ RECORD INTO HERE                        
*                                                                               
         XC    KEY,KEY                                                          
         USING PPSRKD,R6                                                        
*                                                                               
         MVC   PPSRKAGY,AGENCY     SET AGENCY                                   
         MVI   PPSRKMED,C'Z'       SET MEDIA                                    
         MVI   PPSRKRCD,PPSRKIDQ   SET RECORD ID                                
         MVC   PPSRKPMD,0(R4)      SET TEST MEDIA                               
         MVC   PPSRKPUB,BPUB       SET PRINTPAK PUB'S NUMBER                    
*                                                                               
         OI    DMINBTS,X'08'       READ DELETED RECORDS                         
*                                                                               
         GOTO1 HIGH                FIND RECORD                                  
*                                                                               
         CLC   PPSRKEY(PPSRKPCL-PPSRKEY),KEYSAVE   OKAY IF NO MATCH             
         BNE   VRE2PCN                             FOUND                        
*                                                                               
         CLC   PPSRKPMD,PUBKMED-PUBKEY+WRKOLDPB    OKAY IF OLD MEDIA            
         BNE   *+10                                                             
         CLC   PPSRKPUB,PUBKPUB-PUBKEY+WRKOLDPB    OKAY IF OLD PUB ID           
         BE    VRE2PCN                                                          
*                                                                               
         TM    PPSRCNTL,X'80'      ELSE MUST BE DELETED                         
         BNO   VREDRER1            DUPLICATE PRINTPAK PUB                       
*                                                                               
VRE2PCN  DS    0H                                                               
*                                                                               
*NOP*    GOTO1 SEQ                 READ NEXT POINTER                            
*                                                                               
         LA    R4,1(R4)            NEXT MEDIA                                   
         B     VRE2PLP             CHECK NEXT MEDIA                             
*                                                                               
ALLMEDT  DC    C'BDILMNOSTVW',X'FF'                                             
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
         L     R4,AIO3             CONTROL SYS I/O AREA                         
*                                                                               
         LA    R6,KEY            ESTABLISH AS PUB/SRDS PASSIVE POINTER          
         ST    R6,AIO              READ RECORD INTO HERE                        
*                                                                               
         XC    KEY,KEY                                                          
         USING PPSRKD,R6       ESTABLISH AS PUB/SRDS PASSIVE POINTER            
*                                                                               
         MVC   PPSRKAGY,AGENCY     SET AGENCY                                   
         MVI   PPSRKMED,C'Z'       SET MEDIA                                    
         MVI   PPSRKRCD,PPSRKIDQ   SET RECORD ID                                
         MVC   PPSRKPMD,QMED       SET PRINTPAK PUB'S MEDIA                     
         MVC   PPSRKPUB,BPUB       SET PUB CODE                                 
         MVC   PPSRKTYP,GPUBNPBT   SET SRDS PUB TYPE                            
         MVC   PPSRKSRD,GPUBNPUB   SET SRDS MID NUMBER                          
*                                                                               
         OI    DMINBTS,X'08'       READ DELETED RECORDS                         
*                                                                               
         GOTO1 HIGH                READ DESIRED POINTER                         
*                                                                               
         CLC   PPSRKEY,KEYSAVE     IF NOT FOUND                                 
         BE    VRUPDPE1                                                         
*                                                                               
         CLI   WRKPUBSW,C'D'       SKIP IF DELETING A PUB                       
         BE    VRUPDPE2                                                         
*                                                                               
         MVC   KEY,KEYSAVE         RESTORE KEY                                  
         MVC   PPSRCNTL,=X'00FF'   INDICATE DIRECTORY ONLY RECORD               
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
         TM    PPSRCNTL,X'80'      IF CURRENT PUB DELETED                       
         BNO   VRUPDPE2                                                         
*                                                                               
         CLI   WRKPUBSW,C'D'          DONE IF WE ARE DELETING                   
         BE    VRUPDPE2                                                         
*                                                                               
         NI    PPSRCNTL,X'FF'-X'80'   ELSE MUST RESTORE OLD POINTER             
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
         L     R4,AIO3             CONTROL SYS I/O AREA                         
*                                                                               
         LA    R6,KEY            ESTABLISH AS PUB/SRDS PASSIVE POINTER          
         ST    R6,AIO              READ RECORD INTO HERE                        
*                                                                               
         XC    KEY,KEY                                                          
         USING PPSRKD,R6       ESTABLISH AS PUB/SRDS PASSIVE POINTER            
*                                                                               
         MVC   PPSRKAGY,AGENCY     SET AGENCY                                   
         MVI   PPSRKMED,C'Z'       SET MEDIA                                    
         MVI   PPSRKRCD,PPSRKIDQ   SET RECORD ID                                
         MVC   PPSRKPMD,PUBKMED-PUBKEY+WRKOLDPB    SET MEDIA                    
         MVC   PPSRKPUB,PUBKPUB-PUBKEY+WRKOLDPB    SET PUB CODE                 
         MVC   PPSRKTYP,GPUBNPBT   SET SRDS PUB TYPE                            
         MVC   PPSRKSRD,GPUBNPUB   SET SRDS MID NUMBER                          
*                                                                               
         OI    DMINBTS,X'08'       READ DELETED RECORDS                         
*                                                                               
         GOTO1 HIGH                READ DESIRED POINTER                         
*                                                                               
         CLC   PPSRKEY,KEYSAVE     MUST FIND IT                                 
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OI    PPSRCNTL,X'80'      DELETE OLD POINTER                           
*                                                                               
         MVI   WRKACTSW,C'A'       INDICATE THERE WAS ACTIVITY                  
*                                                                               
         GOTO1 WRITE                  RE-WRITE RECORD                           
*                                                                               
VRUPDPEX DS    0H                                                               
*                                                                               
*         UPDATE SRDS/PUB PASSIVE POINTER                                       
*                                                                               
         XC    KEY,KEY                                                          
         USING PSRPKD,R6       ESTABLISH AS SRDS/PUB PASSIVE POINTER            
*                                                                               
         MVC   PSRPKAGY,AGENCY     SET AGENCY                                   
         MVI   PSRPKMED,C'Z'       SET MEDIA                                    
         MVI   PSRPKRCD,PSRPKIDQ   SET RECORD ID                                
         MVC   PSRPKTYP,GPUBNPBT   SET SRDS PUB TYPE                            
         MVC   PSRPKSRD,GPUBNPUB   SET SRDS MID NUMBER                          
         MVC   PSRPKPMD,QMED       SET MEDIA FOR PRINTPAK PUB                   
         MVC   PSRPKPUB,BPUB       SET PUB CODE                                 
*                                                                               
         OI    DMINBTS,X'08'       READ DELETED RECORDS                         
*                                                                               
         GOTO1 HIGH                FIND RECORD                                  
*                                                                               
         CLC   PSRPKEY,KEYSAVE     IF RECORD NOT FOUND                          
         BE    VRUPDEP1                                                         
*                                                                               
         CLI   WRKPUBSW,C'D'       SKIP IF DELETING A PUB                       
         BE    VRUPDEP2                                                         
*                                                                               
         MVC   KEY,KEYSAVE         RESTORE KEY                                  
         MVC   PSRPCNTL,=X'00FF'   INDICATE DIRECTORY ONLY RECORD               
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
         TM    PSRPCNTL,X'80'      IF CURRENT PUB DELETED                       
         BNO   VRUPDEP2                                                         
*                                                                               
         CLI   WRKPUBSW,C'D'          DONE IF WE ARE DELETING                   
         BE    VRUPDEP2                                                         
*                                                                               
         NI    PSRPCNTL,X'FF'-X'80'   ELSE MUST RESTORE OLD POINTER             
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
         LA    R6,KEY            ESTABLISH AS SRDS/PUB PASSIVE POINTER          
         ST    R6,AIO              READ RECORD INTO HERE                        
*                                                                               
         XC    KEY,KEY                                                          
         USING PSRPKD,R6       ESTABLISH AS SRDS/PUB PASSIVE POINTER            
*                                                                               
         MVC   PSRPKAGY,AGENCY     SET AGENCY                                   
         MVI   PSRPKMED,C'Z'       SET MEDIA                                    
         MVI   PSRPKRCD,PSRPKIDQ   SET RECORD ID                                
         MVC   PSRPKTYP,GPUBNPBT   SET SRDS PUB TYPE                            
         MVC   PSRPKSRD,GPUBNPUB   SET SRDS MID NUMBER                          
         MVC   PSRPKPMD,PUBKMED-PUBKEY+WRKOLDPB    SET MEDIA                    
         MVC   PSRPKPUB,PUBKPUB-PUBKEY+WRKOLDPB    SET PUB CODE                 
*                                                                               
         OI    DMINBTS,X'08'       READ DELETED RECORDS                         
*                                                                               
         GOTO1 HIGH                READ DESIRED POINTER                         
*                                                                               
         CLC   PSRPKEY,KEYSAVE     MUST FIND IT                                 
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OI    PSRPCNTL,X'80'         ELSE MUST DELETE OLD POINTER              
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
         XC    L'GPUBNKY(L'PUBKEY,R3),L'GPUBNKY(R3) INIT PUBKEY SAVE            
*                                                                               
         OC    BPUB,BPUB           SKIP IF NO PUB                               
         BZ    VREDRPNX               ENTERED                                   
*                                                                               
         CLI   WRKPUBSW,C'D'       IF DELETING PUB                              
         BNE   VREDRPN1                                                         
*                                                                               
         MVC   8(L'SRDPBN1,R2),SPACES   CLEAR OLD NAME                          
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
         MVC   L'GPUBNKY(L'PUBKEY,R3),0(R6) SAVE PUBKEY                         
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
         MVC   8(L'SRDPBN1,R2),PUBNAME  DISPLAY PUB NAME                        
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
         GOTO1 ASWTCN                  RETURN TO CONTROL SYSTEM                 
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'GPUBNKY),0(R3) RESTORE CURRENT SRDS LINK KEY               
*                                                                               
         GOTO1 HIGH                RESTORE FILE POINTER                         
*                                                                               
VREDRCN  DS    0H                                                               
*                                                                               
         MVI   USEIO,C'Y'          INDICATE USER DOES IO                        
         MVI   IOOPT,C'Y'          OVERLAY DOES IO                              
*                                                                               
         GOTO1 ASWTCN            SWITCH TO CONTROL SYSTEM                       
*                                                                               
         BAS   RE,BUMP             BUMP TO START OF NEXT LINE                   
*                                                                               
         LA    R3,L'GPUBNKY+L'PUBKEY(R3) POINT TO NEXT SAVED KEY                
         BCT   R0,VREDRLP                                                       
*                                                                               
VREDRDN  DS    0H                                                               
*                                                                               
VRX      DS    0H                                                               
*                                                                               
         GOTO1 ASWTBK              SWITCH BACK TO USER SYSTEM                   
*                                                                               
         CLI   WRKACTSW,C'A'       IF THERE IS NO ACTIVITY ON SCREEN            
         BNE   DR                     GO SEE IF WE NEED TO PAGE.                
*                                                                               
         B     EXIT                                                             
*                                                                               
VREDRER1 DS    0H                                                               
         MVI   ERROR,EDRDUPPB      DUPLICATE PUB CODE                           
*                                                                               
*****    B     VREDRERR                                                         
*                                                                               
VREDRERR DS    0H                                                               
*                                                                               
         GOTO1 ASWTBK              SWITCH BACK TO USER SYSTEM                   
*                                                                               
         B     ERRXIT                                                           
*                                                                               
         DROP R4                                                                
*                                                                               
EDRDUPPB EQU   181                 PUB ALREADY ASSIGNED                         
         TITLE 'PRSFM13 - DISPLAY RECORD - DR'                                  
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
         TWAXC SRDSNM1H,SRDPBNLH,PROT=Y,TRNS=Y  CLEAR OLD SCREEN                
*                                                                               
         GOTO1 ASWTCN            SWITCH TO CONTROL SYSTEM                       
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
         XC    0(L'GPUBNKY+L'PUBKEY,R3),0(R3) INIT FIRST ENTRY                  
         LA    R3,L'GPUBNKY+L'PUBKEY(R3)    START WITH SECOND ENTRY             
*                                                                               
         LA    R2,SRDSNM1H         FIRST NAME FIELD                             
         LA    R0,((SRDSNMLH-SRDSNM1H)/(SRDSNM2H-SRDSNM1H))+1 # OF LNS          
*                                                                               
         OI    SRDSTNMH+6,X'80'    TRANSMIT FIELD                               
         MVC   SRDSTNM,=C'(CONSUMER)'                                           
         CLI   WRKSTYP,C'C'        CONSUMER TYPE SRDS PUB ?                     
         BE    *+10                YES                                          
         MVC   SRDSTNM,=C'(BUSINESS)'                                           
*                                                                               
DRDISLP  DS    0H                                                               
*                                                                               
         XC    0(L'GPUBNKY+L'PUBKEY,R3),0(R3) INIT SAVEAREA                     
*                                                                               
         LA    R4,KEY              ESTABLISH SRDS LINKAGE RECORD                
         USING GPUBNKYD,R4                                                      
*                                                                               
         CLI   GPUBNREC,GPUBNRCQ   DONE IF NEW RECORD CODE                      
         BNE   DRDISDN                                                          
*                                                                               
         CLI   GPUBNTYP,GPUBNTYQ   DONE IF NEW RECORD TYPE CODE                 
         BNE   DRDISDN                                                          
*                                                                               
         CLC   GPUBNPBT,WRKSTYP    OR IF NEW SRDS PUB TYPE                      
         BNE   DRDISDN                                                          
*                                                                               
         MVC   0(L'GPUBNKY,R3),KEY  SAVE LATEST KEY                             
*                                                                               
         LR    RF,R3                                                            
         SH    RF,=Y(L'GPUBNKY+L'PUBKEY)    BACKUP A SAVED KEY                  
*                                                                               
*                                  READ CONTROL RECORD                          
         L     R6,AIO2                                                          
         GOTO1 DATAMGR,DMCB,(0,=C'GETREC'),=C'GENFIL',KEY+36,(R6),     +        
               DMWORK                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                 NO ERRORS TOLERATED                          
         DC    H'0'                                                             
*                                                                               
         LA    R6,42(R6)           POINT TO FIRST ELEMENT                       
         USING GPUBHD,R6           ESTABLISH AS SRDS NAME ELEMENT               
*                                                                               
         CLI   0(R6),GPUBHEQU                                                   
         BE    *+6                 MUST FIND NAME ELEMENT                       
         DC    H'0'                                                             
*                                                                               
         MVC   8(L'SRDSNM1,R2),GPUBHALN   SRDS PUBNAME (1ST 30 BYTES)           
*                                  USE BELOW IF GENFIL NOT READ                 
*NOP*    MVC   8(L'GPUBNNAM,R2),GPUBNNAM  SRDS PUBNAME (1ST 20 BYTES)           
*                                                                               
         MVC   WRKKEYSV,KEY        SAVE CURRENT SRDS KEY                        
*                                                                               
         DROP  R6                                                               
*                                                                               
         BAS   RE,BUMP             SKIP TO PRINTPAK PUB FIELD                   
*                                                                               
*NOP*    OI    1(R2),X'20'         PROTECT PUB FIELD                            
*                                                                               
*NOP*    TM    1(R2),X'20'         IF FIELD PROTECTED                           
*NOP*    BNO   *+8                                                              
*NOP*    NI    1(R2),X'FF'-X'20'      OPEN IT UP                                
*                                                                               
*        FIND PRINTPAK NUMBER FROM PASSIVE POINTERS                             
*                                                                               
         MVI   USEIO,C'N'          TURN OFF IO INDICATORS                       
         MVI   IOOPT,C'N'                                                       
*                                                                               
         GOTO1 ASWTBK      SWITCH BACK TO USER SYSTEM                           
*                                                                               
         L     R4,AIO3             CONTROL SYS I/O AREA                         
*                                                                               
         LA    RF,KEY              SET I/O AREA FOR KEYS                        
         ST    RF,AIO                                                           
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY            ESTABLISH AS SRDS/PUB PASSIVE POINTER          
         USING PSRPKD,R6                                                        
*                                                                               
         MVC   PSRPKAGY,AGENCY     SET AGENCY                                   
         MVI   PSRPKMED,C'Z'       SET MEDIA                                    
         MVI   PSRPKRCD,PSRPKIDQ   SET RECORD ID                                
         MVC   PSRPKTYP,GPUBNPBT   SET SRDS PUB TYPE                            
         MVC   PSRPKSRD,GPUBNPUB   SET SRDS PUB NUMBER (MID)                    
*                                                                               
         GOTO1 HIGH                FIND RECORD                                  
*                                                                               
DRDISPCL DS    0H                                                               
*                                                                               
         LA    RF,0                DEFAULT FIELD LENGTH                         
*                                                                               
         CLC   PSRPKEY(PSRPKPCL-PSRPKEY),KEYSAVE    SKIP IF NOT                 
         BNE   DRDISPCX                             FOUND                       
*                                                                               
         TM    PSRPCNTL,X'80'      ACCEPT IF NOT DELETED                        
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
         GOTO1 =V(PUBEDIT),DMCB,PSRPKPUB,8(R2),RR=WRKRELO  EXPAND PUB           
*                                                                               
         LA    RF,L'SRDPUB1        ELIMINATE TRAILING SPACES                    
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
*NOP*    TM    1(R2),X'20'         IF FIELD PROTECTED                           
*NOP*    BNO   *+8                                                              
*NOP*    NI    1(R2),X'FF'-X'20'      OPEN IT UP                                
*                                                                               
         CLC   PSRPKEY(PSRPKPCL-PSRPKEY),KEYSAVE    IF POINTER FOUND            
         BNE   *+14                                                             
         MVC   8(1,R2),PSRPKPMD         DISPLAY PRINTPAK MEDIA                  
         B     DRDISMDX                 FOUND                                   
*                                                                               
         MVI   8(R2),C'T'          SET FOR TRADE MEDIA                          
         CLI   WRKSTYP,C'B'        BUSINESS TYPE SRDS PUB ?                     
         BE    *+8                 YES                                          
         MVI   8(R2),C'M'          NO - USE MAGAZINE MEDIA                      
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
         CLC   PSRPKEY(PSRPKPCL-PSRPKEY),KEYSAVE    SKIP IF NOT                 
         BNE   DRDISPNX                             FOUND                       
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
         MVC   PUBKMED,PSRPKPMD-PSRPKEY+WRKKEY     SET MEDIA                    
         MVC   PUBKPUB(6),PSRPKPUB-PSRPKEY+WRKKEY  PUB NUMBER                   
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
         MVC   L'GPUBNKY(L'PUBKEY,R3),KEY  SAVE PUBKEY                          
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
         MVC   8(L'SRDPBN1,R2),PUBNAME  DISPLAY PUB NAME                        
*                                                                               
         DROP  R6                                                               
*                                                                               
DRDISPNX DS    0H                                                               
*                                                                               
         MVC   AIO,AIO3            RESTORE I/OAREA POINTER                      
         MVI   USEIO,C'Y'          INDICATE USER DOES IO                        
         MVI   IOOPT,C'Y'          OVERLAY DOES IO                              
*                                                                               
         GOTO1 ASWTCN                  RETURN TO CONTROL SYSTEM                 
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'GPUBNKY),0(R3) RESTORE CURRENT SRDS LINK KEY               
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
         LA    R3,L'GPUBNKY+L'PUBKEY(R3)    POINT TO NEXT SAVED KEY             
         BCT   R0,DRDISLP                                                       
*                                                                               
DRDISDN  DS    0H                                                               
*                                                                               
DRX      DS    0H                                                               
*                                                                               
         GOTO1 ASWTBK            SWITCH BACK TO USER SYSTEM                     
*                                                                               
         B     EXIT                                                             
*                                                                               
         DROP  R4                                                               
*                                                                               
         TITLE 'PRSFM13 - REPORT RECORDS - LR'                                  
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
         LA    R1,SRDSPECS         SET SPECS AND HOOK ADDRESSES                 
         ST    R1,SPECS                                                         
         LA    R1,SRDHOOK                                                       
         ST    R1,HEADHOOK                                                      
*                                                                               
*                       READ AGENCY FILE FOR NAME AND ADDRESS                   
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PAGYRECD,R4         ESTABLISH AGENCY RECORD                      
*                                                                               
         MVC   PAGYKAGY,AGENCY                                                  
         MVI   PAGYKMED,C'M'       USE MAGAZINE MEDIA                           
         MVI   PAGYKRCD,X'01'      RECORD CODE                                  
*                                                                               
         GOTO1 HIGH                READ IN KEY                                  
         CLC   PAGYKEY,KEYSAVE                                                  
         BE    *+6                 MUST BE FOUND                                
         DC    H'0'                                                             
*                                                                               
         GOTO1 GETREC              GET THE RECORD                               
*                                                                               
         L     R4,AIO              POINT TO RECORD                              
         MVC   WRKAGNM(L'PAGYNAME),PAGYNAME     SAVE AGENCY NAME AND            
         MVC   WRKAGAD(L'PAGYADDR),PAGYADDR     ADDRESS FOR HEADLINES           
*                                                                               
         DROP  R4                                                               
*                                                                               
         GOTO1 ASWTCN            SWITCH TO CONTROL SYSTEM                       
*                                                                               
         XC    KEY,KEY             SET TO FIND RECORD OF TYPE                   
         MVI   KEY,GPUBNRCQ        RECORD CODE                                  
         MVI   KEY+1,GPUBNTYQ      RECORD TYPE CODE                             
*                                                                               
         GOTO1 HIGH                READ IN START KEY                            
         MVC   WRKPBT,GPUBNPBT-GPUBNREC+KEY   SAVE SRDS PUB TYPE CODE           
*                                                                               
LRDISLP  DS    0H                                                               
*                                                                               
         MVC   RPLINE,SPACES       INIT REPORT LINE                             
*                                                                               
         LA    R4,KEY              ESTABLISH SRDS LINKAGE RECORD                
         USING GPUBNKYD,R4                                                      
*                                                                               
         CLI   GPUBNREC,GPUBNRCQ   DONE IF NEW RECORD CODE                      
         BNE   LRDISDN                                                          
*                                                                               
         CLI   GPUBNTYP,GPUBNTYQ   OR IF NEW RECORD TYPE CODE                   
         BNE   LRDISDN                                                          
*                                                                               
         CLC   WRKPBT,GPUBNPBT     SAME SRDS PUB TYPE CODE ?                    
         BE    LRDISLP2            YES                                          
         MVI   FORCEHED,C'Y'       FORCE NEW PAGE                               
         MVC   WRKPBT,GPUBNPBT                                                  
*                                                                               
LRDISLP2 DS    0H                  READ CONTROL RECORD                          
         L     R6,AIO2                                                          
         GOTO1 DATAMGR,DMCB,(0,=C'GETREC'),=C'GENFIL',KEY+36,(R6),     +        
               DMWORK                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                 NO ERRORS TOLERATED                          
         DC    H'0'                                                             
*                                                                               
         LA    R6,42(R6)           POINT TO FIRST ELEMENT                       
         USING GPUBHD,R6           ESTABLISH AS SRDS NAME ELEMENT               
*                                                                               
         CLI   0(R6),GPUBHEQU                                                   
         BE    *+6                 MUST FIND NAME ELEMENT                       
         DC    H'0'                                                             
*                                                                               
         MVC   RPSRDNAM,GPUBHALN   SRDS PUBNAME                                 
*NOP*    MVC   RPSRDNAM,GPUBNNAM   SRDS PUBNAME (IF GENFIL NOT READ)            
*                                                                               
         MVC   WRKKEYSV,KEY        SAVE CURRENT SRDS KEY                        
*                                                                               
         DROP  R6                                                               
*                                                                               
*        FIND PRINTPAK NUMBER FROM PASSIVE POINTERS                             
*                                                                               
         GOTO1 ASWTBK        SWITCH BACK TO USER SYSTEM                         
*                                                                               
         LA    RF,KEY              SET I/O AREA FOR KEYS                        
         ST    RF,AIO                                                           
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY             ESTABLISH AS SRDS/PUB PASSIVE POINTER         
         USING PSRPKD,R6                                                        
*                                                                               
         L     R4,AIO3             POINT TO SRDS LINK KEY                       
*                                                                               
         MVC   PSRPKAGY,AGENCY     SET AGENCY                                   
         MVI   PSRPKMED,C'Z'       SET PRINTPAK MEDIA                           
         MVI   PSRPKRCD,PSRPKIDQ   SET RECORD ID                                
         MVC   PSRPKTYP,GPUBNPBT   SET SRDS PUB TYPE                            
         MVC   PSRPKSRD,GPUBNPUB   SET SRDS PUB NUMBER (MID)                    
*                                                                               
         GOTO1 HIGH                FIND RECORD                                  
*                                                                               
         DROP  R4                                                               
*                                                                               
LRDISPCL DS    0H                                                               
*                                                                               
         CLC   PSRPKEY(PSRPKPMD-PSRPKEY),KEYSAVE    SKIP IF NOT                 
         BNE   LRDISPCX                             FOUND                       
*                                                                               
         TM    PSRPCNTL,X'80'      ACCEPT IF NOT DELETED                        
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
         GOTO1 =V(PUBEDIT),DMCB,PSRPKPUB,RPPUBID,RR=WRKRELO EXPAND PUB          
*                                                                               
LRDISPCX DS    0H                                                               
*                                                                               
         CLC   PSRPKEY(PSRPKPMD-PSRPKEY),KEYSAVE  IF POINTER FOUND              
         BNE   *+14                                                             
         MVC   RPPRTMED,PSRPKPMD        DISPLAY PRINTPAK MEDIA                  
         B     LRDISMDX                 FOUND                                   
*                                                                               
LRDISMDX DS    0H                                                               
*                                                                               
*        DISPLAY PUB NAME                                                       
*                                                                               
         CLC   PSRPKEY(PSRPKPMD-PSRPKEY),KEYSAVE  SKIP IF NOT                   
         BNE   LRDISPNX                           FOUND                         
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
         MVC   PUBKMED,PSRPKPMD-PSRPKEY+WRKKEY        SET MEDIA                 
         MVC   PUBKPUB(6),PSRPKPUB-PSRPKEY+WRKKEY     PUB NUMBER                
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
         DROP  R6                                                               
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
         XC    P2,P2                                                            
         GOTO1 SPOOL,DMCB,(R8)     PRINT A LINE                                 
*                                                                               
         MVC   AIO,AIO3            RESTORE I/OAREA POINTER                      
         MVI   USEIO,C'Y'          INDICATE USER DOES IO                        
         MVI   IOOPT,C'Y'          OVERLAY DOES IO                              
*                                                                               
         GOTO1 ASWTCN                  RETURN TO CONTROL SYSTEM                 
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'GPUBNKY),WRKKEYSV   RESTORE CURRENT SRDS LINK KEY          
*                                                                               
         GOTO1 HIGH                RESTORE FILE POINTER                         
*                                                                               
LRDISCN  DS    0H                                                               
*                                                                               
         MVC   WRKPBT,GPUBNPBT-GPUBNREC+KEY   SAVE SRDS PUB TYPE CODE           
*                                                                               
         GOTO1 SEQ                 FIND NEXT RECORD                             
*                                                                               
         B     LRDISLP                                                          
*                                                                               
LRDISDN  DS    0H                                                               
*                                                                               
LRX      DS    0H                                                               
*                                                                               
         GOTO1 ASWTBK            SWITCH BACK TO USER SYSTEM                     
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
         LA    R2,CONACTH                                                       
         B     ERRXIT                                                           
*                                                                               
ERRXIT   GOTO1 ERREX                                                            
         B     EXIT                                                             
*                                                                               
         TITLE 'PRSFM13 - REPORT SPECS - SRDSPECS'                              
***********************************************************************         
*                                                                     *         
*        REPORT SPECS                                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
*                                                                               
SRDSPECS DS    0H                                                               
         SSPEC H1,51,C'SRDS/PRINTPAK LINKAGE'                                   
         SSPEC H2,51,C'---------------------'                                   
*NOP*    SSPEC H1,99,AGYNAME       AGENCY FILE READ AT TOP OF LR                
*NOP*    SSPEC H2,99,AGYADD        NAME AND ADDRESS SET THERE                   
         SSPEC H3,99,RUN                                                        
         SSPEC H4,99,REQUESTOR                                                  
         SSPEC H5,99,REPORT                                                     
         SSPEC H5,117,PAGE                                                      
         SPACE 1                                                                
         SSPEC H7,1,C'SRDS PUB NAME'                                            
         SSPEC H7,55,C'PRINTPAK PUB CODE'                                       
         SSPEC H7,75,C'MED'                                                     
         SSPEC H7,81,C'PRINTPAK PUB NAME'                                       
         SSPEC H7,103,C'ZONE NAME'                                              
         SPACE 1                                                                
         SSPEC H8,1,C'---------------------------'                              
         SSPEC H8,55,C'-----------------'                                       
         SSPEC H8,75,C'---'                                                     
         SSPEC H8,81,C'-----------------'                                       
         SSPEC H8,103,C'---------'                                              
         SPACE 1                                                                
         DC    X'00'                                                            
         SPACE 2                                                                
         DS    0D                                                               
*                                                                               
SRDHOOK  NTR1                                                                   
         MVC   H1+98(L'PAGYNAME),WRKAGNM      SET AGENCY NAME                   
         MVC   H2+98(L'PAGYADDR),WRKAGAD          AND ADDRESS                   
         MVC   H7+15(10),=C'(CONSUMER)'                                         
         CLI   WRKPBT,C'C'         CONSUMER TYPE SRDS PUB ?                     
         BE    *+10                YES                                          
         MVC   H7+15(10),=C'(BUSINESS)'                                         
SRDHOOKX XIT1                                                                   
*                                                                               
DASHS    DC    64C'-'              DASHES                                       
         EJECT                                                                  
         DS    0F                                                               
         LTORG                                                                  
         TITLE 'PRSFM13 - SWITCH BACK TO USER SYSTEM - SWTBACK'                 
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
         TITLE 'PRSFM13 - SWITCH TO CONTROL SYSTEM - SWTCNTL'                   
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
         MVC   SYSDIR,=CL8'GENDIR' SET FILE NAMES                               
         MVC   SYSFIL,=CL8'GENFIL'                                              
         MVC   DATADISP,=H'42'     SET DISPLACEMENT OF DATA IN RECORD           
         MVC   LKEY,=H'32'         SET KEY LENGTH                               
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
RPSRDNAM DS    CL50                SRDS PUB NAME                                
         DS    CL4                 SPACING                                      
RPPUBID  DS    CL16                PRINTPAK PUB ID                              
         DS    CL5                 SPACING                                      
RPPRTMED DS    CL1                 PRINTPAK MEDIA CODE                          
         DS    CL4                 SPACING                                      
RPPRNAME DS    CL20                PRINTPAK PUB NAME                            
         DS    CL3                 SPACING                                      
RPPRZONE DS    CL20                PRINTPAK PUB ZONE NAME                       
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
WRKSRDS  DS    XL9                 SRDS NUMBER SAVEAREA                         
WRKLRPUB DS    XL6                 PUB NUMBER SAVEAREA FOR LIST                 
WRKKEY   DS    XL(L'PUBKEY)        KEY SAVEAREA                                 
WRKKEYSV DS    XL(L'GPUBNKY)       STARTING KEY SAVEAREA                        
WRKLSTKY DS    XL(L'GPUBNKY)       LAST KEY ON PAGE                             
WRKOLDPB DS    XL(L'PUBKEY)        KEY OF OLD PUB FOR LINK                      
WRKSYS   DS    XL1                 CURRENT SYSTEM SE NUMBER                     
WRKACTNM DS    XL1                 LAST ACTION NUMBER                           
WRKPBT   DS    XL1                 LAST SRDS PUB TYPE (B OR C)                  
WRKPUBSW DS    XL1                 C'D' - DELETING PUB FROM LINKAGE             
WRKACTSW DS    XL1                 C'A' - SOME ACTIVITY ON SCREEN               
*                                    DON'T PAGE                                 
*                                                                               
WRKSPUB  DS    XL30                STARTING PUB NAME                            
WRKSTYP  DS    XL1                 STARTING PUB TYPE                            
WRKAGNM  DS    XL(L'PAGYNAME)      AGENCY NAME SAVED FOR HEADLINES              
WRKAGAD  DS    XL(L'PAGYADDR)      AGENCY ADDRESS SAVED FOR HEADLINES           
ASWTBK   DS    A                                                                
ASWTCN   DS    A                                                                
*                                                                               
SAVKEYS  DS    20XL(L'GPUBNKY+L'PUBKEY)  SAVE AREA FOR 2 KEYS PER LINE          
*                                                                               
         EJECT                                                                  
       ++INCLUDE CTEDRREC                                                       
         EJECT                                                                  
PPSRRECD DSECT                                                                  
       ++INCLUDE PPPUBSRDPP                                                     
         EJECT                                                                  
PSRPRECD DSECT                                                                  
       ++INCLUDE PPSRDPUBPP                                                     
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
**PAN#1  DC    CL21'105PRSFM13   07/17/18'                                      
         END                                                                    
