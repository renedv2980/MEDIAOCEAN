*          DATA SET PRSFM0E    AT LEVEL 096 AS OF 05/01/02                      
*PHASE T41C0EA                                                                  
*INCLUDE PUBEDIT                                                                
*                                                                               
         TITLE 'PRSFM0E PINTPAK-EDR PUB CODE LINKS MAINT'                       
*                                                                               
T41C0E   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T41C0E*,R7,RR=RE                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
         ST    RE,LCLRELO          SAVE RELOCATION FACTOR                       
         ST    RC,LCLWORKA         SAVE WORKAREA ADDRESS                        
*                                                                               
         MVI   USEIO,C'Y'          INDICATE USER DOES IO                        
         MVI   IOOPT,C'Y'          OVERLAY DOES IO                              
         MVI   ACTELOPT,C'N'       NO ACTIVITY ELEMENT PRESENT                  
         LA    RF,KEY              SET I/O AREA FOR KEYS                        
         ST    RF,AIO                                                           
*                                                                               
         CLI   ACTNUM,ACTADD       INVALID ACTIONS                              
         BE    ACTERR                                                           
         CLI   ACTNUM,ACTDEL       INVALID ACTIONS                              
         BE    ACTERR                                                           
         CLI   ACTNUM,ACTREST      INVALID ACTIONS                              
         BE    ACTERR                                                           
*                                                                               
MAIN10   CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    LR                                                               
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
         MVC   AIO,AIO1                                                         
         MVI   USEIO,C'N'          TURN OFF USER DOES IO                        
         MVI   IOOPT,C'N'          TURN OFF OVERLAY DOES IO                     
         MVC   LCLKEY,KEY          SAVE INCOMING KEY                            
*                                                                               
         LA    R2,SUSMEDH          MEDIA                                        
         GOTO1 VALIMED                                                          
*                                                                               
         LA    R2,SUSPUBH          PUB                                          
*                                                                               
*        HANDLE EDR= INPUT                                                      
*                                                                               
         CLC   =C'EDR=',SUSPUB     CHECK FOR KEYWORD 'EDR='                     
         BNE   VKEDRN                                                           
*                                                                               
         CLI   SUSPUBH+5,13        LENGTH MUST BE 4+9                           
         BNE   VREDRER1                                                         
*                                                                               
         MVC   WORK(9),SUSPUB+4    COPY EDR CODE                                
         NC    WORK(9),=9C'0'      KILL NUMERICS                                
         CLC   WORK(9),=9C'0'      MUST BE EQUAL TO BE NUMERIC                  
         BNE   VREDRER1                                                         
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY              ESTABLISH AS EDR/PUB PASSIVE POINTER         
         USING PEDPRECD,R4                                                      
*                                                                               
         MVC   LCLEDR,SUSPUB+4     SAVE EDR CODE                                
*                                                                               
         MVC   PEDPKAGY,AGENCY     SET AGENCY                                   
         MVC   PEDPKMED,QMED       SET MEDIA                                    
         MVI   PEDPKRCD,PEDPKIDQ   SET RECORD ID                                
         MVC   PEDPKEDR,SUSPUB+4   SET EDR CODE                                 
*                                                                               
         MVI   USEIO,C'Y'          INDICATE USER DOES IO                        
         MVI   IOOPT,C'Y'          OVERLAY DOES IO                              
*                                                                               
         LA    RF,KEY              SET I/O AREA FOR KEYS                        
         ST    RF,AIO                                                           
*                                                                               
         GOTO1 HIGH                FIND RECORD                                  
*                                                                               
         CLC   KEY(PEDPKPUB-PEDPKEY),KEYSAVE   MUST FIND A RECORD               
         BNE   VREDRER5                                                         
*                                                                               
         CLI   ACTNUM,ACTLIST      IF LISTING KEEP EDR= IN FIELD                
         BE    VKX                                                              
*                                                                               
         XC    SUSPUB,SUSPUB                                                    
*                                                                               
         GOTO1 =V(PUBEDIT),DMCB,PEDPKPUB,SUSPUB,RR=LCLRELO  EXPAND PUB          
*                                                                               
         OI    SUSPUBH+6,X'80'     TRANSMIT FIELD                               
*                                                                               
         LA    RF,L'SUSPUB         ELIMINATE TRAILING SPACES                    
         LA    R1,SUSPUB-1(RF)                                                  
*                                                                               
         CLI   0(R1),C' '                                                       
         BH    *+10                                                             
         BCTR  R1,0                                                             
         BCT   RF,*-10                                                          
*                                                                               
         STC   RF,SUSPUBH+5        SET INPUT LENGTH                             
*                                                                               
VKEDRN   DS    0H                                                               
*                                                                               
         GOTO1 VALIPUB                                                          
*                                                                               
         MVC   SUSPUBN,PUBNM       DISPLAY PUB NAME                             
         OI    SUSPUBNH+6,X'80'    TRANSMIT FIELD                               
*                                                                               
         MVI   USEIO,C'Y'          INDICATE USER DOES IO                        
         MVI   IOOPT,C'Y'          OVERLAY DOES IO                              
*                                                                               
*        FORMAT BASIC KEY                                                       
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY              ESTABLISH AS EDR/PUB PASSIVE POINTER         
         USING PPEDRECD,R4                                                      
*                                                                               
         MVC   PPEDKAGY,AGENCY     SET AGENCY                                   
         MVC   PPEDKMED,QMED       SET MEDIA                                    
         MVI   PPEDKRCD,PPEDKIDQ   SET RECORD ID                                
         MVC   PPEDKPUB,BPUB       PUB NUMBER                                   
*                                                                               
         LA    RF,KEY              SET I/O AREA FOR KEYS                        
         ST    RF,AIO                                                           
*                                                                               
         GOTO1 HIGH                FIND A RECORD ON THE FILE                    
*                                                                               
VKX      DS    0H                                                               
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        VALIDATE RECORD                                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VR       DS    0H                                                               
*                                                                               
         CLC   LCLPUB,BPUB         IF PUB HAS CHNAGED                           
         BNE   DR                     DISPLAY RECORD                            
*                                                                               
         CLI   ACTNUM,ACTDIS       DETERMINE ACTION                             
         BE    DR                  DISPLAY                                      
*                                                                               
*        FIND END OF EDR TABLE                                                  
*                                                                               
         LA    R3,EDRTAB           START OF EDR TABLE                           
         USING EDRTABD,R3          ESTABLISH ENTRY                              
*                                                                               
         OC    EDRTABD(EDRTABL),EDRTABD LOOK FOR END OF TABLE                   
         BZ    *+12                                                             
         LA    R3,EDRTABD+EDRTABL  POINT TO NEXT ENTRY                          
         B     *-14                                                             
*                                                                               
         ST    R3,EDRNXTA          A(NEXT AVAILABLE SLOT)                       
         ST    R3,EDRENDA          A(STARTING END OF TABLE)                     
*                                                                               
*        VALIDATE 10 FIELDS FOR ENTRY OF NEW EDR CODES                          
*                                                                               
         ZAP   LCLFCTR,=P'10'      MAXIMUM 10 EDR CODES ON FIRST LINE           
         LA    R2,SUSEDR1H         POINT TO FIRST EDR INPUT FIELD               
*                                                                               
VREDRLP  DS    0H                                                               
*                                                                               
         MVI   FLDOPT,C'Y'         OPTIONAL FIELD                               
*                                                                               
         GOTO1 VGETFLD             READ IN FIELD                                
         BZ    VREDRCN             OKAY IF NOT ENTERED                          
*                                                                               
         CH    R1,=Y(EDRCODEL)     CODE HAS A FIXED LENGTH                      
         BNE   VREDRER1                                                         
*                                                                               
         CLI   FLDH+5,9            MAX 9 DIGITS                                 
         BH    VREDRER1                                                         
*                                                                               
         MVC   WORK(9),FLD         COPY EDR CODE                                
         NC    WORK(9),=9C'0'      KILL NUMERICS                                
         CLC   WORK(9),=9C'0'      MUST BE EQUAL TO BE NUMERIC                  
         BNE   VREDRER1                                                         
*                                                                               
*        SEARCH TABLE FOR DUPLICATES                                            
*                                                                               
         LA    R3,EDRTAB           POINT TO START OF EDR CODE TABLE             
*                                                                               
VREDRSL  DS    0H                                                               
*                                                                               
         C     R3,EDRNXTA          DONE IF END OF TABLE REACHED                 
         BNL   VREDRSD                                                          
*                                                                               
         CLC   EDREDR,FLD          ERROR IF MATCH FOUND                         
         BE    VREDRER3                                                         
*                                                                               
VREDRSC  DS    0H                                                               
*                                                                               
         LA    R3,EDRTABL(R3)      BUMP TO NEXT TABLE ENTRY                     
         B     VREDRSL                                                          
*                                                                               
VREDRSD  DS    0H                                                               
*                                                                               
*        MAKE SURE IT IS NOT ASSOCIATED WITH ANOTHER PUB                        
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY              ESTABLISH AS EDR/PUB PASSIVE POINTER         
         USING PEDPRECD,R4                                                      
*                                                                               
         MVC   PEDPKAGY,AGENCY     SET AGENCY                                   
         MVC   PEDPKMED,QMED       SET MEDIA                                    
         MVI   PEDPKRCD,PEDPKIDQ   SET RECORD ID                                
         MVC   PEDPKEDR,FLD        SET EDR CODE                                 
*                                                                               
VREDRS10 DS    0H                                                               
*                                                                               
         OI    DMINBTS,X'08'       READ DELETED RECORDS                         
*                                                                               
         GOTO1 HIGH                FIND RECORD                                  
*                                                                               
*        EDR CODE MUST NOT BE ALREADY ASSIGNED TO ANOTHER PUB                   
*                                                                               
         CLC   PEDPKEY(PEDPKPUB-PEDPKEY),KEYSAVE OKAY IF NO MATCH               
         BNE   VREDR20                FOUND                                     
*                                                                               
         CLC   PEDPKPUB,BPUB       OKAY IF CURRENT PUB ID                       
         BE    VREDR15                                                          
*                                                                               
         TM    PEDPCNTL,X'80'      ELSE MUST BE DELETED                         
         BNO   VREDRER2                                                         
*                                                                               
VREDR15  DS    0H                                                               
*                                                                               
         GOTO1 SEQ                 FIND NEXT ON FILE                            
*                                                                               
         B     VREDRS10                                                         
*                                                                               
VREDR20  DS    0H                                                               
*                                                                               
         MVC   EDREDR,FLD          SAVE EDR CODE IN TABLE                       
         OI    EDRCNTL,EDRADDQ     INDICATE ADDED ENTRY                         
*                                                                               
         LA    R3,EDRTABL(R3)      BUMP TO NEXT AVAILABLE SLOT                  
         XC    EDRTABD(EDRTABL),EDRTABD  INIT NEXT ENTRY                        
         ST    R3,EDRNXTA          SAVE ADDRESS                                 
*                                                                               
VREDRCN  DS    0H                                                               
*                                                                               
         BAS   RE,BUMP             BUMP TO NEXT EDR INPUT FIELD                 
*                                                                               
         SP    LCLFCTR,=P'1'       PROCESS NEXT ONE                             
         BP    VREDRLP                                                          
*                                                                               
VREDRDN  DS    0H                                                               
*                                                                               
*        VALIDATE ANY CHANGES TO OLD EDR CODES                                  
*                                                                               
*                                  NUMBER OF OLD EDR FIELDS                     
         LA    RF,((SUSACL1H-SUSAC11H+1)/(SUSAC21H-SUSAC11H))*5                 
         CVD   RF,DUB                                                           
         ZAP   LCLFCTR,DUB         NUMBER OF EDR FIELDS                         
*                                                                               
         LA    R2,SUSAC11H         POINT TO FIRST OLD EDR ACT FIELD             
         LA    R3,EDRTAB           POINT TO FIRST ENTRY IN TABLE                
         C     R3,EDRENDA          IF PAST ORIGINAL TABLE END                   
         BL    *+8                                                              
         L     R3,EDRNXTA             DEFAULT TO NEXT AVAILABLE                 
*                                                                               
*        VALIDATE ACTION FIELD                                                  
*                                                                               
VREXLP   DS    0H                                                               
*                                                                               
VREXAC   DS    0H                                                               
*                                                                               
         MVI   FLDOPT,C'Y'         OPTIONAL FIELD                               
         GOTO1 VGETFLD             READ IN ACTION FIELD                         
         BZ    VREXACX             OKAY IF NO ENTRY                             
*                                                                               
         CLI   FLD,C'*'            SKIP IF ONLY OLD ACTION                      
         BE    VREXACX                                                          
*                                                                               
         CLI   FLD,C'A'            IF ACTION ADD                                
         BNE   *+8                                                              
         MVI   EDRCNTL,EDRADDQ        SET ACTION INDICATOR                      
*                                                                               
         CLI   FLD,C'C'            IF ACTION CHANGE                             
         BNE   *+8                                                              
         MVI   EDRCNTL,EDRCHAQ        SET ACTION INDICATOR                      
*                                                                               
         CLI   FLD,C'D'            IF ACTION DELETE                             
         BNE   *+8                                                              
         MVI   EDRCNTL,EDRDELQ        SET ACTION INDICATOR                      
*                                                                               
         CLI   FLD,C'R'            IF ACTION RESTORE                            
         BNE   *+8                                                              
         MVI   EDRCNTL,EDRRESQ        SET ACTION INDICATOR                      
*                                                                               
         CLI   EDRCNTL,0           ERROR IF NO VALID ACTION FOUND               
         BE    VREDRER4                                                         
*                                                                               
VREXACX  DS    0H                                                               
*                                                                               
*        VALIDATE EDR CODE                                                      
*                                                                               
         BAS   RE,BUMP             BUMP TO NEXT FIELD                           
*                                                                               
         MVI   FLDOPT,C'Y'         OPTIONAL FIELD                               
*                                                                               
         GOTO1 VGETFLD             READ IN FIELD                                
         BNZ   VREXEX10            HAVE DATA                                    
*                                                                               
         CLI   EDRCNTL,0           NO ACTION MEANS CLEARING IS IGNORED          
         BE    VREXCN                                                           
*                                                                               
         TM    EDRCNTL,EDRDELQ     'DEL' IS ONLY ALLOWED ACTION                 
         BNO   VREDRER4                                                         
*                                                                               
         B     VREXCN                                                           
*                                                                               
VREXEX10 DS    0H                                                               
*                                                                               
         CH    R1,=Y(EDRCODEL)     CODE HAS A FIXED LENGTH                      
         BNE   VREDRER1                                                         
*                                                                               
         MVC   WORK(9),FLD         COPY EDR CODE                                
         NC    WORK(9),=9C'0'      KILL NUMERICS                                
         CLC   WORK(9),=9C'0'      MUST BE EQUAL TO BE NUMERIC                  
         BNE   VREDRER1                                                         
*                                                                               
*        SEARCH TABLE FOR DUPLICATES                                            
*                                                                               
         ST    R3,EDRCURRA         SAVE CURRENT ENTRY POINTER                   
*                                                                               
         LA    R3,EDRTAB           POINT TO START OF EDR CODE TABLE             
*                                                                               
VREXSRL  DS    0H                                                               
*                                                                               
         C     R3,EDRNXTA          DONE IF END OF TABLE REACHED                 
         BNL   VREXSRD                                                          
*                                                                               
         CLC   EDREDR,FLD          IF MATCH FOUND                               
         BNE   VREXSRC                                                          
*                                                                               
         C     R3,EDRCURRA            OKAY IF SAME AS LAST TIME                 
         BE    VREXSRD                                                          
*                                                                               
         B     VREDRER3               ELSE ERROR                                
*                                                                               
VREXSRC  DS    0H                                                               
*                                                                               
         LA    R3,EDRTABL(R3)      BUMP TO NEXT TABLE ENTRY                     
         B     VREXSRL                                                          
*                                                                               
VREXSRD  DS    0H                                                               
*                                                                               
         CLC   EDRCURRA,EDRENDA    SKIP IF A NEW ENTRY                          
         BNL   *+12                                                             
         C     R3,EDRCURRA         SKIP IF ENTRY UNCHANGED                      
         BE    VREXCN                                                           
*                                                                               
*        MAKE SURE IT IS NOT ASSOCIATED WITH ANOTHER PUB                        
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY              ESTABLISH AS EDR/PUB PASSIVE POINTER         
         USING PEDPRECD,R4                                                      
*                                                                               
         MVC   PEDPKAGY,AGENCY     SET AGENCY                                   
         MVC   PEDPKMED,QMED       SET MEDIA                                    
         MVI   PEDPKRCD,PEDPKIDQ   SET RECORD ID                                
         MVC   PEDPKEDR,FLD        SET EDR CODE                                 
*                                                                               
         OI    DMINBTS,X'08'       READ DELETED RECORDS                         
         MVC   LCLKEY,KEY          SAVE KEY                                     
*                                                                               
         GOTO1 HIGH                FIND RECORD                                  
*                                                                               
*        EDR CODE MUST NOT BE ALREADY ASSIGNED TO ANOTHER PUB                   
*                                                                               
VREX10   DS    0H                                                               
*                                                                               
         CLC   PEDPKEY(PEDPKPUB-PEDPKEY),LCLKEY OKAY IF NO MATCH                
         BNE   VREX20                 FOUND                                     
*                                                                               
         CLC   PEDPKPUB,BPUB       OKAY IF CURRENT PUB ID                       
         BE    VREX15                                                           
*                                                                               
         TM    PEDPCNTL,X'80'      ELSE MUST BE DELETED                         
         BNO   VREDRER2                                                         
*                                                                               
VREX15   DS    0H                                                               
*                                                                               
         GOTO1 SEQ                 FIND NEXT ON FILE                            
*                                                                               
         B     VREX10                                                           
*                                                                               
VREX20   DS    0H                                                               
*                                                                               
         L     RF,EDRCURRA         RESTORE CURRENT ENTRY PTR                    
         TM    EDRCNTL-EDRTABD(RF),EDRCHAQ   ACTION MUST BE CHANGE              
         BO    *+12                                                             
         C     RF,EDRENDA          OR PAST END OF OLD DISPLAY                   
         BL    VREDRER3                                                         
*                                                                               
         CR    RF,R3               SKIP IF A NEW ITEM                           
         BE    *+8                                                              
         MVI   EDRCNTL-EDRTABD(RF),EDRDELQ   DELETE OLD ENTRY                   
*                                                                               
         MVC   EDREDR,FLD          SAVE EDR CODE IN NEW ENTRY                   
         OI    EDRCNTL,EDRCHAQ+EDRADDQ  INDICATE CHANGED AND ADDED              
*                                                                               
         LA    R3,EDRTABL(R3)      BUMP TO NEXT AVAILABLE SLOT                  
         XC    EDRTABD(EDRTABL),EDRTABD  INIT NEXT ENTRY                        
         ST    R3,EDRNXTA          SAVE ADDRESS                                 
*                                                                               
         L     R3,EDRCURRA         RESTORE TABLE POINTER                        
*                                                                               
VREXCN   DS    0H                                                               
*                                                                               
         LA    R3,EDRTABL(R3)      BUMP TO NEXT TABLE ENTRY                     
         C     R3,EDRENDA          IF PAST ORIGINAL TABLE END                   
         BL    *+8                                                              
         L     R3,EDRNXTA             DEFAULT TO NEXT AVAILABLE                 
*                                                                               
         BAS   RE,BUMP             BUMP TO NEXT ACTION FIELD                    
*                                                                               
         SP    LCLFCTR,=P'1'       DECREMENT FIELDS COUNTER                     
         BP    VREXLP              PROCESS NEXT ONE                             
*                                                                               
VREXDN   DS    0H                                                               
*                                                                               
*        PROCESS TABLE TO UPDATE DIRECTORY                                      
*                                                                               
         LA    R3,EDRTAB           POINT TO START OF TABLE                      
*                                                                               
VRUPDLP  DS    0H                                                               
*                                                                               
         CLI   EDRCNTL,0           SKIP IF NO ACTIVITY FOR ENTRY                
         BE    VRUPDCN                                                          
*                                                                               
*        UPDATE PUB-EDR PASSIVE POINTER                                         
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY              ESTABLISH AS PUB/EDR PASSIVE POINTER         
         USING PPEDRECD,R4                                                      
*                                                                               
         MVC   PPEDKAGY,AGENCY     SET AGENCY                                   
         MVC   PPEDKMED,QMED       SET MEDIA                                    
         MVI   PPEDKRCD,PPEDKIDQ   SET RECORD ID                                
         MVC   PPEDKPUB,BPUB       SET PUB ID                                   
         MVC   PPEDKEDR,EDREDR     SET EDR CODE                                 
*                                                                               
         OI    DMINBTS,X'08'       READ DELETED RECORDS                         
*                                                                               
         GOTO1 HIGH                FIND RECORD                                  
*                                                                               
         CLC   PPEDKEY,KEYSAVE     IF RECORD NOT FOUND                          
         BE    VRUPDPE1                                                         
*                                                                               
         TM    EDRCNTL,EDRADDQ+EDRCHAQ   MUST BE ADDING OR CHANGING             
         BZ    VREDRER5                                                         
*                                                                               
         MVC   KEY,KEYSAVE         RESTORE KEY                                  
         MVC   PPEDCNTL,=X'00FF'   INDICATE DIRECTORY ONLY RECORD               
*                                                                               
         GOTO1 ADD                 ADD RECORD                                   
*                                                                               
         B     VRUPDPEX                                                         
*                                                                               
VRUPDPE1 DS    0H                  RECORD FOUND                                 
*                                                                               
         TM    EDRCNTL,EDRDELQ     IF DELETING                                  
         BNO   *+12                                                             
         OI    PPEDCNTL,X'80'         SET DELETE BIT                            
         B     *+8                                                              
         NI    PPEDCNTL,X'FF'-X'80'  ELSE TURN OFF DELETE BIT                   
*                                                                               
         GOTO1 WRITE               RE-WRITE RECORD                              
*                                                                               
VRUPDPEX DS    0H                                                               
*                                                                               
*         UPDATE EDR-PUB PASSIVE POINTER                                        
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY              ESTABLISH AS EDR/PUB PASSIVE POINTER         
         USING PEDPRECD,R4                                                      
*                                                                               
         MVC   PEDPKAGY,AGENCY     SET AGENCY                                   
         MVC   PEDPKMED,QMED       SET MEDIA                                    
         MVI   PEDPKRCD,PEDPKIDQ   SET RECORD ID                                
         MVC   PEDPKPUB,BPUB       SET PUB ID                                   
         MVC   PEDPKEDR,EDREDR     SET EDR CODE                                 
*                                                                               
         OI    DMINBTS,X'08'       READ DELETED RECORDS                         
*                                                                               
         GOTO1 HIGH                FIND RECORD                                  
*                                                                               
         CLC   PEDPKEY,KEYSAVE     IF RECORD NOT FOUND                          
         BE    VRUPDEP1                                                         
*                                                                               
         TM    EDRCNTL,EDRADDQ+EDRCHAQ   MUST BE ADDING OR CHANGING             
         BZ    VREDRER5                                                         
*                                                                               
         MVC   KEY,KEYSAVE         RESTORE KEY                                  
         MVC   PEDPCNTL,=X'00FF'   INDICATE DIRECTORY ONLY RECORD               
*                                                                               
         GOTO1 ADD                 ADD RECORD                                   
*                                                                               
         B     VRUPDEPX                                                         
*                                                                               
VRUPDEP1 DS    0H                  RECORD FOUND                                 
*                                                                               
         TM    EDRCNTL,EDRDELQ     IF DELETING                                  
         BNO   *+12                                                             
         OI    PEDPCNTL,X'80'         SET DELETE BIT                            
         B     *+8                                                              
*                                                                               
         NI    PEDPCNTL,X'FF'-X'80'  ELSE TURN OFF DELETE BIT                   
*                                                                               
         GOTO1 WRITE               RE-WRITE RECORD                              
*                                                                               
VRUPDEPX DS    0H                                                               
*                                                                               
VRUPDCN  DS    0H                                                               
*                                                                               
         LA    R3,EDRTABL(R3)      BUMP TO NEXT TABLE ENTRY                     
         C     R3,EDRNXTA          CHECK FOR END OF TABLE                       
         BL    VRUPDLP                                                          
*                                                                               
*        BUBBLE SORT TABLE BY EDR CODE                                          
*                                                                               
         LA    R3,EDRTAB           POINT TO START OF TABLE                      
         USING EDRTABD,R3          ESTABLISH AS EDR TABLE ENTRY                 
*                                                                               
         C     R3,EDRNXTA          EXIT IF TABLE HAS NO ENTRY                   
         BNL   VRSRTX                                                           
*                                                                               
         L     RE,EDRNXTA          END OF TABLE                                 
         SH    RE,=Y(2*EDRTABL)    START OF PENULTIMATE ENTRY                   
*                                                                               
VRSRTLP  DS    0H                                                               
*                                                                               
         CR    RE,R3               CHECK FOR DATA IN TABLE                      
         BL    VRSRTDN             AT MOST ONE ENTRY LEFT TO SORT               
*                                                                               
         LR    RF,R3                                                            
*                                                                               
VRSRTL1  DS    0H                                                               
*                                                                               
         CLC   EDREDR-EDRTABD(L'EDREDR,RF),EDREDR-EDRTABD+EDRTABL(RF)           
         BNH   VRSRTC1                                                          
*                                                                               
         XC    0(EDRTABL,RF),EDRTABL(RF)                                        
         XC    EDRTABL(EDRTABL,RF),0(RF)                                        
         XC    0(EDRTABL,RF),EDRTABL(RF)                                        
*                                                                               
VRSRTC1  DS    0H                                                               
*                                                                               
         LA    RF,EDRTABL(RF)                                                   
         CR    RF,RE               CHECK FOR END OF TABLE                       
         BNH   VRSRTL1                                                          
*                                                                               
VRSRTD1  DS    0H                                                               
*                                                                               
VRSRTCN  DS    0H                                                               
*                                                                               
         SH    RE,=Y(EDRTABL)      BACK UP TABLE END                            
         B     VRSRTLP                                                          
*                                                                               
VRSRTDN  DS    0H                                                               
*                                                                               
VRSRTX   DS    0H                                                               
*                                                                               
         B     DR1                 GO DISPLAY ALL EDR CODES                     
*                                                                               
VREDRER1 DS    0H                                                               
         MVI   ERROR,PWEED9NM      EDR CODE MUST BE 9 DIGITS                    
         B     VREDRERR                                                         
VREDRER2 DS    0H                                                               
         MVI   ERROR,PWEEDDUP      EDR ASSIGNED TO THIS PUB                     
         B     VREDRERR                                                         
VREDRER3 DS    0H                                                               
         MVI   ERROR,PWEEDDUP      EDR ASIGNED TO ANOTHER PUB                   
         B     VREDRERR                                                         
VREDRER4 DS    0H                                                               
         MVI   ERROR,INVALID       INVALID ACTION                               
         B     VREDRERR                                                         
VREDRER5 DS    0H                                                               
         MVI   ERROR,NOTFOUND      RECORD NOT FOUND                             
         B     VREDRERR                                                         
*                                                                               
VREDRERR DS    0H                                                               
*                                                                               
         GOTO1 =A(BLDEDR),RR=LCLRELO  RE-BUILD TABLE OF EXISTING CODES          
*                                                                               
         GOTO1 ERREX                                                            
*                                                                               
VRX      DS    0H                                                               
         B     EXIT                                                             
EDRNOTV  EQU   136                 EDR MUST BE 9 DIGITS                         
EDRDUP   EQU   137                 EDR ALREADY ASSIGNED                         
         TITLE 'PRWRI0E - DISPLAY RECORD - DR'                                  
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
         MVC   LCLPUB,BPUB         SAVE PUB ID                                  
*                                                                               
         GOTO1 =A(BLDEDR),RR=LCLRELO  BUILD TABLE OF EXISTING EDR CODES         
*                                                                               
DR1      DS    0H                                                               
*                                                                               
         TWAXC SUSEDR1H            CLEAR OLD SCREEN                             
*                                                                               
         LA    R3,EDRTAB           POINT TO START OF TABLE                      
         USING EDRTABD,R3          ESTABLISH AS EDR TABLE ENTRY                 
*                                                                               
         C     R3,EDRNXTA          EXIT IF TABLE HAS NO ENTRY                   
         BNL   DRX                                                              
*                                                                               
*        DISPLAY TABLE CONTENTS                                                 
*                                                                               
         LA    R2,SUSAC11H         FIRST ACTION FIELD                           
*                                                                               
DRDISLP  DS    0H                                                               
*                                                                               
         CLI   EDRCNTL,0           SKIP IF NO ACTIONS ASSOCIATED                
         BE    DRDISACX                                                         
*                                                                               
         MVI   8(R2),C'*'          INDICATE OLD ACTION                          
*                                                                               
         TM    EDRCNTL,EDRADDQ                                                  
         BNO   *+8                                                              
         MVI   9(R2),C'A'          ADDITION                                     
*                                                                               
         TM    EDRCNTL,EDRDELQ                                                  
         BNO   *+8                                                              
         MVI   9(R2),C'D'          DELETION                                     
*                                                                               
         TM    EDRCNTL,EDRRESQ                                                  
         BNO   *+8                                                              
         MVI   9(R2),C'R'          RESTORATION                                  
*                                                                               
         TM    EDRCNTL,EDRCHAQ                                                  
         BNO   *+8                                                              
         MVI   9(R2),C'C'          CHANGE                                       
*                                                                               
DRDISACX DS    0H                                                               
*                                                                               
         OI    6(R2),X'80'         TRANSMIT FIELD                               
*                                                                               
         BAS   RE,BUMP             BUMP TO EDR FIELD                            
*                                                                               
         MVC   8(L'EDREDR,R2),EDREDR   DISPLAY EDR CODE                         
*                                                                               
         CLC   EDREDR,SPACES       IF WE HAVE ACTUAL DATA                       
         BNH   *+8                                                              
         MVI   5(R2),L'EDREDR         SET FIELD LENGTH                          
*                                                                               
         TM    EDRCNTL,EDRDELQ     KEEP ONLY DEL BIT ON                         
         BNO   *+12                                                             
         MVI   EDRCNTL,EDRDELQ                                                  
         B     *+8                                                              
         MVI   EDRCNTL,0                                                        
*                                                                               
         OI    6(R2),X'80'         TRANSMIT FIELD                               
*                                                                               
DRDISCN  DS    0H                                                               
*                                                                               
         BAS   RE,BUMP             BUMP TO ACT FIELD                            
*                                                                               
         LA    R3,EDRTABL(R3)      POINT TO NEXT ENTRY IN TABLE                 
         C     R3,EDRNXTA          CHECK FOR END OF TABLE                       
         BL    DRDISLP                                                          
*                                                                               
DRDISDN  DS    0H                                                               
*                                                                               
DRX      DS    0H                                                               
         B     VK                  GO VALIDATE KEY                              
         DROP  R4                                                               
*                                                                               
         TITLE 'PRWRI0E - DISPLAY KEY - DK'                                     
***********************************************************************         
*                                                                     *         
*        DISPLAY KEY                                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DK       DS    0H                                                               
*                                                                               
         LA    R4,KEY              ESTABLISH AS PUB/EDR PASSIVE POINTER         
         USING PPEDRECD,R4                                                      
*                                                                               
         MVC   SUSMED(1),PPEDKMED  SET MEDIA                                    
         OI    SUSMEDH+6,X'80'     TRANSMIT FIELD                               
         MVI   SUSMEDH+5,1         SET INPUT LENGTH                             
*                                                                               
         CLI   PPEDKRCD,PEDPKIDQ   SKIP IF EDR/PUB                              
         BE    DKEDR                                                            
*                                                                               
         GOTO1 =V(PUBEDIT),DMCB,PPEDKPUB,SUSPUB,RR=LCLRELO  EXPAND PUB          
*                                                                               
         LA    RF,L'SUSPUB         ELIMINATE TRAILING SPACES                    
         LA    R1,SUSPUB-1(RF)                                                  
*                                                                               
         CLI   0(R1),C' '                                                       
         BH    *+10                                                             
         BCTR  R1,0                                                             
         BCT   RF,*-10                                                          
*                                                                               
         STC   RF,SUSPUBH+5        SET INPUT LENGTH                             
*                                                                               
         B     DKX                                                              
*                                                                               
DKEDR    DS    0H                                                               
*                                                                               
         MVC   SUSPUB(4),=C'EDR='  SET EDR KEYWORD                              
         MVC   SUSPUB+4(L'PEDPKEDR),PEDPKEDR-PEDPKEY(R4)  SET EDR CODE          
*                                                                               
         MVI   SUSPUBH+5,4+L'PEDPKEDR SET INPUT LENGTH                          
         OI    SUSPUBH+6,X'80'     TRANSMIT FIELD                               
*                                                                               
DKX      B     VK                  GO VALIDATE KEY                              
         TITLE 'PRWRI0E - LIST/REPORT RECORDS - LR'                             
***********************************************************************         
*                                                                     *         
*        LIST/REPORT RECORDS                                          *         
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
         LA    R2,LISTAR           DEFAULT TO LISTING ON SCREEN                 
         CLI   MODE,LISTRECS       IF NOT DOING LIST                            
         BE    *+8                                                              
         LA    R2,P                   USE OFF-LINE PRINT AREA                   
*                                                                               
         USING LISTD,R2            ESTABLISH LIST AREA                          
*                                                                               
         XC    LCLLRPUB,LCLLRPUB   INIT PUB SAVEAREA                            
*                                                                               
         CLC   =C'EDR=',SULPUB     DIFFERENT LOGIC FOR EDR=                     
         BE    LREDR                                                            
*                                                                               
         LA    R1,EDRSPECS         SET SPECS ADDRESS                            
         ST    R1,SPECS                                                         
*                                                                               
         LA    R3,LISTEDR1         START OF EDR FIELDS                          
         LA    R0,4                NUMBER OF EDR FIELDS                         
*                                                                               
         LA    R4,KEY              ESTABLISH AS PUB/EDR PASSIVE POINTER         
         USING PPEDRECD,R4                                                      
*                                                                               
         OC    KEY,KEY             IF FIRST TIME FOR LIST                       
         BNZ   LRKEYX                 BUILD SKELETON KEY                        
*                                                                               
         MVC   PPEDKAGY,AGENCY     SET AGENCY                                   
         MVC   PPEDKMED,QMED       SET MEDIA                                    
         MVI   PPEDKRCD,PPEDKIDQ   SET RECORD ID                                
         MVC   PPEDKPUB,BPUB       SET PUB ID                                   
*                                                                               
         CLI   MODE,LISTRECS       SKIP IF NOT DOING LIST                       
         BNE   LRKEYX                                                           
*                                                                               
         MVC   SULHDR,SPACES       INIT HEADLINES                               
         MVC   SULHDR1,SPACES                                                   
*                                                                               
         OI    SULHDRH+6,X'80'     TRANSMIT HEADLINES                           
         OI    SULHDR1H+6,X'80'                                                 
*                                                                               
         MVC   SULHDR(3),=X'E28593'  LOWERCASE 'SEL'                            
         MVC   SULHDR1(3),=C'---'                                               
*                                                                               
         LA    R2,SULHDR+5         SET COLUMN TITLES                            
         USING LISTD,R2            ESTABLISH LIST AREA                          
*                                                                               
         MVC   LISTPUB(3),=X'D7A482'     LOWERCASE 'PUB'                        
         MVC   LISTPUBN(4),=X'D5819485'  LOWERCASE 'NAME'                       
         MVC   LISTEDR1(9),=X'C5C4D94083968485A2' 'EDR CODES'                   
*                                                                               
         LA    R2,SULHDR1+5        SECOND HEADLINE FOR UNDERLINING              
*                                                                               
         MVC   LISTPUB,DASHS       UNDERLINE                                    
         MVC   LISTPUBN,DASHS      UNDERLINE                                    
         MVC   LISTEDR1(39),DASHS  UNDERLINE                                    
*                                                                               
         LA    R2,LISTAR           RE-POINT TO LIST AREA                        
         CLI   MODE,LISTRECS       IF NOT DOING LIST                            
         BE    *+8                                                              
         LA    R2,P                   USE OFF-LINE PRINT AREA                   
*                                                                               
LRKEYX   DS    0H                                                               
*                                                                               
         NI    DMINBTS,X'FF'-X'08'  IGNORE DELETED RECORDS                      
*                                                                               
         GOTO1 HIGH                READ FIRST RECORD OF TYPE                    
*                                                                               
         B     LRLOOP10                                                         
*                                                                               
LRLOOP   DS    0H                                                               
*                                                                               
         GOTO1 SEQ                 READ NEXT KEY OF TYPE                        
*                                                                               
LRLOOP10 DS    0H                                                               
*                                                                               
         CLC   KEY(PPEDKPUB-PPEDKEY),KEYSAVE                                    
         BNE   LRDONE              DONE ON CHANGE OF RECORD TYPE                
*                                                                               
         CLC   KEY(PPEDKEDR-PPEDKEY),KEYSAVE                                    
         BE    LRLOOP20            ON CHANGE OF PUB                             
*                                                                               
         XC    LCLLRPUB,LCLLRPUB      INIT SAVED PUB ID                         
*                                                                               
         CLC   LISTLINE,SPACES        SKIP IF NEW LINE ALREADY                  
         BNH   LRLOOP20                                                         
*                                     ELSE DISPLAY CURRENT LINE                 
         CLI   MODE,LISTRECS       IF NOT DOING LIST                            
         BNE   LRLOOP15                                                         
*                                                                               
         GOTO1 LISTMON                RETURN TO LIST MONITOR                    
*                                                                               
         B     LRLOOP18                                                         
*                                                                               
LRLOOP15 DS    0H                                                               
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)     PRINT A LINE                                 
*                                                                               
LRLOOP18 DS    0H                                                               
*                                                                               
         MVC   LISTLINE,SPACES        CLEAR OUT DATA                            
*                                                                               
         LA    R3,LISTEDR1            START OF EDR FIELDS                       
         LA    R0,4                   NUMBER OF EDR FIELDS                      
*                                                                               
LRLOOP20 DS    0H                                                               
*                                                                               
         CLC   PPEDKPUB,LCLLRPUB   IF PUB UNCHANGED                             
         BE    LRLOOP30               SKIP PRINTING                             
*                                                                               
         MVC   LCLLRPUB,PPEDKPUB   SAVE PUB ID                                  
*                                                                               
*        READ PUB RECORD                                                        
*                                                                               
         MVC   LCLKEY,KEY          SAVE CURRENT KEY                             
         MVI   USEIO,C'N'          TURN OFF USER DOES IO                        
         MVI   IOOPT,C'N'          TURN OFF OVERLAY DOES IO                     
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PUBRECD,R4                                                       
*                                                                               
         MVC   PUBKMED,QMED                                                     
         MVC   PUBKPUB(6),PPEDKPUB-PPEDKEY+LCLKEY  PUB NUMBER                   
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
         L     R6,AIO1             READ PUB RECORD                              
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
         MVC   PUBNM,PUBNAME                                                    
*                                                                               
         DROP  R6                                                               
*                                                                               
         LA    RF,KEY              SET I/O AREA FOR KEYS                        
         ST    RF,AIO                                                           
         MVI   USEIO,C'Y'          INDICATE USER DOES IO                        
         MVI   IOOPT,C'Y'          OVERLAY DOES IO                              
*                                                                               
         MVC   KEY,LCLKEY          RESTORE CURRENT KEY                          
         USING PPEDRECD,R4                                                      
*                                                                               
         GOTO1 HIGH                RESTORE FILE POINTER                         
*                                                                               
         GOTO1 =V(PUBEDIT),DMCB,PPEDKPUB,LISTPUB,RR=LCLRELO PRINT PUB           
*                                                                               
         MVC   LISTPUBN,PUBNM      DISPLAY PUB NAME                             
*                                                                               
LRLOOP30 DS    0H                                                               
*                                                                               
         MVC   0(L'LISTEDR1,R3),PPEDKEDR   DISPLAY EDR CODE                     
*                                                                               
LRCONT   DS    0H                                                               
*                                                                               
         LA    R3,L'LISTEDR1+1(R3) POINT TO NEXT EDR AREA                       
         BCT   R0,LRLOOP                                                        
*                                                                               
LRCONT1  DS    0H                                                               
*                                                                               
         CLI   MODE,LISTRECS       IF NOT DOING LIST                            
         BNE   LRCONT15                                                         
*                                                                               
         GOTO1 LISTMON                RETURN TO LIST MONITOR                    
*                                                                               
         B     LRCONT18                                                         
*                                                                               
LRCONT15 DS    0H                                                               
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)     PRINT LINE                                   
*                                                                               
LRCONT18 DS    0H                                                               
*                                                                               
         MVC   LISTLINE,SPACES     CLEAR OUT DATA                               
*                                                                               
         LA    R3,LISTEDR1         START OF EDR FIELDS                          
         LA    R0,4                NUMBER OF EDR FIELDS                         
*                                                                               
         B     LRLOOP                                                           
*                                                                               
LRDONE   DS    0H                                                               
*                                                                               
         CLC   LISTLINE,SPACES     SKIP IF NEW LINE ALREADY                     
         BNH   LRDONE10                                                         
*                                  ELSE DISPLAY CURRENT LINE                    
         CLI   MODE,LISTRECS       IF NOT DOING LIST                            
         BNE   LRDONE15                                                         
*                                                                               
         GOTO1 LISTMON                RETURN TO LIST MONITOR                    
*                                                                               
         B     LRDONE18                                                         
*                                                                               
LRDONE15 DS    0H                                                               
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)     PRINT LINE                                   
*                                                                               
LRDONE18 DS    0H                                                               
*                                                                               
         MVC   LISTLINE,SPACES     CLEAR OUT DATA                               
*                                                                               
LRDONE10 DS    0H                                                               
*                                                                               
LRX      DS    0H                                                               
         B     EXIT                                                             
*                                                                               
LREDR    DS    0H                                                               
*                                                                               
         LA    R1,EDRSPEC1         SET SPECS ADDRESS                            
         ST    R1,SPECS                                                         
*                                                                               
         LA    R4,KEY              ESTABLISH AS EDR/PUB PASSIVE POINTER         
         USING PEDPRECD,R4                                                      
*                                                                               
         OC    KEY,KEY             IF FIRST TIME FOR LIST                       
         BNZ   LREKYX                 BUILD SKELETON KEY                        
*                                                                               
         MVC   SULHDR,SPACES       INIT HEADLINES                               
         MVC   SULHDR1,SPACES                                                   
*                                                                               
         OI    SULHDRH+6,X'80'     TRANSMIT HEADLINES                           
         OI    SULHDR1H+6,X'80'                                                 
*                                                                               
         MVC   PEDPKAGY,AGENCY     SET AGENCY                                   
         MVC   PEDPKMED,QMED       SET MEDIA                                    
         MVI   PEDPKRCD,PEDPKIDQ   SET RECORD ID                                
         MVC   PEDPKEDR,LCLEDR     SET PUB ID                                   
*                                                                               
         CLI   MODE,LISTRECS       SKIP IF NOT DOING LIST                       
         BNE   LREKYX                                                           
*                                                                               
         MVC   SULHDR(3),=X'E28593'  LOWERCASE 'SEL'                            
         MVC   SULHDR1(3),=C'---'                                               
*                                                                               
         LA    R2,SULHDR+5         SET COLUMN TITLES                            
         USING LIS1D,R2            ESTABLISH LIST AREA                          
*                                                                               
         MVC   LIS1EDR(8),=X'C5C4D94083968485' 'EDR CODE'                       
         MVC   LIS1PUB(3),=X'D7A482'     LOWERCASE 'PUB'                        
         MVC   LIS1PUBN(4),=X'D5819485'  LOWERCASE 'NAME'                       
*                                                                               
         LA    R2,SULHDR1+5        SECOND HEADLINE FOR UNDERLINING              
*                                                                               
         MVC   LIS1EDR,DASHS       UNDERLINE                                    
         MVC   LIS1PUB,DASHS       UNDERLINE                                    
         MVC   LIS1PUBN,DASHS      UNDERLINE                                    
*                                                                               
         LA    R2,LISTAR           DEFAULT TO LISTING ON SCREEN                 
         CLI   MODE,LISTRECS       IF NOT DOING LIST                            
         BE    *+8                                                              
         LA    R2,P                   USE OFF-LINE PRINT AREA                   
*                                                                               
LREKYX   DS    0H                                                               
*                                                                               
         NI    DMINBTS,X'FF'-X'08'  IGNORE DELETED RECORDS                      
*                                                                               
         GOTO1 HIGH                READ FIRST RECORD OF TYPE                    
*                                                                               
         B     LREDRL10                                                         
*                                                                               
LREDRLP  DS    0H                                                               
*                                                                               
         GOTO1 SEQ                 READ NEXT KEY OF TYPE                        
*                                                                               
LREDRL10 DS    0H                                                               
*                                                                               
         CLC   KEY(PEDPKEDR-PEDPKEY),KEYSAVE                                    
         BNE   LREDRDN             DONE ON CHANGE OF RECORD TYPE                
*                                                                               
         MVC   LIS1LINE,SPACES     CLEAR OUT DATA                               
*                                                                               
         MVC   LIS1EDR,PEDPKEDR    DISPLAY EDR CODE                             
*                                                                               
         MVC   LCLLRPUB,PEDPKPUB   SAVE PUB ID                                  
*                                                                               
*        READ PUB RECORD                                                        
*                                                                               
         MVC   LCLKEY,KEY          SAVE CURRENT KEY                             
         MVI   USEIO,C'N'          TURN OFF USER DOES IO                        
         MVI   IOOPT,C'N'          TURN OFF OVERLAY DOES IO                     
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PUBRECD,R4                                                       
*                                                                               
         MVC   PUBKMED,QMED                                                     
         MVC   PUBKPUB(6),PEDPKPUB-PEDPKEY+LCLKEY  PUB NUMBER                   
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
         L     R6,AIO1             READ PUB RECORD                              
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
         MVC   PUBNM,PUBNAME                                                    
*                                                                               
         DROP  R6                                                               
*                                                                               
         LA    RF,KEY              SET I/O AREA FOR KEYS                        
         ST    RF,AIO                                                           
         MVI   USEIO,C'Y'          INDICATE USER DOES IO                        
         MVI   IOOPT,C'Y'          OVERLAY DOES IO                              
*                                                                               
         MVC   KEY,LCLKEY          RESTORE CURRENT KEY                          
         USING PEDPRECD,R4                                                      
*                                                                               
         GOTO1 HIGH                RESTORE FILE POINTER                         
*                                                                               
         GOTO1 =V(PUBEDIT),DMCB,PEDPKPUB,LIS1PUB,RR=LCLRELO PRINT PUB           
*                                                                               
         MVC   LIS1PUBN,PUBNM      DISPLAY PUB NAME                             
*                                                                               
LREDRCN  DS    0H                                                               
*                                                                               
         CLI   MODE,LISTRECS       IF NOT DOING LIST                            
         BNE   LREDRCN1                                                         
*                                                                               
         GOTO1 LISTMON                RETURN TO LIST MONITOR                    
*                                                                               
         B     LREDRCN2                                                         
*                                                                               
LREDRCN1 DS    0H                                                               
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)     PRINT LINE                                   
*                                                                               
LREDRCN2 DS    0H                                                               
*                                                                               
         MVC   LIS1LINE,SPACES     CLEAR OUT DATA                               
*                                                                               
         B     LREDRLP                                                          
*                                                                               
LREDRDN  DS    0H                                                               
*                                                                               
LREDRX   DS    0H                                                               
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
         TITLE 'PRWRI0E - REPORT SPECS - EDRSPECS'                              
***********************************************************************         
*                                                                     *         
*        BUILD TABLE OF EDR EQUIVALENT CODES FROM FILE                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
*                                                                               
EDRSPECS DS    0H                                                               
         SSPEC H1,1,RUN                                                         
         SSPEC H1,60,REQUESTOR                                                  
         SSPEC H2,60,REPORT                                                     
         SSPEC H2,73,PAGE                                                       
         SPACE 1                                                                
         SSPEC H2,28,C'PRINTPAK/EDR CODE ASSIGNMENTS'                           
         SSPEC H3,28,C'-----------------------------'                           
         SPACE 1                                                                
         SSPEC H5,1,C'PUB'                                                      
         SSPEC H5,15,C'NAME'                                                    
         SSPEC H5,36,C'EDR'                                                     
         SSPEC H5,46,C'EDR'                                                     
         SSPEC H5,56,C'EDR'                                                     
         SSPEC H5,66,C'EDR'                                                     
         SPACE 1                                                                
         SSPEC H6,1,C'--------------'                                           
         SSPEC H6,15,C'--------------------'                                    
         SSPEC H6,36,C'---------'                                               
         SSPEC H6,46,C'---------'                                               
         SSPEC H6,56,C'---------'                                               
         SSPEC H6,66,C'---------'                                               
         SPACE 1                                                                
         DC    X'00'                                                            
EDRSPEC1 DS    0H                                                               
         SSPEC H1,1,RUN                                                         
         SSPEC H1,60,REQUESTOR                                                  
         SSPEC H2,60,REPORT                                                     
         SSPEC H2,73,PAGE                                                       
         SPACE 1                                                                
         SSPEC H2,28,C'PRINTPAK/EDR CODE ASSIGNMENTS'                           
         SSPEC H3,28,C'-----------------------------'                           
         SPACE 1                                                                
         SSPEC H5,1,C'EDR'                                                      
         SSPEC H5,10,C'PUB'                                                     
         SSPEC H5,25,C'NAME'                                                    
         SPACE 1                                                                
         SSPEC H6,1,C'---------'                                                
         SSPEC H6,10,C'--------------'                                          
         SSPEC H6,25,C'--------------------'                                    
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
DASHS    DC    64C'-'              DASHES                                       
         DS    0F                                                               
         LTORG                                                                  
         TITLE 'PRWRI0E - BUILD EDR CODE TABLE-BLDEDR'                          
***********************************************************************         
*                                                                     *         
*        BUILD TABLE OF EDR EQUIVALENT CODES FROM FILE                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
BLDEDR   NMOD1 0,**#BED                                                         
*                                                                               
         L     RC,LCLWORKA         RESTORE WORK AREA POINTER                    
*                                                                               
         LA    R3,EDRTAB           ESTABLISH EDR TABLE                          
         USING EDRTABD,R3                                                       
         XC    EDRTABD(EDRTABL),EDRTABD  INIT FIRST ENTRY                       
*                                                                               
         LA    R4,KEY              ESTABLISH KEY AS PUB-EDR PASSIVE PTR         
         USING PPEDRECD,R4                                                      
*                                                                               
         XC    PPEDKEY,PPEDKEY     INIT KEY                                     
         MVC   PPEDKAGY,AGENCY     SET AGENCY                                   
         MVC   PPEDKMED,QMED       SET MEDIA                                    
         MVI   PPEDKRCD,PPEDKIDQ   SET RECORD ID                                
         MVC   PPEDKPUB,BPUB       SET PRINTPAK PUB ID                          
*                                                                               
         LA    R2,EDRTABX          POINT TO END OF TABLE                        
*                                                                               
         NI    DMINBTS,X'FF'-X'08'  IGNORE DELETED RECORDS                      
*                                                                               
         GOTO1 HIGH                READ FIRST PASSIVE POINTER                   
*                                                                               
         B     BEDEDRL1                                                         
*                                                                               
BEDEDRLP DS    0H                                                               
*                                                                               
         GOTO1 SEQ                 READ NEXT  PASSIVE POINTER                   
*                                                                               
BEDEDRL1 DS    0H                                                               
*                                                                               
         CLC   KEY(PPEDKEDR-PPEDKEY),KEYSAVE DONE IF PUB CHANGED                
         BNE   BEDEDRDN                                                         
*                                                                               
         TM    PPEDCNTL,X'80'      SKIP DELETED RECORDS                         
         BO    BEDEDRCN                                                         
*                                                                               
         MVC   EDREDR,PPEDKEDR     SAVE EDR CODE                                
*                                                                               
         LA    R3,EDRTABL(R3)      BUMP TO NEXT TABLE ENTRY                     
         XC    EDRTABD(EDRTABL),EDRTABD  INIT NEXT ENTRY                        
*                                                                               
BEDEDRCN DS    0H                                                               
*                                                                               
         CR    R3,R2               CONTINUE IF TABLE HAS ROOM                   
         BL    BEDEDRLP                                                         
*                                                                               
         DC    H'0'                SHOULD NEVER GET HERE                        
*                                                                               
BEDEDRDN DS    0H                                                               
*                                                                               
         ST    R3,EDRNXTA          SAVE A(NEXT SLOT IN TABLE)                   
         ST    R3,EDRENDA          A(STARTING END OF TABLE)                     
*                                                                               
BLDEDRX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
*                                                                               
*        TABLE OF EDR CODES EQUATED TO PRINTPAK PUB CODE                        
*                                                                               
EDRTABD  DSECT                     EDR-PUB TABLE                                
EDRCNTL  DS    XL1                 CONTROL BYTE                                 
EDRADDQ  EQU   X'80'               ADDED TABLE ENTRY                            
EDRDELQ  EQU   X'40'               DELETED TABLE ENTRY                          
EDRCHAQ  EQU   X'20'               CHANGED TABLE ENTRY                          
EDRRESQ  EQU   X'10'               RESTORE TABLE ENTRY                          
*                                                                               
EDREDR   DS    CL9                 EDR SYSTEM CODE                              
EDRTABL  EQU   *-EDRTABD           LENGTH OF TABLE ENTRY                        
*                                                                               
EDRCODEL EQU   9                   LENGTH OF EDR CODE                           
*                                                                               
         TITLE 'PRSFM0E - LIST AND REPORT DISPLAY LINE - LISTD'                 
***********************************************************************         
*                                                                     *         
*        LIST SCREEN DETAIL AND REPORT LINE LAYOUT                    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LISTD    DSECT                     LIST LINE                                    
LISTLINE DS    0XL75               LISTLINE                                     
LISTPUB  DS    CL13                PUB ID                                       
         DS    CL1                 SPACING                                      
LISTPUBN DS    CL20                PUB NAME                                     
         DS    CL1                 SPACING                                      
LISTEDR1 DS    CL9                 EDR CODE                                     
         DS    CL1                 SPACING                                      
LISTEDR2 DS    CL9                 EDR CODE                                     
         DS    CL1                 SPACING                                      
LISTEDR3 DS    CL9                 EDR CODE                                     
         DS    CL1                 SPACING                                      
LISTEDR4 DS    CL9                 EDR CODE                                     
         TITLE 'PRSFM0E - LIST AND REPORT DISPLAY LINE - LIS2D'                 
***********************************************************************         
*                                                                     *         
*        LIST SCREEN DETAIL AND REPORT LINE LAYOUT - EDR MAJOR        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LIS1D    DSECT                     LIS2 LINE                                    
LIS1LINE DS    0XL75               LIS2LINE                                     
LIS1EDR  DS    CL9                 EDR CODE                                     
         DS    CL1                 SPACING                                      
LIS1PUB  DS    CL13                PUB ID                                       
         DS    CL1                 SPACING                                      
LIS1PUBN DS    CL20                PUB NAME                                     
         EJECT                                                                  
       ++INCLUDE PRSFMFFD                                                       
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFMFED                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFMEED                                                       
         EJECT                                                                  
       ++INCLUDE PRSFMWORKD                                                     
         SPACE 1                                                                
         ORG   SYSSPARE                                                         
*                                                                               
LCLRELO  DS    A                   THIS MODULE'S RELO FACTOR                    
LCLWORKA DS    A                   COMMON WORKAREA ADDRESS                      
LCLFCTR  DS    PL4                 FIELDS ON SCREEN COUNTER                     
LCLPUB   DS    XL6                 PUB NUMBER SAVEAREA                          
LCLEDR   DS    XL9                 EDR NUMBER SAVEAREA                          
LCLLRPUB DS    XL6                 PUB NUMBER SAVEAREA FOR LIST                 
LCLKEY   DS    XL25                KEY SAVEAREA                                 
*                                                                               
EDRCURRA DS    A                   A(CURRENT        ENTRY IN TABLE)             
EDRNXTA  DS    A                   A(NEXT AVAILABLE ENTRY IN TABLE)             
EDRENDA  DS    A                   A(END OF TABLE AT START)                     
EDRTABNQ EQU   (SYSDEND-*)/EDRTABL   NUMBER OF TABLE ENTRIES                    
EDRTAB   DS    XL(EDRTABNQ*EDRTABL)  EDR CODE TABLE                             
EDRTABX  EQU   *-1                 END OF EDR TABLE                             
*                                                                               
         EJECT                                                                  
*       ++INCLUDE PRGENFILE                                                     
*       ++INCLUDE PRGLOBEQUS                                                    
*       ++INCLUDE DDSPLWORKD                                                    
*       ++INCLUDE DDSPOOLD                                                      
         PRINT OFF                                                              
       ++INCLUDE PRGENFILE                                                      
       ++INCLUDE PRGLOBEQUS                                                     
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'096PRSFM0E   05/01/02'                                      
         END                                                                    
