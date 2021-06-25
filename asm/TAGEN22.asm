*          DATA SET TAGEN22    AT LEVEL 089 AS OF 03/22/11                      
*PHASE T70222C,*                                                                
         TITLE 'T70222 - LIEN MAINTENANCE'                                      
T70222   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70222                                                         
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
         SPACE 1                                                                
         GOTO1 INITIAL,DMCB,PFTAB                                               
*                                                                               
         TM    TGSYSTAT,TASYSPID                                                
         BZ    LIN00                                                            
         MVC   SLNSHED(13),=C'Performer Pid'                                    
         OI    SLNSHEDH+6,X'80'                                                 
*                                                                               
LIN00    MVC   SLNCUH,=CL11'Currency'                                           
         OI    SLNCUHH+6,X'80'                                                  
*                                                                               
         CLI   MODE,VALKEY         FIRST TIME IN                                
         BNE   LIN10                                                            
*                                                                               
         LA    R2,SLNSSNH          S/S NUM                                      
         TM    TGSYSTAT,TASYSPID   ARE WE USING PID#                            
         BZ    LIN06                                                            
         CLI   SLNSSNH+5,0                                                      
         BE    LIN06A                                                           
         CLI   SLNSSNH+5,9         MUST CHECK FOR LENGTH HERE SINCE             
         BE    LIN06               RECVAL CALL DOES NOT CHECK FOR               
         CLI   SLNSSNH+5,6         INVALID ENTRIES DUE TO X'40' PARAM           
         BNE   FLDINV                                                           
         MVC   TGPID,SLNSSN                                                     
LIN06A   OC    TGPID,TGPID                                                      
         BZ    FLDMISS                                                          
         GOTO1 SSNUNPK,DMCB,TGPID,TGSSN                                         
         BNE   LIN06                                                            
         MVC   SLNSSN,TGSSN                                                     
         MVI   SLNSSNH+5,9                                                      
*                                                                               
LIN06    GOTO1 RECVAL,DMCB,TLW4CDQ,(X'08',SLNSSNH),SLNSSNNH  VAL PERF.          
         TM    TGSYSTAT,TASYSPID   ARE WE USING PID#                            
         BZ    LIN07                                                            
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVC   SLNSSN,SPACES                                                    
         MVC   SLNSSN(L'TGPID),TGPID                                            
         MVI   SLNSSNH+5,6                                                      
         OI    SLNSSNH+6,X'80'                                                  
*                                                                               
LIN07    GOTO1 RECVAL,DMCB,TLLNCDQ,(X'40',SLNLINH)                              
         B     XIT                                                              
         SPACE 3                                                                
LIN10    CLI   THISLSEL,C'D'       IF DELETING FROM A LIST                      
         BE    *+12                DON'T DISPLAY TILL XRECDEL                   
         CLI   MODE,DISPREC                                                     
         BE    LIN15                                                            
         CLI   MODE,XRECPUT                                                     
         BE    LIN13                                                            
         CLI   MODE,XRECDEL        IF MODE IS RECORD DELETED                    
         BNE   LIN11                                                            
         GOTO1 ADDPTRS,DMCB,PTRS   HANDLE PASSIVE POINTERS                      
         BAS   RE,CHKLIEN          IF NO OTHER LIENS EXIST, SET NO LIEN         
         B     LIN15                                                            
LIN11    CLI   MODE,XRECREST       IF MODE IS RECORD RESTORED                   
         BE    *+12                                                             
         CLI   MODE,XRECADD        OR NEW RECORD ADDED                          
         BNE   LIN20                                                            
         GOTO1 ADDPTRS,DMCB,PTRS   HANDLE PASSIVE POINTERS                      
         BAS   RE,CHANGEW4         SET LIEN RECORD PRESENT IN W4                
         B     LIN15                                                            
         SPACE                                                                  
LIN13    GOTO1 ADDPTRS,DMCB,PTRS   HANDLE PASSIVE POINTERS                      
         SPACE                                                                  
LIN15    BAS   RE,DISPLAY          (RE-)DISPLAY THE RECORD                      
         B     XIT                                                              
         EJECT                                                                  
LIN20    CLI   MODE,VALREC         IF MODE IS VALIDATE RECORD                   
         BE    LIN25                                                            
         CLI   MODE,RECDEL         OR DELETE RECORD                             
         BE    LIN25                                                            
         CLI   MODE,RECREST        OR RESTORE RECORD                            
         BNE   LIN30                                                            
         SPACE                                                                  
LIN25    GOTO1 SAVPTRS,DMCB,PTRS   SAVE PASSIVE POINTERS                        
         SPACE                                                                  
         CLI   MODE,RECDEL         IF MODE IS DELETE RECORD                     
         BE    CHKDEL              CHECK THAT RECORD IS OK FOR DELETION         
         CLI   MODE,RECREST                                                     
         BE    XIT                 XIT IF MODE IS RESTORE                       
         BAS   RE,BLDREC                                                        
         B     XIT                                                              
         SPACE 3                                                                
LIN30    CLI   MODE,DISPKEY                                                     
         BNE   XIT                                                              
         MVC   SVKEY,KEY           SAVE KEY AND ADDRESS FOR DELETE              
         L     R3,AIO                                                           
         USING TLLND,R3                                                         
         MVC   SLNSSN,TLLNSSN      PERFORMER S/S                                
         OI    SLNSSNH+6,X'80'     TRANSMIT                                     
         MVI   SLNSSNH+5,9         SET LENGTH FOR RECVAL                        
         MVC   AIO,AIO2            MAKE SURE DON'T CREAM RECORD                 
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'08',SLNSSNH),SLNSSNNH PERFORMER           
*                                                                               
         TM    TGSYSTAT,TASYSPID   ARE WE USING PID#                            
         BZ    LIN40                                                            
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVC   SLNSSN,SPACES                                                    
         MVC   SLNSSN(L'TGPID),TGPID                                            
         MVI   SLNSSNH+5,6                                                      
         OI    SLNSSNH+6,X'80'                                                  
*                                                                               
LIN40    MVC   AIO,AIO1            RESTORE AIO                                  
         MVC   SLNLIN,TLLNLIN      REFERENCE NUMBER                             
         OI    SLNLINH+6,X'80'     TRANSMIT                                     
         MVC   KEY(L'SVKEY),SVKEY  RESTORE KEY                                  
         B     XIT                                                              
         EJECT                                                                  
*              DISPLAY THE RECORD                                               
         SPACE 1                                                                
DISPLAY  NTR1                                                                   
         TWAXC SLNEMPH                                                          
         XC    SLNEMPN,SLNEMPN     CLEAR EMPLOYER NAME                          
         OI    SLNEMPNH+6,X'80'                                                 
         XC    SLNEFT,SLNEFT       CLEAR EFT                                    
         OI    SLNEFTH+6,X'80'                                                  
         XC    SLNPAYN,SLNPAYN     CLEAR LIEN PAYEE NAME                        
         OI    SLNPAYNH+6,X'80'                                                 
         XC    SLNUNIN,SLNUNIN     CLEAR TAX UNIT NAME                          
         OI    SLNUNINH+6,X'80'                                                 
         OI    SLNCOLH+6,X'80'     TRANSMIT COLLECTED AMOUNT                    
         OI    SLNBALH+6,X'80'     TRANSMIT BALANCE AMOUNT                      
         XC    SLNBAL,SLNBAL       CLEAR BALANCE AMOUNT                         
         SPACE                                                                  
         MVI   ELCODE,TALNELQ      GET LIEN DETAILS ELEMENT                     
         L     R4,AIO                                                           
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         USING TALND,R4                                                         
         MVC   SLNEMP,TALNEMP      EMPLOYER                                     
         MVC   AIO,AIO2            MAKE SURE DON'T CREAM RECORD                 
         GOTO1 RECVAL,DMCB,TLEMCDQ,(X'88',SLNEMP),SLNEMPNH  GET NAME            
         MVC   AIO,AIO1            RESTORE AIO                                  
*                                                                               
         MVI   SLNEFT,C'N'         EFT                                          
         TM    TALNSTAT,TALNSEFT                                                
         BZ    *+8                                                              
         MVI   SLNEFT,C'Y'                                                      
*                                                                               
         MVC   SLNTYPE,TALNTYPE    LIEN TYPE                                    
         MVC   SLNPAYE,TALNPAYE    LIEN PAYEE                                   
         MVI   SLNPAYEH+5,9                                                     
         MVC   AIO,AIO2            MAKE SURE DON'T CREAM RECORD                 
         MVC   SVTGSSN,TGSSN       SAVE GLOBAL SSN                              
*                                                                               
DISP02   GOTO1 RECVAL,DMCB,TLW4CDQ,(X'0C',SLNPAYEH),SLNPAYNH LIEN PAYEE         
*                                                                               
         TM    TGSYSTAT,TASYSPID   ARE WE USING PID#                            
         BZ    DISP05                                                           
         GOTO1 SSNPACK,DMCB,TGSSN,TGDUB                                         
         MVC   SLNPAYE,SPACES                                                   
         MVC   SLNPAYE(L'TGPID),TGDUB                                           
         MVI   SLNPAYEH+5,6                                                     
         OI    SLNPAYEH+6,X'80'                                                 
*                                                                               
DISP05   MVC   AIO,AIO1            RESTORE AIO                                  
         MVC   TGSSN,SVTGSSN       RESTORE GLOBAL SSN                           
         SPACE                                                                  
         TM    TALNSTAT,TALNSCAN   TEST CANADIAN DOLLARS                        
         BZ    *+8                                                              
         MVI   SLNCUR,C'C'                                                      
         TM    TALNSTAT,TALNSEUR   TEST EUROS                                   
         BZ    DISP07                                                           
         MVI   SLNCUR,C'E'                                                      
         B     DISP07                                                           
         SPACE                                                                  
         TM    TALNSTAT,TALNSCAN   TEST CANADIAN DOLLARS                        
         BZ    *+8                                                              
         MVI   SLNCUR,C'Y'                                                      
         SPACE                                                                  
DISP07   OC    TALNUNIT,TALNUNIT                                                
         BZ    DISP10                                                           
         MVC   SLNUNIT,TALNUNIT         TAX UNIT                                
         GOTO1 TAXVAL,DMCB,(3,TALNUNIT) GET TAX UNIT NAME                       
         BNE   DISP10                                                           
         MVC   SLNUNIN,TGTANAME                                                 
         OI    SLNUNITH+4,X'20'     SET PREVIOUSLY VALIDATED                    
DISP10   GOTO1 DATCON,DMCB,(1,TALNEXP),(8,SLNEXP)  EXPIRATION DATE              
         CLC   TALNTYPE,=AL2(TALNTYGT)  IF GT - NO LIMIT TO AMOUNT DUE          
         BE    DISP12                                                           
         EDIT  TALNDUE,(10,SLNDUE),2,ALIGN=LEFT    AMOUNT DUE                   
DISP12   EDIT  TALNXMPT,(10,SLNXMPT),2,ALIGN=LEFT  EXEMPT AMT                   
         OC    TALNAMT,TALNAMT                                                  
         BZ    DISP15                                                           
         EDIT  TALNAMT,(10,SLNAMT),2,ALIGN=LEFT    DEDUCT AMT                   
         B     DISP16                                                           
DISP15   EDIT  TALNPCT,(6,SLNPCT),2,ALIGN=LEFT     DEDUCT PERCENTAGE            
         SPACE 1                                                                
DISP16   EDIT  TALNCOL,(10,SLNCOL),2,ALIGN=LEFT    COLLECTED AMT                
         CLC   TALNTYPE,=AL2(TALNTYGT)  IF GT - DON'T SHOW BALANCE              
         BE    DISP18                           LIMITED BY EXP. DATE            
         ICM   R1,15,TALNDUE                                                    
         ICM   R3,15,TALNCOL                                                    
         SR    R1,R3                                                            
         EDIT  (R1),(10,SLNBAL),2,ALIGN=LEFT       BALANCE AMOUNT               
         SPACE                                                                  
DISP18   GOTO1 CHAROUT,DMCB,TACMELQ,(3,SLNCOMMH),TACMTYPG NARRATIVE             
         GOTO1 CHAROUT,DMCB,TACMELQ,(1,SLNCAACH),TACMTYPL CASE/ACC#             
         GOTO1 ACTVOUT,DMCB,SLNLCHGH LAST CHANGED                               
         B     XIT                                                              
         EJECT                                                                  
*              BUILD THE RECORD                                                 
         SPACE 1                                                                
         USING TALND,R4                                                         
BLDREC   NTR1                                                                   
         XC    COLAMT,COLAMT       INIT COLLECTED AMOUNT TO 0 FOR ADD           
         MVI   ELCODE,TALNELQ      LIEN DETAILS ELEMENT                         
         L     R4,AIO                                                           
         BAS   RE,GETEL            IF THERE'S CURRENT                           
         BNE   BLDR3                                                            
         MVC   COLAMT,TALNCOL      SAVE COLLECTED AMOUNT                        
         GOTO1 REMELEM             DELETE CURRENT                               
         SPACE                                                                  
BLDR3    LA    R4,ELEMENT                                                       
         XC    ELEMENT,ELEMENT     BUILD NEW ELEMENT                            
         MVI   TALNEL,TALNELQ                                                   
         MVI   TALNLEN,TALNLNQ                                                  
*                                                                               
         LA    R2,SLNEFTH          EFT                                          
         NI    TALNSTAT,X'FF'-TALNSEFT                                          
         CLI   8(R2),C'N'                                                       
         BE    BLDR3A                                                           
         CLI   8(R2),C'Y'                                                       
         BNE   FLDINV                                                           
         OI    TALNSTAT,TALNSEFT                                                
*                                                                               
BLDR3A   LA    R2,SLNEMPH          EMPLOYER                                     
         GOTO1 ANY                                                              
         GOTO1 RECVAL,DMCB,TLEMCDQ,SLNEMPH                                      
         MVC   TALNEMP,TGEMP                                                    
         SPACE                                                                  
**ALWAYS TM    SLNTYPEH+4,X'20'    IF LIEN TYPE NOT PREV VALIDATED              
**DO     BO    *+8                                                              
         BAS   RE,VALLIEN          VALIDATE LIEN TYPE                           
         MVC   TALNTYPE,SLNTYPE                                                 
         MVC   TALNRNK,RANKNUM     RANK                                         
         LA    R2,SLNPAYEH         LIEN PAYEE REQUIRED                          
         CLI   5(R2),0                                                          
         BE    FLDMISS                                                          
         TM    4(R2),X'20'         IF NOT PREVIOUSLY VALIDATED                  
         BO    BLDR5                                                            
         SPACE                                                                  
         MVC   AIO,AIO2            MAKE SURE DON'T CREAM RECORD                 
         MVC   SVTGSSN,TGSSN       SAVE GLOBAL SSN                              
*                                                                               
         LA    R2,SLNPAYEH         S/S NUM                                      
         TM    TGSYSTAT,TASYSPID   ARE WE USING PID#                            
         BZ    BLDR4                                                            
         CLI   SLNPAYEH+5,0                                                     
         BE    FLDMISS                                                          
         CLI   SLNPAYEH+5,9        MUST CHECK FOR LENGTH HERE SINCE             
         BE    BLDR4               RECVAL CALL DOES NOT CHECK FOR               
         CLI   SLNPAYEH+5,6        INVALID ENTRIES DUE TO X'40' PARAM           
         BNE   FLDINV                                                           
         MVC   TGPID,SLNPAYE                                                    
         GOTO1 SSNUNPK,DMCB,TGPID,TGSSN                                         
         BNE   BLDR4                                                            
         MVC   SLNPAYE,TGSSN                                                    
         MVI   SLNPAYEH+5,9                                                     
*                                                                               
BLDR4    GOTO1 RECVAL,DMCB,TLW4CDQ,SLNPAYEH LIEN PAYEE                          
*                                                                               
         TM    TGSYSTAT,TASYSPID   ARE WE USING PID#                            
         BZ    BLDR4A                                                           
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVC   SLNPAYE,SPACES                                                   
         MVC   SLNPAYE(L'TGPID),TGPID                                           
         MVI   SLNPAYEH+5,6                                                     
         OI    SLNPAYEH+6,X'80'                                                 
*                                                                               
BLDR4A   MVC   AIO,AIO1            RESTORE AIO                                  
         MVC   TGSSN,SVTGSSN       RESTORE GLOBAL SSN                           
         SPACE                                                                  
BLDR5    DS    0H                                                               
         MVC   TALNPAYE,SLNPAYE                                                 
*                                                                               
         TM    TGSYSTAT,TASYSPID   ARE WE USING PID#                            
         BZ    BLDR6                                                            
         MVC   SVTGPID,TGPID       SAVE GLOBAL PID                              
         MVC   TGPID,SLNPAYE                                                    
         GOTO1 SSNUNPK,DMCB,TGPID,TALNPAYE                                      
         MVI   SLNPAYEH+5,9                                                     
         MVC   TGPID,SVTGPID       RESTORE GLOBAL PID                           
*                                                                               
BLDR6    LA    R2,SLNEXPH          EXPIRATION DATE REQUIRED                     
         CLI   5(R2),0                                                          
         BE    FLDMISS                                                          
         GOTO1 DATVAL,DMCB,SLNEXP,DUB                                           
         OC    DMCB(4),DMCB                                                     
         BZ    DATEINV                                                          
         GOTO1 DATCON,DMCB,DUB,(1,TALNEXP)                                      
         SPACE                                                                  
         LA    R2,SLNCURH          CURRENCY FIELD                               
         CLI   5(R2),0                                                          
         BE    BLDR7                                                            
         SPACE                                                                  
         CLI   8(R2),C'C'                                                       
         BNE   *+12                                                             
         OI    TALNSTAT,TALNSCAN                                                
         B     BLDR7                                                            
         CLI   8(R2),C'E'                                                       
         BNE   *+12                                                             
         OI    TALNSTAT,TALNSEUR                                                
         B     BLDR7                                                            
         CLI   8(R2),C'U'                                                       
         BNE   FLDINV                                                           
         SPACE                                                                  
BLDR7    LA    R2,SLNUNITH                                                      
         ZIC   R3,5(R2)                                                         
         LTR   R3,R3               IF THERE'S TAX UNIT INPUT                    
         BZ    BLDR10                                                           
         TM    4(R2),X'20'         AND IF NOT PREVIOUSLY VALIDATED              
         BO    BLDR8                                                            
         GOTO1 TAXVAL,DMCB,((R3),8(R2)) VALIDATE IT                             
         BNE   FLDINV                                                           
         OI    4(R2),X'20'         SET PREVIOUSLY VALIDATED                     
BLDR8    MVC   TALNUNIT,SLNUNIT                                                 
         OC    TALNUNIT,SPACES                                                  
BLDR10   LA    R2,SLNDUEH          AMOUNT DUE REQUIRED                          
         CLC   TALNTYPE,=AL2(TALNTYGT)  IF GT- UNLIMITED AMOUNT DUE             
         BNE   BLDR11                                                           
         CLI   5(R2),0                                                          
         BNE   FLDINV                                                           
         XC    DMCB+4(4),DMCB+4                                                 
         B     BLDR15                                                           
BLDR11   BAS   RE,VALCASH                                                       
         BNE   FLDMISS                                                          
         CLI   ACTNUM,ACTADD       IF ACTION IS ADD                             
         BNE   BLDR12                                                           
         CLC   DMCB+4(4),=F'0'                                                  
         BE    INVAMT              ERROR IF AMOUNT DUE = 0                      
BLDR12   CLC   COLAMT,DMCB+4                                                    
         BH    LESSCOL             ERROR IF < COLLECTED (FOR CHANGE)            
BLDR15   MVC   TALNDUE,DMCB+4                                                   
         SPACE 1                                                                
         LA    R2,SLNXMPTH         OPTIONAL EXEMPT AMOUNT-DEFAULT IS 0          
         BAS   RE,VALCASH                                                       
         BNE   *+10                                                             
         MVC   TALNXMPT,DMCB+4                                                  
         SPACE                                                                  
         LA    R2,SLNAMTH                                                       
         CLI   5(R2),0             IF NO DEDUCT AMOUNT INPUT                    
         BNE   BLDR20                                                           
         CLI   SLNPCTH+5,0         AND NO DEDUCT PERCENTAGE INPUT               
         BNE   BLDR25                                                           
         B     FLDMISS             MISSING INPUT ERROR                          
         SPACE                                                                  
BLDR20   BAS   RE,VALCASH          VALIDATE DEDUCT AMOUNT                       
         CLC   TALNDUE,DMCB+4                                                   
         BL    OVERDUE             ERROR IF > AMOUNT DUE                        
         CLC   DMCB+4(4),=F'0'                                                  
         BE    FLDINV              ERROR IF DEDUCT AMOUNT = 0                   
         MVC   TALNAMT,DMCB+4                                                   
         LA    R2,SLNPCTH                                                       
         CLI   5(R2),0                                                          
         BNE   NOINPUT             ERROR IF DEDUCT % ALSO INPUT                 
         B     BLDR30                                                           
BLDR25   LA    R2,SLNPCTH          DEDUCT PERCENTAGE IS INPUT                   
         BAS   RE,VALCASH                                                       
         CLC   DMCB+4(4),=F'10000'                                              
         BH    FLDINV              ERROR IF > 100                               
         CLC   DMCB+4(4),=F'0'                                                  
         BE    FLDINV              ERROR IF DEDUCT % = 0                        
         MVC   TALNPCT,DMCB+6                                                   
BLDR30   MVC   TALNCOL,COLAMT      COLLECTED AMOUNT                             
         GOTO1 ADDELEM             ADD NEW ELEMENT                              
         SPACE                                                                  
         GOTO1 NAMIN,DMCB,(3,TACMELQ),(X'80',SLNCOMMH),TACMTYPG COMMENT         
         GOTO1 NAMIN,DMCB,(1,TACMELQ),(X'80',SLNCAACH),TACMTYPL CASE/AC         
         GOTO1 ACTVIN,DMCB,SLNLCHGH LAST CHANGED                                
         L     R3,AIO                                                           
         MVC   KEY,0(R3)           RESTORE KEY                                  
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE LIEN TYPE                                               
         SPACE                                                                  
VALLIEN  NTR1                                                                   
         LA    R2,SLNTYPEH         R2=A(LIEN TYPE HEADER)                       
         CLI   5(R2),0                                                          
         BE    FLDMISS             LIEN TYPE REQUIRED                           
         LA    R3,LIENTAB          R3=A(ENTRY IN LIEN TYPE TABLE)               
VALL10   CLI   0(R3),X'FF'         IF NOT END OF TABLE                          
         BE    FLDINV                                                           
         CLC   8(2,R2),0(R3)       MATCH ON LIEN TYPE                           
         BE    VALL20                                                           
         LA    R3,L'LIENTAB(R3)    GET NEXT ENTRY                               
         B     VALL10                                                           
         SPACE                                                                  
VALL20   MVC   RANKNUM,2(R3)       SAVE RANK                                    
*****    OI    4(R2),X'20'         SET PREVIOUSLY VALIDATED                     
         B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE CHECKS THAT THE COLLECTED AMOUNT ON A LIEN               
*              RECORD IS 0 BEFORE ALLOWING DELETION OF THE RECORD               
         SPACE                                                                  
         USING TALND,R4                                                         
CHKDEL   DS    0H                                                               
         L     R4,AIO                                                           
         MVI   ELCODE,TALNELQ      GET LIEN DETAILS ELEMENT                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R2,SLNCOLH          R2=A(COLLECTED AMOUNT FIELD HEADER)          
         CLC   TALNCOL,=F'0'       IF RECORD HAS BEEN USED                      
         BE    XIT                                                              
         BAS   RE,DISPLAY          DISPLAY RECORD WE CANNOT DELETE              
         B     NODELETE            GIVE ERROR                                   
******************************* CHECK BALANCE = 0 FOR DELETE                    
*        LA    R2,SLNBALH          R2=A(BALANCE FIELD HEADER)                   
*        ICM   R1,15,TALNDUE       TOTAL DUE                                    
*        ICM   R3,15,TALNCOL       - TOTAL COLLECTED                            
*        SR    R1,R3               = TOTAL BALANCE                              
*        C     R1,=F'0'            IF BALANCE NOT 0                             
*        BE    XIT                                                              
*        BAS   RE,DISPLAY          DISPLAY RECORD WE CANNOT DELETE              
*        B     NODELETE            GIVE ERROR                                   
*******************************                                                 
         EJECT                                                                  
*              ROUTINE GETS W4 DETAILS ELEMENT IN W4 RECORD                     
*              AND TURNS ON BIT THAT SAYS LIEN RECORD PRESENT                   
         SPACE                                                                  
CHANGEW4 NTR1                                                                   
         MVC   AIO,AIO2            MAKE SURE DON'T CREAM RECORD                 
*                                                                               
         LA    R2,SLNSSNH          S/S NUM                                      
         TM    TGSYSTAT,TASYSPID   ARE WE USING PID#                            
         BZ    CHGW410                                                          
         CLI   SLNSSNH+5,0                                                      
         BE    FLDMISS                                                          
         CLI   SLNSSNH+5,9         MUST CHECK FOR LENGTH HERE SINCE             
         BE    CHGW410             RECVAL CALL DOES NOT CHECK FOR               
         CLI   SLNSSNH+5,6         INVALID ENTRIES DUE TO X'40' PARAM           
         BNE   FLDINV                                                           
         MVC   TGPID,SLNSSN                                                     
         GOTO1 SSNUNPK,DMCB,TGPID,TGSSN                                         
         BNE   CHGW410                                                          
         MVC   SLNSSN,TGSSN                                                     
         MVI   SLNSSNH+5,9                                                      
*                                                                               
CHGW410  GOTO1 RECVAL,DMCB,TLW4CDQ,(X'34',SLNSSNH) GET W4 RECORD                
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
*                                                                               
         TM    TGSYSTAT,TASYSPID   ARE WE USING PID#                            
         BZ    CHGW420                                                          
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVC   SLNSSN,SPACES                                                    
         MVC   SLNSSN(L'TGPID),TGPID                                            
         MVI   SLNSSNH+5,6                                                      
         OI    SLNSSNH+6,X'80'                                                  
*                                                                               
CHGW420  MVI   ELCODE,TAW4ELQ      GET W4 DETAILS ELEMENT                       
         L     R4,AIO                                                           
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         USING TAW4D,R4                                                         
         OI    TAW4STAT,TAW4STLN   SET LIEN RECORD PRESENT                      
         GOTO1 PUTREC              WRITE BACK CHANGED                           
         MVC   AIO,AIO1            RESTORE AIO                                  
         B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE CHECKS IF THERE ARE ANY EXISTING LIEN RECORDS            
*              IF NOT, TURNS OFF BIT IN W4 RECORD                               
         SPACE                                                                  
CHKLIEN  NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R3,KEY              BUILD KEY & READ HIGH                        
         L     R4,AIO                                                           
         MVC   KEY(24),0(R4)       USING SAME SS NUM AS RECORD                  
         NI    DMINBTS,X'F7'       DON'T READ DELETED                           
         GOTO1 HIGH                                                             
         SPACE                                                                  
         CLC   KEY(24),KEYSAVE     COMPARE - IGNORING LIEN CODE                 
         BE    XIT                 XIT - ANOTHER RECORD EXISTS                  
         SPACE                                                                  
         MVC   AIO,AIO2            CHANGE IO AREAS                              
*                                                                               
         LA    R2,SLNSSNH          S/S NUM                                      
         TM    TGSYSTAT,TASYSPID   ARE WE USING PID#                            
         BZ    CHKLI10                                                          
         CLI   SLNSSNH+5,0                                                      
         BE    FLDMISS                                                          
         CLI   SLNSSNH+5,9         MUST CHECK FOR LENGTH HERE SINCE             
         BE    CHKLI10             RECVAL CALL DOES NOT CHECK FOR               
         CLI   SLNSSNH+5,6         INVALID ENTRIES DUE TO X'40' PARAM           
         BNE   FLDINV                                                           
         MVC   TGPID,SLNSSN                                                     
         GOTO1 SSNUNPK,DMCB,TGPID,TGSSN                                         
         BNE   CHKLI10                                                          
         MVC   SLNSSN,TGSSN                                                     
         MVI   SLNSSNH+5,9                                                      
*                                                                               
CHKLI10  GOTO1 RECVAL,DMCB,TLW4CDQ,(X'34',SLNSSNH)       UPDATE REC             
         BE    *+6                 RECORD WAS JUST READ - MUST BE THERE         
         DC    H'0'                                                             
         L     R4,AIO              GET W4 ELEMENT                               
         USING TAW4D,R4                                                         
         MVI   ELCODE,TAW4ELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         NI    TAW4STAT,X'FF'-TAW4STLN TURN OFF LIEN BIT                        
         GOTO1 PUTREC                                                           
         MVC   AIO,AIO1                                                         
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE CASH FIELD WHOSE HEADER IS AT R2                        
         SPACE                                                                  
VALCASH  NTR1                                                                   
         ZIC   R3,5(R2)                                                         
         LTR   R3,R3                                                            
         BZ    NO                  SET CC NOT EQUAL IF NO INPUT                 
         LA    R4,8(R2)                                                         
         GOTO1 CASHVAL,DMCB,(R4),(R3) VALIDATE IT                               
         CLI   DMCB,X'FF'                                                       
         BE    INVAMT                                                           
         L     R1,DMCB+4                                                        
         LTR   R1,R1                                                            
         BM    INVAMT              ERROR IF NEGATIVE                            
         B     YES                                                              
         EJECT                                                                  
*              LOCAL ERROR/EXIT ROUTINES                                        
         SPACE                                                                  
FLDMISS  MVI   ERROR,MISSING       MISSING INPUT FIELD                          
         B     THEEND                                                           
         SPACE                                                                  
FLDINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     THEEND                                                           
         SPACE                                                                  
DATEINV  MVI   ERROR,INVDATE       INVALID DATE                                 
         B     THEEND                                                           
         SPACE                                                                  
INVAMT   MVI   ERROR,ERINVAMT      INVALID AMOUNT                               
         B     THEEND                                                           
         SPACE                                                                  
NOINPUT  MVI   ERROR,ERNOINP       INPUT NOT ALLOWED                            
         B     THEEND                                                           
         SPACE                                                                  
OVERDUE  MVI   ERROR,EROVDUE       AMOUNT GREATER THAN DUE                      
         B     THEEND                                                           
         SPACE                                                                  
LESSCOL  MVI   ERROR,ERLESSC       AMOUNT LESS THAN COLLECTED                   
         B     THEEND                                                           
         SPACE                                                                  
NODELETE MVI   ERROR,ERNOCOLL      CAN'T DELETE IF CREDIT <> 0                  
         B     THEEND                                                           
         SPACE                                                                  
THEEND   GOTO1 EXIT,DMCB,0                                                      
         SPACE 2                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         SPACE 2                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 2                                                                
*              LIEN TYPE AND RANK TABLE                                         
         SPACE                                                                  
LIENTAB  DS    0CL3                                                             
         DC    AL2(TALNTYCS),AL1(TALNR10)                                       
         DC    AL2(TALNTYFD),AL1(TALNR20)                                       
         DC    AL2(TALNTYST),AL1(TALNR30)                                       
         DC    AL2(TALNTYWG),AL1(TALNR40)                                       
         DC    AL2(TALNTYGG),AL1(TALNR50)                                       
         DC    AL2(TALNTYGT),AL1(TALNR60)                                       
         DC    X'FF'                                                            
         SPACE 2                                                                
PFTAB    DS    0C                  PF KEYS TABLE                                
         SPACE                                                                  
         DC    AL1(PF13X-*,13,0,0,0)                                            
         DC    CL3' ',CL8'PAYEE',CL8'DISPLAY'                                   
PF13     DC    AL1(KEYTYGLB,L'TGSSN-1),AL2(TGSSN-TGD)                           
PF13X    EQU   *                                                                
         SPACE                                                                  
         DC    AL1(PF14X-*,14,0,(PF14X-PF14)/KEYLNQ,0)                          
         DC    CL3' ',CL8'COMMERCL',CL8'LIST'                                   
PF14     DC    AL1(KEYTYCOM,0),AL2(0)                                           
*        DC    AL1(KEYTYCOM,0),AL2(0)                                           
*        DC    AL1(KEYTYCOM,0),AL2(0)                                           
*        DC    AL1(KEYTYGLB,L'TGSSN-1),AL2(TGSSN-TGD)                           
PF14X    EQU   *                                                                
         SPACE                                                                  
         DC    AL1(PF15X-*,15,0,0,0)                                            
         DC    CL3' ',CL8'YTD',CL8'DISPLAY'                                     
PF15X    EQU   *                                                                
         SPACE                                                                  
         DC    AL1(PF16X-*,16,0,0,0)                                            
         DC    CL3' ',CL8'LTRACK',CL8'LIST'                                     
PF16X    EQU   *                                                                
         SPACE                                                                  
         DC    AL1(PF17X-*,17,0,(PF17X-PF17)/KEYLNQ,0)                          
         DC    CL3' ',CL8'DUECOMP',CL8'LIST'                                    
PF17     DC    AL1(KEYTYGLB,L'TGSSN-1),AL2(TGSSN-TGD)                           
PF17X    EQU   *                                                                
         SPACE                                                                  
         DC    AL1(PF18X-*,18,0,0,0)                                            
         DC    CL3' ',CL8'GRT',CL8'LIST'                                        
PF18X    EQU   *                                                                
         SPACE                                                                  
         DC    X'FF'                                                            
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR22D                                                       
         EJECT                                                                  
         ORG   SLNWORK                                                          
RANKNUM  DS    CL1                 RANK NUMBER                                  
SVTGSSN  DS    CL9                 SAVED GLOBAL SSN                             
SVTGPID  DS    CL6                 SAVED GLOBAL PID                             
SVKEY    DS    CL38                SAVED KEY WITH ADDRESS                       
COLAMT   DS    CL4                 OLD COLLECTED AMOUNT                         
PTRS     DS    CL(L'TLDRREC*2+1)   SAVED ACTIVE AND 1 PASSIVE PTR               
         EJECT                                                                  
* DDGENTWA                                                                      
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TASYSEQUS                                                                     
* TASYSDSECT                                                                    
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'089TAGEN22   03/22/11'                                      
         END                                                                    
