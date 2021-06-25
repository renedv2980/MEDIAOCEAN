*          DATA SET ACBAT3D    AT LEVEL 017 AS OF 03/30/06                      
*                                                                               
*PHASE T61B3DA                                                                  
         TITLE 'MULTIPLE BILLABLE/NON-BILLABLE EXPENSE ENTRY'                   
*                                                                               
*        BATCH TYPE 61                                                          
*                                                                               
T61B3D   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 PROGDX-PROGD,T61B3D**,R7,R8,RR=R5,CLEAR=YES                      
         USING TWAD,RA                                                          
         L     R9,4(R1)                                                         
         USING GWS,R9                                                           
         USING PROGD,RC                                                         
         ST    R5,RELOA                                                         
         LH    R1,=Y(EXCBLK-PROGD)                                              
         LA    R1,PROGD(R1)                                                     
         ST    R1,AEXCELD          SET POINTER TO EXCEL BLOCK                   
         LH    R1,=Y(CATBLK-PROGD)                                              
         LA    R1,PROGD(R1)                                                     
         ST    R1,ACATD            SET POINTER TO CATCALL BLOCK                 
*                                                                               
* BUILD SCREEN DIRECTORY                                                        
*                                                                               
MUL      LA    R1,FSTTAB                                                        
         CLI   TWASCRN,X'E1'                                                    
         BE    *+8                                                              
         LA    R1,SECTAB                                                        
*                                                                               
MUL1     CLI   0(R1),X'FF'         TEST FOR EOT                                 
         BE    MUL2                                                             
         LM    RE,RF,0(R1)                                                      
         LA    RE,TWAD(RE)         FORM READ ADDRESS OF SCREEN FIELD            
         LA    RF,PROGD(RF)        FORM ADDRESS OF ADCON                        
         ST    RE,0(RF)                                                         
         LA    R1,L'FSTTAB(R1)                                                  
         B     MUL1                                                             
*                                                                               
MUL2     ZAP   CSHTOT,CSLSTCUR+LSTBCSHA-LSTTABD(L'LSTBCSHA)                     
*                                                                               
MUL3     LA    R2,BASSRVH          PREPARE BH MSG FLD                           
         OI    1(R2),X'01'         TURN ON MODIFIED BIT                         
         OI    6(R2),X'80'         TURN ON TRANSMIT BIT                         
         CLI   TWASCRN,X'E1'       TEST FOR FIRST SCREEN                        
         BNE   MUL4                YES                                          
*                                                                               
         OI    ATRACDH+6,X'80'     CLEAR DISPLAY FLAGS                          
         MVC   ATRACD,SPACES                                                    
         OI    ATRCDDH+6,X'80'                                                  
         MVC   ATRCDD,SPACES                                                    
         OI    ATRAC2DH+6,X'80'                                                 
         MVC   ATRAC2D,SPACES                                                   
         OI    ATRCDD2H+6,X'80'                                                 
         MVC   ATRCDD2,SPACES                                                   
*                                                                               
MUL4     MVI   KEYCHG,C'N'                                                      
         CLI   TWASCRN,X'E1'       TEST FOR FIRST SCREEN                        
         BNE   *+8                 NO                                           
         BAS   RE,VALHED                                                        
         CLI   PFKEY,X'FF'                                                      
         BNE   *+8                                                              
         MVI   PFKEY,0                                                          
*                                                                               
         CLI   PFKEY,0             TEST FOR ANY PFKEY                           
         BE    *+8                 NO                                           
         BAS   RE,PF                                                            
         BAS   RE,TSTEDT           TEST ANYTHING TO EDIT                        
         BE    MUL6                                                             
*                                                                               
         CLI   TWASCRN,X'E1'       TEST FIRST SCREEN                            
         BNE   MUL5                NO                                           
         CLI   KEYCHG,C'Y'         TEST IF HEADER CHANGED                       
         BNE   MUL5                NO                                           
         BAS   RE,ERASE            YES-CLEAR LOWER SCREEN                       
*                                                                               
MUL5     L     R2,ADETH                                                         
         MVC   FVMSGNO,=AL2(AE$DEBIT)     INPUT DEBIT INFORMATION               
         B     ERRXIT                                                           
*                                                                               
MUL6     BAS   RE,EDT              MANAGE THE EDIT                              
*                                                                               
MUL8     LA    R2,ATRDOCH                                                       
         CLI   TWASCRN,X'E1'                                                    
         BE    *+8                                                              
         LA    R2,ATSCLIH                                                       
         MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
         B     EXIT                RETURN TO BASE                               
         EJECT                                                                  
*                                                                               
*------------------------------------------------------------                   
* VALIDATE HEADLINE FIELDS -- REVIEW ERROR EXITS                                
*------------------------------------------------------------                   
*                                                                               
VALHED   NTR1  ,                                                                
         BAS   RE,EDDOC                                                         
*                                                                               
         BAS   RE,EDDAT                                                         
         BNE   ERRXIT                                                           
*                                                                               
VALHED2  LA    R2,ATRURGH         VALIDATE URGENT                               
         BAS   RE,TSTKEY                                                        
         GOTO1 AFVAL,(R2)                                                       
         MVC   SVURG,FVIFLD                                                     
         BNE   VALHED4             NOTHING IN FIELD                             
         CLI   FVIFLD,C'U'         TEST FOR VALID INPUT                         
         BE    VALHED4             YES                                          
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     ERRXIT                                                           
*                                                                               
VALHED4  BAS   RE,EDCASH                                                        
*                                                                               
         LA    RE,SVVENVAL         CLEAR SAVED VENDOR VALUES                    
         LA    RF,SVVENLNQ                                                      
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         ZAP   SVDISC,=P'0'       INIT DISCOUNT SAVES                           
         ZAP   SVDISC2,=P'0'                                                    
*                                                                               
VALHED6  LA    R2,ATRACCH         PRODUCTION VENDOR FIELD                       
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0            IS THERE A PROD VENDOR?                       
         BE    VALHED8            NO                                            
*                                                                               
         MVI   TYPE,C'P'          SET PROD VENDOR SWITCH                        
         BAS   RE,VEN             EDIT VENDOR AND CD FIELDS                     
*                                                                               
VALHED8  BAS   RE,SETVEN           FOR O+M, COPY PROD VENDOR                    
         LA    R1,ATRACC2H         TEST FOR AN EXPENSE VENDOR                   
         CLI   5(R1),0                                                          
         BE    VALHED9                                                          
*                                                                               
         LA    R2,ATRACC2H                                                      
         MVI   TYPE,C'E'           NOTE EXPENSE VENDOR                          
         BAS   RE,VEN              NOW VERIFY EXP VENDOR                        
         B     VALHED10                                                         
*                                                                               
VALHED9  TM    COMPSTA2,X'04'      IS VENDOR REQUIRED?                          
         BZ    VALHED10            NO                                           
         CLI   SVCRACCT,0          WAS PROD VENDOR ENTERED?                     
         BNE   VALHED10            YES                                          
         CLI   SVCRACC2,0          WAS EXP VENDOR ENTERED?                      
         BNE   VALHED10            YES                                          
         LA    R2,ATRACCH          POINT TO PROD VENDOR FIELD                   
         MVC   FVMSGNO,=AL2(AE$PEVIR)   HAVE AT LEAST 1 VENDOR                  
         B     ERRXIT              ERROR                                        
*                                                                               
VALHED10 LA    R2,ATRACC2H                                                      
         BAS   RE,TSTKEY                                                        
         GOTO1 ANARRSCN,DMCB,(2,ANARH),0                                        
         MVC   SVNAR,BOELEM                                                     
         MVC   SVNARLEN,BOHALF1                                                 
*                                                                               
VALHED12 OI    ATRDOCH+4,X'20'                                                  
         OI    ATRDAT1H+4,X'20'                                                 
         OI    ATRURGH+4,X'20'                                                  
         OI    ATRCSHH+4,X'20'                                                  
         OI    ATRACCH+4,X'20'                                                  
         OI    ATRACC2H+4,X'20'                                                 
*                                                                               
VALHEDX  B     CURSIT                                                           
         SPACE 2                                                                
* SUB-ROUTINE TO COPY THE PRODUCTION VENDOR TO EXPENSE VENDOR                   
* FOR THE O+M COMPANIES                                                         
*                                                                               
SETVEN   TM    BCCPYST5,CPYSVEND   COPY THE VENDOR?                             
         BNOR  RE                  NO                                           
*                                                                               
SETVEN2  ST    RE,SAVERE                                                        
         GOTO1 AFVAL,ATRACCH                                                    
         BNE   SETVENX             NOTHING TO COPY                              
         MVC   FLD,FVIFLD                                                       
         LA    R2,ATRACC2H                                                      
         TM    FVIHDR+4,X'20'      TEST IF PRODUCTION VENDOR CHANGED            
         BZ    *+12                YES                                          
         CLI   5(R2),0             TEST FOR EMPTY EXPENSE VENDOR                
         BNE   SETVENX             NO-ASSUME ITS OK NOW                         
         MVC   5(1,R2),FVILEN      SET NEW INPUT LENGTH                         
         NI    4(R2),X'FF'-X'20'                                                
         BAS   RE,MOVEFLD                                                       
         OI    6(R2),X'80'                                                      
*                                                                               
SETVENX  L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO TEST FOR CHANGE IN KEY FIELDS                                  
*                                                                               
TSTKEY   TM    4(R2),X'20'                                                      
         BOR   RE                                                               
         MVI   KEYCHG,C'Y'                                                      
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
*-----------------------------------------------------------------              
* ROUTINE TO TEST FOR ANYTHING TO EDIT--AT EXIT, CC=EQ IF YES                   
* CC=NEQ FOR NO                                                                 
*-----------------------------------------------------------------              
*                                                                               
TSTEDT   NTR1  ,                                                                
*                                                                               
         LA    R6,2                                                             
         CLI   TWASCRN,X'E1'                                                    
         BE    *+8                                                              
         LA    R6,3                                                             
*                                                                               
         L     R2,ADETH                                                         
*                                                                               
* NO EDIT IS NEEDED IF ALL FIELDS ARE PREVIOUSLY VALIDATED OR IF ALL            
* FIELDS ARE EMPTY.  HOWEVER IF USER JUST OVERTYPES A FIELD, PROCEED            
* WITH EDIT.                                                                    
*                                                                               
TSTEDT2  BAS   RE,SETLIN                                                        
         LA    R3,ACLIH                                                         
         LA    R5,NDATAFLD-1                                                    
*                                                                               
TSTEDT4  L     R2,0(R3)                                                         
         TM    1(R2),X'20'         TEST PROTECTED FIELD                         
         BO    TSTEDT6             YES                                          
         TM    4(R2),X'20'         TEST IF PREVIOUSLY VALIDATED                 
         BO    TSTEDT5             YES                                          
*                                                                               
         CLI   5(R2),0             TEST FOR EMPTY FIELD                         
         BE    TSTEDT8             YES                                          
         B     TSTEDTY             NO-DO AN EDIT                                
*                                                                               
TSTEDT5  TM    4(R2),X'80'         TEST IF INPUT THIS TIME                      
         BZ    TSTEDT6             NO                                           
         CLI   5(R2),0             TEST IF INPUT IN FIELD                       
         BNE   TSTEDTY             YES-WE ARE EDITING                           
*                                                                               
TSTEDT6  LA    R3,4(R3)                                                         
         BCT   R5,TSTEDT4                                                       
*                                                                               
         L     R2,ANEXT                                                         
         BCT   R6,TSTEDT2                                                       
         B     TSTEDTN             NO EDIT IS NEEDED                            
*                                                                               
* CHECK THIS LINE CONSISTS OF EMPTY FIELDS                                      
*                                                                               
TSTEDT8  LA    R3,ACLIH                                                         
         LA    R5,NDATAFLD-1                                                    
*                                                                               
TSTEDT9  L     R2,0(R3)                                                         
         TM    1(R2),X'20'                                                      
         BO    *+12                                                             
         CLI   5(R2),0             TEST FOR EMPTY FIELD                         
         BNE   TSTEDTY             NO-SOMETHING TO EDIT                         
*                                                                               
         LA    R3,4(R3)            NEXT FIELD POINTER                           
         BCT   R5,TSTEDT9                                                       
*                                                                               
         L     R2,ANEXT                                                         
         BCT   R6,*+8                                                           
         B     TSTEDTN             NOTHING TO EDIT                              
*                                                                               
* CHECK REST OF SCREEN IS EMPTY--FLAG ANY FIELDS WITH INPUT                     
*                                                                               
TSTEDT10 BAS   RE,SETLIN                                                        
         LA    R3,ACLIH                                                         
         LA    R5,NDATAFLD-1                                                    
*                                                                               
TSTEDT11 L     R2,0(R3)                                                         
         TM    1(R2),X'20'         TEST FOR PROTECTED FIELD                     
         BO    TSTEDT12                                                         
         CLI   5(R2),0             MAKE SURE FIELD IS EMPTY                     
         BE    TSTEDT12                                                         
*                                                                               
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     ERRXIT                                                           
*                                                                               
TSTEDT12 LA    R3,4(R3)                                                         
         BCT   R5,TSTEDT11                                                      
*                                                                               
         L     R2,ANEXT                                                         
         BCT   R6,TSTEDT10                                                      
         B     TSTEDTN                                                          
*                                                                               
TSTEDTN  LTR   RB,RB               SET CC=NEQ                                   
         B     TSTEDTX                                                          
*                                                                               
TSTEDTY  CR    RB,RB               SET CC=EQ                                    
*                                                                               
TSTEDTX  B     CURSIT                                                           
         EJECT                                                                  
*                                                                               
*-----------------------------------------------------------------              
* ROUTINE TO PROCESS PFKEYS                                                     
*-----------------------------------------------------------------              
*                                                                               
PF       NTR1  ,                                                                
         CLI   PFKEY,PFK09         TEST PF9=SWAP TO SECOND SCREEN               
         BNE   PF10                                                             
*                                                                               
         CLI   TWASCRN,X'E1'       TEST FOR FIRST SCREEN                        
         BNE   PFR                 NO-ITS AN INVALID PFKEY                      
*                                                                               
         LA    RE,CSLSTCUR+LSTBITMA-LSTTABD                                     
         OC    0(L'LSTBITMA,RE),0(RE) TEST ANY ITEMS ADDED                      
         BZ    PFR                 NO-DO NOT ALLOW ACCESS TO 2ND SCREEN         
*                                                                               
         LA    R0,OSVALS           NOTE-NTRSES ZEROES OSVALS                    
         LA    R1,SAVEEND-OSVALS                                                
         LR    RF,R1               SAVE OSVALS AREA                             
         LA    RE,IOAREA                                                        
         MVCL  RE,R0                                                            
*                                                                               
         GOTO1 ANTRSES,0           SAVE SCREEN                                  
         L     RE,ATIA                                                          
         LA    RF,OSVALS-TWAD                                                   
         LA    R0,TWAD                                                          
         LR    R1,RF                                                            
         MVCL  RE,R0               COPY SCREEN TO TIA                           
*                                                                               
         LA    R0,OSVALS                                                        
         LA    R1,SAVEEND-OSVALS                                                
         LR    RF,R1               RESTORE OSVALS AREA                          
         LA    RE,IOAREA                                                        
         MVCL  R0,RE                                                            
*                                                                               
         MVI   CSSPROG,1           SET SPROG FOR SECOND SCREEN PFKEYS           
         GOTO1 AOVRSCR,BOPARM,(X'EA',BASOLY2H)                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R3,FLDTAB                                                        
*                                                                               
PF1      CLI   0(R3),X'FF'         TEST FOR EOT                                 
         BE    PF2                 YES                                          
*                                                                               
         LH    R2,2(R3)            GET DISP TO NEW FIELD                        
         LA    R2,TWAD(R2)         POINT TO NEW FIELD                           
         LH    R1,0(R3)            GET DISP TO OLD FIELD                        
         A     R1,ATIA                                                          
         GOTO1 AFVAL                                                            
         MVC   FLD,FVIFLD          FETCH EXTRACTED DATA                         
         BAS   RE,MOVEFLD          COPY TO NEW FIELD                            
         OI    6(R2),X'80'         XMIT IT BACK                                 
         LA    R3,L'FLDTAB(R3)                                                  
         B     PF1                                                              
*                                                                               
PF2      LA    R2,ATSACCNH         DISPLAY ACCOUNT NAMES IN 1 FIELD             
         BAS   RE,ACCN             BUILD ACCOUNT NAME FIELD                     
         BAS   RE,MOVEFLD                                                       
         OI    6(R2),X'80'                                                      
*                                                                               
PF4      MVC   FVMSGNO,=AL2(AE$DEBIT)                                           
         LA    R2,ATSCLIH                                                       
         B     ERRXIT                                                           
*                                                                               
PF10     CLI   PFKEY,PFK10         TEST PF10=GO BACK TO FIRST SCREEN            
         BNE   PF20                                                             
*                                                                               
         CLI   TWASCRN,X'EA'       TEST FOR SECOND SCREEN                       
         BNE   PFR                 NO-ITS AN ERROR                              
*                                                                               
         LA    R2,BASOLY1H         FIND ITEM COUNT LINE                         
         BAS   RE,BUMP                                                          
         GOTO1 AFVAL,(R2)                                                       
         MVC   FLD,FVIFLD          SAVE LINE FROM SECOND SCREEN                 
*                                                                               
         GOTO1 AXITSES             RESTORE SCREEN, ETC.                         
         BAS   RE,MOVEFLD          PRESERVE 2ND SCREEN ITEM COUNT ETC.          
         MVI   CSSPROG,0                                                        
         XC    FVMSGNO,FVMSGNO     CLEAR ANY MESSAGE NUMBER                     
         MVI   FVOMTYP,0                                                        
         B     PF21                NOW ERASE THE LOWER SCREEN                   
*                                                                               
PF20     CLI   PFKEY,PFK11         TEST PF11=ERASE SCREEN                       
         BNE   PFR                                                              
*                                                                               
PF21     BAS   RE,ERASE                                                         
*                                                                               
PF25     CLI   PFKEY,PFK11         TEST PF11=ERASE SCREEN                       
         BE    PF26                YES                                          
*                                                                               
         LA    R2,ATRDOCH                                                       
         LA    R3,FSTREST                                                       
         B     MYERR                                                            
*                                                                               
PF26     LA    R3,SCRERAS                                                       
         L     R2,ADETH                                                         
         B     MYERR                                                            
*                                                                               
PFR      MVC   FVMSGNO,=AL2(AE$IVPFK)                                           
         L     R2,ADETH            XMIT FIRST DETAIL FIELD BACK                 
         OI    6(R2),X'80'+X'01'   AS MODIFIED TO GET INPUT THIS TIME           
         L     R2,TIACURS          ON NEXT ENTRY                                
         B     ERRXIT                                                           
*                                                                               
PFX      B     EXIT                                                             
         SPACE 2                                                                
* SUB-ROUTINE TO ERASE THE LOWER SCREEN                                         
*                                                                               
ERASE    NTR1  ,                                                                
         LA    R2,ATRCLIH                                                       
         LA    R6,2                                                             
         CLI   TWASCRN,X'E1'                                                    
         BE    *+12                                                             
         LA    R2,ATSCLIH                                                       
         LA    R6,3                                                             
*                                                                               
ERASE2   BAS   RE,SETLIN                                                        
         MVC   FLD,BCSPACES                                                     
         LA    R3,ACLIH                                                         
         LA    R0,NDATAFLD-1                                                    
*                                                                               
ERASE4   L     R2,0(R3)                                                         
         BAS   RE,MOVEFLD                                                       
         OI    6(R2),X'80'                                                      
         LA    R3,4(R3)                                                         
         BCT   R0,ERASE4                                                        
*                                                                               
         L     R2,ANEXT                                                         
         BCT   R6,ERASE2                                                        
*                                                                               
ERASEX   B     CURSIT                                                           
         EJECT                                                                  
*                                                                               
*-----------------------------------------------------------------              
* ROUTINE TO MANAGE EDIT                                                        
*-----------------------------------------------------------------              
*                                                                               
EDT      NTR1  ,                                                                
         LA    R6,2                R6=N'DETAIL LINES ON SCREEN                  
         CLI   TWASCRN,X'E1'       TEST FOR FIRST SCREEN                        
         BE    *+8                                                              
         LA    R6,3                                                             
*                                                                               
         L     R2,ADETH            R2=A(FIRST ITEM FIELD)                       
*                                                                               
* FIRST CHECK FOR ZERO LINE, IF YES-EDIT IS DONE                                
*                                                                               
EDT2     BAS   RE,SETLIN           SET FIELD ADDRESSES                          
         LA    R3,ACLIH            R3=A(FIELD POINTER)                          
         LA    R5,NDATAFLD-1       R5=N'FIELDS IN ITEM                          
*                                                                               
EDT4     L     R2,0(R3)            R2=A(FIELD)                                  
         TM    1(R2),X'20'         TEST FOR PROTECTED FIELD                     
         BO    *+12                YES                                          
         CLI   5(R2),0             TEST FOR EMPTY FIELD                         
         BNE   EDT6                NO-EDIT THE FIELD                            
*                                                                               
EDT5     LA    R3,4(R3)            NEXT FIELD POINTER                           
         BCT   R5,EDT4                                                          
         B     EDTX                ZERO LINE--EDIT IS DONE                      
*                                                                               
* FOUND A NON-ZERO FIELD--CHECK THAT LINE HAS NO CHANGED OR                     
* OVERTYPED FIELDS                                                              
*                                                                               
EDT6     LA    R3,ACLIH            RESET TO FIRST FIELD POINTER                 
         LA    R5,NDATAFLD-1       RESET COUNTER                                
*                                                                               
EDT7     L     R2,0(R3)                                                         
         TM    1(R2),X'20'                                                      
         BO    EDT8                                                             
         TM    4(R2),X'20'         TEST FOR CHANGED FIELD                       
         BZ    EDT9                YES-EDIT THIS LINE                           
*                                                                               
         TM    4(R2),X'80'         TEST IF INPUT THIS TIME                      
         BZ    EDT8                NO                                           
         CLI   5(R2),0             TEST IF INPUT IN FIELD                       
         BNE   EDT9                YES-FIELD WAS OVERTYPED                      
*                                                                               
EDT8     LA    R3,4(R3)                                                         
         BCT   R5,EDT7                                                          
         B     EDT10               SET FOR NEXT LINE                            
*                                                                               
EDT9     BAS   RE,PROC             EDIT LINE                                    
*                                                                               
EDT10    L     R2,ANEXT                                                         
         BCT   R6,EDT2                                                          
*                                                                               
EDTX     B     CURSIT                                                           
         EJECT                                                                  
*                                                                               
*---------------------------------------------------------------------          
* PROCESS A DETAIL LINE--AT ENTRY, LINE ADCONS HAVE BEEN SET BY SETLIN          
*---------------------------------------------------------------------          
*                                                                               
PROC     NTR1  ,                                                                
         LA    RE,DETVALS          CLEAR DETAIL VALUES                          
         LA    RF,DETVALNQ                                                      
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         ZAP   INAMNT,=P'0'        INIT AMOUNT FIELDS                           
         ZAP   CDAMNT,=P'0'                                                     
*                                                                               
         MVC   FLD,BCSPACES        CLEAR NAME FIELDS                            
         L     R2,ANAM1H                                                        
         BAS   RE,MOVEFLD                                                       
         OI    6(R2),X'80'         XMIT THEM BACK                               
         L     R2,AWCNH                                                         
         BAS   RE,MOVEFLD                                                       
         OI    6(R2),X'80'                                                      
         L     R2,ANAM2H                                                        
         BAS   RE,MOVEFLD                                                       
         OI    6(R2),X'80'                                                      
*                                                                               
*                                                                               
PROC4    L     R2,AWCH             R2=A(COMMISSIONABLE FIELD)                   
         CLI   5(R2),0             TEST FOR NO INPUT                            
         BNE   PROC6                                                            
         L     R2,ANWCH            R2=A(NON-COMM FIELD)                         
         CLI   5(R2),0             TEST FOR INPUT                               
         BE    PROC10              NO-THIS IS AN EXPENSE ITEM                   
*                                                                               
PROC6    MVI   TYPE,C'P'           NOTE PRODUCTION ITEM                         
         LM    R2,R3,AWCH          R2=A(COMM WC), R3=A(NON-COMM WC)             
         CLI   5(R2),0             TEST FOR INPUT IN BOTH                       
         BE    PROC7               NOTHING IN FIRST                             
         CLI   5(R3),0                                                          
         BE    PROC7                                                            
         MVC   FVMSGNO,=AL2(AE$MEWC)                                            
         B     ERRXIT                                                           
*                                                                               
PROC7    CLC   8(2,R2),=C'99'      W/C 99 NOT ALLOWED                           
         BE    *+18                                                             
         CLC   8(2,R3),=C'99'      W/C 99 NOT ALLOWED                           
         BNE   PROC8                                                            
         LA    R2,0(R3)            SET CURSOR FOR ERROR POSITION                
         MVC   FVMSGNO,=AL2(AE$WC99)                                            
         B     ERRXIT                                                           
*                                                                               
PROC8    CLI   5(R2),0             TEST FOR COMMISSIONABLE INPUT                
         BE    *+10                                                             
         MVI   COMSW,C'Y'                                                       
         LR    R3,R2               R3=A(INPUT WC FIELD)                         
*                                                                               
         L     R2,ACLIH                                                         
         CLI   TWASCRN,X'EA'       TEST FOR SECOND SCREEN                       
         BE    *+8                                                              
         LA    R2,ATRACCH                                                       
         CLI   SVCRACCT,0          IS THERE AN PROD VENDOR?                     
         BNE   PROC9               YES                                          
         TM    COMPSTA2,X'04'      IS PROD VEND REQUIRED?                       
         BO    *+12                YES - FLAG ERROR                             
         CLI   SVCSHNUM,0          IS THERE A CASH ACCT?                        
         BNE   PROC9               YES                                          
         MVC   FVMSGNO,=AL2(AE$VMEXP)   NEED PROD VENDOR TRAP ERROR             
         B     ERRXIT                                                           
*                                                                               
PROC9    ST    R3,SAVEWC           A(WORK CODE INPUT)                           
         BAS   RE,PRODET           YES, EDIT LINE FOR ADV. EXP.                 
         BNZ   ERRXIT                                                           
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BE    PROC20              GOOD RETURN SET PVALS FOR LINE               
         L     R2,FVADDR                                                        
         B     ERRXIT                                                           
*                                                                               
PROC10   L     R2,ACLIH                                                         
         CLI   TWASCRN,X'EA'       TEST FOR SECOND SCREEN                       
         BE    *+8                                                              
         LA    R2,ATRACC2H                                                      
         CLI   SVCRACC2,0          IS THERE AN EXP VENDOR?                      
         BNE   PROC15              YES                                          
         TM    COMPSTA2,X'04'      IS EXP VENDOR REQUIRED?                      
         BNZ   *+12                YES - FLAG ERROR                             
         CLI   SVCSHNUM,0          IS THERE A CASH ACCT                         
         BNE   PROC15              YES                                          
*                                                                               
PROC12   MVC   FVMSGNO,=AL2(AE$VMEXP)                                           
         B     ERRXIT                                                           
*                                                                               
PROC15   MVI   TYPE,C'E'                                                        
         BAS   RE,EXPDET           EDIT LINE FOR AGY. EXP.                      
         BNZ   ERRXIT                                                           
*                                                                               
PROC20   LA    R3,ACLIH            R3=A(FIELD POINTER)                          
         LA    R0,NDATAFLD-1       R0=LOOP COUNTER                              
*                                                                               
PROC22   L     R2,0(R3)                                                         
         TM    1(R2),X'20'                                                      
         BO    *+12                                                             
         OI    4(R2),X'20'                                                      
         OI    6(R2),X'80'                                                      
         LA    R3,4(R3)                                                         
         BCT   R0,PROC22                                                        
*                                                                               
PROCX    B     CURSIT                                                           
         EJECT                                                                  
*--------------------------------------------------------------                 
*        EDIT VENDOR FIELD--AT ENTRY, R2=A(VENDOR FIELD)                        
*        TYPE=P FOR PRODUCTION VENDOR, TYPE=E FOR EXPENSE VENDOR                
*--------------------------------------------------------------                 
*                                                                               
VEN      NTR1  ,                                                                
         ZIC   R3,5(R2)           =LEN OF CREDIT ACCOUNT INPUT                  
         BCTR  R3,0               MINUS 1 FOR EX INSTRUCTION                    
         LA    R1,8(R2)                                                         
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY     FROM TDA                                      
         CLI   8(R2),C'*'         UNIT LEDGER INPUT OVERRIDE                    
         BE    VEN4               YES                                           
         LA    RF,COMPEL                                                        
         USING ACCOMPD,RF                                                       
         CLI   TYPE,C'P'          IS IT PROD VENDOR?                            
         BNE   VEN2               NO                                            
         MVC   KEY+1(2),ACMPSUPP  ASSUME UNLESS OVERRIDDEN                      
         B     VEN12                                                            
*                                                                               
VEN2     MVC   KEY+1(1),=C'S'                                                   
         MVC   KEY+2(1),ACMPSUPX  ASSUME UNLESS OVERRIDDEN                      
         B     VEN12                                                            
         DROP  RF                                                               
*                                                                               
VEN4     CLI   SVCSHNUM,0         IS THERE A CASH ACCOUNT?                      
         BNE   VEN6               YES - *SC NOT ALLOWED                         
         CLC   9(2,R2),=C'SC'     IS IT CASH ACCT?                              
         BE    VEN10                                                            
*                                                                               
VEN6     MVC   FVMSGNO,=AL2(AE$INACP)                                           
         LA    RF,ADVCLIST        LIST OF OVERRIDES FOR PROD VEND               
         CLI   TYPE,C'P'          IS IT PRODUCTION VENDOR?                      
         BE    VEN8               YES                                           
         LA    RF,AGYCLIST        LIST OF OVERRIDES FOR EXP VEND                
*                                                                               
VEN8     CLI   0(RF),X'FF'        END OF LIST                                   
         BE    ERRXIT                                                           
         CLC   9(2,R2),0(RF)      INPUT TO LIST                                 
         BE    VEN10                                                            
         LA    RF,2(RF)           NEXT VALID ENTRY TO CREDIT                    
         B     VEN8                                                             
*                                                                               
VEN10    MVC   KEY+1(2),9(R2)     2 CHAR U/L FROM INPUT                         
         SH    R3,=H'3'           SUBTRACT *U/L FROM INPUT LEN                  
         LA    R1,11(R2)          POINT TO ACCT INPUT                           
VEN12    EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),0(R1)     CREDIT ACCT TO KEY                            
*                                                                               
VEN14    CLI   TYPE,C'P'          IS THIS A PROD VENDOR?                        
         BNE   VEN16              NO                                            
         LA    R1,ATRCDH          CD FOR PROD VENDOR                            
         B     *+8                                                              
*                                                                               
VEN16    LA    R1,ATRCD2H         CD FOR EXP VENDOR                             
         CLI   5(R1),0            INPUT                                         
         BE    VEN22              NO                                            
         CLI   SVCSHNUM,0         IS THERE A CASH ACCOUNT?                      
         BE    VEN20              NO                                            
*                                                                               
VEN18    MVC   FVMSGNO,=AL2(AE$CDALL)     NO CASH DISCOUNT ALLOWED              
         B     ERRXIT             ERROR                                         
*                                                                               
VEN20    CLI   8(R1),C'N'         IS THERE CASH DISCOUNT                        
         BE    VEN22              NO GO GET ACCT                                
         CLI   8(R1),C'Y'                                                       
         BE    VEN22                                                            
         LR    R2,R1                                                            
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     ERRXIT                                                           
*                                                                               
VEN22    MVC   RKEY,KEY           READ VENDOR FOR CD AND OTHER INFO             
         GOTO1 ARDHI,AIOAREA1                                                   
         L     RE,AIOAREA1                                                      
         CLC   KEYSAVE,0(RE)      WAS REC RETRIEVED                             
         BE    VEN24                                                            
         MVC   FVMSGNO,=AL2(AE$INACC)                                           
         B     ERRXIT                                                           
*                                                                               
VEN24    LA    R0,KEY              MOVE RECORD TO LOCAL STORAGE                 
         LA    R1,1000                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         BAS   RE,GETVEN           GET VENDOR DATA                              
*                                                                               
VEN26    SR    R6,R6              CLEAR FOR GETACC USE                          
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         GOTO1 AGETACC,DMCB,KEY,(R6) DONE MYSELF TO HANDLE DFLT                 
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   ERRXIT                                                           
*                                                                               
VEN28    MVC   FVMSGNO,=AL2(AE$INACP)                                           
         TM    ACCTSTAT,X'80'     BALANCE ELEMENT                               
         BZ    ERRXIT                                                           
         TM    ACCTSTAT,X'30'     LOCKED OR CLOSED                              
         BNZ   ERRXIT                                                           
*                                                                               
* PRODUCTION VENDOR FINISH                                                      
*                                                                               
VEN30    CLI   TYPE,C'P'          IS THIS A PROD VENDOR?                        
         BNE   VEN36              NO                                            
         CLI   ATRACCH+5,0        WAS PROD VENDOR INPUT?                        
         BE    VENX               NO                                            
         MVC   SVCRACCT,ACCTNUM   SAVE PROD VEN KEY                             
         MVC   SVACCTNM,ACCTNAME  SAVE PROD VEN NAME                            
         MVC   PRSTAT,ACCTSTAT    PRO 2C BIT(X'04')                             
*                                                                               
VEN32    DS    0H                                                               
         CLI   SVCSHNUM,0         IS THERE A CASH ACCT?                         
         BE    VEN34              NO - DON'T WANT 2C OR 27                      
         TM    PRSTAT,X'04'       DO WE NEED 2C FOR PROD VENDOR?                
         BZ    VEN34              NO.                                           
         MVC   KEY(15),SVCRACCT   ACCT #                                        
         MVC   KEY+1(2),=C'2C'    U/L                                           
         BAS   RE,GETACC                                                        
         MVI   PCONSULT,C'Y'      SET 2C SWITCH                                 
         MVC   P2CNUM,ACCTNUM                                                   
         MVC   P2CNAM,ACCTNAME                                                  
         MVC   KEY+1(14),=CL14'27999'  BUILD 27 ACCT                            
         BAS   RE,GETACC                                                        
         MVC   PROTROL,ACCTNUM                                                  
         MVC   PROTROLN,ACCTNAME                                                
*                                                                               
VEN34    LA    R3,ATRACDH                                                       
         OI    6(R3),X'80'        SET TRANSBIT                                  
         MVC   8(36,R3),SVACCTNM  PRODUCTION NAME                               
         B     VENX                                                             
         EJECT                                                                  
*                                                                               
* EXPENSE VENDOR FINISH                                                         
*                                                                               
VEN36    CLI   ATRACC2H+5,0       WAS EXP VENDOR INPUT?                         
         BE    VENX                                                             
         MVC   SVCRACC2,ACCTNUM   EXP VENDOR ACCT #                             
         MVC   SVACCTN2,ACCTNAME  EXP VENDOR NAME                               
         MVC   EXSTAT,ACCTSTAT    EXP 2C BIT (X'04')                            
         MVI   V29SW,0                                                          
         MVC   KEYSAVE(15),KEY                                                  
         MVC   KEY(15),SVCRACC2   EXP VENDOR.                                   
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'ACCOUNT',KEY,KEY .                
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R1,IOAREA                                                        
         SR    RE,RE                                                            
*                                                                               
VEN38    CLI   0(R1),0            IS IT END OF RECORD?                          
         BE    VEN44                                                            
         CLI   0(R1),ACSTELQ      IS IT X'30'?                                  
         BE    VEN40                                                            
         IC    RE,1(R1)                                                         
         AR    R1,RE                                                            
         B     VEN38                                                            
*                                                                               
         USING ACSTATD,R1         STATUS DSECT                                  
VEN40    TM    ACSTSTX,X'80'      DO WE NEED VENDOR AS CONTRA?                  
         BZ    VEN44              NO.                                           
VEN42    MVI   V29SW,C'Y'         YES.                                          
*                                                                               
VEN44    MVC   KEY(15),KEYSAVE    SAVE KEY                                      
*                                                                               
VEN46    CLI   SVCSHNUM,0         IS THERE A CASH ACCT?                         
         BE    VEN48              NO - DON'T WANT 2C OR 27                      
         TM    EXSTAT,X'04'       DO WE NEED 2C FOR EXP VENDOR?                 
         BZ    VEN48              NO.                                           
*                                                                               
         MVC   KEY(15),SVCRACC2                                                 
         MVC   KEY+1(2),=C'2C'                                                  
         BAS   RE,GETACC                                                        
         MVI   ECONSULT,C'Y'                                                    
         MVC   E2CNUM,ACCTNUM                                                   
         MVC   E2CNAM,ACCTNAME                                                  
         MVC   KEY+1(14),=CL14'27999'   BUILD 27                                
         BAS   RE,GETACC          GET ACCT                                      
         MVC   EXPTROL,ACCTNUM                                                  
         MVC   EXPTROLN,ACCTNAME                                                
*                                                                               
VEN48    DS    0H                                                               
         LA    R3,ATRAC2DH                                                      
         OI    6(R3),X'80'         SET TRANS                                    
         MVC   8(36,R3),SVACCTN2   DISPLAY EXP VEND EXPANSION                   
*                                                                               
VENX     B     CURSIT                                                           
         EJECT                                                                  
*                                                                               
*--------------------------------------------------------------                 
*        GET VENDOR RECORD DATA--AT ENTRY, IOAREA CONTAINS VENDOR               
*        AND TYPE=P IF VENDOR IS PRODUCTION                                     
*--------------------------------------------------------------                 
*                                                                               
GETVEN   NTR1  ,                                                                
         LA    R4,IOAREA          FIND DISCOUNT ELEMENT                         
*                                                                               
GETVEN2  CLI   0(R4),0                                                          
         BNE   GETVEN6            DIDN'T FIND DISCOUNT ELEMENT                  
         CLI   TYPE,C'P'          IS THIS A PROD VENDOR?                        
         BNE   GETVEN4            NO                                            
         OI    ATRCDH+6,X'80'     PROD VENDOR CD                                
         MVI   ATRCD,C' '                                                       
         B     GETVENX            SKIP CD FOR PROD VENDOR                       
*                                                                               
GETVEN4  OI    ATRCD2H+6,X'80'    EXP VENDOR CD                                 
         MVI   ATRCD2,C' '                                                      
         B     GETVENX            SKIP CD FOR EXP VENDOR                        
*                                                                               
GETVEN6  CLI   0(R4),RATEDSCQ     IS THIS A DIS ELEM?                           
         BE    GETVEN10           YES                                           
         CLI   0(R4),ITCELQ        TEST FOR INPUT TAX DEFAULT                   
         BE    GETVEN16                                                         
GETVEN8  ZIC   R3,1(R4)           LEN OF CURRENT EL                             
         AR    R4,R3              TO NEXT EL                                    
         B     GETVEN2                                                          
*                                                                               
GETVEN10 CLI   SVCSHNUM,0         TEST FOR CASH ACCOUNT                         
         BNE   GETVEN8             YES-NO CASH DISCOUNT ALLOWED                 
         LA    RE,ATRCDH           SET POINTER TO CD FIELD                      
         CLI   TYPE,C'P'           TEST PROD OR EXP VENDOR                      
         BE    *+8                                                              
         LA    RE,ATRCD2H                                                       
         CLI   8(RE),C'N'          TEST CD SUPPRESSED                           
         BE    GETVEN8             YES                                          
*                                                                               
         MVC   HALF,2(R4)         PLACE CD IN SVDISC SAVE FLD                   
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         CLI   TYPE,C'P'          IS THIS A PROD VENDOR?                        
         BNE   GETVEN12           NO                                            
         ZAP   SVDISC,DUB         CD VALUE FOR PROD VENDOR                      
         LA    R5,SVDISC                                                        
         LA    R1,ATRCDH                                                        
         LA    R6,ATRCDD                                                        
         B     GETVEN14           OUTPUT CD                                     
*                                                                               
GETVEN12 ZAP   SVDISC2,DUB        CD VALUE FOR EXP VENDOR                       
         LA    R5,SVDISC2                                                       
         LA    R1,ATRCD2H                                                       
         LA    R6,ATRCDD2                                                       
*                                                                               
GETVEN14 OI    6(R1),X'80'                                                      
         MVI   8(R1),C'Y'                                                       
         EDIT  (P3,(R5)),(6,(R6)),2                                             
         B     GETVEN8                                                          
*                                                                               
         USING ITCELD,R4                                                        
GETVEN16 LA    RE,SVPVTAXT         SET POINTER TO DEFAULT TAX                   
         CLI   TYPE,C'P'           TEST FOR PRODUCTION VENDOR                   
         BE    *+8                 YES                                          
         LA    RE,SVEVTAXT                                                      
         CLI   0(RE),0             TEST IF WE ALREADY HAVE DEFAULT              
         BNE   GETVEN8             YES-NEXT ELEMENT                             
         CLC   SVDATE1,ITCEFFD     TEST TRANS DATE >= EFF DATE                  
         BL    GETVEN8                                                          
         MVC   0(1,RE),ITCTYPE     SET DEFAULT TYPE                             
         B     GETVEN8                                                          
*                                                                               
GETVENX  B     CURSIT                                                           
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*--------------------------------------------------------------                 
*        EDIT DOCUMENT NUMBER FIELD--USER CAN INPUT TWO NUMBERS                 
*--------------------------------------------------------------                 
*                                                                               
EDDOC    NTR1  ,                                                                
         MVI   FVMINL,1            REQUIRE SOME INPUT                           
         LA    R2,ATRDOCH                                                       
         BAS   RE,TSTKEY                                                        
         GOTO1 AFVAL,(R2)                                                       
         BNE   ERRXIT                                                           
*                                                                               
         MVC   SVREF,BCSPACES      CLEAR REFERENCE NUMBER                       
         GOTO1 SCANNER,DMCB,FVIHDR,(2,WORK),0                                   
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         CLI   4(R1),0             TEST FOR INVALID SYNTAX                      
         BE    ERRXIT                                                           
*                                                                               
         CLI   WORK+0,6            NO MORE THAN 6 CHARACTERS                    
         BH    ERRXIT                                                           
         MVC   SVDOCLEN,WORK+0                                                  
         MVC   SVDOC,WORK+12                                                    
         CLC   SVDOC,BCSPACES      TEST FOR EMPTY FIELD                         
         BE    ERRXIT                                                           
*                                                                               
         CLI   4(R1),2             TEST FOR TWO COMPONENTS                      
         BNE   EDDOCX                                                           
*                                                                               
         CLI   WORK+32+0,6                                                      
         BH    ERRXIT                                                           
         MVC   SVREF,WORK+32+12                                                 
         CLC   SVREF,BCSPACES                                                   
         BE    ERRXIT                                                           
*                                                                               
EDDOCX   B     CURSIT                                                           
         EJECT                                                                  
*                                                                               
*--------------------------------------------------------------                 
*        EDIT TRANSACTION DATE FIELD--ON EXIT, R2=A(FIELD) AND                  
*        CC=EQ IF OK, NEQ IF ERRXIT SVDATE1 CONTAINS DATE                       
*--------------------------------------------------------------                 
*                                                                               
EDDAT    NTR1  ,                                                                
         MVC   FVMSGNO,=AL2(AE$INDAT)                                           
         LA    R2,ATRDAT1H         DATE FLD                                     
         BAS   RE,TSTKEY                                                        
         LA    R3,SVDATE1                                                       
         CLI   5(R2),0                                                          
         BNE   EDDAT2                                                           
         BAS   RE,GETODAY          DEFAULT TO CURRENT DAY                       
         B     EDDAT4                                                           
*                                                                               
EDDAT2   GOTO1 DATVAL,DMCB,(0,8(R2)),WORK  EDIT DATE INPUT                      
         OC    DMCB(4),DMCB                                                     
         BZ    EDDATR                                                           
*                                                                               
EDDAT4   GOTO1 DATCON,DMCB,(0,WORK),(1,(R3)) PACK IT Y/M/D                      
         CLI   0(R3),X'70'                                                      
         BL    EDDATR                                                           
         GOTO1 DATECHK,DMCB,(R3)                                                
         CLI   DMCB,X'FF'                                                       
         BE    EDDATR                                                           
         CLI   5(R2),0             WAS THERE INPUT                              
         BNE   EDDATY              YES                                          
         OI    6(R2),X'80'         SET TRANS                                    
         GOTO1 DATCON,DMCB,(1,(R3)),(8,8(R2)) DISPLAY DFLT DATE                 
*                                                                               
EDDATY   CR    RB,RB               SET CC=EQ                                    
         B     EDDATX                                                           
*                                                                               
EDDATR   CLI   *,0                 SET CC=NEQ                                   
*                                                                               
EDDATX   XIT1  REGS=(R2)                                                        
         EJECT                                                                  
*                                                                               
*--------------------------------------------------------------                 
*        EDIT CASH ACCOUNT FIELD--SETS SVCSHNUM=CASH ACCOUNT                    
*        CODE AND SVCSHNAM=ACCOUNT NAME--EXIT DIRECTLY TO ERROR                 
*--------------------------------------------------------------                 
*                                                                               
EDCASH   NTR1  ,                                                                
         XC    SVCSHNUM(51),SVCSHNUM  VALIDATE CASH ACCT                        
         LA    R2,ATRCSHH                                                       
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0             TEST FOR INPUT                               
         BNE   EDCASH2             YES                                          
*                                                                               
         MVC   FLD,BCSPACES                                                     
         LA    R2,ATRCSDH          CLEAR DESCRIPTION FIELD                      
         BAS   RE,MOVEFLD                                                       
         OI    6(R2),X'80'                                                      
         B     EDCASHX                                                          
*                                                                               
EDCASH2  ZIC   R3,5(R2)           LENGTH OF CASH ACCT                           
         BCTR  R3,0                                                             
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),=C'SC'    CASH U/L                                      
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),8(R2)     CASH ACCT CODE                                
         SR    R6,R6                                                            
         BAS   RE,GETACC                                                        
         MVC   FVMSGNO,=AL2(AE$INACP)                                           
         BAS   RE,CHECKACC        VERIFY ACCT                                   
*                                                                               
EDCASH4  MVC   SVCSHNUM,ACCTNUM   CASH ACCT #                                   
         MVC   SVCSHNAM,ACCTNAME  CASH ACCT NAME                                
         LA    R3,ATRCSDH                                                       
         OI    6(R3),X'80'                                                      
         MVC   8(36,R3),ACCTNAME  DISPLAY CASH ACCT EXPANSION                   
*                                                                               
EDCASH6  LA    R2,ATRCDH                                                        
         CLI   5(R2),0            IS THERE A PROD CD?                           
         BNE   EDCASH8            YES - FLAG ERROR                              
*                                                                               
         LA    R2,ATRCD2H                                                       
         CLI   5(R2),0            IS THERE AN EXP CD?                           
         BE    EDCASHX            YES - FLAG ERROR                              
*                                                                               
EDCASH8  MVC   FVMSGNO,=AL2(AE$CDALL)   NO CASH DISCOUNT ALLOWED                
         B     ERRXIT             ERROR                                         
*                                                                               
EDCASHX  B     CURSIT                                                           
         EJECT                                                                  
*-------------------------------------------------------------                  
*        CONTROL LOGIC FOR AN PRODUCTION DETAIL ITEM                            
*        AT ENTRY, SAVEWC=A(WORKCODE FIELD TO EDIT)                             
*-------------------------------------------------------------                  
*                                                                               
PRODET   NTR1  ,                                                                
         L     R2,ACLIH                                                         
         TM    4(R2),X'20'         TEST IF FIELD CHANGED                        
         BO    PRODET1                                                          
*                                                                               
         BAS   RE,CLEARX           CLEAR X-JOB RELATED FIELDS                   
         L     R1,APROH                                                         
         NI    4(R1),X'FF'-X'20'   TURN OFF PREVIOUS VALID                      
         L     R1,AJOBH                                                         
         NI    4(R1),X'FF'-X'20'                                                
*                                                                               
PRODET1  CLI   8(R2),C'*'          TEST FOR DUPLICATION                         
         BNE   *+8                                                              
         BAS   RE,DUP                                                           
         BAS   RE,ANY              REQUIRE INPUT                                
         BAS   RE,VALCLI                                                        
         OI    4(R2),X'20'         NOTE AS VALID NOW FOR CLEARX                 
*                                                                               
PRODET2  L     R2,APROH                                                         
         TM    4(R2),X'20'         TEST IF FIELD CHANGED                        
         BO    *+16                                                             
         BAS   RE,CLEARX                                                        
         L     R1,AJOBH                                                         
         NI    4(R1),X'FF'-X'20'                                                
*                                                                               
         CLI   8(R2),C'*'                                                       
         BNE   *+8                                                              
         BAS   RE,DUP                                                           
         BAS   RE,ANY                                                           
         BAS   RE,VALPRO                                                        
         OI    4(R2),X'20'                                                      
*                                                                               
PRODET4  L     R2,AJOBH                                                         
         TM    4(R2),X'20'                                                      
         BO    *+8                                                              
         BAS   RE,CLEARX                                                        
*                                                                               
         CLI   8(R2),C'*'                                                       
         BNE   *+8                                                              
         BAS   RE,DUP                                                           
         BAS   RE,VALJOB                                                        
         OI    4(R2),X'20'                                                      
         GOTO1 ASETJOB,DMCB,SJJOBNUM                                            
         ORG   *-2                                                              
         L     RE,SAVEWC           PASS WORKCODE UNLESS IT'S *                  
         LA    RE,8(RE)                                                         
         CLI   0(RE),C'*'                                                       
         BE    *+12                                                             
         ST    RE,4(R1)                                                         
         MVI   0(R1),X'80'                                                      
         BASR  RE,RF                                                            
*                                                                               
         GOTO1 AOPTVAL                                                          
         BNE   ERRXIT              ERROR                                        
*                                                                               
PRODET6  L     R2,SAVEWC           =A(CURRENT WORK CODE INPUT)                  
         CLI   8(R2),C'*'          REPEAT FEATURE                               
         BNE   *+8                                                              
         BAS   RE,DUP                                                           
         BAS   RE,VALWC                                                         
*                                                                               
         L     R2,ACOFFH           CREDIT OFFICE                                
         CLI   8(R2),C'*'                                                       
         BNE   *+8                                                              
         BAS   RE,DUP                                                           
         BAS   RE,COFF                                                          
*                                                                               
PRODET8  L     R2,AAMTH                                                         
         CLI   8(R2),C'*'                                                       
         BNE   *+8                                                              
         BAS   RE,DUP                                                           
         BAS   RE,EDAMT                                                         
*                                                                               
PRODET10 BAS   RE,XJOB             X-JOB FIELDS EDIT                            
*                                                                               
         CLI   CHECK,C' '          CHECKING AMOUNT ?                            
         BE    PRODET12            NO                                           
         MVC   FVMSGNO,=AL2(AE$AEEWC)                                           
         GOTO1 AWRKVAL,DMCB,SVWRKCD                                             
         BH    ERRXIT                                                           
*                                                                               
PRODET12 CP    SVDISC,=P'0'        SAVED DISCOUNT                               
         BE    PRODET14                                                         
         ZAP   DUB,INAMNT          CALCULATE CD                                 
         MP    DUB,SVDISC                                                       
         SRP   DUB,64-4,5          ROUNDED DIVIDE BY 10,000                     
         ZAP   CDAMNT,DUB                                                       
*                                                                               
PRODET14 ZAP   DUB,INAMNT                                                       
         SP    DUB,CDAMNT                                                       
         CP    SVDISC,=P'0'        IS THERE CASH SVDISC                         
         BE    PRODET20            NO                                           
         CLI   SVCSHNUM,0          IS THERE A CASH ACCT?                        
         BNE   PRODET20            YES - NO CD ALLOWED                          
         CLI   PASSCD,C'N'         PASSING CD TO CLIENT                         
         BNE   PRODET20            YES - REDUCE PAYABLE                         
*                                                                               
         BAS   RE,VALCD            FIND/VALIDATE CASH DISCOUNT A/C              
*                                                                               
PRODET20 GOTO1 =A(PPOST),DMCB,(RC),RR=RELOA                                     
         BE    *+12                OK                                           
         L     R2,FVADDR                                                        
         B     ERRXIT                                                           
*                                                                               
         BAS   RE,NAMONE           DISPLAY FIRST NAME FIELD                     
         BAS   RE,NAMTWO           AND DO SECOND FIELD                          
         SR    R0,R0               SET GOOD CC                                  
*                                                                               
PRODETX  B     CURSIT                                                           
*                                                                               
*                                                                               
DLIST    DS    0H U/L'S VALID TO DEBIT                                          
         DC    C'SASBSFSLSC'       GIDEON - CASH OK                             
         DC    X'FF'                                                            
*                                                                               
ADVCLIST DS    0H    PRD. U/L'S VALID TO CREDIT                                 
         DC    C'SXSWSYSBSA'       SPACE OK-CASH NOT ALLOWED                    
         DC    X'FF'                                                            
*                                                                               
AGYCLIST DS    0H    EXP. U/L'S VALID TO CREDIT                                 
         DC    C'SVSWSXSYSBSASFSL' SPACE OK-CASH NOT ALLOWED                    
         DC    X'FF'                                                            
         EJECT                                                                  
*-------------------------------------------------------------                  
*        CONTROL LOGIC FOR AN EXPENSE DETAIL ITEM                               
*-------------------------------------------------------------                  
*                                                                               
EXPDET   NTR1  ,                                                                
         XC    CLIPROF,CLIPROF                                                  
         XC    PRODPROF,PRODPROF                                                
         XC    JOBPROF,JOBPROF                                                  
         BAS   RE,EDEXP                                                         
*                                                                               
         CLI   STFSW,C'Y'          TEST FOR STAFF ANALYSIS                      
         BNE   *+8                 NO                                           
         BAS   RE,GETSTF           GET THE STAFF LEDGER                         
*                                                                               
         L     R2,AFOFFH                                                        
         CLI   8(R2),C'*'          TEST FOR REPEAT FEATURE                      
         BNE   *+8                                                              
         BAS   RE,DUP                                                           
*                                                                               
         CLC   8(2,R2),=C'++'      SUPPLY THE OFFICE?                           
         BNE   EXPD12              NO                                           
         TM    BCCPYST5,CPYSNCST   YES, ON NEW COST?                            
         BO    EXPD02              YES                                          
*                                                                               
         MVI   COSTSW,C'N'                                                      
         TM    BCCPYST1,CPYSCOST   TEST COST ACCOUNTING                         
         BZ    EXPD04                                                           
         CLI   COSTANAL,C' '                                                    
         BE    EXPD04                                                           
         MVI   COSTSW,C'Y'                                                      
         B     EXPD04                                                           
*                                                                               
EXPD02   L     R1,ACATD                                                         
         USING CATD,R1                                                          
         XC    CATD(CATLNQ),CATD                                                
         MVC   CATDMGR,DATAMGR                                                  
         MVC   CATSEAC,POSTACC                                                  
         MVC   CATDPT,DEPT                                                      
         GOTO1 VCATCALL                                                         
         MVC   COSTSW,CATPST                                                    
*                                                                               
EXPD04   CLI   COSTSW,C'Y'         TEST FOR ANALYZED BY CLIENT                  
         BE    EXPD06              YES, ++  OK                                  
         MVC   FVMSGNO,=AL2(AE$NAWTT)                                           
         B     ERRXIT                                                           
*                                                                               
EXPD06   L     R2,ACLIH            IF ++ VALID, WE NEED THE CLIENT              
         CLI   5(R2),0                                                          
         BE    EXPD08                                                           
         BAS   RE,VALCLI                                                        
         OI    4(R2),X'20'                                                      
*                                                                               
         L     R2,APROH            AND THE PRODUCT                              
         CLI   5(R2),0                                                          
         BE    EXPD08                                                           
         BAS   RE,VALPRO                                                        
         OI    4(R2),X'20'                                                      
         B     EXPD10                                                           
*                                                                               
EXPD08   MVC   FVMSGNO,=AL2(AE$MISIF)                                           
         B     ERRXIT                                                           
*                                                                               
EXPD10   BAS   RE,PROFMERG                                                      
         LA    R4,PROFILE                                                       
         USING ACPROFD,R4                                                       
         MVC   PRDOFF,ACPROFFC     GET CLIENT/PRODUCT OFFICE CODE               
         L     R2,AFOFFH           AND MOVE TO SCREEN                           
         MVC   FLD,PRDOFF                                                       
         BAS   RE,MOVEFLD                                                       
         OI    6(R2),X'80'                                                      
         MVC   FLD,PRDOFF                                                       
         BAS   RE,MOVEFLD                                                       
*                                                                               
EXPD12   BAS   RE,FOFF                                                          
*                                                                               
         L     R2,AFOFFH                                                        
         CLI   8(R2),C'*'          TEST FOR REPEAT FEATURE                      
         BNE   *+8                                                              
         BAS   RE,DUP                                                           
         BAS   RE,AOFF                                                          
*                                                                               
         L     R2,ACOFFH                                                        
         CLI   8(R2),C'*'                                                       
         BNE   *+8                                                              
         BAS   RE,DUP                                                           
         BAS   RE,COFF                                                          
*                                                                               
         L     R2,ADEPTH                                                        
         CLI   8(R2),C'*'                                                       
         BNE   *+8                                                              
         BAS   RE,DUP                                                           
         BAS   RE,DEP              EDIT DEPARTMENT/VALIDATE ACCOUNTS            
*                                                                               
         TM    BCCPYST5,CPYSNCST   TEST NEW COST ACCOUNTING                     
         BO    EXPD14              YES                                          
*                                                                               
         MVI   COSTSW,C'N'                                                      
         TM    BCCPYST1,CPYSCOST   TEST COST ACCOUNTING                         
         BZ    EXPD20                                                           
         CLI   COSTANAL,C' '                                                    
         BE    EXPD20                                                           
         MVI   COSTSW,C'Y'                                                      
         B     EXPD18                                                           
*                                                                               
EXPD14   L     R1,ACATD                                                         
         USING CATD,R1                                                          
         XC    CATD(CATLNQ),CATD                                                
         MVC   CATDMGR,DATAMGR                                                  
         MVC   CATSEAC,POSTACC                                                  
         MVC   CATOFF,ANAOFF                                                    
         MVC   CATDPT,DEPT                                                      
         GOTO1 VCATCALL                                                         
         MVC   COSTSW,CATPST                                                    
         L     R2,AEXPH                                                         
         MVC   KEY,BCSPACES                                                     
         MVC   KEY(L'ACTKCULA),CATACC3 EXTRACT KEY FOR ERROR                    
         CLI   CATERR,0                                                         
         BE    EXPD16              NO ERROR                                     
         MVC   FVMSGNO,=AL2(AE$IANAL)                                           
         ST    R2,FVADDR                                                        
         B     ERRXIT                                                           
*                                                                               
EXPD16   CLI   CATPST,C'N'         TEST TO POST                                 
         BE    EXPD18                                                           
         MVC   COSTANAL,CATCDE     SAVE CATEGORY CODE                           
         MVC   CR13NUM,CATACC3     AND 13 ACCOUNT                               
*                                                                               
EXPD18   CLI   COSTSW,C'Y'         TEST FOR COST ACCOUNTING                     
         BNE   EXPD20                                                           
*                                                                               
         BAS   RE,COST             VALIDATE COST ACCOUNTING ACCOUNTS            
         L     R2,ACLIH            REQUIRE CLIENT INPUT                         
         CLI   5(R2),0             TEST FOR INPUT                               
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$MISIF)                                           
         B     ERRXIT                                                           
*                                                                               
         TM    BCCPYST5,CPYSEXPP   TEST IF PRODUCT ALSO REQUIRED                
         BZ    EXPD20                                                           
         L     R2,APROH            YES-MAKE SURE ITS THERE                      
         CLI   5(R2),0             TEST FOR INPUT                               
         BNE   EXPD20                                                           
         MVC   FVMSGNO,=AL2(AE$MISIF)                                           
         B     ERRXIT                                                           
*                                                                               
EXPD20   L     R2,ASTFH                                                         
         CLI   8(R2),C'*'          TEST FOR REPEAT FEATURE                      
         BNE   *+8                                                              
         BAS   RE,DUP                                                           
         CLI   STFSW,C'Y'          TEST FOR STAFF ANALYSIS                      
         BE    EXPD22                                                           
*                                                                               
         CLI   5(R2),0             NO STAFF ANALYSIS--FORCE                     
         BE    EXPD22              NO INPUT                                     
         MVC   FVMSGNO,=AL2(AE$ANFST)                                           
         B     ERRXIT                                                           
*                                                                               
EXPD22   CLI   STFSW,C'Y'          TEST FOR STAFF ANALYSIS                      
         BE    EXPD24              YES                                          
         CLI   COSTSW,C'Y'         TEST COST ACCOUNTING                         
         BE    EXPD24              YES                                          
*                                                                               
         L     R2,ACLIH            THEN NO INPUT TO CLI/PRO                     
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         CLI   5(R2),0                                                          
         BNE   ERRXIT                                                           
         L     R2,APROH                                                         
         CLI   5(R2),0                                                          
         BNE   ERRXIT                                                           
         B     EXPD26                                                           
*                                                                               
EXPD24   L     R2,ACLIH                                                         
         CLI   8(R2),C'*'                                                       
         BNE   *+8                                                              
         BAS   RE,DUP                                                           
         BAS   RE,VALCLI                                                        
*                                                                               
         L     R2,APROH                                                         
         CLI   8(R2),C'*'                                                       
         BNE   *+8                                                              
         BAS   RE,DUP                                                           
         BAS   RE,VALPRO                                                        
*                                                                               
EXPD26   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         L     R2,AJOBH                                                         
         CLI   5(R2),0                                                          
         BNE   ERRXIT                                                           
*                                                                               
         CLI   COSTSW,C'Y'         TEST FOR COST ACCOUNTING                     
         BE    EXPD28              YES-MUST GET 1C ACCOUNT                      
         CLI   STFSW,C'Y'          TEST FOR STAFF ANALYSIS                      
         BNE   EXPD30              NO-SKIP 1C ACCOUNT FETCH                     
         OC    CLICODE,CLICODE     TEST IF AT LEAST CLIENT INPUT                
         BZ    EXPD30              NO-SKIP 1C ACCOUNT FETCH                     
*                                                                               
EXPD28   BAS   RE,PROFMERG                                                      
         SR    R6,R6               NO MORE PROFILES                             
         LA    R4,PROFILE          FIND 1/C ACCOUNT                             
         USING ACPROFD,R4                                                       
         MVC   COSTNUM,ACPRCOST                                                 
         BAS   RE,SET1C            HANDLE ANY OVERRIDES TO 1C A/C               
         MVC   KEY,SPACES                                                       
         MVC   KEY(15),COSTNUM                                                  
         SR    R6,R6                                                            
         BAS   RE,GETACC                                                        
         BAS   RE,CHECKACC                                                      
         MVC   COSTNUM,ACCODE                                                   
         MVC   COSTNAME,ACNAME                                                  
         DROP  R4                                                               
*                                                                               
EXPD30   CLI   STFSW,C'Y'          TEST FOR STAFF ANALYSIS                      
         BNE   *+8                                                              
         BAS   RE,STF              YES-HANDLE STAFF AND 29 A/C                  
*                                                                               
         L     R2,AAMTH                                                         
         CLI   8(R2),C'*'          REPEAT FEATURE                               
         BNE   *+8                                                              
         BAS   RE,DUP              EDIT FOR REPEAT                              
         BAS   RE,EDAMT                                                         
         AP    CSHTOT,INAMNT                                                    
         ZAP   TRANSAMT,INAMNT                                                  
*                                                                               
         CP    SVDISC2,=P'0'       SAVED DISCOUNT                               
         BE    EXPD32                                                           
         ZAP   DUB,INAMNT          CALCULATE CD                                 
         MP    DUB,SVDISC2                                                      
         SRP   DUB,64-4,5          ROUNDED DIVIDE BY 10,000                     
         ZAP   CDAMNT,DUB                                                       
*                                                                               
         BAS   RE,VALECD           VALIDATE EXPENSE CD ACCOUNT                  
*                                                                               
EXPD32   DS    0H                                                               
         GOTO1 =A(EPOST),DMCB,(RC),RR=RELOA                                     
         BE    *+12                                                             
         L     R2,FVADDR                                                        
         B     ERRXIT                                                           
*                                                                               
         BAS   RE,NAMONE                                                        
         BAS   RE,NAMTWO                                                        
         SR    R0,R0               SET GOOD CC                                  
*                                                                               
EXPDX    B     CURSIT                                                           
         EJECT                                                                  
*--------------------------------------------------------------                 
*        DISPLAY CASH AND VENDOR ACCOUNTS FOR SECOND SCREEN                     
*        OUTPUTS TO FLD                                                         
*--------------------------------------------------------------                 
*                                                                               
ACCN     NTR1  ,                                                                
         SR    R1,R1                                                            
         CLI   SVCSHNUM,0                                                       
         BE    *+8                                                              
         LA    R1,1(R1)                                                         
         CLI   SVCRACCT,0                                                       
         BE    *+8                                                              
         LA    R1,1(R1)                                                         
         CLI   SVCRACC2,0                                                       
         BE    *+8                                                              
         LA    R1,1(R1)                                                         
         STC   R1,BYTE                                                          
*                                                                               
         LA    R0,36               SET NAME LENGTH=36                           
         CLI   BYTE,2              TEST FOR 2 ACCOUNTS                          
         BL    ACCN2               LOW-ITS 1 AND WE'RE DONE                     
         LA    R0,30                                                            
         BE    ACCN2                                                            
         LA    R0,20                                                            
*                                                                               
ACCN2    ST    R0,NAMELEN          SAVE NAME LENGTH                             
         MVC   FLD,BCSPACES                                                     
         LA    R4,FLD                                                           
*                                                                               
ACCN6    CLI   SVCSHNUM,0          TEST FOR A CASH ACCOUNT                      
         BE    ACCN8               NO                                           
*                                                                               
         MVC   WORK,BCSPACES                                                    
         MVC   WORK(5),=C'CASH='                                                
         MVC   WORK+5(L'SVCSHNAM),SVCSHNAM                                      
         GOTO1 SQUASHER,DMCB,WORK+5,L'SVCSHNAM                                  
         L     R3,NAMELEN                                                       
         LA    R3,5(R3)                                                         
         GOTO1 ADDNAM,DMCB,WORK,(R3)                                            
*                                                                               
ACCN8    CLC   SVCRACCT,SVCRACC2   TEST PROD VENDOR=EXP VENDOR                  
         BNE   ACCN10              NO                                           
         CLI   SVCRACCT,0          TEST FOR A VENDOR                            
         BE    ACCNX               NO-ALL DONE                                  
*                                                                               
         MVC   WORK,BCSPACES                                                    
         MVC   WORK(7),=C'VENDOR='                                              
         MVC   WORK+7(L'SVACCTNM),SVACCTNM                                      
         GOTO1 SQUASHER,DMCB,WORK+7,L'SVACCTNM                                  
         L     R3,NAMELEN                                                       
         LA    R3,7(R3)                                                         
         GOTO1 ADDNAM,DMCB,WORK,(R3)                                            
         B     ACCNX                                                            
*                                                                               
ACCN10   CLI   SVCRACCT,0          TEST PROD VENDOR                             
         BE    ACCN12              NO                                           
*                                                                               
         MVC   WORK,BCSPACES                                                    
         MVC   WORK(5),=C'PROD='                                                
         MVC   WORK+5(L'SVACCTNM),SVACCTNM                                      
         GOTO1 SQUASHER,DMCB,WORK+5,L'SVACCTNM                                  
         L     R3,NAMELEN                                                       
         LA    R3,5(R3)                                                         
         GOTO1 ADDNAM,DMCB,WORK,(R3)                                            
*                                                                               
ACCN12   CLI   SVCRACC2,0                                                       
         BE    ACCNX                                                            
*                                                                               
         MVC   WORK,BCSPACES                                                    
         MVC   WORK(4),=C'EXP='                                                 
         MVC   WORK+4(L'SVACCTN2),SVACCTN2                                      
         GOTO1 SQUASHER,DMCB,WORK+4,L'SVACCTN2                                  
         L     R3,NAMELEN                                                       
         LA    R3,4(R3)                                                         
         GOTO1 ADDNAM,DMCB,WORK,(R3)                                            
*                                                                               
ACCNX    B     CURSIT                                                           
         EJECT                                                                  
*--------------------------------------------------------------                 
*        DISPLAY FIRST NAME FIELD PROTECTED DATA                                
*        EXPENSE - FULL EXPENSE A/C NAME                                        
*        PRODUCTION - CLIENT/PRODUCT/JOB NAMES                                  
*        XJOB - CLIENT/PRODUCT/JOB NAMES EXPENSE A/C                            
*--------------------------------------------------------------                 
*                                                                               
NAMONE   NTR1  ,                                                                
         L     R2,ANAM1H                                                        
         MVC   FLD,BCSPACES        CLEAR OUTPUT DATA                            
         LA    R4,FLD              R4=OUTPUT POINTER                            
*                                                                               
         CLI   TYPE,C'E'           TEST EXPENSE ITEM                            
         BE    NAMONE4             YES                                          
*                                                                               
         GOTO1 SQUASHER,DMCB,SJCLINAM,L'SJCLINAM                                
         GOTO1 SQUASHER,DMCB,SJPRONAM,L'SJPRONAM                                
         GOTO1 SQUASHER,DMCB,SJJOBNAM,L'SJJOBNAM                                
         LA    R0,22               SET MAX LEN FOR A NAME                       
         TM    ACOPSTAT,ACOXJOB    TEST FOR XJOB                                
         BZ    NAMONE2             NO                                           
*                                                                               
         L     R6,AEXCELD                                                       
         USING EXCELD,R6                                                        
         GOTO1 SQUASHER,DMCB,EXCSENM,L'EXCSENM                                  
         LA    R0,18               SET MAX LEN FOR XJOB NAMES                   
*                                                                               
NAMONE2  GOTO1 ADDNAM,DMCB,SJCLINAM,(R0)                                        
         GOTO1 ADDNAM,DMCB,SJPRONAM,(R0)                                        
         GOTO1 ADDNAM,DMCB,SJJOBNAM,(R0)                                        
         TM    ACOPSTAT,ACOXJOB                                                 
         BZ    NAMONE6                                                          
*                                                                               
         GOTO1 ADDNAM,DMCB,EXCSENM,20                                           
         B     NAMONE6                                                          
*                                                                               
NAMONE4  MVC   FLD(L'POSTACCN),POSTACCN                                         
*                                                                               
NAMONE6  L     R2,ANAM1H                                                        
         BAS   RE,MOVEFLD                                                       
*                                                                               
NAMONEX  B     CURSIT                                                           
         DROP  R6                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO ADD A NAME TO OUTPUT STREAM AND TO UPDATE POINTER              
*                                                                               
* AT ENTRY, R4=A(OUTPUT), P1=A(NAME TO ATTACH), P2=MAX LEN OF NAME              
*                                                                               
ADDNAM   ST    RE,FULL                                                          
         LM    RE,RF,0(R1)                                                      
         LR    R1,RF                                                            
         BCTR  R1,0                MOVE FOR MAXIMUM LENGTH                      
         EX    R1,*+4                                                           
         MVC   0(0,R4),0(RE)                                                    
         LA    R4,0(R1,R4)         POINT TO LAST BYTE                           
         LA    R1,1(R1)            RESTORE LENGTH                               
         CLI   0(R4),C' '          TEST FOR SIGNIFICANT BYTE                    
         BH    *+10                                                             
         BCTR  R4,0                                                             
         BCT   R1,*-10                                                          
         LA    R4,2(R4)            ADVANCE POINTER FOR NEXT NAME                
         L     RE,FULL                                                          
         BR    RE                                                               
         EJECT                                                                  
*--------------------------------------------------------------                 
*        DISPLAY SECOND NAME FIELD PROTECTED DATA                               
*        EXPENSE - FIN OFF/CRD OFF/ANA OFF/DEPT NAME/STAFF NAME                 
*        PRODUCTION - CRD OFFICE                                                
*        XJOB - SAME AS EXPENSE                                                 
*--------------------------------------------------------------                 
*                                                                               
NAMTWO   NTR1  ,                                                                
         L     R2,ANAM2H                                                        
         MVC   FLD,BCSPACES        CLEAR OUTPUT DATA                            
         LA    R4,FLD              R4=OUTPUT POINTER                            
         CLI   TYPE,C'P'           TEST PRODUCTION                              
         BNE   NAMTWO2             NO                                           
*                                                                               
         L     R1,AFOFFH                                                        
         CLC   5(2,R1),=C'++'                                                   
         BNE   NAMTWO1                                                          
         MVC   FVMSGNO,=AL2(AE$AITL)   YES-FLAG AN ERROR                        
         B     ERRXIT                                                           
*                                                                               
NAMTWO1  GOTO1 AFVAL                                                            
         MVC   FINOFF,FVIFLD       EXTRACT OFFICE CODE                          
         L     R1,ACOFFH                                                        
         GOTO1 AFVAL                                                            
         MVC   CRDOFF,FVIFLD                                                    
         L     R1,AAOFFH                                                        
         GOTO1 AFVAL                                                            
         MVC   ANAOFF,FVIFLD                                                    
*                                                                               
* GET OFFICE SHORT NAMES                                                        
*                                                                               
NAMTWO2  MVC   ACNAMESH,BCSPACES   CLEAR SHORT NAME                             
         LA    R3,IOKEY                                                         
         USING OFFRECD,R3                                                       
         MVC   OFFKEY,BCSPACES                                                  
         MVI   OFFKTYP,OFFKTYPQ                                                 
         MVC   OFFKCPY,CUABIN                                                   
         MVC   OFFKOFF,FINOFF                                                   
         CLC   FINOFF,BCSPACES                                                  
         BNH   NAMTWO4                                                          
         GOTO1 AIO,IOREAD+IOACCFIL+IO1                                          
         BNE   NAMTWO4                                                          
         L     R1,AIO1                                                          
         LA    R5,ACCORFST(R1)     POINT TO FIRST ELEM                          
         GOTO1 AGETELS,DMCB,(R5),0                                              
*                                                                               
NAMTWO4  CLC   ACNAMESH,BCSPACES                                                
         BNH   NAMTWO5                                                          
         GOTO1 ADDNAM,DMCB,ACNAMESH,L'ACNAMESH                                  
*                                                                               
NAMTWO5  CLC   FINOFF,CRDOFF                                                    
         BE    NAMTWO6                                                          
*                                                                               
         MVC   ACNAMESH,BCSPACES   CLEAR SHORT NAME                             
         CLC   CRDOFF,BCSPACES     TEST FOR AN OFFICE CODE                      
         BNH   NAMTWO6             NO                                           
*                                                                               
         MVC   OFFKOFF,CRDOFF                                                   
         GOTO1 AIO,IOREAD+IOACCFIL+IO1                                          
         BNE   NAMTWO6                                                          
         L     R1,AIO1                                                          
         LA    R5,ACCORFST(R1)                                                  
         GOTO1 AGETELS,DMCB,(R5),0                                              
*                                                                               
NAMTWO6  CLC   ACNAMESH,BCSPACES                                                
         BNH   NAMTWO8                                                          
         GOTO1 ADDNAM,DMCB,ACNAMESH,L'ACNAMESH                                  
*                                                                               
NAMTWO8  CLC   ANAOFF,CRDOFF       TEST CREDIT OFFICE=ANALYSIS OFF              
         BE    NAMTWO9             YES                                          
*                                                                               
         MVC   ACNAMESH,BCSPACES   RE-CLEAR NAME                                
         CLC   ANAOFF,BCSPACES     TEST FOR AN ANALYSIS OFFICE                  
         BNH   NAMTWO9             NO                                           
*                                                                               
         MVC   OFFKOFF,ANAOFF      PLUG IN NEW OFFICE IN KEY                    
         GOTO1 AIO,IOREAD+IOACCFIL+IO1                                          
         BNE   NAMTWO9             COULD NOT FIND OFFICE                        
         L     R1,AIO1                                                          
         LA    R5,ACCORFST(R1)                                                  
         GOTO1 AGETELS,DMCB,(R5),0                                              
*                                                                               
NAMTWO9  CLC   ACNAMESH,BCSPACES                                                
         BNH   NAMTWO10                                                         
         GOTO1 ADDNAM,DMCB,ACNAMESH,L'ACNAMESH                                  
*                                                                               
* DEPARTMENT/STAFF NAMES                                                        
*                                                                               
NAMTWO10 L     R6,AEXCELD                                                       
         USING EXCELD,R6                                                        
         CLI   TYPE,C'E'           TEST FOR EXPENSE                             
         BE    NAMTWO15            YES                                          
         TM    ACOPSTAT,ACOXJOB    TEST FOR XJOB                                
         BZ    NAMTWO20            NO-ALL DONE NOW                              
*                                                                               
         CLC   EXC2DNM,BCSPACES    TEST FOR DEPT NAME                           
         BNH   NAMTWO11            NONE                                         
         GOTO1 SQUASHER,DMCB,EXC2DNM,L'EXC2DNM                                  
         GOTO1 ADDNAM,DMCB,EXC2DNM,19                                           
*                                                                               
NAMTWO11 CLC   EXC2PNM,BCSPACES    TEST FOR STAFF NAME                          
         BNH   NAMTWO20                                                         
         GOTO1 SQUASHER,DMCB,EXC2PNM,L'EXC2PNM                                  
         GOTO1 ADDNAM,DMCB,EXC2PNM,19                                           
         B     NAMTWO20                                                         
*                                                                               
NAMTWO15 CLC   DEPNAME,BCSPACES    TEST FOR DEPT NAME                           
         BNH   NAMTWO16            NONE                                         
*                                                                               
         GOTO1 SQUASHER,DMCB,DEPNAME,L'DEPNAME                                  
         GOTO1 ADDNAM,DMCB,DEPNAME,20                                           
*                                                                               
NAMTWO16 CLC   STAFFNAM,BCSPACES                                                
         BNH   NAMTWO20                                                         
*                                                                               
         GOTO1 SQUASHER,DMCB,STAFFNAM,L'STAFFNAM                                
         GOTO1 ADDNAM,DMCB,STAFFNAM,20                                          
*                                                                               
NAMTWO20 BAS   RE,MOVEFLD                                                       
*                                                                               
NAMTWOX  B     CURSIT                                                           
         DROP  R3,R6                                                            
         EJECT                                                                  
*--------------------------------------------------------------                 
*        REPEAT FEATURE ROUTINE-R2=A(CURRENT FLD HDR WITH '*' INPUT)            
*--------------------------------------------------------------                 
*                                                                               
DUP      NTR1  ,                                                                
         LR    R1,R2               R1=A(SOURCE FIELD)                           
         L     R4,ANEXT            COMPUTE L'DETAIL LINE IN R4                  
         L     RE,ACLIH                                                         
         SR    R4,RE                                                            
         SR    R1,R4               BACK UP TO PREVIOUS LINE                     
         L     R3,ADETH            R3=A(FIRST DETAIL LINE)                      
         CR    R1,R3               TEST IF USER IS ON FIRST LINE                
         BNL   DUP2                NO                                           
*                                                                               
         MVC   FVMSGNO,=AL2(AE$AITL)   YES-FLAG AN ERROR                        
         B     ERRXIT                                                           
*                                                                               
DUP2     GOTO1 AFVAL                                                            
         BE    DUP4                SOMETHING IN FIELD                           
         MVC   FVMSGNO,=AL2(AE$NODUP)    NOTHING TO DUPLICATE                   
         B     ERRXIT                                                           
*                                                                               
DUP4     MVC   5(1,R2),FVILEN      SET NEW LENGTH                               
         MVC   FLD,FVIFLD                                                       
         BAS   RE,MOVEFLD          MOVE IN NEW DATA                             
         OI    6(R2),X'80'         XMIT IT BACK                                 
*                                                                               
DUPX     B     CURSIT                                                           
         EJECT                                                                  
*                                                                               
*--------------------------------------------------------------                 
*        VALIDATE CLIENT FIELD--AT ENTRY TYPE IS SET                            
*        CALLER HAS ALREADY CHECKED FOR INPUT IF REQUIRED FIELD                 
*--------------------------------------------------------------                 
*                                                                               
VALCLI   NTR1  ,                                                                
         L     R2,ACLIH                                                         
         MVC   FVMAXL,BCCLILEN     MAXIMUM=CLIENT CODE LENGTH                   
         GOTO1 AFVAL,(R2)                                                       
         BE    VALCLI2                                                          
         BL    VALCLIX                                                          
         B     ERRXIT                                                           
*                                                                               
VALCLI2  LA    R6,CLIPROF                                                       
         LA    R4,KEY                                                           
         USING ACTRECD,R4                                                       
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(2),BCCPYEL+(CPYPROD-CPYELD)                              
         MVC   ACTKACT,FVIFLD      CLIENT CODE                                  
         BAS   RE,GETACC                                                        
*                                                                               
         CLI   TYPE,C'E'           TEST EXPENSE TRANSACTION                     
         BE    VALCLI6             YES-SKIP LOCKED/SECURITY TEST                
*                                                                               
         MVC   FVMSGNO,=AL2(AE$INACP)                                           
         TM    ACBSTAT,ACBSLOCK    TEST IF LOCKED                               
         BO    ERRXIT                                                           
*                                                                               
VALCLI4  CLC   TWAACCS(2),BCSPACES TEST ANY OLD SECURITY                        
         BNH   VALCLI6             NO                                           
         CLI   TWAACCS,C'*'        TEST FOR SINGLE OFFICE                       
         BE    VALCLI6             YES                                          
         CLI   TWAACCS,C'$'        TEST FOR OFFICE LIST                         
         BE    VALCLI6                                                          
         CLC   TWAACCS(2),FVIFLD   MATCH ON CLIENT CODE                         
         BNE   VALCLI6             NO-OK                                        
         MVC   FVMSGNO,=AL2(AE$SECLK)                                           
         B     ERRXIT                                                           
*                                                                               
VALCLI6  MVC   SJCLINUM,ACCODE     EXTRACT CLIENT KEY                           
         MVC   SJCLINAM,ACNAME                                                  
         MVC   CLIPRO,ACCODE                                                    
         MVC   CLIPRON,ACNAME                                                   
         MVC   CLICODE,FVIFLD      SAVE CLIENT CODE                             
                                                                                
         LA    R1,FFTELEM          BUILD FFTEL                                  
         USING FFTELD,R1                                                        
         MVI   FFTEL,FFTELQ                                                     
         MVI   FFTLN,FFTLN1Q+L'FFTDLEN+L'FFTCLPRA                               
         MVI   FFTTYPE,FFTTCLPR                                                 
         MVI   FFTSEQ,0                                                         
         MVI   FFTDLEN,L'FFTCLPRA                                               
         MVC   FFTCLAC,CLIPRO+3                                                 
         OC    FFTCLPRA,SPACES                                                  
         DROP  R1                                                               
                                                                                
VALCLIX  B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*--------------------------------------------------------------                 
*        VALIDATE PRODUCT FIELD--AT ENTRY TYPE IS SET                           
*        CALLER HAS ALREADY CHECKED FOR INPUT IF REQUIRED FIELD                 
*--------------------------------------------------------------                 
*                                                                               
VALPRO   NTR1  ,                                                                
         L     R2,APROH                                                         
         ZIC   RE,BCPROLEN         RE=L'CLIENT+L'PRODUCT                        
         ZIC   RF,BCCLILEN         RF=L'CLIENT                                  
         SR    RE,RF               RE=L'PRODUCT                                 
         STC   RE,FVMAXL                                                        
         GOTO1 AFVAL,(R2)                                                       
         BE    VALPRO2                                                          
         BL    VALPROX                                                          
         B     ERRXIT                                                           
*                                                                               
VALPRO2  LA    R6,PRODPROF                                                      
         LA    R4,KEY                                                           
         USING ACTRECD,R4                                                       
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(2),BCCPYEL+(CPYPROD-CPYELD)                              
         MVC   ACTKACT,SJCLINUM+(ACTKACT-ACTRECD)                               
         ZIC   R3,BCCLILEN                                                      
         LA    R3,ACTKACT(R3)      POINT R3 AT PRODUCT CODE POSITION            
         ZIC   R1,FVXLEN           R1=EXECUTE LENGTH                            
         EX    R1,*+4                                                           
         MVC   0(0,R3),FVIFLD      EXTRACT PRODUCT CODE                         
*                                                                               
VALPRO4  BAS   RE,GETACC                                                        
         CLI   TYPE,C'E'           TEST EXPENSE ITEM                            
         BE    VALPRO6                                                          
         MVC   FVMSGNO,=AL2(AE$INACP)                                           
         TM    ACBSTAT,ACBSLOCK    TEST LOCKED IF PRODUCTION                    
         BO    ERRXIT                                                           
*                                                                               
VALPRO6  MVC   SJPRONUM,ACCODE     EXTRACT PRODUCT KEY/NAME                     
         MVC   SJPRONAM,ACNAME                                                  
         MVC   CLIPRO,ACCODE                                                    
         MVC   CLIPRON,ACNAME                                                   
         MVC   PRODCODE,FVIFLD                                                  
                                                                                
         LA    R1,FFTELEM          UPDATE FFTEL                                 
         USING FFTELD,R1                                                        
         MVC   FFTPRAC,CLIPRO+6                                                 
         OC    FFTCLPRA,SPACES                                                  
         DROP  R1                                                               
                                                                                
VALPROX  B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*--------------------------------------------------------------                 
*        VALIDATE JOB FIELD--ONLY CALLED FOR PRODUCTION                         
*--------------------------------------------------------------                 
*                                                                               
VALJOB   NTR1  ,                                                                
         L     R2,AJOBH                                                         
         ZIC   RE,BCJOBLEN         RE=L'CLIENT+L'PRODUCT+L'JOB                  
         ZIC   RF,BCPROLEN         RF=L'CLIENT+L'PRODUCT                        
         SR    RE,RF               RE=L'JOB                                     
         STC   RE,FVMAXL                                                        
         MVI   FVMINL,1            YES-FORCE A MISSING INPUT FIELD              
         GOTO1 AFVAL,(R2)                                                       
         BNE   ERRXIT                                                           
*                                                                               
VALJOB2  LA    R6,JOBPROF                                                       
         LA    R4,KEY                                                           
         USING ACTRECD,R4                                                       
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(2),BCCPYEL+(CPYPROD-CPYELD)                              
         MVC   ACTKACT,SJPRONUM+(ACTKACT-ACTRECD)                               
         ZIC   R3,BCPROLEN                                                      
         LA    R3,ACTKACT(R3)      POINT R3 AT JOB CODE POSITION                
         ZIC   R1,FVXLEN           R1=EXECUTE LENGTH                            
         EX    R1,*+4                                                           
         MVC   0(0,R3),FVIFLD      EXTRACT JOB CODE                             
*                                                                               
VALJOB4  BAS   RE,GETACC                                                        
         BAS   RE,CHECKACC                                                      
         MVC   FVMSGNO,=AL2(AE$ACTCL)                                           
         TM    ACBSTAT,ACBSCLSE    TEST IF CLOSED JOB                           
         BO    ERRXIT                                                           
         MVC   SJJOBNUM,ACCODE     EXTRACT JOB KEY/NAME                         
         MVC   SJJOBNAM,ACNAME                                                  
*                                                                               
VALJOB6  GOTO1 AMRGPRF                                                          
         LA    R6,PSCOMPPR                                                      
         USING PPRELD,R6                                                        
         MVC   SJ1CAC,PPRCOST      GET COST ACCOUTING POINTER (1C)              
         MVC   SJOFFC,PPRGAOFF     SAVE PRODUCTION OFFICE                       
         MVC   JOBNUM,FVIFLD       SAVE JOB CODE                                
*                                                                               
VALJOBX  B     EXIT                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
*                                                                               
*--------------------------------------------------------------                 
*        SET OPTION DEFAULTS AND VALIDATE XJOB FIELDS                           
*--------------------------------------------------------------                 
*                                                                               
XJOB     NTR1  ,                                                                
         TM    ACOPSTAT,ACOXJOB    TEST FOR XJOB                                
         BO    XJOB1               YES                                          
*                                                                               
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         L     R2,AEXPH            FOR REGULAR JOB, DO NOT ALLOW INPUT          
         CLI   5(R2),0             IN XJOB FIELDS                               
         BNE   ERRXIT                                                           
*                                                                               
         L     R2,AFOFFH                                                        
         CLI   5(R2),0                                                          
         BNE   ERRXIT                                                           
*                                                                               
         L     R2,AAOFFH                                                        
         CLI   5(R2),0                                                          
         BNE   ERRXIT                                                           
*                                                                               
         L     R2,ADEPTH                                                        
         CLI   5(R2),0                                                          
         BNE   ERRXIT                                                           
*                                                                               
         L     R2,ASTFH                                                         
         CLI   5(R2),0                                                          
         BNE   ERRXIT                                                           
*                                                                               
         B     XJOBX                                                            
*                                                                               
XJOB1    L     R4,AGOXBLK                                                       
         USING GOXBLOCK,R4                                                      
         L     R2,AFOFFH                                                        
         CLI   5(R2),0                                                          
         BE    XJOB2                                                            
         CLC   8(2,R2),=C'++'                                                   
         BNE   XJOB2                                                            
         MVC   FVMSGNO,=AL2(AE$NAWTT)                                           
         B     ERRXIT                                                           
*                                                                               
XJOB2    BAS   RE,FOFF             VALIDATE FINANCIAL OFFICE                    
*                                                                               
         L     R2,AAOFFH                                                        
         CLI   5(R2),0             TEST IF ANALYSIS OFFICE ENTERED              
         BNE   XJOB4               YES                                          
*                                                                               
         MVC   FLD,BCSPACES                                                     
         MVC   FLD(L'GOAWOAOF),GOAWOAOF PICK UP ANALYSIS OFFICE OPTION          
         OC    GOAWOAOF,GOAWOAOF   TEST FOR ONE                                 
         BNZ   XJOB3               YES                                          
         MVC   FLD(L'FINOFF),FINOFF ELSE USE FINANCIAL OFFICE                   
         OC    FINOFF,FINOFF                                                    
         BNZ   XJOB3                                                            
         MVC   FLD(L'SJOFFC),SJOFFC USE CLIENT OFFICE LASTLY                    
*                                                                               
XJOB3    BAS   RE,MOVEFLD                                                       
         MVC   5(1,R2),BCOFFLEN                                                 
*                                                                               
XJOB4    L     R2,ADEPTH                                                        
         CLI   5(R2),0             TEST IF DEPARTMENT INPUT                     
         BNE   XJOB6               YES                                          
*                                                                               
         MVC   FLD,BCSPACES                                                     
         MVC   FLD(L'GOAWODEP),GOAWODEP                                         
         MVC   5(1,R2),BCDPTLEN                                                 
         BAS   RE,MOVEFLD                                                       
*                                                                               
XJOB6    L     R2,ASTFH                                                         
         CLI   5(R2),0                                                          
         BNE   XJOB8                                                            
         MVC   FLD,BCSPACES                                                     
         MVC   FLD(L'GOAWOSTF),GOAWOSTF                                         
         BAS   RE,MOVEFLD                                                       
         MVI   5(R2),L'GOAWOSTF                                                 
*                                                                               
XJOB8    L     R2,AEXPH                                                         
         CLI   5(R2),0                                                          
         BNE   XJOB10                                                           
*                                                                               
         OC    GOAWOA,GOAWOA       TEST FOR WRITE-OFF ACCOUNT                   
         BZ    XJOB10              NONE                                         
         MVC   FLD,BCSPACES                                                     
         CLC   GOAWOA(2),=C'SE'    TEST FOR EXPENSE LEDGER                      
         BE    *+18                YES                                          
         MVI   FLD,C'*'                                                         
         MVC   FLD+1(L'GOAWOA),GOAWOA                                           
         B     *+10                                                             
         MVC   FLD(L'GOAWOA-2),GOAWOA+2 ONLY NEED A/C CODE FOR SE               
         BAS   RE,MOVEFLD                                                       
*                                                                               
XJOB10   L     R3,AEXCELD                                                       
         USING EXCELD,R3                                                        
         LR    RE,R3                                                            
         LA    RF,EXCELNQ                                                       
         SR    R1,R1               CLEAR EXCEL BLOCK                            
         MVCL  RE,R0                                                            
*                                                                               
         MVI   EXCACT,EXCAVAL      VALIDATE CALL                                
         MVC   EXCAEXP,AEXPH       PASS A(EXPENSE A/C HEADER)                   
         MVC   EXCAANO,AAOFFH                                                   
         MVC   EXCADEP,ADEPTH                                                   
         MVC   EXCASTF,ASTFH                                                    
         ST    R9,EXCAGWS                                                       
         GOTO1 VEXCEL,EXCELD                                                    
         BE    XJOBX                                                            
         L     R2,FVADDR                                                        
         B     ERRXIT                                                           
*                                                                               
XJOBX    B     EXIT                                                             
         DROP  R3,R4                                                            
         EJECT                                                                  
*                                                                               
*--------------------------------------------------------------                 
*        CLEAR X-JOB RELATED FIELDS ON CHANGE OF JOB KEYS                       
*--------------------------------------------------------------                 
*                                                                               
CLEARX   NTR1  ,                                                                
         MVC   FLD,BCSPACES                                                     
         L     R2,AFOFFH           FINANCIAL OFFICE                             
         TM    4(R2),X'80'         TEST IF FIELD INPUT                          
         BO    *+16                YES                                          
         BAS   RE,MOVEFLD          NO-CLEAR IT                                  
         MVI   5(R2),0                                                          
         OI    6(R2),X'80'                                                      
*                                                                               
         L     R2,AAOFFH           ANALYSIS OFFICE                              
         TM    4(R2),X'80'         TEST IF FIELD INPUT                          
         BO    *+16                YES                                          
         BAS   RE,MOVEFLD          NO-CLEAR IT                                  
         MVI   5(R2),0                                                          
         OI    6(R2),X'80'                                                      
*                                                                               
         L     R2,AEXPH            EXPENSE ACCOUNT                              
         TM    4(R2),X'80'         TEST IF FIELD INPUT                          
         BO    *+16                YES                                          
         BAS   RE,MOVEFLD          NO-CLEAR IT                                  
         MVI   5(R2),0                                                          
         OI    6(R2),X'80'                                                      
*                                                                               
         L     R2,ADEPTH           DEPARTMENT                                   
         TM    4(R2),X'80'         TEST IF FIELD INPUT                          
         BO    *+16                YES                                          
         BAS   RE,MOVEFLD          NO-CLEAR IT                                  
         MVI   5(R2),0                                                          
         OI    6(R2),X'80'                                                      
*                                                                               
         L     R2,ASTFH            STAFF                                        
         TM    4(R2),X'80'         TEST IF FIELD INPUT                          
         BO    *+16                YES                                          
         BAS   RE,MOVEFLD          NO-CLEAR IT                                  
         MVI   5(R2),0                                                          
         OI    6(R2),X'80'                                                      
*                                                                               
         B     CURSIT                                                           
         EJECT                                                                  
*                                                                               
*--------------------------------------------------------------                 
*        VALIDATE FINANCIAL OFFICE FIELD--OUTPUTS TO FINOFF                     
*--------------------------------------------------------------                 
*                                                                               
FOFF     NTR1  ,                                                                
         L     R2,AFOFFH                                                        
         MVC   FVMAXL,BCOFFLEN                                                  
*                                                                               
FOFF1    GOTO1 AFVAL,(R2)                                                       
         BE    FOFF4               SOMETHING IN FIELD                           
         BH    ERRXIT              INPUT DATA TOO LONG                          
*                                                                               
* NO DATA IN FIELD                                                              
*                                                                               
FOFF2    CLI   TYPE,C'E'           NO INPUT-TEST FOR EXPENSE                    
         BNE   FOFF3               NO                                           
*                                                                               
         TM    BCCPYST1,CPYSOROE   TEST OFFICE REQUIRED                         
         BZ    FOFFX               NO                                           
         MVC   FVMSGNO,=AL2(FVFNONE)                                            
         B     ERRXIT                                                           
*                                                                               
FOFF3    TM    ACOPSTAT,ACOXJOB    ITS PRODUCTION-TEST FOR XJOB                 
         BZ    FOFFX               NO-EXIT                                      
*                                                                               
         L     R4,AGOXBLK                                                       
         USING GOXBLOCK,R4                                                      
         OC    GOAWOFOF,GOAWOFOF   TEST FOR OPTION VALUE                        
         BZ    FOFFX               NO-EXIT                                      
*                                                                               
         MVC   FLD,BCSPACES                                                     
         MVC   FLD(L'GOAWOFOF),GOAWOFOF SET OPTION OFFICE IN FIELD              
         BAS   RE,MOVEFLD                                                       
         MVC   5(1,R2),BCOFFLEN                                                 
         B     FOFF1               GO BACK AND RE-VALIDATE FIELD                
*                                                                               
* DATA IN FIELD                                                                 
*                                                                               
FOFF4    CLI   TYPE,C'E'           TEST FOR EXPENSE                             
         BE    FOFF6               YES                                          
         TM    ACOPSTAT,ACOXJOB    ITS PRODUCTION-TEST FOR XJOB                 
         BO    FOFF6               YES-ITS OK TO HAVE VALUE IN FIELD            
         MVC   FLD,BCSPACES        CLEAR THE FIELD OUT                          
         BAS   RE,MOVEFLD                                                       
         OI    6(R2),X'80'                                                      
         B     FOFFX                                                            
*                                                                               
FOFF6    GOTO1 AVALOFFC,BOPARM,(X'80',FVIFLD)                                   
         BNE   ERRXIT                                                           
         MVC   FINOFF,ACOFFC       SET FINANCIAL OFFICE                         
*                                                                               
FOFFX    B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*--------------------------------------------------------------                 
*        VALIDATE ANALYSIS OFFICE FIELD--OUTPUTS TO ANAOFF                      
*        AND ANOELEM IF INPUT                                                   
*--------------------------------------------------------------                 
*                                                                               
AOFF     NTR1  ,                                                                
         L     R2,AAOFFH                                                        
         MVC   FVMAXL,BCOFFLEN                                                  
         XC    ANOELEM,ANOELEM                                                  
*                                                                               
AOFF1    GOTO1 AFVAL,(R2)                                                       
         BE    AOFF4               SOMETHING IN FIELD                           
         BH    ERRXIT              INPUT DATA TOO LONG                          
*                                                                               
* NO DATA IN FIELD                                                              
*                                                                               
AOFF2    MVC   ANAOFF,FINOFF       COPY ANAOFF FROM FINANCIAL OFFICE            
         B     AOFFX                                                            
*                                                                               
* DATA IN FIELD                                                                 
*                                                                               
*                                                                               
AOFF4    GOTO1 AVALOFFC,BOPARM,(X'80',FVIFLD)                                   
         BNE   ERRXIT                                                           
*                                                                               
         MVC   ANAOFF,ACOFFC       SET ANALYSIS OFFICE                          
         LA    R6,ANOELEM                                                       
         USING ANOELD,R6                                                        
         MVI   ANOEL,ANOELQ                                                     
         MVI   ANOLN,ANOLNQ                                                     
         MVI   ANOTYPE,ANOTPER                                                  
         MVC   ANOOFFC,ANAOFF                                                   
*                                                                               
AOFFX    B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
*--------------------------------------------------------------                 
*        VALIDATE CREDIT OFFICE FIELD--OUTPUTS TO CRDOFF                        
*--------------------------------------------------------------                 
*                                                                               
COFF     NTR1  ,                                                                
         L     R2,ACOFFH                                                        
         MVC   FVMAXL,BCOFFLEN                                                  
*                                                                               
         GOTO1 AFVAL,(R2)                                                       
         BH    ERRXIT              INPUT DATA TOO LONG                          
         BL    COFFX               EXIT FOR NO INPUT                            
*                                                                               
* DATA IN FIELD                                                                 
*                                                                               
COFF2    GOTO1 AVALOFFC,BOPARM,(X'80',FVIFLD)                                   
         BNE   ERRXIT                                                           
*                                                                               
         MVC   CRDOFF,ACOFFC       SET CREDIT OFFICE                            
*                                                                               
COFFX    B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*--------------------------------------------------------------                 
*        VALIDATE EXPENSE ACCOUNT CODE--AT ENTRY R2=A(FLDH)                     
*--------------------------------------------------------------                 
*                                                                               
EDEXP    NTR1  ,                                                                
         L     R2,AEXPH                                                         
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,(R2)                                                       
         BNE   ERRXIT                                                           
*                                                                               
         LA    R4,KEY                                                           
         USING ACTRECD,R4                                                       
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,COMPANY                                                  
         CLI   FVIFLD,C'*'         REPEAT FEATURE                               
         BNE   EDEXP10                                                          
         ZIC   RE,FVILEN                                                        
         CH    RE,=H'1'                                                         
         BNH   EDEXP8              MORE THAN 2 INDICATES DEFAULT ACT            
*                                                                               
EDEXP2   MVC   FVMSGNO,=AL2(AE$INACP)                                           
         LA    RF,DLIST            A(LIST OF VALID U/L'S TO DEBIT)              
*                                                                               
EDEXP4   CLI   0(RF),X'FF'         END OF LIST                                  
         BE    ERRXIT                                                           
         CLC   FVIFLD+1(2),0(RF)   INPUT TO LIST                                
         BE    EDEXP6                                                           
         LA    RF,2(RF)                                                         
         B     EDEXP4                                                           
*                                                                               
EDEXP6   LR    RF,RE                                                            
         MVC   ACTKUNT(2),FVIFLD+1 DEFAULT U/L TO KEY                           
         SH    RF,=H'3'            DEDUCT FOR *UL                               
         LTR   RF,RF                                                            
         BM    ERRXIT                                                           
         LA    R5,FVIFLD+3         A(REST OF DFLT ACCT)                         
         B     EDEXP15                                                          
*                                                                               
EDEXP8   BAS   RE,DUP              EDIT FOR REPEAT                              
         GOTO1 AFVAL,(R2)                                                       
         ZIC   RE,FVILEN           LEN FOR DFLT ACT                             
         CLI   8(R2),C'*'          CHK IF ITS DFLT ACT INPUT                    
         BE    EDEXP2              NO                                           
*                                                                               
EDEXP10  MVC   ACTKUNT(2),=C'SE'   U/L DEBIT                                    
         ZIC   RF,FVILEN           INPUT LEN                                    
         MVC   FVMSGNO,=AL2(AE$ACLNG)                                           
         CHI   RF,L'ACTKACT                                                     
         BH    ERRXIT                                                           
         LA    R5,FVIFLD                                                        
*                                                                               
EDEXP15  BCTR  RF,0                SUB 1 FOR EXECUTE                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   ACTKACT(0),0(R5)    EXP ACC TO KEY                               
         SR    R6,R6               NO PROFILES                                  
         BAS   RE,GETACC                                                        
         BAS   RE,CHECKACC                                                      
         MVC   POSTACC,ACCTNUM                                                  
         MVC   POSTACCN,ACCTNAME                                                
*                                                                               
         MVI   STFSW,C'N'          INITIALIZE SWITCHES                          
         MVI   DEPSW,C'N'                                                       
         MVI   OFFSW,C'N'                                                       
*                                                                               
         TM    BCCPYST1,CPYSOROE   MANDATORY OFFICE                             
         BZ    *+8                                                              
         MVI   OFFSW,C'Y'                                                       
*                                                                               
         TM    ACCTSTAT,X'40'      PERSONAL EXPENSE (STAFF=Y)                   
         BZ    *+8                                                              
         MVI   STFSW,C'Y'                                                       
*                                                                               
         TM    ACCTSTAT,X'08'      DEPT EXPENSE (DEPT=Y)                        
         BZ    *+8                                                              
         MVI   DEPSW,C'Y'                                                       
*                                                                               
EDEXP20  MVC   COSTANAL,BCSPACES                                                
         MVC   COSTANAL(1),ACCTCOST  (ANALYSIS=)                                
         OI    COSTANAL,X'40'                                                   
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',=C'ACCOUNT'),('RSTELQ',AIO1),0                 
         CLI   12(R1),0            TEST IF ELEMENT FOUND                        
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,12(R1)                                                        
         USING RSTELD,R6                                                        
         MVC   POSTCNTR,RSTCCTR    COST CENTER                                  
         MVC   POSTCPOS,RSTCCTRR   COST POSITION                                
*                                                                               
         CLI   TENO,X'F0'          LENGTH OF ACCT WANTED                        
         BL    EDEXPX                                                           
*                                                                               
* THIS SHOULD BE CHECKED--USE OF FIELD LENGTH                                   
*                                                                               
         L     R2,AEXPH                                                         
         PACK  DUB,TENO                                                         
         CVB   R4,DUB                                                           
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         ZIC   R6,5(R2)            INPUT LENGTH                                 
         SR    R6,R4               IF DESIRED LEN GREATER THAN GIVEN            
         BM    ERRXIT              ERROR                                        
*                                                                               
EDEXPX   B     CURSIT                                                           
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*--------------------------------------------------------------                 
*        GET STAFF LEDGER VALUES -- SETS LEVEL AND STAFFL                       
*--------------------------------------------------------------                 
*                                                                               
GETSTF   NTR1  ,                                                                
         LA    R4,IOKEY                                                         
         USING LDGRECD,R4                                                       
         MVC   LDGKEY,BCSPACES                                                  
         MVC   LDGKCPY,CUABIN                                                   
         MVC   LDGKUNT(2),=C'2P'   GET 2P LEDGER VALUES                         
         GOTO1 AGETLDG                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
GETSTF2  ICM   R1,15,ACALDG                                                     
         USING LDGTABD,R1                                                       
         MVI   LEVEL,3             SET 2P LEDGER LEVEL                          
         ZIC   R5,LDGTLVB          GET DISPLACEMENT TO STAFF CODE               
         CLI   LDGTLVC,L'ACTKACT   TEST FOR 3 LEVEL LEDGER                      
         BE    GETSTF4             YES                                          
*                                                                               
         IC    R5,LDGTLVA                                                       
         MVI   LEVEL,2                                                          
         CLI   LDGTLVB,L'ACTKACT   TEST FOR 2 LEVEL LEDGER                      
         BE    GETSTF4                                                          
*                                                                               
         SR    R5,R5                                                            
         MVI   LEVEL,1                                                          
         CLI   LDGTLVA,L'ACTKACT                                                
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
GETSTF4  LA    RE,L'ACTKACT                                                     
         SR    RE,R5                                                            
         STC   RE,STAFFL           SAVE L'STAFF CODE                            
*                                                                               
GETSTFX  B     CURSIT                                                           
         DROP  R1,R4                                                            
         EJECT                                                                  
*                                                                               
*--------------------------------------------------------------                 
*        VALIDATE DEPARTMENT ANALYSIS ACCOUNTS                                  
*      --SHOULD FIELD BE CLEARED AUTOMATICALLY FOR EXTRANEOUS INPUT?            
*--------------------------------------------------------------                 
*                                                                               
DEP      NTR1  ,                                                                
         L     R2,ADEPTH                                                        
         MVC   FVMAXL,BCDPTLEN     SET MAXIMUM LENGTH                           
         MVI   FVMINL,1            SET SOME INPUT REQUIRED                      
         CLI   DEPSW,C'Y'          TEST DEPARTMENT ANALYSIS                     
         BE    DEP2                YES                                          
         CLI   STFSW,C'Y'          TEST STAFF ANALYSIS                          
         BNE   *+12                NO                                           
         CLI   LEVEL,1             TEST 1 LEVEL 2P LEDGER                       
         BH    DEP2                NO                                           
*                                                                               
         MVI   FVMINL,0            DEPARTMENT IS NOT REQUIRED                   
*                                                                               
DEP2     MVC   BYTE,FVMINL         SAVE REQUIRED VALUE                          
         GOTO1 AFVAL,(R2)                                                       
         BH    ERRXIT                                                           
         BE    *+16                SOMETHING WAS INPUT                          
         CLI   BYTE,1              TEST IF REQUIRED                             
         BE    ERRXIT              YES-EXIT WITH ERROR                          
         B     DEPX                NO-EXIT ITS OK                               
*                                                                               
         CLI   BYTE,1              TEST FIELD IS REQUIRED                       
         BE    DEP3                YES                                          
*                                                                               
         MVC   FVMSGNO,=AL2(AE$ANFDP)                                           
         B     ERRXIT                                                           
*                                                                               
DEP3     MVC   DEPT,FVIFLD                                                      
         CLI   DEPSW,C'N'          TEST FOR DEPARTMENT ANALYSIS                 
         BE    DEPX                NO-EXIT WITHOUT VALIDATING ACCOUNTS          
*                                                                               
* VALIDATE 2D ACCOUNT                                                           
*                                                                               
DEP4     LA    R4,KEY                                                           
         USING ACTRECD,R4                                                       
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(2),=C'2D'                                                
         LA    R1,ACTKACT                                                       
*                                                                               
         CLI   OFFSW,C'Y'          OFFICE IN ACCT                               
         BNE   DEP6                                                             
         ZIC   RF,BCOFFLEN                                                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),ANAOFF      OFFICE                                       
         LA    R1,1(RF,R1)                                                      
*                                                                               
DEP6     ZIC   RF,BCDPTLEN                                                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),DEPT        DEPARTMENT                                   
*                                                                               
DEP8     SR    R6,R6                                                            
         BAS   RE,GETACC                                                        
         BAS   RE,CHECKACC                                                      
         MVC   DEPNAME,ACCTNAME                                                 
         MVC   DEPNUM,ACCTNUM                                                   
         MVC   DEPSTFN,ACCTNAME                                                 
*                                                                               
* VALIDATE 28 ACCOUNT                                                           
*                                                                               
DEP10    MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCULA,POSTACC       SET EXPENSE ACCOUNT                       
         MVC   ACTKUNT(2),=C'28'                                                
         BAS   RE,GETACC                                                        
         BAS   RE,CHECKACC                                                      
         MVC   CRDSNUM,ACCTNUM                                                  
         MVC   CRDSNAME,ACCTNAME                                                
*                                                                               
DEPX     B     CURSIT                                                           
         EJECT                                                                  
*                                                                               
*--------------------------------------------------------------                 
*        VALIDATE COST ACCOUNTING ACCOUNTS                                      
*--------------------------------------------------------------                 
*                                                                               
COST     NTR1  ,                                                                
*                                                                               
* VALIDATE 1P ACCOUNT                                                           
*                                                                               
         LA    R4,KEY                                                           
         USING ACTRECD,R4                                                       
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(2),=C'1P'   SET UP 1/P ACCOUNT                           
         TM    BCCPYST5,CPYSNCST   TEST NEW COST ACCOUNTING                     
         BZ    *+14                NO                                           
         MVC   ACTKACT,=C'999999999999'                                         
         B     COST4                                                            
*                                                                               
COST1    LA    RF,ACTKACT                                                       
*                                                                               
         CLI   OFFSW,C'N'                                                       
         BE    COST2                                                            
         ZIC   R1,BCOFFLEN                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),ANAOFF      OFFICE                                       
         LA    RF,1(R1,RF)                                                      
*                                                                               
COST2    LA    R3,=C'9999'         DEFAULT DEPT                                 
         CLI   DEPSW,C'Y'                                                       
         BNE   *+8                                                              
         LA    R3,DEPT             OR REAL DEPT                                 
*                                                                               
         ZIC   R1,BCDPTLEN                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),0(R3)                                                    
         LA    RF,1(R1,RF)                                                      
         MVC   0(1,RF),COSTANAL                                                 
*                                                                               
COST4    SR    R6,R6               NO PROFILES                                  
         BAS   RE,GETACC                                                        
         BAS   RE,CHECKACC                                                      
         MVC   CRCNUM,ACCTNUM                                                   
         MVC   CRCNAME,ACCTNAME                                                 
*                                                                               
COSTX    B     CURSIT                                                           
         EJECT                                                                  
*                                                                               
*--------------------------------------------------------------                 
*        VALIDATE STAFF ANALYSIS ACCOUNTS--ASSUMES 2P LEDGER                    
*        HAS ALREADY BEEN READ AND LEVEL=2P LEDGER LEVELS                       
*--------------------------------------------------------------                 
*                                                                               
STF      NTR1  ,                                                                
*                                                                               
* EXTRACT STAFF CODE                                                            
*                                                                               
         L     R2,ASTFH                                                         
         MVC   FVMAXL,STAFFL       SET MAXIMUM LENGTH                           
         MVI   FVMINL,1            REQUIRE SOMETHING                            
         GOTO1 AFVAL,(R2)                                                       
         BNE   ERRXIT                                                           
         MVC   STAFF,FVIFLD                                                     
*                                                                               
* CONSTRUCT AND VALIDATE 2P ACCOUNT KEY                                         
*                                                                               
STF1     LA    R4,KEY                                                           
         USING ACTRECD,R4                                                       
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(2),=C'2P'                                                
         LA    R1,ACTKACT                                                       
*                                                                               
         CLI   LEVEL,3             ONLY MOVE OFFICE FOR 3 LEVEL ACCTS           
         BL    STF2                                                             
         ZIC   RF,BCOFFLEN                                                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),ANAOFF      MOVE ANALYSIS OFFICE INTO KEY                
         LA    R1,1(RF,R1)         BUMP TO NEXT SPOT IN KEY                     
*                                                                               
STF2     CLI   LEVEL,2                                                          
         BL    STF4                                                             
         ZIC   RF,BCDPTLEN                                                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),DEPT                                                     
         LA    R1,1(RF,R1)                                                      
*                                                                               
STF4     ZIC   RF,STAFFL                                                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),STAFF       MOVE STAFF INTO KEY                          
*                                                                               
STF6     SR    R6,R6               NO PROFILES                                  
         BAS   RE,GETACC                                                        
         BAS   RE,CHECKACC                                                      
         MVC   STAFFNUM,ACCTNUM                                                 
         MVC   STAFFNAM,ACCTNAME                                                
         MVC   DEPSTFN,ACCTNAME                                                 
*                                                                               
* CONSTRUCT AND VALIDATE 29 ACCOUNT KEY                                         
*                                                                               
STF8     MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(2),=C'29'   SET UP 2/9 ACCOUNT                           
         MVC   ACTKACT,=12C'9'     DEFAULT IS 999999 ETC.                       
*                                                                               
STF10    OC    COSTNUM,COSTNUM                                                  
         BZ    *+10                                                             
         MVC   ACTKACT,COSTNUM+3   OR USE COSTING                               
         BAS   RE,GETACC                                                        
         MVC   FVMSGNO,=AL2(AE$INACP)                                           
         BAS   RE,CHECKACC                                                      
         MVC   CRPSNUM,ACCTNUM                                                  
         MVC   CRPSNAME,ACCTNAME                                                
*                                                                               
STFX     B     CURSIT                                                           
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*----------------------------------------------------------------               
* VALIDATE WORKCODE--CHECK ERRXIT EXITS--AT ENTRY, R2=A(WC FIELD)               
*----------------------------------------------------------------               
*                                                                               
VALWC    NTR1  ,                                                                
         MVI   FVMINL,2                                                         
         GOTO1 AFVAL,(R2)                                                       
         BNE   ERRXIT                                                           
*                                                                               
         L     R4,AGOBLOCK                                                      
         USING GOBLOCKD,R4                                                      
         MVI   PASSCD,C'Y'                                                      
         TM    ACOPSTAT,ACOXJOB    IS THIS AN X-JOB?                            
         BO    VALWC1              YES, LEAVE PASSCD AS Y                       
         MVI   PASSCD,C'N'                                                      
         CLI   COCDPASS,C'N'       IS COMP. KEEPING ALL CD'S                    
         BE    *+10                YES                                          
         MVC   PASSCD,GOCLICD                                                   
*                                                                               
VALWC1   LA    RF,GOUWLIST                                                      
         LA    R0,6                                                             
VALWC2   CLC   FVIFLD(2),0(RF)     CHK FOR MATCH ON NON-BILLABLE WC'S           
         BE    ERRXIT                                                           
         LA    RF,2(RF)                                                         
         BCT   R0,*-14                                                          
         DROP  R4                                                               
*                                                                               
*READ ANALYSIS RECORD FOR VALID WORK CODE                                       
*                                                                               
VALWC4   GOTO1 AGETWC,FVIFLD                                                    
         BNE   ERRXIT                                                           
         MVC   SVWRKNAM,BCSPACES                                                
         MVC   SVWRKNAM(L'ACANDESC),WORK                                        
         MVC   SVWRKCD(2),FVIFLD                                                
         MVC   FLD(L'SVWRKNAM),SVWRKNAM                                         
         L     R2,AWCNH            R2=A(WORKCODE NAME FIELD)                    
         BAS   RE,MOVEFLD                                                       
*                                                                               
VALWCX   B     CURSIT                                                           
         EJECT                                                                  
*                                                                               
*--------------------------------------------------------------                 
*        VALIDATE PRODUCTION CASH DISCOUNT INCOME ACCOUNT                       
*--------------------------------------------------------------                 
*                                                                               
VALCD    NTR1  ,                                                                
         MVC   KEY,SPACES                                                       
         MVI   KEY,X'09'           READ MEDIA RECORD                            
         MVC   KEY+1(1),COMPANY                                                 
         MVC   KEY+2(1),SJJOBNUM+9 1ST BYTE OF JOB IS MEDIA CODE                
         BAS   RE,HIGH                                                          
         CLC   KEY(15),KEYSAVE                                                  
         BE    VALCD2                                                           
         MVC   FVMSGNO,=AL2(FVFSET)                                             
         MVC   BASMSG,=CL60'ERRXIT-MISSING MEDIA RECORD'                        
         B     ERRXIT                                                           
*                                                                               
VALCD2   MVI   ELCODE,ACMDELQ                                                   
         L     R3,AIOAREA1                                                      
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ACMEDIAD,R3                                                      
         MVC   KEY,SPACES                                                       
         MVC   KEY(L'ACMDCSHD),ACMDCSHD                                         
         CLC   ACMDCSHD,SPACES                                                  
         BH    VALCD10                                                          
*                                                                               
VALCD4   DS    0H                                                               
         MVC   KEY,SPACES          READ LEDGER RECORD                           
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),=C'SJ'     PRODUCTION LEDGER                            
         BAS   RE,HIGH                                                          
         CLC   KEY(15),KEYSAVE                                                  
         BE    VALCD6                                                           
         MVC   FVMSGNO,=AL2(FVFSET)                                             
         MVC   BASMSG,=CL60'ERRXIT-MISSING SJ LEDGER RECORD'                    
         B     ERRXIT                                                           
*                                                                               
VALCD6   MVI   ELCODE,ACLTELQ      GET COMMISSION ACCT FROM LEDGER REC          
         L     R3,AIOAREA1                                                      
         BAS   RE,GETEL                                                         
         BNE   VALCD8              USE DEFAULT OF SIMD                          
         USING ACLEDGD,R3                                                       
         CLC   ACLTCDAC,SPACES                                                  
         BNH   VALCD8                                                           
*                                                                               
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(14),ACLTCDAC                                               
         B     VALCD10                                                          
*                                                                               
VALCD8   MVC   KEY,SPACES          ELSE POST TO INCOME                          
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(4),=C'SIMD'                                                
*                                                                               
VALCD10  L     R2,AAMTH            A(CURRENT AMOUNT HEADER)                     
         SR    R6,R6                                                            
         BAS   RE,GETACC                                                        
         BAS   RE,CHECKACC                                                      
         MVC   SIMDAC,ACCTNUM                                                   
         MVC   SIMDACN,ACCTNAME                                                 
*                                                                               
VALCDX   B     CURSIT                                                           
         EJECT                                                                  
* VALIDATE EXPENSE CASH DISCOUNT INCOME ACCOUNT--CHECK ERROR EXITS              
*                                                                               
VALECD   NTR1  ,                                                                
         L     R2,AAMTH            DEFAULT COMMISSION ACCT                      
*                                                                               
         MVC   KEY,SPACES          READ LEDGER RECORD                           
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(1),=C'S'                                                   
         LA    RF,COMPEL                                                        
         USING ACCOMPD,RF                                                       
         MVC   KEY+2(1),ACMPSUPX   EXPENSE VENDOR FROM CO REC                   
         DROP  RF                                                               
*                                                                               
         CLC   ATRACC2(1),=C'*'                                                 
         BNE   *+10                                                             
         MVC   KEY+1(2),ATRACC2+1  OVERRIDE ACCT                                
*                                                                               
         BAS   RE,HIGH                                                          
         CLC   KEY(15),KEYSAVE                                                  
         BE    VALECD2                                                          
         MVC   FVMSGNO,=AL2(FVFSET)                                             
         MVC   BASMSG,=CL60'ERRXIT-MISSING EXPENSE LEDGER RECORD'               
         MVC   BASMSG+28(2),0(R1)                                               
         B     ERRXIT                                                           
*                                                                               
VALECD2  MVI   ELCODE,ACLTELQ      GET COMMISSION ACCT FROM LEDGER REC          
         L     R3,AIOAREA1                                                      
         BAS   RE,GETEL                                                         
         BNE   VALECD4                                                          
         USING ACLEDGD,R3                                                       
         CLC   ACLTCDAC,SPACES                                                  
         BNH   VALECD4                                                          
*                                                                               
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(14),ACLTCDAC                                               
         SR    R6,R6                                                            
         BAS   RE,GETACC                                                        
         BAS   RE,CHECKACC                                                      
         MVC   SIMDAC,ACCTNUM      CD ACCT TAKEN FROM LEDGER RECORD             
         MVC   SIMDACN,ACCTNAME                                                 
         B     VALECDX                                                          
         DROP  R3                                                               
*                                                                               
VALECD4  MVC   KEY(15),SPACES                                                   
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(4),=C'SIMD'                                                
         SR    R6,R6                                                            
         BAS   RE,GETACC                                                        
         BAS   RE,CHECKACC                                                      
         MVC   SIMDAC,ACCTNUM                                                   
         MVC   SIMDACN,ACCTNAME                                                 
*                                                                               
VALECDX  B     CURSIT                                                           
         EJECT                                                                  
*                                                                               
*--------------------------------------------------------------                 
*        PERFORM 1C ACCOUNT OVERRIDE--AT ENTRY, COSTNUM=A/C CODE                
*        ON EXIT, R1,R3 ARE CLOBBERED AND COSTNUM=A/C CODE                      
*--------------------------------------------------------------                 
*                                                                               
SET1C    ST    RE,DUB                                                           
         OC    POSTCNTR,BCSPACES                                                
         LA    R3,COSTNUM+7        R3=A(REPLACEMENT POSITION POINTER)           
         CLI   POSTCPOS,0          TEST COST POSITION OVERRIDE                  
         BE    SET1C2              NO                                           
*                                                                               
         ZIC   R3,POSTCPOS         YES                                          
         LA    R3,COSTNUM+2(R3)                                                 
*                                                                               
SET1C2   LA    R1,POSTCNTR         R1=A(OVERRIDE COST CENTER)                   
         LA    RE,3                RE=LOOP COUNTER                              
*                                                                               
SET1C4   CLI   0(R1),C' '          TEST FOR OVERRIDE CHARACTER                  
         BE    *+10                NO                                           
         MVC   0(1,R3),0(R1)                                                    
         LA    R1,1(R1)                                                         
         LA    R3,1(R3)                                                         
         BCT   RE,SET1C4                                                        
*                                                                               
SET1CX   L     RE,DUB                                                           
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
*--------------------------------------------------------------                 
*        EDIT AMOUNT FIELD--AT ENTRY, R2=A(FLDH)                                
*--------------------------------------------------------------                 
*                                                                               
EDAMT    NTR1  ,                                                                
         MVI   FVMINL,1            REQUIRE SOME INPUT                           
         L     R2,AAMTH                                                         
         GOTO1 AFVAL,(R2)                                                       
         BNE   ERRXIT                                                           
*                                                                               
EDAMT2   ZIC   R0,FVILEN           LENGTH OF AMOUNT INPUT                       
         GOTO1 CASHVAL,DMCB,(X'82',FVIFLD),(R0)                                 
         MVC   FVMSGNO,=AL2(AE$INAMT)                                           
         CLI   0(R1),0                                                          
         BNE   ERRXIT                                                           
         ZAP   INAMNT,4(8,R1)      SAVE CURRENT AMOUNT                          
*                                                                               
EDAMT4   AP    CSHTOT,INAMNT       ACCUM AMMOUNT                                
*                                                                               
EDAMTX   B     CURSIT                                                           
         EJECT                                                                  
*--------------------------------------------------------------                 
*        ERROR ROUTINE                                                          
*--------------------------------------------------------------                 
*                                                                               
*              R2=A(INVALID FIELD)                                              
*              R3=A(ERRXIT MESSAGE)                                             
*                                                                               
MYERR    DS    0H                                                               
         MVC   FVMSGNO,=AL2(FVFSET)                                             
         MVC   BASMSG,SPACES                                                    
         MVC   BASMSG(L'ERRMSG),0(R3)                                           
         B     ERRXIT                                                           
         SPACE 3                                                                
         GETEL R3,DATADISP,ELCODE                                               
         SPACE 2                                                                
* AT ENTRY, R2=A(CLIENT FIELD HEADER)                                           
*                                                                               
SETLIN   ST    RE,FULL                                                          
         LA    R0,NDATAFLD                                                      
         LA    R1,ACLIH                                                         
         LA    RE,DISPTAB                                                       
SETLIN2  LA    RF,0(R2)            GET A(FIRST FIELD)                           
         A     RF,0(RE)            ADD IN DISPLACEMENT                          
         ST    RF,0(R1)                                                         
         LA    R1,4(R1)            NEXT POINTER                                 
         LA    RE,4(RE)            NEXT DISPLACEMENT                            
         BCT   R0,SETLIN2                                                       
         L     RE,FULL                                                          
         BR    RE                                                               
         SPACE 2                                                                
BUMP     ZIC   RF,0(R2)            BUMP TO NEXT HEADER                          
         AR    R2,RF                                                            
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO MOVE OUT DISPLAY DATA                                          
*                                                                               
MOVEFLD  ST    RE,FULL                                                          
         ZIC   R1,0(R2)                                                         
         LA    RE,8+1                                                           
         TM    1(R2),X'02'         TEST FOR EXTENDED FIELD                      
         BZ    *+8                                                              
         LA    RE,8+8+1                                                         
         SR    R1,RE                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),FLD                                                      
*                                                                               
MOVEFLDX L     RE,FULL                                                          
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
* US SCREEN TABLE  ( BYTES 0-3=DISP TO FIELD, BYTES 4-7=DISP TO ADCON )         
*                                                                               
FSTTAB   DS    0D                                                               
         DC    AL4(ATRDOCH-TWAD),AL4(ADOCH-PROGD)                               
         DC    AL4(ATRDAT1H-TWAD),AL4(ADATH-PROGD)                              
         DC    AL4(ATRCSHH-TWAD),AL4(ACSHH-PROGD)                               
         DC    AL4(ATRACCH-TWAD),AL4(AVENH-PROGD)                               
         DC    AL4(ATRNARH-TWAD),AL4(ANARH-PROGD)                               
         DC    AL4(ATRCLIH-TWAD),AL4(ADETH-PROGD)                               
         DC    AL4(ATRTOTH-TWAD),AL4(ATOTH-PROGD)                               
         DC    X'FF'                                                            
         SPACE 2                                                                
SECTAB   DS    0D                                                               
         DC    AL4(ATSDOCH-TWAD),AL4(ADOCH-PROGD)                               
         DC    AL4(ATSDAT1H-TWAD),AL4(ADATH-PROGD)                              
         DC    AL4(ATSCSHH-TWAD),AL4(ACSHH-PROGD)                               
         DC    AL4(ATSACCH-TWAD),AL4(AVENH-PROGD)                               
         DC    AL4(0),AL4(ANARH-PROGD)                                          
         DC    AL4(ATSCLIH-TWAD),AL4(ADETH-PROGD)                               
         DC    AL4(0),AL4(ATOTH-PROGD)                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
FLDTAB   DS    0F                                                               
         DC    Y(ATRDOCH-TWAD),Y(ATSDOCH-TWAD)                                  
         DC    Y(ATRDAT1H-TWAD),Y(ATSDAT1H-TWAD)                                
         DC    Y(ATRCSHH-TWAD),Y(ATSCSHH-TWAD)                                  
         DC    Y(ATRACCH-TWAD),Y(ATSACCH-TWAD)                                  
         DC    Y(ATRACC2H-TWAD),Y(ATSACC2H-TWAD)                                
         DC    X'FF'                                                            
         SPACE 2                                                                
* TABLE OF DISPLACEMENTS FOR FIELDS IN EACH DETAIL LINE                         
*                                                                               
DISPTAB  DS    0A                                                               
         DC    AL4(ATRCLIH-ATRCLIH)                                             
         DC    AL4(ATRPROH-ATRCLIH)                                             
         DC    AL4(ATRJOBH-ATRCLIH)                                             
         DC    AL4(ATREXH-ATRCLIH)                                              
         DC    AL4(ATRNAM1H-ATRCLIH)                                            
         DC    AL4(ATRWCH-ATRCLIH)                                              
         DC    AL4(ATRNWCH-ATRCLIH)                                             
         DC    AL4(ATRWCNH-ATRCLIH)                                             
         DC    AL4(ATRAMH-ATRCLIH)                                              
         DC    AL4(ATRFOFFH-ATRCLIH)                                            
         DC    AL4(ATRCOFFH-ATRCLIH)                                            
         DC    AL4(ATRAOFFH-ATRCLIH)                                            
         DC    AL4(ATRDEPTH-ATRCLIH)                                            
         DC    AL4(ATRSTFH-ATRCLIH)                                             
         DC    AL4(ATRNAM2H-ATRCLIH)                                            
         DC    AL4(ATRCLI2H-ATRCLIH)                                            
         EJECT                                                                  
*--------------------------------------------------------------                 
*        ACBATCODE                                                              
*--------------------------------------------------------------                 
*                                                                               
       ++INCLUDE ACBATCODE                                                      
         EJECT                                                                  
*--------------------------------------------------------------                 
*        LITERAL DECLARATIONS                                                   
*--------------------------------------------------------------                 
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------                 
*              ERRXIT MESSAGE TABLE                                             
*--------------------------------------------------------------                 
*                                                                               
ERRMSG   DS    0CL42                                                            
SCRERAS  DC    CL42'SCREEN ERASED'                                              
FSTREST  DC    CL42'FIRST SCREEN RESTORED'                                      
         EJECT                                                                  
*--------------------------------------------------------------                 
*        BUILD 64 ELEMENT                                                       
*--------------------------------------------------------------                 
*                                                                               
PPOST    DS    0D                                                               
         NMOD1 0,*PPOST*,R7                                                     
         L     RC,0(R1)                                                         
         LA    R2,IOAREA           CLEAR FOR CURRENT DTL LINE ELEMENTS          
         LA    R3,1000                                                          
         LA    R1,0                                                             
         MVCL  R2,R0                                                            
         LA    R8,IOAREA+2                                                      
         USING DLDESCD,R8                                                       
         MVI   DLDSEL,DLDSELQ                                                   
         MVC   DLDSREF,SVDOC                                                    
         MVC   DLDSDATE,SVDATE1                                                 
         MVI   DLDSSBRF,0                                                       
         MVI   DLDSSTAT,0                                                       
         OI    DLDSSTAT,X'08'      AUTHORIZED INVOICE INDICATION                
         CLI   SVURG,C'U'                                                       
         BNE   PPOST02                                                          
         OI    DLDSSTAT,X'40'                                                   
*                                                                               
PPOST02  SR    R1,R1               GET NARRATIVE LENGTH                         
         ICM   R1,3,SVNARLEN                                                    
         BZ    *+6                                                              
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   DLDSNARR(0),SVNAR                                                
         LA    R5,DLDSNARR-DLDESCD  LEN OF NARRATIVE-ELEMENT                    
         AH    R5,SVNARLEN         0 OR LEN RETURNED FROM NARRSCAN              
         STC   R5,DLDSLEN                                                       
         AR    R8,R5               ADVANCE A(IOWORK LENGTH OF ELEMENT)          
*                                                                               
         CLC   SVREF,BCSPACES      TEST FOR EXTRA REFERENCE NUMBER              
         BE    PPOST04             NO                                           
*                                                                               
         USING OTHELD,R8                                                        
         MVI   OTHEL,OTHELQ                                                     
         MVI   OTHLN,OTHLN1Q                                                    
         MVC   OTHNUM(OTHLN1Q-2),BCSPACES                                       
         MVC   OTHNUM(L'SVREF),SVREF                                            
         ZIC   R1,OTHLN                                                         
         AR    R8,R1                                                            
*                                                                               
PPOST04  TM    ACOPSTAT,ACOXJOB    TEST FOR XJOB                                
         BZ    PPOST06             NO                                           
*                                                                               
         USING SPDELD,R8                                                        
         MVI   SPDEL,SPDELQ        ADD ELEMENT WITH REAL CAC                    
         MVI   SPDLN,SPDLN1Q+L'ACTKCULA-1                                       
         MVC   SPDACCS(L'ACTKCULA-1),SVCSHNUM                                   
         OC    SVCRACCT,SVCRACCT   TEST FOR VENDOR                              
         BZ    *+18                NO-MUST USE CASH                             
         TM    BCCPYST3,CPYSCA22   TEST TO USE CASH AS CAC(EXPENSE BIT)         
         BO    *+10                YES                                          
         MVC   SPDACCS(L'ACTKCULA-1),SVCRACCT+1 NO-USE VENDOR                   
         ZIC   R1,SPDLN                                                         
         AR    R8,R1                                                            
*                                                                               
PPOST06  CP    CDAMNT,=P'0'        IS THERE DISCOUNT                            
         BE    PPOST08             NO                                           
         CLI   SVCSHNUM,0          IS THERE A CASH ACCT?                        
         BNE   PPOST08             YES - NO CD ALLOWED                          
         CLI   PASSCD,C'N'         SKIP CD IF WE KEEP IT                        
         BE    PPOST08                                                          
*                                                                               
         USING TRCASHD,R8                                                       
         ZAP   TRCSAMNT,CDAMNT     CD ELEMENT FOR JOB                           
         MVI   TRCSEL,TRCSELQ                                                   
         MVI   TRCSLEN,TRCSLNQ1                                                 
         MVI   TRCSTYPE,C'D'                                                    
         ZIC   R5,TRCSLEN                                                       
         AR    R8,R5                                                            
*                                                                               
         USING SCIELD,R8                                                        
PPOST08  TM    ACOPSTAT,ACOXJOB    TEST FOR XJOB                                
         BZ    PPOST14             NO                                           
*                                                                               
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN1Q                                                    
         MVI   SCITYPE,SCITSJXP                                                 
         ZAP   SCIAMNT,INAMNT      CARRY FINANCIAL POSTING AMT                  
         CP    CDAMNT,=P'0'        IS THERE A DISCOUNT?                         
         BE    PPOST10             NO                                           
         CLI   SVCSHNUM,0          YES, IS THERE A CASH ACCOUNT?                
         BNE   PPOST10             YES, CD NOT ALLOWED                          
         SP    SCIAMNT,CDAMNT                                                   
*                                                                               
PPOST10  ZIC   R5,SCILN                                                         
         AR    R8,R5                                                            
*                                                                               
         LA    R1,TRSELEM                                                       
         USING TRSELD,R1                                                        
         MVI   TRSEL,TRSELQ        BUILD SKELETAL TRANS STATUS ELEM             
         MVI   TRSLN,TRSLNQ                                                     
         OI    TRSSTAT3,TRSSXJOB   SET X-JOB BIT ON                             
         MVC   0(L'TRSELEM,R8),TRSELEM                                          
         LA    R8,L'TRSELEM(R8)                                                 
         DROP  R1                                                               
                                                                                
PPOST14  CLI   SVCSHNUM,0          IS THERE A CASH ACCOUNT?                     
         BNE   PPOST18             YES, NO PAYABLE POSTING THEN                 
         CLC   SVCRACCT+1(2),=C'SW'                                             
         BE    PPOST16                                                          
         CLC   SVCRACCT+1(2),=C'SX'                                             
         BE    PPOST16                                                          
         CLC   SVCRACCT+1(2),=C'SY'                                             
         BE    PPOST16                                                          
         CLC   SVCRACCT+1(2),=C'SV'                                             
         BNE   PPOST18                                                          
*                                                                               
         USING PAKELD,R8                                                        
PPOST16  MVI   PAKEL,PAKELQ        PAYABLE ACCOUNT ELEMENT                      
         MVI   PAKLN,PAKLNQ                                                     
         MVC   PAKACC,SVCRACCT                                                  
         MVC   PAKOFF,SJOFFC                                                    
         OC    CRDOFF,CRDOFF                                                    
         BZ    *+10                                                             
         MVC   PAKOFF,CRDOFF                                                    
         MVC   PAKCON,SJCLINUM                                                  
         CLC   SVCRACCT+1(2),=C'SX'                                             
         BNE   *+10                                                             
         MVC   PAKCON,SJJOBNUM                                                  
         MVC   PAKDATE,SVDATE1                                                  
         MVC   PAKREF,SVDOC                                                     
         ZIC   R5,PAKLN                                                         
         AR    R8,R5                                                            
                                                                                
PPOST18  BRAS   RE,ADDADB                                                       
         EJECT                                                                  
*--------------------------------------------------------------                 
*        BUILD 69 DEBIT ELEMENT                                                 
*--------------------------------------------------------------                 
*                                                                               
         USING DLPOSTD,R8                                                       
         MVI   DLPSEL,DLPSEDRQ     DEBIT JOB                                    
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSANAL,SPACES                                                  
         MVC   DLPSDBAC,SJJOBNUM   DEBIT JOB - SJ                               
         MVC   DLPSDBNM,SJJOBNAM                                                
         MVC   DLPSCRAC,SVCRACCT   CONTRA = PROD VENDOR                         
         MVC   DLPSCRNM,SVACCTNM                                                
*                                                                               
         CLI   SVCRACCT,0          IS THERE A VENDOR?                           
         BE    PPOST20             NO - MUST USE CASH                           
         CLI   SVCSHNUM,0          DO WE HAVE CASH ACCT?                        
         BE    PPOST22             NO.                                          
         TM    COMPSTA2,X'02'      DO WE WANT CONTRA TO BE CASH?                
         BZ    PPOST22             NO                                           
*                                                                               
PPOST20  MVC   DLPSCRAC,SVCSHNUM   CASH ACCT NUMBER.                            
         MVC   DLPSCRNM,SVCSHNAM   CASH ACCT NAME.                              
*                                                                               
PPOST22  TM    ACOPSTAT,ACOXJOB    TEST XJOB                                    
         BZ    PPOST24                                                          
         L     R3,AEXCELD                                                       
         USING EXCELD,R3                                                        
         MVC   DLPSCRAC,EXCSEAC    SET CAC=EXPENSE JOB                          
         MVC   DLPSCRNM,EXCSENM                                                 
         DROP  R3                                                               
*                                                                               
PPOST24  MVI   DLPSTYPE,0                                                       
         CLI   COMSW,C'Y'                                                       
         BE    PPOST26                                                          
         OI    DLPSTYPE,X'40'      SET FOR NO COMMISSION                        
                                                                                
PPOST26  MVC   DLPSANAL,SVWRKCD                                                 
*                                                                               
         ZAP   DLPSAMNT,INAMNT     NET                                          
         CP    CDAMNT,=P'0'        IS DISCOUNT = 0                              
         BE    PPOST28             YES                                          
         CLI   SVCSHNUM,0          IS THERE A CASH ACCT?                        
         BNE   PPOST28             YES - NO CD ALLOWED                          
         CLI   PASSCD,C'N'         DO WE PASS CD ALONG?                         
         BE    PPOST28             NO                                           
         SP    DLPSAMNT,CDAMNT     NET - CD                                     
                                                                                
PPOST28  ZIC   R4,DLPSLEN                                                       
         TM    ACOPSTAT,ACOXJOB    TEST FOR XJOB                                
         BZ    *+10                                                             
         ZAP   DLPSAMNT,=P'0'      YES-MAKE A ZERO FINANCIAL POSTING            
         AR    R8,R4                                                            
         EJECT                                                                  
*--------------------------------------------------------------                 
*        BUILD OTHERS ELEMENT FOR PRD AND JOB                                   
*--------------------------------------------------------------                 
*                                                                               
         USING ACOTHERD,R8                                                      
         MVC   ACOTEL(2),=X'230F'  BUILD 'OTHERS' ELEMENT FOR                   
         MVC   ACOTNUM(13),SPACES  PRODUCT AND JOB                              
         MVC   ACOTNUM(6),PRODCODE                                              
         MVC   ACOTNUM+6(6),JOBNUM                                              
         ZIC   R3,ACOTLEN                                                       
         AR    R8,R3                                                            
         SPACE 2                                                                
*--------------------------------------------------------------                 
*        BUILD 50 CD EL FOR SUPPLIER                                            
*--------------------------------------------------------------                 
*                                                                               
         CP    CDAMNT,=P'0'                                                     
         BE    PPOST30             CD FOR SUPPLIER                              
         CLI   SVCSHNUM,0          IS THERE A CASH ACCT?                        
         BNE   PPOST30             YES - NO CD ALLOWED                          
         USING TRCASHD,R8                                                       
         ZAP   TRCSAMNT,CDAMNT     CD ELEMENT FOR SUPPLIER                      
         MVI   TRCSEL,TRCSELQ                                                   
         MVI   TRCSLEN,TRCSLNQ1                                                 
         MVI   TRCSTYPE,C'D'                                                    
         ZIC   R3,TRCSLEN                                                       
         AR    R8,R3                                                            
*                                                                               
PPOST30  TM    ACOPSTAT,ACOXJOB                                                 
         BZ    PPOST32                                                          
         MVC   0(L'TRSELEM,R8),TRSELEM                                          
         LA    R8,L'TRSELEM(R8)                                                 
*                                                                               
         USING EXCELD,R3                                                        
         USING SPDELD,R8                                                        
         L     R3,AEXCELD                                                       
         MVI   SPDEL,SPDELQ        ADD ELEMENT WITH EXPENSE                     
         MVI   SPDLN,SPDLN1Q+L'EXCSEAC-1                                        
         MVC   SPDACCS(L'EXCSEAC-1),EXCSEAC                                     
         ZIC   R1,SPDLN                                                         
         AR    R8,R1                                                            
         DROP  R3,R8                                                            
*                                                                               
PPOST32  BRAS   RE,ADDADB                                                       
*                                                                               
         USING DLPOSTD,R8                                                       
         MVI   DLPSEL,DLPSECRQ     CREDIT SUPPLIER                              
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVI   DLPSTYPE,0                                                       
         MVC   DLPSDBAC,SJCLINUM   CONTRA = PROD VENDOR                         
         MVC   DLPSDBNM,SJCLINAM                                                
         MVC   DLPSCRAC,SVCRACCT   CR JOB - SJ                                  
         MVC   DLPSCRNM,SVACCTNM                                                
         CLC   SVCRACCT+1(2),=C'SX'                                             
         BNE   PPOST34                                                          
         MVC   DLPSDBAC,SJJOBNUM   OR CLI/PRD/JOB FOR EXPENSE VENDORS           
         MVC   DLPSDBNM,SJJOBNAM                                                
*                                                                               
PPOST34  CLI   SVCSHNUM,0          IS THERE A CASH ACCOUNT?                     
         BE    PPOST36             NO.                                          
*                                                                               
         MVC   DLPSDBAC+6(3),SJPRONUM+6      IF CASH WE NEED PROD               
         MVC   DLPSCRAC,SVCSHNUM   CASH ACCT NUMBER.                            
         MVC   DLPSCRNM,SVCSHNAM   CASH ACCT NAME.                              
*                                                                               
PPOST36  MVC   DLPSANAL,SJOFFC                                                  
         OC    CRDOFF,CRDOFF       TEST FOR CREDIT OFFICE                       
         BZ    *+10                                                             
         MVC   DLPSANAL,CRDOFF     YES-USE IT FOR CREDIT POSTING                
         ZAP   DLPSAMNT,INAMNT                                                  
         CLI   SVCSHNUM,0          IS THERE A CASH ACCOUNT?                     
         BNE   PPOST38             YES - NO CD ALLOWED                          
         SP    DLPSAMNT,CDAMNT                                                  
                                                                                
PPOST38  ZIC   R3,DLPSLEN                                                       
         LR    R6,R8               BUILD CREDIT POSTING TO CD-INCOME AC         
         AR    R8,R3                                                            
*                                                                               
         CP    CDAMNT,=P'0'        DID VENDOR HAVE DISCT                        
         BE    PPOST40             NO                                           
         CLI   SVCSHNUM,0          IS THERE A CASH ACCT?                        
         BNE   PPOST40             YES - NO CD ALLOWED                          
         CLI   PASSCD,C'N'         CD FOR SUPPLIER-CLIENT MODE                  
         BNE   PPOST40                                                          
*                                                                               
         TM    ACOPSTAT,ACOXJOB                                                 
         BZ    *+14                                                             
         MVC   0(L'TRSELEM,R8),TRSELEM                                          
         LA    R8,L'TRSELEM(R8)                                                 
*                                                                               
         BRAS   RE,ADDADB                                                       
*                                                                               
         BCTR  R3,R0               SUBTRACT 1 FROM R3                           
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R8),0(R6)                                                    
         LA    R3,1(R3)                                                         
         MVC   DLPSCRAC,SIMDAC                                                  
         MVC   DLPSCRNM,SIMDACN                                                 
         MVC   DLPSDBAC,SJPRONUM   CLI/PROD AS CONTRA                           
         MVC   DLPSDBNM,SJPRONAM                                                
         ZAP   DLPSAMNT,CDAMNT                                                  
         ZIC   R3,DLPSLEN                                                       
         LR    R6,R8                                                            
         AR    R8,R3                                                            
*                                                                               
PPOST40  CLI   PCONSULT,C'Y'       SHOULD WE POST TO 2C?                        
         BNE   PPOST42             NO.                                          
*                                                                               
         USING DLPOSTD,R8                                                       
         TM    ACOPSTAT,ACOXJOB                                                 
         BZ    *+14                                                             
         MVC   DLPOSTD(L'TRSELEM),TRSELEM                                       
         LA    R8,L'TRSELEM(R8)                                                 
*                                                                               
         BRAS   RE,ADDADB                                                       
*                                                                               
         MVI   DLPSEL,DLPSEDCQ     DB & CR                                      
         MVI   DLPSLEN,DLPSLNQ     LENGTH                                       
         MVC   DLPSDBAC,P2CNUM     DB ACCT #                                    
         MVC   DLPSDBNM,P2CNAM     DB ACCT NAME                                 
         MVC   DLPSCRAC,PROTROL    CR ACCT #                                    
         MVC   DLPSCRNM,PROTROLN   CR ACCT NAME                                 
         OI    DLPSTYPE,X'80'                                                   
         MVC   DLPSANAL,SJOFFC     OFF                                          
         OC    CRDOFF,CRDOFF       TEST FOR CREDIT OFFICE OVERRIDE              
         BZ    *+10                                                             
         MVC   DLPSANAL,CRDOFF                                                  
         ZAP   DLPSAMNT,INAMNT     AMT                                          
         ZIC   R4,DLPSLEN          LEN                                          
         AR    R8,R4               INCREMENT                                    
*                                                                               
PPOST42  TM    ACOPSTAT,ACOXJOB    TEST XJOB                                    
         BZ    PPOST50             NO                                           
*                                                                               
         BRAS   RE,ADDADB                                                       
*                                                                               
         L     R3,AEXCELD                                                       
         USING EXCELD,R3                                                        
         MVI   EXCACT,EXCAPST      POST CALL                                    
         ST    R9,EXCAGWS                                                       
         ST    R8,EXCADAY                                                       
         MVC   EXCFINO,FINOFF                                                   
         OC    FINOFF,FINOFF       TEST FOR FINANCIAL OFFICE                    
         BNZ   *+10                                                             
         MVC   EXCFINO,SJOFFC                                                   
*                                                                               
         MVC   EXCSECAC,SVCRACCT   EXPENSE POSTING CAC                          
         MVC   EXCSECAN,SVACCTNM   AND CAC NAME                                 
         CLI   SVCRACCT,0          TEST FOR A VENDOR                            
         BE    PPOST44             NO-USE CASH ACCOUNT                          
         CLI   SVCSHNUM,0          TEST FOR CASH ACCOUNT                        
         BE    PPOST46             NO                                           
         TM    BCCPYST3,CPYSCA22   TEST EXPENSE BIT TO USE CASH AS CAC          
         BZ    PPOST46             NO                                           
*                                                                               
PPOST44  MVC   EXCSECAC,SVCSHNUM                                                
         MVC   EXCSECAN,SVCSHNAM                                                
*                                                                               
PPOST46  ZAP   EXCAMNT,INAMNT      SET POSTING AMOUNT                           
         CP    CDAMNT,=P'0'        TEST FOR CASH DISCOUNT                       
         BE    PPOST48             NO                                           
         CLI   SVCSHNUM,0          TEST POSTING TO CASH ACCOUNT                 
         BNE   PPOST48             YES                                          
         SP    EXCAMNT,CDAMNT      DEDUCT CD FROM POSTING                       
         ZAP   EXCDAMNT,CDAMNT                                                  
*                                                                               
PPOST48  GOTO1 VEXCEL,EXCELD                                                    
         BNE   POSTX                                                            
*                                                                               
         L     R8,EXCADAY                                                       
*                                                                               
PPOST50  MVI   0(R8),0             MARK END                                     
         LA    R8,1(R8)            END ADDR                                     
         LA    R3,IOAREA           START ADDR                                   
         SR    R8,R3                                                            
         STH   R8,HALF             TOTAL LEN                                    
         MVC   IOAREA(2),HALF      TO START OF REC                              
         BAS   RE,PUTDAYX          ADD RECORDS TO TRANSACTION FILE              
*                                                                               
         XC    WORK,WORK           ADD ENTRY TO TWA1                            
         ZIC   R1,SVDOCLEN                                                      
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   WORK(0),SVDOC       REF                                          
         L     R3,DMCB+8           RETURNED FROM PUTDAY                         
         MVC   WORK+10(4),0(R3)    DISK ADDRESS                                 
         ZAP   TRANSAMT,INAMNT                                                  
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         MVC   FVADDR,AAMTH        PRESET ERROR POSITION                        
         GOTO1 AADACDAY,BOPARM,IOAREA,BOPL61,BOWORK1                            
         CLC   FVMSGNO,=AL2(FVFOK) SET CC ON EXIT                               
*                                                                               
POSTX    XMOD1 1                   RETURN                                       
         DROP  R3                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------                 
*        ELS FOR AGENCY ACCRUED EXPENSES                                        
*--------------------------------------------------------------                 
*                                                                               
EPOST    NMOD1 0,*EPOST*,R7                                                     
         L     RC,0(R1)                                                         
         LA    R2,IOAREA           CLEAR FOR CURRENT DTL LINE ELEMENTS          
         LA    R3,1000                                                          
         LA    R1,0                                                             
         MVCL  R2,R0                                                            
         LA    R8,IOAREA+2                                                      
         USING DLDESCD,R8                                                       
         MVI   DLDSEL,DLDSELQ                                                   
         MVC   DLDSREF,SVDOC                                                    
         MVC   DLDSDATE,SVDATE1                                                 
         MVI   DLDSSBRF,0                                                       
         MVI   DLDSSTAT,0                                                       
         OI    DLDSSTAT,X'08'      AUTHORIZED INVOICE INDICATION                
         CLI   SVURG,C'U'                                                       
         BNE   EXP2                                                             
         OI    DLDSSTAT,X'40'                                                   
*                                                                               
EXP2     SR    R1,R1               GET NARRATIVE LENGTH                         
         ICM   R1,3,SVNARLEN                                                    
         BZ    *+6                                                              
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   DLDSNARR(0),SVNAR                                                
         LA    R5,DLDSNARR-DLDESCD  LEN OF NARRATIVE-ELEMENT                    
         AH    R5,SVNARLEN         0 OR LEN RETURNED FROM NARRSCAN              
         STC   R5,DLDSLEN                                                       
         AR    R8,R5               ADVANCE A(IOWORK LENGTH OF ELEMENT)          
*                                                                               
         CLC   SVREF,BCSPACES                                                   
         BE    EXP4                                                             
*                                                                               
         USING OTHELD,R8                                                        
         MVI   OTHEL,OTHELQ                                                     
         MVI   OTHLN,OTHLN1Q                                                    
         MVC   OTHNUM(OTHLN1Q-2),BCSPACES                                       
         MVC   OTHNUM(L'SVREF),SVREF                                            
         ZIC   R1,OTHLN                                                         
         AR    R8,R1                                                            
*                                                                               
EXP4     DS    0H                                                               
         CP    CDAMNT,=P'0'                                                     
         BE    EXP5                CD FOR SUPPLIER                              
         CLI   SVCSHNUM,0          IS THERE A CASH ACCT?                        
         BNE   EXP5                YES - NO CD ALLOWED                          
         TM    COMPSTAT,X'08'      PASS CD TO CLIENT? (CD IN COMP)              
         BNZ   EXP5                NO                                           
*                                                                               
         USING TRCASHD,R8                                                       
         ZAP   TRCSAMNT,CDAMNT     CD ELEMENT FOR SUPPLIER                      
         MVI   TRCSEL,TRCSELQ                                                   
         MVI   TRCSLEN,TRCSLNQ1                                                 
         MVI   TRCSTYPE,C'D'                                                    
         ZIC   R4,TRCSLEN                                                       
         AR    R8,R4                                                            
*                                                                               
EXP5     CLI   ANOELEM,0                                                        
         BE    *+14                                                             
         MVC   0(L'ANOELEM,R8),ANOELEM                                          
         LA    R8,L'ANOELEM(R8)                                                 
*                                                                               
         BRAS   RE,ADDADB                                                       
*                                                                               
         USING FFTELD,R8                                                        
         MVI   FFTEL,FFTELQ                                                     
         MVI   FFTLN,FFTLN1Q+L'FFTDLEN+L'FFTCOFF                                
         MVI   FFTTYPE,FFTTCOFF                                                 
         MVI   FFTSEQ,0                                                         
         MVI   FFTDLEN,L'FFTCOFF                                                
         MVC   FFTCOFF,CRDOFF                                                   
         OC    CRDOFF,CRDOFF                                                    
         BNZ   *+10                                                             
         MVC   FFTCOFF,FINOFF                                                   
         OC    FFTCOFF,SPACES                                                   
         ZIC   R4,FFTLN                                                         
         AR    R8,R4                                                            
*                                                                               
         USING DLPOSTD,R8                                                       
EXP6     MVI   DLPSEL,DLPSEDRQ     DEBIT                                        
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSDBAC(51),POSTACC       DEBIT ACCOUNT - DEF EXP               
         MVC   DLPSCRAC(51),SVCRACC2      CONTRA ACCOUNT - DEF VENDOR           
*                                                                               
         CLI   SVCRACC2,0          IS THERE A EXP VENDOR?                       
         BE    EXP8                NO - MUST USE CASH                           
*                                                                               
         CLI   SVCSHNUM,0          CASH ACCOUNT?                                
         BE    EXP10               NO                                           
*                                                                               
         TM    COMPSTA3,X'08'      CASH ACCT REQ?                               
         BZ    EXP10               NO..                                         
*                                                                               
EXP8     MVC   DLPSCRAC(51),SVCSHNUM      CASH ACCT # & NAME                    
*                                                                               
EXP10    MVC   DLPSANAL,FINOFF                                                  
         ZAP   DLPSAMNT,INAMNT                                                  
         CP    CDAMNT,=P'0'        IS DIC = 0                                   
         BE    EXP12               YES                                          
         CLI   SVCSHNUM,0          DO WE HAVE A CASH ACCT?                      
         BNE   EXP12               YES - NO CD ALLOWED                          
         TM    COMPSTAT,X'08'      PASS CD TO CLIENT?                           
         BNZ   EXP12               NO                                           
         SP    DLPSAMNT,CDAMNT     NET LESS C.D                                 
*                                                                               
EXP12    ZAP   EXPOST,DLPSAMNT     SAVE EXPENSE POSTING                         
         ZIC   R4,DLPSLEN                                                       
         AR    R8,R4                                                            
         EJECT                                                                  
*                                                                               
         CP    CDAMNT,=P'0'                                                     
         BE    EXP27               CD FOR SUPPLIER                              
         CLI   SVCSHNUM,0          DO WE HAVE CASH ACCT?                        
         BNE   EXP27               YES - NO CD ALLOWED                          
         TM    COMPSTAT,X'08'      PASS CD TO CLIENT?                           
         BNZ   EXP27               NO                                           
         USING TRCASHD,R8                                                       
         ZAP   TRCSAMNT,CDAMNT     CD ELEMENT FOR SUPPLIER                      
         MVI   TRCSEL,TRCSELQ                                                   
         MVI   TRCSLEN,TRCSLNQ1                                                 
         MVI   TRCSTYPE,C'D'                                                    
         ZIC   R4,TRCSLEN                                                       
         AR    R8,R4                                                            
*                                                                               
EXP27    DS    0H                                                               
         SPACE 1                                                                
EXP30    CLI   ANOELEM,0                                                        
         BE    *+14                                                             
         MVC   0(L'ANOELEM,R8),ANOELEM                                          
         LA    R8,L'ANOELEM(R8)                                                 
*                                                                               
         BRAS   RE,ADDADB                                                       
*                                                                               
         USING FFTELD,R8                                                        
         MVI   FFTEL,FFTELQ                                                     
         MVI   FFTLN,FFTLN1Q+L'FFTDLEN+L'FFTCOFF                                
         MVI   FFTTYPE,FFTTCOFF                                                 
         MVI   FFTSEQ,0                                                         
         MVI   FFTDLEN,L'FFTCOFF                                                
         MVC   FFTCOFF,FINOFF                                                   
         OC    FFTCOFF,SPACES                                                   
         ZIC   R4,FFTLN                                                         
         AR    R8,R4                                                            
*                                                                               
         USING DLPOSTD,R8                                                       
         MVI   DLPSEL,DLPSECRQ     CREDIT                                       
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSDBAC(51),POSTACC        CONTRA ACCOUNT - EXP                 
         MVC   DLPSCRAC(51),SVCRACC2       CREDIT ACCOUNT - VENDOR              
         MVC   DLPSANAL,FINOFF                                                  
         OC    CRDOFF,CRDOFF       TEST FOR CREDIT OFFICE                       
         BZ    *+10                NO                                           
         MVC   DLPSANAL,CRDOFF                                                  
         CLI   SVCSHNUM,0          IS THERE A CASH ACCT?                        
         BE    EXP34               NO.                                          
*                                                                               
         MVC   DLPSCRAC,SVCSHNUM   CASH ACCT #                                  
         MVC   DLPSCRNM,SVCSHNAM   CASH ACCT NAME                               
*                                                                               
EXP34    ZAP   DLPSAMNT,INAMNT     NET                                          
         CLI   SVCSHNUM,0          IS THERE A CASH ACCT?                        
         BNE   EXP36               YES - NO CD ALLOWED                          
         SP    DLPSAMNT,CDAMNT     NET - CD                                     
*                                                                               
EXP36    ZIC   R4,DLPSLEN                                                       
         AR    R8,R4                                                            
*                                                                               
         CP    CDAMNT,=P'0'        DID VENDOR HAVE DISCT                        
         BE    EXP40               NO                                           
         CLI   SVCSHNUM,0          IS THERE A CASH ACCT?                        
         BNE   EXP40               YES - NO CD ALLOWED                          
         TM    COMPSTAT,X'08'      DO WE PASS CD TO CLIENT?                     
         BZ    EXP40               YES                                          
*                                                                               
         CLI   ANOELEM,0                                                        
         BE    *+14                                                             
         MVC   0(L'ANOELEM,R8),ANOELEM                                          
         LA    R8,L'ANOELEM(R8)                                                 
*                                                                               
         BRAS   RE,ADDADB                                                       
*                                                                               
         MVI   DLPSEL,DLPSECRQ     CREDIT EXP WITH CD AMOUNT                    
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSDBAC,SVCRACC2                                                
         MVC   DLPSDBNM,SVACCTN2                                                
         MVC   DLPSCRAC,SIMDAC                                                  
         MVC   DLPSCRNM,SIMDACN                                                 
         MVI   DLPSTYPE,0                                                       
         ZAP   DLPSAMNT,CDAMNT                                                  
         MVC   DLPSANAL,FINOFF                                                  
         ZIC   R4,DLPSLEN                                                       
         LR    R6,R8                                                            
         AR    R8,R4                                                            
         B     EXP40                                                            
         EJECT                                                                  
*--------------------------------------------------------------                 
*        2P WITH 29 CONTRA POSTING                                              
*--------------------------------------------------------------                 
*                                                                               
EXP40    CLI   STFSW,C'Y'                                                       
         BNE   EXP50                                                            
*                                                                               
         CLI   ANOELEM,0                                                        
         BE    *+14                                                             
         MVC   0(L'ANOELEM,R8),ANOELEM                                          
         LA    R8,L'ANOELEM(R8)                                                 
*                                                                               
         BRAS   RE,ADDADB                                                       
*                                                                               
         USING DLPOSTD,R8                                                       
         MVI   DLPSEL,DLPSEDRQ     DEBIT STAFF                                  
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSDBAC,STAFFNUM   DEBIT 2P                                     
         MVC   DLPSDBNM,STAFFNAM                                                
         MVC   DLPSCRNM,CRPSNAME   CONTRA 29                                    
         ZAP   DLPSAMNT,EXPOST                                                  
         OI    DLPSTYPE,X'80'      SUBSIDIARY FROM HERE                         
         MVI   DLPSCRAC,C'*'       CONTRA IS *EXPENSE-CLIENT                    
         MVC   DLPSANAL,FINOFF                                                  
*                                                                               
         L     R2,AEXPH            CREDIT SE ACCT                               
         ZIC   R1,5(R2)            INPUT LENGTH                                 
         BCTR  R1,0                FOR EX INST.                                 
*                                                                               
         CLI   8(R2),C'*'                                                       
         BNE   *+8                                                              
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DLPSCRAC+1(0),POSTACC+3                                          
*                                                                               
         CLI   TENO,X'F0'          LENGTH OF ACCT WANTED                        
         BL    EXP42                                                            
         PACK  DUB,TENO                                                         
         CVB   R4,DUB                                                           
         MVC   DLPSCRAC+1(14),SPACES                                            
         L     R2,AEXPH                                                         
         LA    R6,1(R1)            INPUT LENGTH                                 
         SR    R6,R4               IF DESIRED LEN GREATER THAN GIVEN            
         BNM   *+6                                                              
         DC    H'0'                SHOULD HAVE CAUGHT THIS EARLIER              
         LA    R3,8(R6,R2)         R3=A(DESIRED PORTION ACCT INPUT)             
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   DLPSCRAC+1(0),0(R3) = * +DESIRED PORTION ACCT INPUT              
         LR    R1,R4                                                            
*                                                                               
EXP42    STC   R1,BYTE             LEN OF ACCT INPUT DESIRED                    
         LA    RF,DLPSCRAC+2(R1)                                                
         MVI   0(RF),C'-'                                                       
         LA    R1,DLPSCRAC+14                                                   
         SR    R1,RF                                                            
         BCTR  R1,R0                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   1(0,RF),CRPSNUM+3   2/9 ACCT KEY                                 
         MVC   WORK(15),DLPSCRAC   SAVE FOR DEBIT SIDE OF CRD CLI EL            
         ZIC   R4,DLPSLEN                                                       
         AR    R8,R4                                                            
         EJECT                                                                  
*--------------------------------------------------------------                 
*        29 WITH 2P CONTRA POSTING                                              
*--------------------------------------------------------------                 
*                                                                               
         CLI   ANOELEM,0                                                        
         BE    *+14                                                             
         MVC   0(L'ANOELEM,R8),ANOELEM                                          
         LA    R8,L'ANOELEM(R8)                                                 
*                                                                               
         BRAS   RE,ADDADB                                                       
*                                                                               
         USING DLPOSTD,R8                                                       
         MVI   DLPSEL,DLPSECRQ     CREDIT CLIENT                                
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSDBNM,STAFFNAM   CONTRA 2P                                    
         MVC   DLPSCRNM,CRPSNAME   CREDIT 29                                    
         ZAP   DLPSAMNT,EXPOST                                                  
         OI    DLPSTYPE,X'80'      SUBSIDIARY FROM HERE                         
         MVC   DLPSDBAC(15),WORK   SAVED FROM DEBIT ELEMENT                     
         MVC   DLPSCRAC,CRPSNUM                                                 
         SR    R1,R1                                                            
         IC    R1,BYTE                                                          
         LA    R1,1(R1)                                                         
         LA    RF,DLPSDBAC+1(R1)                                                
         LA    R1,DLPSDBAC+14                                                   
         SR    R1,RF                                                            
         BCTR  R1,R0                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   1(0,RF),STAFFNUM+3  CONTRA IS *EXPENSE-STAFF                     
         MVC   DLPSANAL,FINOFF                                                  
*                                                                               
         CLI   V29SW,C'Y'          SHOULD CONTRA BE VENDOR?                     
         BNE   EXP44               NO                                           
         MVC   DLPSDBAC,SVCRACC2   EXP VENDOR                                   
         MVC   DLPSDBNM,SVACCTN2   EXP VENDOR NAME                              
*                                                                               
EXP44    ZIC   R4,DLPSLEN                                                       
         AR    R8,R4                                                            
         B     EXP50                                                            
         EJECT                                                                  
*--------------------------------------------------------------                 
*        2D WITH 28 CONTRA POSTING  DR                                          
*        28 WITH 2D CONTRA POSTING  CR                                          
*--------------------------------------------------------------                 
*                                                                               
EXP50    DS    0H                                                               
         CLI   DEPSW,C'Y'                                                       
         BNE   EXP52                                                            
*                                                                               
         CLI   ANOELEM,0           TEST TO ADD ANALYSIS OFFICE ELEMENT          
         BE    *+14                NO                                           
         MVC   0(L'ANOELEM,R8),ANOELEM                                          
         LA    R8,L'ANOELEM(R8)                                                 
*                                                                               
         BRAS   RE,ADDADB                                                       
*                                                                               
         USING DLPOSTD,R8                                                       
         MVI   DLPSEL,DLPSEDCQ                                                  
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSDBAC,DEPNUM     DEBIT 2D   CONTRA = 28                       
         MVC   DLPSDBNM,DEPNAME                                                 
         MVC   DLPSCRAC,CRDSNUM    CREDIT 28  CONTRA = 2D                       
         MVC   DLPSCRNM,CRDSNAME                                                
         ZAP   DLPSAMNT,EXPOST                                                  
         MVC   DLPSANAL,FINOFF                                                  
         OI    DLPSTYPE,X'80'      IN CASE WE MISSED IT ON ELEMENT 2            
         ZIC   R4,DLPSLEN                                                       
         AR    R8,R4                                                            
         B     EXP52                                                            
         EJECT                                                                  
*--------------------------------------------------------------                 
*        2C WITH 27 CONTRA POSTING                                              
*        27 WITH 2C CONTRA POSTING                                              
*--------------------------------------------------------------                 
*                                                                               
EXP52    CLI   ECONSULT,C'Y'       DO WE NEED EXP 2C?                           
         BNE   EXP60               NO.                                          
*                                                                               
         CLI   ANOELEM,0           TEST TO ADD ANALYSIS OFFICE ELEMENT          
         BE    *+14                NO                                           
         MVC   0(L'ANOELEM,R8),ANOELEM                                          
         LA    R8,L'ANOELEM(R8)                                                 
*                                                                               
         BRAS   RE,ADDADB                                                       
*                                                                               
         USING DLPOSTD,R8                                                       
         MVI   DLPSEL,DLPSEDCQ     DB & CVR                                     
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSDBAC,E2CNUM     ACCT #                                       
         MVC   DLPSDBNM,E2CNAM     ACCT NAME                                    
         MVC   DLPSCRAC,EXPTROL    27                                           
         MVC   DLPSCRNM,EXPTROLN   27                                           
         ZAP   DLPSAMNT,INAMNT     AMT                                          
         MVC   DLPSANAL,FINOFF                                                  
         OC    CRDOFF,CRDOFF       USE CREDIT OFFICE IF AVAILABLE               
         BZ    *+10                                                             
         MVC   DLPSANAL,CRDOFF                                                  
         OI    DLPSTYPE,X'80'                                                   
         ZIC   R4,DLPSLEN          LEN                                          
         AR    R8,R4               INCREMENT                                    
         B     EXP60                                                            
         EJECT                                                                  
*--------------------------------------------------------------                 
*        1C WITH 1P CONTRA POSTING                                              
*--------------------------------------------------------------                 
*                                                                               
EXP60    CLI   COSTSW,C'Y'                                                      
         BNE   EXP70                                                            
*                                                                               
         CLI   ANOELEM,0           TEST TO ADD ANALYSIS OFFICE ELEMENT          
         BE    *+14                NO                                           
         MVC   0(L'ANOELEM,R8),ANOELEM                                          
         LA    R8,L'ANOELEM(R8)                                                 
*                                                                               
         BRAS   RE,ADDADB                                                       
*                                                                               
         USING DLPOSTD,R8                                                       
         MVI   DLPSEL,DLPSEDRQ     DEBIT DEPT C/A CLIENT                        
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSCRAC,COSTNUM    CONTRA  1C                                   
         MVC   DLPSCRNM,COSTNAME                                                
         MVC   DLPSDBAC,CRCNUM     DEBIT 1P                                     
         MVC   DLPSDBNM,CRCNAME                                                 
         ZAP   DLPSAMNT,EXPOST                                                  
         OI    DLPSTYPE,X'80'                                                   
         MVC   DLPSANAL,FINOFF                                                  
         ZIC   R4,DLPSLEN                                                       
         AR    R8,R4                                                            
*                                                                               
         CLI   ANOELEM,0           TEST TO ADD ANALYSIS OFFICE ELEM             
         BE    *+14                NO                                           
         MVC   0(L'ANOELEM,R8),ANOELEM                                          
         LA    R8,L'ANOELEM(R8)                                                 
*                                                                               
         BRAS   RE,ADDADB                                                       
*                                                                               
         USING DLPOSTD,R8                                                       
         MVI   DLPSEL,DLPSECRQ     CREDIT CLIENT                                
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSCRAC,COSTNUM    CR 1C                                        
         MVC   DLPSCRNM,COSTNAME                                                
*                                                                               
         MVC   DLPSDBAC(1),COMPANY                                              
         MVC   DLPSDBAC+1(2),=C'13'                                             
         MVC   DLPSDBAC+3(12),SPACES                                            
         MVC   DLPSDBAC+3(L'COSTANAL),COSTANAL                                  
         TM    BCCPYST5,CPYSNCST   NEW COST?                                    
         BZ    *+10                NO                                           
         MVC   DLPSDBAC,CR13NUM    YES, GET CONTRA FROM CATCALL                 
*                                                                               
         MVC   DLPSDBNM,CRCNAME                                                 
         OI    DLPSTYPE,X'80'                                                   
         MVC   DLPSANAL,FINOFF                                                  
         ZAP   DLPSAMNT,EXPOST                                                  
         ZIC   R4,DLPSLEN                                                       
         LR    R6,R8                                                            
         AR    R8,R4                                                            
         B     EXP70                                                            
         EJECT                                                                  
*--------------------------------------------------------------                 
*        PUT TO ACCDAY FILE                                                     
*--------------------------------------------------------------                 
*                                                                               
EXP70    MVI   0(R8),0             MARK END                                     
         LA    R8,1(R8)            END ADDR                                     
         LA    R3,IOAREA           START ADDR                                   
         SR    R8,R3                                                            
         STH   R8,HALF             TOTAL LEN                                    
         MVC   IOAREA(2),HALF      TO START OF REC                              
         BAS   RE,PUTDAYX          ADD RECORDS TO TRANSACTION FILE              
*                                                                               
         XC    WORK,WORK           ADD ENTRY TO TWA1                            
         ZIC   R1,SVDOCLEN                                                      
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   WORK(0),SVDOC       REF                                          
         L     R3,DMCB+8           RETURNED FROM PUTDAY                         
         MVC   WORK+10(4),0(R3)    DISK ADDRESS                                 
         ZAP   TRANSAMT,INAMNT                                                  
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         MVC   FVADDR,AAMTH        PRESET ERROR POSITION                        
         GOTO1 AADACDAY,BOPARM,IOAREA,BOPL61,BOWORK1                            
         CLC   FVMSGNO,=AL2(FVFOK) SET CC ON EXIT                               
*                                                                               
EXPX     XMOD1 1                   RETURN                                       
         EJECT                                                                  
*--------------------------------------------------------------                 
*        ADD A RECORD TO THE DAILY TRANSACTION FILE                             
*--------------------------------------------------------------                 
*                                                                               
PUTDAYX  LA    R1,BCFULL                                                        
         XC    0(4,R1),0(R1)       CLEAR DISK ADDRESS                           
         ST    R1,BOPARM+8         SET A(DISK ADDRESS)                          
         BR    RE                                                               
         EJECT                                                                  
*--------------------------------------------------------------                 
*        ADD FREEFORM ELEMENT FOR CLIENT AND PRODUCT                            
*--------------------------------------------------------------                 
ADDADB   NTR1  BASE=*,LABEL=*                                                   
         CLI   FFTELEM,0           DO WE HAVE A CLIENT AND PRODUCT?             
         BE    ADDADBX             NO, DONE                                     
         LA    R1,FFTELEM                                                       
         USING FFTELD,R1                                                        
         SR    RF,RF                                                            
         IC    RF,FFTLN                                                         
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   0(0,R8),FFTELEM                                                  
         LA    R8,1(RF,R8)         R8 TO NEXT AREA                              
                                                                                
ADDADBX  XIT1  REGS=(R8)                                                        
         EJECT                                                                  
*--------------------------------------------------------------                 
*        LITERAL DECLARATIONS                                                   
*--------------------------------------------------------------                 
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------                 
*        WORKING STORAGE DSECT                                                  
*--------------------------------------------------------------                 
*                                                                               
PROGD    DSECT                                                                  
*                                                                               
RELOA    DS    F                                                                
AEXCELD  DS    A                                                                
ACATD    DS    A                                                                
*                                                                               
ADOCH    DS    A                   A(DOCUMENT FIELD HEADER)                     
ADATH    DS    A                   A(TRANSACTION DATE FIELD HEADER)             
ACSHH    DS    A                   A(CASH ACCOUNT FIELD HEADER)                 
AVENH    DS    A                   A(PRODUCTION VENDOR FIELD HEADER)            
ANARH    DS    A                   A(FIRST NARRATIVE FIELD HEADER)              
ADETH    DS    A                   A(FIRST DETAIL LINE FIELD HEADER)            
ATOTH    DS    A                   A(TOTALS FIELD HEADER)                       
*                                                                               
ACLIH    DS    A                   A(CLIENT FIELD)                              
APROH    DS    A                   A(PRODUCT FIELD)                             
AJOBH    DS    A                   A(JOB FIELD HEADER)                          
AEXPH    DS    A                   A(EXPENSE A/C FIELD)                         
ANAM1H   DS    A                   A(FIRST NAME FIELD)                          
AWCH     DS    A                   A(COMISSIONABLE W/C FIELD)                   
ANWCH    DS    A                   A(NON-COMMISSIONABLE W/C FIELD)              
AWCNH    DS    A                   A(WORKCODE NAME FIELD)                       
AAMTH    DS    A                   A(AMOUNT FIELD)                              
AFOFFH   DS    A                   A(FINANCIAL OFFICE)                          
ACOFFH   DS    A                   A(CREDIT OFFICE)                             
AAOFFH   DS    A                   A(ANALYSIS OFFICE)                           
ADEPTH   DS    A                   A(DEPARTMENT CODE FIELD)                     
ASTFH    DS    A                   A(STAFF CODE FIELD)                          
ANAM2H   DS    A                                                                
ANEXT    DS    A                                                                
NDATAFLD EQU   (*-ACLIH)/4                                                      
*                                                                               
SAVERE   DS    A                                                                
NAMELEN  DS    A                                                                
CSHTOT   DS    PL6                 ACCUMULATING ITEM TOTAL                      
ELCODE   DS    XL1                 USE FOR ELEMENT                              
BYTE     DS    CL1                                                              
KEYCHG   DS    CL1                 Y/N=HEADER KEY FIELD CHANGE                  
*                                                                               
* DETAIL VALUES AREA IS CLEARED BEFORE EDITING AND POSTING EACH ITEM            
*                                                                               
DETVALS  DS    0C                  DETAIL VALUES AREA                           
*                                                                               
SIMDSW   DS    CL1                                                              
SIMDAC   DS    CL15                                                             
SIMDACN  DS    CL36                                                             
*                                                                               
SJCLINUM DS    CL15                                                             
SJCLINAM DS    CL36                                                             
SJPRONUM DS    CL15                                                             
SJPRONAM DS    CL36                                                             
SJJOBNUM DS    CL15                                                             
SJJOBNAM DS    CL36                                                             
SJOFFC   DS    CL2                 SJ OFFICE CODE                               
SJ1CAC   DS    CL15                1C ACCOUNT POINTER                           
CLICODE  DS    CL6                 CLIENT CODE                                  
PRODCODE DS    CL6                 PRODUCT CODE                                 
JOBNUM   DS    CL6                 JOB NUMBER                                   
CLIPRO   DS    CL15                CLI,PRO ACCT KEY                             
CLIPRON  DS    CL36                CLI,PRD NAME FOR DISP                        
*                                                                               
FINOFF   DS    CL2                 FINANCIAL OFFICE                             
ANAOFF   DS    CL2                 ANALYSIS OFFICE                              
CRDOFF   DS    CL2                 CREDIT OFFICE                                
PRDOFF   DS    CL2                 PRODUCTION OFFICE                            
*                                                                               
ANOELEM  DS    XL(ANOLNQ)          ANALYSED OFFICE ELEMENT                      
TRSELEM  DS    XL(TRSLNQ)          TRANSACTION STATUS ELEMENT                   
FFTELEM  DS    XL(FFTLN1Q+L'FFTDLEN+L'FFTCLPRA) CLIENT PRODUCT ELEMENT          
*                                                                               
DEPT     DS    CL4                                                              
STAFF    DS    CL12                                                             
STAFFL   DS    XL1                                                              
LEVEL    DS    XL1                                                              
*                                                                               
TYPE     DS    CL1                 E=EXPENSE,P=PRODUCTION                       
OFFSW    DS    CL1                                                              
DEPSW    DS    CL1                                                              
STFSW    DS    CL1                                                              
COSTSW   DS    CL1                                                              
*                                                                               
EXPOST   DS    PL6                 EXPENSE AMOUNT (LESS CD IF OK)               
CDAMNT   DS    PL6                                                              
*                                                                               
SAVEWC   DS    A                   A(CURRENT WORK CODE FIELD HEADER)            
SVWRKCD  DS    CL2                                                              
INAMNT   DS    PL6                 INVOICE AMOUNT                               
SVWRKNAM DS    CL35                WORK CODE EXPANSION                          
PASSCD   DS    CL1                 PASS CD TO CLIENT                            
COMSW    DS    CL1                 COMMISSIONABLE WORK CODE INDICATOR           
*                                                                               
POSTACC  DS    CL15                AGY EXP KEY                                  
POSTACCN DS    CL36                AGY EXP NAME                                 
POSTCNTR DS    CL3                 COST CENTER CODE                             
POSTCPOS DS    XL1                 COST CENTER POSITION IN KEY                  
COSTANAL DS    CL5                                                              
*                                                                               
CRDSNUM  DS    CL15                2/8 ACCT KEY                                 
CRDSNAME DS    CL36                NAME                                         
COSTNUM  DS    CL15                1/C ACCT KEY                                 
COSTNAME DS    CL36                NAME                                         
CRCNUM   DS    CL15                1/P ACCT KEY                                 
CRCNAME  DS    CL36                NAME                                         
CR13NUM  DS    CL15                1/3 ACCT KEY                                 
CRPSNUM  DS    CL15                2/9 ACCT KEY                                 
CRPSNAME DS    CL36                NAME                                         
STAFFNUM DS    CL15                2/P STAFF ACCT KEY                           
STAFFNAM DS    CL36                STAFF NAME                                   
DEPNUM   DS    CL15                DEPT ACCT KEY                                
DEPNAME  DS    CL36                DEPT NAME                                    
DEPSTFN  DS    CL36                OFF,DEPT,STAFF NAME FOR DISP                 
*                                                                               
DETVALNQ EQU   *-DETVALS                                                        
*                                                                               
KEY      DS    CL49                                                             
IOAREA   DS    2000C                                                            
*                                                                               
         DS    0D                                                               
EXCBLK   DS    XL(EXCELNQ)                                                      
*                                                                               
         DS    0D                                                               
CATBLK   DS    XL(CATLNQ)                                                       
PROGDX   DS    0C                                                               
         EJECT                                                                  
*--------------------------------------------------------------                 
*        ACEXCELD                                                               
*--------------------------------------------------------------                 
*                                                                               
       ++INCLUDE ACEXCELD                                                       
         EJECT                                                                  
*--------------------------------------------------------------                 
*        ACBATDSECT                                                             
*--------------------------------------------------------------                 
*                                                                               
       ++INCLUDE ACBATDSECT                                                     
         EJECT                                                                  
*--------------------------------------------------------------                 
*        ACBATE1D -- DSECT TO COVER FIRST SCREEN                                
*--------------------------------------------------------------                 
*                                                                               
       ++INCLUDE ACBATE1D                                                       
         EJECT                                                                  
*--------------------------------------------------------------                 
*        ACBATEAD -- DSECT TO COVER SECOND SCREEN                               
*--------------------------------------------------------------                 
*                                                                               
         ORG   BASOLY2H                                                         
       ++INCLUDE ACBATEAD                                                       
         EJECT                                                                  
*--------------------------------------------------------------                 
*        STORAGE ATTATCHED TO TWA                                               
*--------------------------------------------------------------                 
*                                                                               
         ORG   TWAHOLE                                                          
SVDOCLEN DS    XL1                                                              
SVDOC    DS    CL6                                                              
SVREF    DS    CL6                 REFERENCE NUMBER                             
SVDATE1  DS    CL3                                                              
SVURG    DS    CL1                 URGENT FIELD                                 
SVNARLEN DS    H                   LENGTH OF NARRATIVE                          
SVNAR    DS    CL120               NARRATIVE FIELD(S)                           
*                                                                               
SVCSHNUM DS    CL15                SC CASH ACCT #                               
SVCSHNAM DS    CL36                SC CASH ACCT NAME                            
*                                                                               
SVVENVAL DS    0C                  SAVED VENDOR VALUES                          
SVCRACCT DS    CL15                PROD VEN KEY                                 
SVACCTNM DS    CL36                PROD VEN NAME                                
SVCRACC2 DS    CL15                EXP VEN KEY                                  
SVACCTN2 DS    CL36                EXP VEN NAME                                 
SVDISC   DS    PL3                 CD VAL FOR PROD VENDS                        
SVDISC2  DS    PL3                 CD VAL FOR EXP VENDS                         
SVPVTAXT DS    CL1                 DEFAULT PRODUCTION TAX TYPE                  
SVEVTAXT DS    CL1                 DEFAULT EXPENSE TAX TYPE                     
*                                                                               
P2CNUM   DS    CL15                2C NUM  PROD                                 
P2CNAM   DS    CL36                2C NAME PROD                                 
E2CNUM   DS    CL15                2C NUM  EXP                                  
E2CNAM   DS    CL36                2C NAME EXP                                  
PROTROL  DS    CL15                27 NUM  PROD                                 
PROTROLN DS    CL36                27 NAME PROD                                 
EXPTROL  DS    CL15                27 NUM  EXP                                  
EXPTROLN DS    CL36                27 NAME EXP                                  
PCONSULT DS    CL1                 1099(2C) VENDOR SWITCH PROD VENDOR.          
ECONSULT DS    CL1                 1099(2C) VENDOR SWITCH EXP VENDOR.           
PRSTAT   DS    CL1                 PROD VENDOR 1099 STATUS                      
EXSTAT   DS    CL1                 EXP VENDOR 1099 STATUS                       
V29SW    DS    CL1                 EXPENSE VENDOR CONTRA.                       
SVVENLNQ EQU   *-SVVENVAL          LENGTH OF VENDOR VALUES                      
SAVEEND  EQU   *                   END OF SAVE AREA                             
         EJECT                                                                  
*--------------------------------------------------------------                 
*        OTHER DSECTS USED                                                      
*--------------------------------------------------------------                 
*                                                                               
* ACCATCALLD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACCATCALLD                                                     
         PRINT ON                                                               
* ACGENBOTH                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
* ACGENDAY                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENDAY                                                       
         PRINT ON                                                               
* DDFLDIND                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDFLDIND                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017ACBAT3D   03/30/06'                                      
         END                                                                    
