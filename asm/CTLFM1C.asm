*          DATA SET CTLFM1C    AT LEVEL 024 AS OF 09/05/03                      
*PHASE TA021CA,+0                                                               
*INCLUDE SCINKEY                                                                
         TITLE 'CTLFM1C - CONTROL FILE MAINT - PERSONAL AUTHS'                  
CTLFM1C  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKX-WORKD,**LFM**,RA,RR=RE                                     
         USING WORKD,RC            RC=A(TEMP W/S)                               
         L     R1,=V(SCINKEY)                                                   
         AR    R1,RE                                                            
         ST    R1,VSCINKEY                                                      
         USING LFMTEMP,R9          R9=A(GLOBAL W/S)                             
         L     R2,PARM+12          A(COMFACS)                                   
         USING COMFACSD,R2                                                      
         GOTO1 CDATCON,DMCB,(5,0),(3,REPDATE)                                   
         MVC   VDATCON,CDATCON                                                  
         MVC   VREPORT,CREPORT                                                  
         DROP  R2                                                               
         L     R3,ATWA                                                          
         USING LFMSAVE,R3          R3=A(SAVE W/S)                               
         LR    R2,R3                                                            
         USING CTLFMFFD,R2         R2=A(TWA)                                    
         LA    R4,IOAREA                                                        
         USING CT0REC,R4           R4=A(RECORD)                                 
         ST    R4,AREC                                                          
         LA    R1,BASTYPEH                                                      
         ST    R1,FADR                                                          
         TM    TWAAUTH-TWAD(R2),X'80'                                           
         BZ    EIRT                                                             
         EJECT                                                                  
* VALIDATE KEY FIELDS & BUILD KEY                                               
*                                                                               
KEYVAL   XC    CT0KEY,CT0KEY                                                    
         MVI   CT0KTYP,CT0KTEQU                                                 
         MVC   CT0KAGY,TWAAGY-TWAD(R2)                                          
*                                  CHECK REPORT                                 
         GOTO1 AFVAL,PERREQH                                                    
         BZ    *+8                 FIELD NOT INPUT                              
         B     PRNTREP                                                          
*                                  VALIDATE TYPE CODE                           
         GOTO1 AFVAL,PERTYPEH                                                   
         BZ    *+10                                                             
         MVC   CT0KOFFC,FLD                                                     
*                                  VALIDATE LAST NAME                           
         GOTO1 AFVAL,PERNAMEH                                                   
         BNZ   *+16                                                             
         CLI   PERTYPEH+5,0        TEST IF TYPE CODE INPUT                      
         BNE   EXIT                                                             
         B     KEYV2               NO - KEY MUST BE SECRET CODE                 
         MVC   CT0KLAST,FLD                                                     
*                                  VALIDATE INITIALS                            
         GOTO1 AFVAL,PERINSH                                                    
         BZ    EXIT                                                             
         MVC   CT0KFI(2),FLD                                                    
         B     KEYV8                                                            
*                                  INPUT KEY IS SECRET CODE                     
KEYV2    LA    R1,PERNAMEH                                                      
         ST    R1,FADR                                                          
         CLI   ACTN,ADD            IF ACTION ADD TYPE/NAME REQUIRED             
         BE    EMIF                                                             
         CLI   PERCODEH+5,0        TEST IF SECRET CODE INPUT                    
         BE    EMIF                                                             
         GOTO1 AFVAL,PERCODEH                                                   
         MVC   CT0KCODE,FLD                                                     
         MVC   KEY,CT0KEY          READ SECRET CODE RECORD                      
         GOTO1 AREAD                                                            
         BZ    EIIO                                                             
         TM    DMCB+8,X'10'                                                     
         BO    ERNF                                                             
         TM    DMCB+8,X'02'        TEST IF RECORD DELETED                       
         BO    ERID                                                             
         TM    CT0STAT,X'40'       TEST IF NEW PERSON PASSWORD                  
         BZ    KEYV3                                                            
         LA    R1,PERCODEH                                                      
         ST    R1,FADR                                                          
         B     EIIF                                                             
*                                  FIND POINTER ELEMENT                         
KEYV3    LA    R1,CT0DATA                                                       
         SR    RE,RE                                                            
KEYV4    CLI   0(R1),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(2,R1),=X'0318'                                                 
         BE    *+14                                                             
         IC    RE,1(R1)                                                         
         AR    R1,RE                                                            
         B     KEYV4                                                            
*                                  OUTPUT TYPE CODE/NAME/INITIALS               
         OC    2(2,R1),2(R1)                                                    
         BZ    KEYV6                                                            
         MVC   PERTYPE,2(R1)                                                    
         OI    PERTYPEH+6,X'80'                                                 
         MVI   PERTYPEH+5,L'PERTYPE                                             
         NI    PERTYPEH+4,X'FF'-X'08'                                           
KEYV6    MVC   PERNAME,4(R1)                                                    
         OI    PERNAMEH+6,X'80'                                                 
         MVI   PERNAMEH+5,L'PERNAME                                             
         NI    PERNAMEH+4,X'FF'-X'08'                                           
         MVC   PERINS,22(R1)                                                    
         OI    PERINSH+6,X'80'                                                  
         MVI   PERINSH+5,L'PERINS                                               
         NI    PERINSH+4,X'FF'-X'08'                                            
         B     KEYVAL              GO RE-VALIDATE KEY                           
*                                  VALIDATE SYSTEM                              
KEYV8    MVI   SYSTEM,0                                                         
         GOTO1 AFVAL,PERSYSH                                                    
         BZ    KEYVC                                                            
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                R1=L'INPUT-1                                 
         L     RE,ASYSTBL          RE=A(SYSTEM TABLE)                           
         USING SYSLSTD,RE                                                       
*                                  MATCH INPUT WITH TABLE                       
KEYVA    CLI   SYSLNUM,0           TEST E-O-T                                   
         BE    EIIF                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   FLD(0),SYSLNAME                                                  
         BE    *+12                                                             
         LA    RE,SYSLLEN(RE)                                                   
         B     KEYVA                                                            
         MVC   SYSTEM,SYSLNUM      SET SYSTEM NUMBER                            
         CLC   PERSYS(L'SYSLNAME),SYSLNAME                                      
         BE    *+14                                                             
         MVC   PERSYS(L'SYSLNAME),SYSLNAME     OUTPUT FULL NAME                 
         OI    PERSYSH+6,X'80'                                                  
         DROP  RE                                                               
*                                  CHECK THIS/LAST ACTIONS VALID                
KEYVC    MVC   KEY,CT0KEY                                                       
         MVC   KEYNEXT,KEY                                                      
         LA    R1,PERTYPEH                                                      
         ST    R1,FADR                                                          
         CLI   ACTN,CHANGE         IF ACTION=CHANGE                             
         BNE   KEYVE                                                            
         MVI   ACTN,DISPLAY                                                     
         TM    ACTINDS,X'80'       IF ACTION=MOVE                               
         BNZ   MOVE                                                             
         CLC   KEY,LKEY            KEY & SYSTEM MUST NOT CHANGE                 
         BNE   KEYVE                                                            
         CLC   SYSTEM,LSYSTEM                                                   
         BNE   KEYVE                                                            
         MVI   ACTN,CHANGE         IF THEY DID SET ACTION=DISPLAY               
         B     KEYVE                                                            
         EJECT                                                                  
*                                  ALL FIELDS MUST REMAIN EXCEPT                
*                                  TYPE CODE FOR MOVE                           
MOVE     CLI   LACTN,DISPLAY       WAS LAST ACTN DISPLAY?                       
         BNE   KEYVE                                                            
         CLC   BASTYPE,=C'AUTH    ' IS THIS AN AUTH REC?                        
         BNE   KEYVE                                                            
         LA    R1,PERNAMEH         CHECK FIELDS FOR DATA                        
M10      TM    4(R1),X'80'                                                      
         BNZ   KEYVE                                                            
         CLI   0(R1),0                                                          
         BE    M20                                                              
         ZIC   R0,0(R1)                                                         
         AR    R1,R0                                                            
         B     M10                                                              
M20      MVI   ACTN,CHANGE         MOVE OKAY                                    
         MVC   KEY,LKEY                                                         
         MVI   UPDATE,C'Y'         READ FOR UPDATE                              
         GOTO1 AREAD                                                            
         BZ    EIIO                                                             
         B     DATAVAL                                                          
*                                  READ RECORD & CHECK ACTION                   
KEYVE    CLI   ACTN,DISPLAY                                                     
         BE    *+8                                                              
         MVI   UPDATE,C'Y'         SET R-F-O FOR UPDATABLE ACTIONS              
         GOTO1 AREAD                                                            
         BZ    EIIO                                                             
         TM    DMCB+8,X'10'                                                     
         BZ    *+16                                                             
         CLI   ACTN,ADD            N/F ONLY VALID FOR ADD                       
         BE    DATAVAL                                                          
         B     ERNF                                                             
KEYVEA   CLI   ACTN,ADD            F NOT VALID FOR ADD                          
         BE    ERAE                                                             
         TM    DMCB+8,X'02'                                                     
         BZ    *+12                                                             
         CLI   ACTN,DISPLAY        DELETED REC CAN ONLY BE DISPLAYED            
         BNE   ERNF                                                             
         CLI   ACTN,CHANGE                                                      
         BE    DATAVAL                                                          
         B     DISPREC                                                          
         EJECT                                                                  
* PRINT REPORT                                                                  
*                                                                               
PRNTREP  DS    0H                                                               
         XC    REPBLK,REPBLK       CLEAR REPORT PARAMETER BLOCK                 
         LA    R0,REPHS                                                         
         ST    R0,REPABUF                                                       
         LA    R0,HEAD             GET ADDR OF HEADING SPEC                     
         ST    R0,REPAPHS                                                       
         L     R1,PARM+12          GET ADDR OF COMFACS                          
         ST    R1,REPACOM                                                       
         MVI   REPHEADN,REPHN      NUMBER OF HEAD LINES                         
         MVI   REPPRNTN,REPPN      NUMBER OF PRINT LINES                        
         MVI   REPWIDTH,REPWREGQ   WIDTH                                        
         MVI   REPSUBPG,1                                                       
         MVC   REPAPQB,ATIA                                                     
         MVC   REPSYSID,=C'TA'                                                  
         MVC   REPPRGID,=C'02'                                                  
         MVC   REPSUBID,PERREQ     REQUESTOR                                    
         MVC   REPPSWD,PERPWD      GET PASSWORD FOR QUEUE                       
*                                                                               
PR07     MVI   REPACTN,REPAINI     INITIALIZATION                               
         GOTO1 VREPORT,REPBLK                                                   
         MVI   REPACTN,REPAOPN     OPEN REPORT                                  
         BASR  RE,RF                                                            
         CLI   REPERRS,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   REPACTN,REPAPUT                                                  
         OI    REPHEADI,REPHFRCE   FORCE NEW PAGE                               
* PRINT REPORT ON NAME ORDER                                                    
         MVI   KEY,CT0KTEQU                                                     
         MVC   KEY+1(2),TWAAGY-TWAD(R2)                                         
         ST    R4,AREC                                                          
         GOTO1 AREADHI             READ FIRST RECORD                            
         BZ    EIIO                                                             
         B     PR20                DISPLAY FIRST REC                            
PR10     GOTO1 ARSEQ                                                            
         BZ    EIIO                                                             
PR20     DS    0H                                                               
         CLI   0(R4),C'0'          ARE WE STILL ON AUTH RECS?                   
         BNE   PR100                                                            
         CLC   1(2,R4),TWAAGY-TWAD(R2) SAME AGY???                              
         BNE   PR10                                                             
         OC    CT0KEYS(12),CT0KEYS IS THIS A LAST NAME KEY?                     
         BZ    PR10                NO? GET NEXT REC                             
         TM    27(R4),X'80'        IS THIS A MOVED RECORD?                      
         BNZ   PR10                                                             
         CLI   PERACT,C'B'                                                      
         BE    PR25                                                             
         CLI   PERACT,C'A'                                                      
         BNE   PR23                                                             
         TM    27(R4),X'20'        IS THIS A LOCKED RECORD?                     
         BNZ   PR10                YES? GET NEXT REC                            
         B     PR25                                                             
PR23     CLI   PERACT,C'I'                                                      
         BE    PR24                                                             
         LA    R1,PERACTH                                                       
         ST    R1,FADR                                                          
         B     EMIF                                                             
PR24     TM    27(R4),X'20'        IS THIS A LOCKED RECORD?                     
         BZ    PR10                NO? GET NEXT REC                             
         B     PR25                                                             
PR25     LA    R8,REPP1                                                         
         MVC   0(L'CT0KOFFC,R8),CT0KOFFC                                        
         LA    R8,L'CT0KOFFC+3(R8)                                              
         MVC   0(L'CT0KLAST,R8),CT0KLAST                                        
         LA    R8,L'CT0KLAST+1(R8)                                              
         MVC   0(L'CT0KFI,R8),CT0KFI                                            
         LA    R8,L'CT0KFI+1(R8)                                                
         MVC   0(L'CT0KMI,R8),CT0KMI                                            
         LA    R8,L'CT0KFI+1(R8)                                                
* GET CODE (030C EL)                                                            
         LA    R5,CT0DATA                                                       
         SR    RE,RE                                                            
PR30     CLI   0(R5),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(2,R5),=X'030C'                                                 
         BE    *+14                                                             
         IC    RE,1(R5)                                                         
         AR    R5,RE                                                            
         B     PR30                                                             
*                                                                               
         MVC   0(10,R8),2(R5)                                                   
         LA    R8,11(R8)                                                        
* GET ACTIVITY DATE (01 EL)                                                     
         LA    R5,CT0DATA                                                       
         SR    RE,RE                                                            
PR40     CLI   0(R5),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R5),X'01'                                                      
         BE    *+14                                                             
         IC    RE,1(R5)                                                         
         AR    R5,RE                                                            
         B     PR40                                                             
*                                                                               
         GOTO1 VDATCON,DMCB,(3,2(R5)),(5,(R8))                                  
         LA    R8,9(R8)                                                         
* GET SYSTEMS (21 EL'S)                                                         
         LA    R5,CT0DATA                                                       
         SR    RE,RE                                                            
PR50     CLI   0(R5),0                                                          
         BE    PR60                                                             
         CLC   0(1,R5),=X'21'                                                   
         BE    *+14                                                             
         IC    RE,1(R5)                                                         
         AR    R5,RE                                                            
         B     PR50                                                             
*                                                                               
         LR    R1,R5               SAVE OFF R5                                  
         L     R5,ASYSTBL                                                       
         XR    R6,R6               ASYSTBL IS PAST LENGTH AND SVC SYS           
         LA    R6,SYSLLEN(R6)                                                   
         LA    R6,6(R6)                                                         
         SR    R5,R6                                                            
         USING SYSLSTD,R5          R5 NOW AT BEGINNING OF SYSTBL                
         LH    R6,0(R5)            ENTRY LENGTH                                 
         L     R7,2(R5)            END OF SYSLST                                
         LA    R5,6(R5)            FIRST ENTRY                                  
*                                                                               
         CLC   2(1,R1),SYSLNUM                                                  
         BE    *+10                                                             
         BXLE  R5,R6,*-10                                                       
         DC    H'0'                                                             
         MVC   0(3,R8),SYSLSHRT                                                 
         DROP  R5                                                               
*                                                                               
         LA    R8,4(R8)            EACH SYS USES 4 SPACES (SHRT NAME)           
         LR    R5,R1               RESTORE R5                                   
         ZIC   RE,1(R5)                                                         
         AR    R5,RE               POINT AT NEXT EL                             
         B     PR50                                                             
*                                                                               
PR60     DS    0H                                                               
         LA    R8,REPP1                                                         
         LA    R8,80(R8)           POINT AT STATUS FIELD                        
         TM    27(R4),X'20'        TEST IF PASSWORD ACTIVE                      
         BNZ   *+14                                                             
         MVC   0(6,R8),=C'ACTIVE'                                               
         B     *+10                                                             
         MVC   0(8,R8),=C'INACTIVE'                                             
*                                                                               
         GOTO1 VREPORT,REPBLK                                                   
         CLI   REPERRS,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         B     PR10                                                             
* PRINT REPORT ON CODE ORDER                                                    
PR100    DS    0H                                                               
         MVI   REPSUBPG,2          CHANGE TO HEADING 2                          
         MVI   REPACTN,REPAPUT                                                  
         OI    REPHEADI,REPHFRCE   FORCE NEW PAGE                               
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,CT0KTEQU                                                     
         MVC   KEY+1(2),TWAAGY-TWAD(R2)                                         
         ST    R4,AREC                                                          
         GOTO1 AREADHI             READ FIRST RECORD                            
         BZ    EIIO                                                             
         B     PR120               DISPLAY FIRST REC                            
PR110    GOTO1 ARSEQ                                                            
         BZ    EIIO                                                             
PR120    DS    0H                                                               
         CLI   0(R4),C'0'          ARE WE STILL ON AUTH RECS?                   
         BNE   PR170                                                            
         CLC   1(2,R4),TWAAGY-TWAD(R2) SAME AGY???                              
         BNE   PR110                                                            
         OC    CT0KEYS(20),CT0KEYS IS THIS AN AUTH-NUM KEY?                     
         BZ    PR110               YES?  GET NEXT REC                           
         OC    CT0KEYS(12),CT0KEYS IS THIS A LAST-NAME KEY?                     
         BNZ   PR110               YES? GET NEXT REC                            
         TM    27(R4),X'80'        IS THIS A 'MOVED' RECORD?                    
         BNZ   PR110               YES? DON'T PRINT                             
         CLI   PERACT,C'B'                                                      
         BE    PR125                                                            
         CLI   PERACT,C'A'                                                      
         BNE   PR123                                                            
         TM    27(R4),X'20'        IS THIS A LOCKED RECORD?                     
         BNZ   PR110                YES? GET NEXT REC                           
         B     PR125                                                            
* MUST BE 'I'                                                                   
PR123    TM    27(R4),X'20'        IS THIS A LOCKED RECORD?                     
         BZ    PR110               NO? GET NEXT REC                             
         B     PR125                                                            
PR125    LA    R8,REPP1                                                         
         MVC   0(L'CT0KCODE,R8),CT0KCODE                                        
         LA    R8,L'CT0KCODE+1(R8)                                              
* GET TYPE, LNAME, FI,MI (0318 EL)                                              
         LA    R5,CT0DATA                                                       
         SR    RE,RE                                                            
PR130    CLI   0(R5),0                                                          
         BE    PR132               ELEMENT NOT PRESENT FOR NEW SECURITY         
         CLC   0(2,R5),=X'0318'                                                 
         BE    *+14                                                             
         IC    RE,1(R5)                                                         
         AR    R5,RE                                                            
         B     PR130                                                            
*                                                                               
         MVC   1(2,R8),2(R5)                                                    
         MVC   5(18,R8),4(R5)                                                   
         MVC   24(1,R8),22(R5)                                                  
         MVC   26(1,R8),23(R5)                                                  
         B     PR136                                                            
*                                                                               
* GET PERSONAL ID IF NEW SECURITY RECORD STRUCTURE                              
*                                                                               
PR132    LA    R5,CT0DATA                                                       
         SR    RE,RE                                                            
PR134    CLI   0(R5),0                                                          
         BNE   *+6                                                              
         DC    H'00'                                                            
         CLC   0(1,R5),=X'C3'                                                   
         BE    *+14                                                             
         IC    RE,1(R5)                                                         
         AR    R5,RE                                                            
         B     PR134                                                            
*                                                                               
         MVC   1(8,R8),2(R5)                                                    
*                                                                               
PR136    LA    R8,28(R8)                                                        
* GET ACTIVITY DATE (01 EL)                                                     
         LA    R5,CT0DATA                                                       
         SR    RE,RE                                                            
PR140    CLI   0(R5),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R5),X'01'                                                      
         BE    *+14                                                             
         IC    RE,1(R5)                                                         
         AR    R5,RE                                                            
         B     PR140                                                            
*                                                                               
         GOTO1 VDATCON,DMCB,(3,2(R5)),(5,(R8))                                  
         LA    R8,9(R8)                                                         
* GET SYSTEMS (21 EL'S)                                                         
         LA    R5,CT0DATA                                                       
         SR    RE,RE                                                            
PR150    CLI   0(R5),0                                                          
         BE    PR160                                                            
         CLC   0(1,R5),=X'21'                                                   
         BE    *+14                                                             
         IC    RE,1(R5)                                                         
         AR    R5,RE                                                            
         B     PR150                                                            
*                                                                               
         LR    R1,R5               SAVE OFF R5                                  
         L     R5,ASYSTBL                                                       
         XR    R6,R6               ASYSTBL PTS PAST SVC SYS & LEN & END         
         LA    R6,SYSLLEN(R6)                                                   
         LA    R6,6(R6)                                                         
         SR    R5,R6                                                            
         USING SYSLSTD,R5          R5 NOW PTS AT BEGIN OF SYSTBL                
         LH    R6,0(R5)            ENTRY LENGTH                                 
         L     R7,2(R5)            END OF SYSLST                                
         LA    R5,6(R5)            FIRST ENTRY                                  
*                                                                               
         CLC   2(1,R1),SYSLNUM                                                  
         BE    *+10                                                             
         BXLE  R5,R6,*-10                                                       
         DC    H'0'                                                             
         MVC   0(3,R8),SYSLSHRT                                                 
         DROP  R5                                                               
*                                                                               
         LA    R8,4(R8)            SHRT SYS HAVE 3 CHARS + BLNK                 
         LR    R5,R1               RESTORE R5                                   
         ZIC   RE,1(R5)            GET NEXT EL                                  
         AR    R5,RE                                                            
         B     PR150                                                            
*                                                                               
PR160    DS    0H                                                               
         LA    R8,REPP1                                                         
         LA    R8,80(R8)                                                        
         TM    27(R4),X'20'        LOCKED PASSWORD?                             
         BNZ   *+14                                                             
         MVC   0(6,R8),=C'ACTIVE'                                               
         B     *+10                                                             
         MVC   0(8,R8),=C'INACTIVE'                                             
*                                                                               
         GOTO1 VREPORT,REPBLK                                                   
         CLI   REPERRS,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         B     PR110                                                            
* CLOSE REPORT                                                                  
PR170    MVI   REPACTN,REPACLO     CLOSE REPORT                                 
         GOTO1 VREPORT,REPBLK                                                   
         CLI   REPERRS,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   PERREQ,=C'   '      CLEAR REQUEST FIELD                          
         OI    PERREQH+6,X'80'     XMIT FIELD                                   
         MVC   PERACT,=C' '        CLEAR A/I/B FIELD                            
         OI    PERACTH+6,X'80'     XMIT FIELD                                   
         MVC   PERPWD,=C'      '   CLEAR PASSWORD FIELD                         
         OI    PERPWDH+6,X'80'     XMIT FIELD                                   
         LA    R1,BASACTNH         POSN CURSOR AT ACTN FIELD                    
         ST    R1,FADR                                                          
         MVI   FERN,X'FE'                                                       
         XC    BASHDR,BASHDR                                                    
         MVC   BASHDR(13),=C'REPORT QUEUED'                                     
         OI    BASHDRH+6,X'80'                                                  
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
* DISPLAY RECORD                                                                
*                                                                               
DISPREC  XC    PERCODE,PERCODE                                                  
         OI    PERCODEH+6,X'80'                                                 
         XC    PERCMNT,PERCMNT                                                  
         OI    PERCMNTH+6,X'80'                                                 
         XC    PERID1,PERID1       CLEAR DOWN TWA                               
         OI    PERID1H+6,X'80'                                                  
         XC    PERID2,PERID2                                                    
         OI    PERID2H+6,X'80'                                                  
         TWAXC PERACCSH                                                         
         MVI   IDCNT,0                                                          
         CLI   SYSTEM,0                                                         
         BE    *+8                                                              
         BAS   RE,GETSE            GET SE VALUES FOR SYSTEM                     
         XC    ASYSEL,ASYSEL                                                    
         LA    R5,CT0DATA          R5=A(FIRST ELEMENT)                          
*                                                                               
DISP2    CLI   0(R5),0             TEST E-O-R                                   
         BE    DISPEND                                                          
         CLI   0(R5),X'02'         DESCRIPTION ELEMENT                          
         BE    DISPDE                                                           
         CLI   0(R5),X'03'         POINTER ELEMENTS                             
         BE    DISPSC                                                           
         CLI   0(R5),X'20'         ID ELEMENT                                   
         BE    DISPID                                                           
         CLI   0(R5),X'21'         SYSTEM ELEMENT                               
         BE    DISPSS                                                           
*                                  BUMP TO NEXT ELEMENT                         
DISP4    ZIC   R1,1(R5)                                                         
         AR    R5,R1                                                            
         B     DISP2                                                            
*                                  DISPLAY USER COMMENTS                        
DISPDE   ZIC   R1,1(R5)                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     DISP4                                                            
         MVC   PERCMNT(0),2(R5)                                                 
*                                  DISPLAY SECRET CODE                          
         USING CTPASD,R5                                                        
DISPSC   CLI   CTPASLEN,12                                                      
         BNE   DISP4                                                            
         MVC   PERCODE,CTPASDTA                                                 
         B     DISP4                                                            
*                                  SAVE A(CORRECT SYSTEM ELEMENT)               
         USING CTSYSD,R5                                                        
DISPSS   CLI   SYSTEM,0                                                         
         BE    DISP4                                                            
         CLC   CTSYSNUM,SYSTEM                                                  
         BNE   DISP4                                                            
         ST    R5,ASYSEL                                                        
         B     DISP4                                                            
         EJECT                                                                  
* END OF RECORD ON DISPLAY                                                      
*                                                                               
DISPEND  CLI   IDCNT,0             VALID ID'S                                   
         BE    DISPEND2                                                         
         ZIC   R0,IDCNT                                                         
         GOTO1 VSCINKEY,DMCB,(2,PERID1H),(20,BLOCK2),(R0)                       
*                                                                               
DISPEND2 OC    ASYSEL,ASYSEL                                                    
         BZ    DISPENDX                                                         
         BAS   RE,DISPSYS          FORMAT SYSTEM BLOCK & DISPLAY                
         ZIC   R0,PGCNT                                                         
         GOTO1 VSCINKEY,DMCB,(2,PERPRG1H),(20,BLOCK1),(R0)                      
*                                  SET NEXT ACTION & EXIT                       
DISPENDX MVC   LSYSTEM,SYSTEM                                                   
         TM    CT0STAT,X'A0'       TEST RECORD LOCKED                           
         BZ    DISPEND3                                                         
         MVI   NACTN,OKRES                                                      
         B     EXIT                                                             
DISPEND3 TM    ACTINDS,X'80'       IF ACTION=MOVE                               
         BZ    *+12                                                             
         LA    R1,PERTYPEH                                                      
         B     *+8                                                              
         LA    R1,PERCODEH                                                      
         ST    R1,FADR                                                          
         MVI   NACTN,OKCHA+OKDEL                                                
         B     EXIT                                                             
         EJECT                                                                  
* ADD AN ID ELEMENT TO ID BLOCK (BLOCK2)                                        
*                                                                               
DISPID   SR    R1,R1                                                            
         IC    R1,IDCNT            BUMP BLOCK COUNT                             
         LR    R6,R1                                                            
         LA    R1,1(R1)                                                         
         STC   R1,IDCNT                                                         
         MH    R6,=H'20'                                                        
         LA    R6,BLOCK2(R6)       POINT TO ENTRY IN BLOCK                      
         MVI   0(R6),C' '          AND CLEAR IT                                 
         MVC   1(19,R6),0(R6)                                                   
         IC    R1,1(R5)                                                         
         OC    2(2,R5),2(R5)                                                    
         BNZ   *+10                                                             
         MVC   2(2,R5),=C'L='                                                   
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R6),2(R5)       MOVE IN ID                                   
         B     DISP4                                                            
         EJECT                                                                  
* BUILD PROGRAM ACCESS LIST INTO BLOCK1 AND SET PGCNT TO NUMBER OF              
* ENTRIES IN BLOCK.                                                             
*                                                                               
DISPSYS  NTR1                                                                   
         BAS   RE,GETSE            GET A(SELIST ENTRY) INTO ASE AND             
         L     R5,ASYSEL           A(PGMS) INTO APGM                            
         USING CTSYSEL,R5                                                       
         LA    R6,CTSYSPGM         R6=A(AUTHS)                                  
         ZIC   R7,CTSYSLEN                                                      
         MVI   PGCNT,0             SET COUNT                                    
         CLI   CTSYSLEN,16         CHECK FOR ALL= VALUE ONLY                    
         BE    DISPSYS8                                                         
*                                                                               
DISPSYS2 CH    R7,=H'16'                                                        
         BNH   DISPSYS8                                                         
         MVC   PROGRAM,0(R6)                                                    
         LA    R6,1(R6)                                                         
         BAS   RE,GETPRGN          GET PROGRAM NAME                             
         CLI   PROGRAM,X'FF'       SET TO X'FF' IF N/F                          
         BE    DISPSYS6                                                         
DISPSYS4 SR    R1,R1                                                            
         IC    R1,PGCNT            BUMP BLOCK COUNT                             
         LA    RE,1(R1)                                                         
         STC   RE,PGCNT                                                         
         MH    R1,=H'20'                                                        
         LA    R1,BLOCK1(R1)       GET A(BLOCK ENTRY)                           
         MVI   0(R1),C' '                                                       
         MVC   1(19,R1),0(R1)                                                   
         LR    R8,R1                                                            
         MVC   0(4,R8),PGNAME                                                   
         LA    R8,4(R8)                                                         
         CLI   0(R8),C' '                                                       
         BNE   *+8                                                              
         BCT   R8,*-8                                                           
         MVI   1(R8),C'='                                                       
         MVI   2(R8),C'Y'          AUTH IS Y N OR XXXX                          
         CLC   0(2,R6),YAUTH                                                    
         BE    DISPSYS6                                                         
         MVI   2(R8),C'N'                                                       
         CLC   0(2,R6),NAUTH                                                    
         BE    DISPSYS6                                                         
         GOTO1 VHEXOUT,DMCB,(R6),2(R8),2,=C'TOG'                                
*                                                                               
DISPSYS6 LA    R8,CTSYSALL         EXIT IF ALL=VALUE JUST DONE                  
         CR    R8,R6                                                            
         BE    DISPSYSA                                                         
         LA    R6,2(R6)                                                         
         SH    R7,=H'3'                                                         
         B     DISPSYS2                                                         
*                                                                               
DISPSYS8 LA    R6,CTSYSALL                                                      
         MVC   PGNAME,=CL8'ALL'                                                 
         B     DISPSYS4                                                         
*                                                                               
DISPSYSA OC    CTSYSLMT,CTSYSLMT                                                
         BZ    DISPSYSX                                                         
         MVC   PERACCS(L'CTSYSLMT),CTSYSLMT                                     
         MVI   PERACCSH+7,L'CTSYSLMT                                            
         CLC   CTSYSLMT(2),=XL2'FFFF'                                           
         BNE   DISPSYA1                                                         
         MVC   PERACCS(2),=CL2'L=' LIMIT ACCESS LIST CODE                       
         B     DISPSYSX                                                         
*                                                                               
DISPSYA1 EQU   *                                                                
*&&UK                                                                           
         CLI   CTSYSNUM,4          TEST UK/MEDIA                                
         BNE   DISPSYSE                                                         
         OC    CTSYSLMT,CTSYSLMT                                                
         BZ    DISPSYSX                                                         
         LA    RE,CTSYSLMT                                                      
         LA    RF,PERACCS                                                       
         SR    R1,R1                                                            
         LA    R0,4                                                             
DISPSYSB IC    R1,0(RE)                                                         
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(2,RF),DUB                                                      
         CLI   0(RF),C'0'                                                       
         BE    *+12                                                             
         LA    RF,1(RF)                                                         
         B     *+10                                                             
         MVC   0(1,RF),1(RF)                                                    
         LA    RF,1(RF)                                                         
         MVI   0(RF),C','                                                       
         LA    RF,1(RF)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,DISPSYSB                                                      
         BCTR  RF,0                                                             
         MVI   0(RF),C' '                                                       
         B     DISPSYSX                                                         
*&&                                                                             
*&&US                                                                           
         CLI   CTSYSNUM,3          TEST NETWORK                                 
         BE    *+12                                                             
         CLI   CTSYSNUM,2          TEST SPOT                                    
         BNE   DISPSYSC                                                         
*                                                                               
         MVC   DMCB+4(4),=X'D9000A15'  CLUNPK                                   
         L     RE,AFACLIST                                                      
         USING SYSFACD,RE                                                       
         L     RF,VCALLOV                                                       
         DROP  RE                                                               
         GOTO1 (RF),DMCB,0                                                      
         L     RF,0(R1)                                                         
         GOTO1 (RF),(R1),CTSYSLMT,PERACCS                                       
*                                                                               
DISPSYSC CLI   CTSYSNUM,6          TEST ACC                                     
         BNE   DISPSYSE                                                         
         CLC   CTSYSLMT+3(3),=C'TAL'                                            
         BNE   DISPSYSX                                                         
         MVC   PERACCS(2),=C'T='   FORMAT T=ULCC (DPS TALENT)                   
         MVC   PERACCS+2(2),CTSYSLMT                                            
         GOTO1 VHEXOUT,DMCB,CTSYSLMT+2,PERACCS+4,1                              
         B     DISPSYSX                                                         
*&&                                                                             
DISPSYSE CLI   CTSYSNUM,14         TEST PERSONNEL                               
         BNE   DISPSYSX                                                         
         XC    PERACCS,PERACCS                                                  
         OC    CTSYSLMT+2(2),CTSYSLMT+2                                         
         BZ    DISPSYSX                                                         
         LA    RE,PERACCS          FORMAT N(NN)-N(NN)                           
         ZIC   R0,CTSYSLMT+2                                                    
         EDIT  (R0),(3,0(RE)),ALIGN=LEFT                                        
         AR    RE,R0                                                            
         MVI   0(RE),C'-'                                                       
         ZIC   R0,CTSYSLMT+3                                                    
         EDIT  (R0),(3,1(RE)),ALIGN=LEFT                                        
*                                                                               
DISPSYSX B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
* ADD/CHANGE RECORD                                                             
*                                                                               
DATAVAL  MVC   KEYSAVE,KEY                                                      
         TM    ACTINDS,X'80'       IS THIS A MOVE?                              
         BNZ   VALMOVE             YES? BRANCH                                  
         CLI   ACTN,ADD                                                         
         BNE   DATAV2                                                           
         XC    CT0KEY,CT0KEY       BUILD KEY OF SECRET CODE RECORD              
         MVI   CT0KTYP,CT0KTEQU                                                 
         MVC   CT0KAGY,TWAAGY-TWAD(R2)                                          
         GOTO1 AFVAL,PERCODEH                                                   
         BZ    EXIT                                                             
         MVC   CT0KCODE,FLD                                                     
         MVC   SECCODE,FLD                                                      
         MVC   KEY,CT0KEY          READ RECORD                                  
         MVI   UPDATE,C'Y'                                                      
         GOTO1 AREAD                                                            
         BZ    EIIO                                                             
         TM    DMCB+8,X'10'        MUST BE NOT FOUND                            
         BZ    ERAE                                                             
         MVC   KEY,KEYSAVE         RESTORE KEY & BUILD VIRGIN RECORD            
         MVI   TEMP,0                                                           
         GOTO1 ABLDREC                                                          
         GOTO1 ABLDACT                                                          
         GOTO1 APUTEL                                                           
         B     DATAV15                                                          
*                                  VALIDATE MOVE OF OFFICE                      
VALMOVE  GOTO1 AFVAL,PERTYPEH                                                   
         BNZ   VM10                                                             
         MVI   FERN,1                                                           
         B     EXIT                                                             
* CHECK TO SEE WHICH TYPE OF KEY WE HAVE                                        
VM10     OC    CT0KEYS(12),CT0KEYS IS THIS FULL KEY?                            
         BNZ   VM50                YES, CONTINUE                                
         LA    R1,CT0DATA          ELSE USE 03 ELEM TO MAKE FULL KEY            
         SR    RE,RE                                                            
VM20     CLI   0(R1),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(2,R1),=X'0318'                                                 
         BE    *+14                                                             
         IC    RE,1(R1)                                                         
         AR    R1,RE                                                            
         B     VM20                                                             
         MVC   KEY+3(22),2(R1)     MAKE FULL KEY FROM 03                        
         GOTO1 AREAD                                                            
         BZ    EIIO                                                             
*                                                                               
VM50     MVC   LOFFC,CT0KOFFC      SAVE OLD OFFICE CODE                         
         MVC   KEY+3(2),FLD                                                     
         GOTO1 AREAD               SEE IF NEW KEY EXISTS                        
         BZ    EIIO                                                             
         TM    DMCB+8,X'10'        TEST IF REC FOUND                            
         BZ    ERAE                ONLY NF ALLOWED                              
*                                                                               
         MVC   KEY+3(2),LOFFC      READ OLD KEY                                 
         MVI   UPDATE,C'Y'                                                      
         GOTO1 AREAD                                                            
         BZ    EIIO                                                             
         OI    CT0KEY+27,X'80'     SET DELETE BIT AND WRITE BACK                
         BAS   RE,ACTIVITY                                                      
         GOTO1 AWRITE                                                           
         BZ    EIIO                                                             
*                                                                               
         NI    CT0KEY+27,X'7F'     TURN OFF DELETE BIT                          
         MVC   CT0KOFFC,FLD        SET NEW OFFICE CODE                          
         BAS   RE,ACTIVITY                                                      
         GOTO1 AADD                ADD NEW KEY                                  
         BZ    EIIO                                                             
*                                                                               
* USE 030C ELEM TO GET CODE KEY, AND CHANGE 0318 ELEM THERE                     
*                                                                               
         LA    R1,CT0DATA          ELSE USE 03 ELEM TO MAKE FULL KEY            
         SR    RE,RE                                                            
VM60     CLI   0(R1),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(2,R1),=X'030C'                                                 
         BE    *+14                                                             
         IC    RE,1(R1)                                                         
         AR    R1,RE                                                            
         B     VM60                                                             
         XC    KEY+3(22),KEY+3     CLEAR NAME FROM KEY                          
         MVC   KEY+15(10),2(R1)    MAKE FULL KEY FROM 03                        
         MVI   UPDATE,C'Y'                                                      
         GOTO1 AREAD                                                            
         BZ    EIIO                                                             
         TM    DMCB+8,X'10'        TEST IF REC FOUND                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R1,CT0DATA                                                       
         SR    RE,RE                                                            
VM70     CLI   0(R1),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(2,R1),=X'0318'    FIND 0318 EL AND CHANGE                      
         BE    *+14                                                             
         IC    RE,1(R1)                                                         
         AR    R1,RE                                                            
         B     VM70                                                             
         MVC   2(2,R1),FLD         CHANGE ELEM                                  
         BAS   RE,ACTIVITY                                                      
         GOTO1 AWRITE                                                           
         BZ    EIIO                                                             
*                                                                               
* USE 0304 ELEM TO GET AUTH KEY, AND CHANGE 0318 ELEM THERE                     
*                                                                               
         LA    R1,CT0DATA          ELSE USE 03 ELEM TO MAKE FULL KEY            
         SR    RE,RE                                                            
VM80     CLI   0(R1),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(2,R1),=X'0304'                                                 
         BE    *+14                                                             
         IC    RE,1(R1)                                                         
         AR    R1,RE                                                            
         B     VM80                                                             
         XC    KEY+3(22),KEY+3     CLEAR NAME FROM KEY                          
         MVC   KEY+23(02),2(R1)    MAKE FULL KEY FROM 03                        
         MVI   UPDATE,C'Y'                                                      
         GOTO1 AREAD                                                            
         BZ    EIIO                                                             
         TM    DMCB+8,X'10'        TEST IF REC FOUND                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R1,CT0DATA                                                       
         SR    RE,RE                                                            
VM90     CLI   0(R1),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(2,R1),=X'0318'    FIND 0318 EL AND CHANGE                      
         BE    *+14                                                             
         IC    RE,1(R1)                                                         
         AR    R1,RE                                                            
         B     VM90                                                             
         MVC   2(2,R1),FLD         CHANGE ELEM                                  
         BAS   RE,ACTIVITY                                                      
         GOTO1 AWRITE                                                           
         BZ    EIIO                                                             
*                                                                               
         B     EXIT                                                             
*                                  STRIP DOWN RECORD IN CORE                    
DATAV2   LA    R5,CT0DATA                                                       
         SR    R1,R1                                                            
DATAV4   CLI   0(R5),0             TEST E-O-R                                   
         BE    DATAV14                                                          
         CLI   0(R5),X'01'         ACTIVITY ELEMENT                             
         BE    DATAV6                                                           
         CLI   0(R5),X'02'         DESCRIPTION ELEMENT                          
         BE    DATAV6                                                           
         CLI   0(R5),X'03'         POINTER ELEMENT                              
         BE    DATAV10                                                          
         CLI   0(R5),X'20'         USER-ID ELEMENT                              
         BE    DATAV6                                                           
         CLI   0(R5),X'21'         SYSTEM ELEMENT                               
         BE    DATAV12                                                          
         B     DATAV8                                                           
*                                  SET ELEMENT DELETED                          
DATAV6   MVI   0(R5),X'FF'                                                      
*                                  DUMP TO NEXT ELEMENT                         
DATAV8   IC    R1,1(R5)                                                         
         AR    R5,R1                                                            
         B     DATAV4                                                           
*                                  SAVE POINTER VALUES & DELETE                 
         USING CTPASD,R5                                                        
DATAV10  CLI   CTPASLEN,4                                                       
         BNE   *+14                                                             
         MVC   SECNUM,CTPASDTA                                                  
         B     DATAV6                                                           
         MVC   SECCODE2,CTPASDTA                                                
         B     DATAV6                                                           
*                                  DELETE SYSTEM ELEMENT                        
         USING CTSYSD,R5                                                        
DATAV12  CLI   SYSTEM,0                                                         
         BE    DATAV8                                                           
         CLC   SYSTEM,CTSYSNUM                                                  
         BNE   DATAV8                                                           
         B     DATAV6                                                           
*                                                                               
DATAV14  MVI   TEMP,X'FF'          DELETE UNWANTED ELEMENTS                     
         GOTO1 ADELEL                                                           
         GOTO1 ABLDACT                                                          
         GOTO1 APUTEL              ADD NEW ACTIVITY ELEMENT                     
         GOTO1 AFVAL,PERCODEH                                                   
         BZ    EXIT                                                             
         MVC   SECCODE,FLD                                                      
*                                                                               
         CLC   SECCODE,SECCODE2    TEST IF SECRET CODE CHANGED                  
         BE    DATAV15                                                          
         LA    R4,IOAREA2          YES - TEST IF CODE ALREADY USED              
         XC    CT0KEY,CT0KEY       BUILD KEY OF SECRET CODE RECORD              
         MVI   CT0KTYP,CT0KTEQU                                                 
         MVC   CT0KAGY,TWAAGY-TWAD(R2)                                          
         MVC   CT0KCODE,SECCODE                                                 
         MVC   KEY,CT0KEY          READ RECORD                                  
         MVI   UPDATE,C'Y'                                                      
         ST    R4,AREC                                                          
         GOTO1 AREAD                                                            
         BZ    EIIO                                                             
         TM    DMCB+8,X'10'        ERROR IF RECORD EXISTS                       
         BZ    ERAE                                                             
         LA    R4,IOAREA                                                        
         ST    R4,AREC                                                          
         MVC   KEY,KEYSAVE                                                      
         B     DATAV15                                                          
         EJECT                                                                  
* BUILD DESCRIPTION ELEMENT                                                     
*                                                                               
DATAV15  GOTO1 AFVAL,PERCMNTH                                                   
         BZ    DATAV16                                                          
         ZIC   R1,FLDH+5                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TEMP+2(0),FLD                                                    
         LA    R1,2(R1)                                                         
         STC   R1,TEMP+1                                                        
         MVI   TEMP,X'02'                                                       
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
         B     DATAV16                                                          
         EJECT                                                                  
* VALIDATE AND BUILD USER-ID ELEMENTS                                           
*                                                                               
DATAV16  LA    R7,PERID1H                                                       
         MVI   FLAG,0                                                           
         LA    R8,2                                                             
*                                                                               
DATAV18  TM    1(R7),X'20'         SKIP PROTECTED FIELDS                        
         BZ    *+12                                                             
         SR    R1,R1                                                            
         IC    R1,0(R7)                                                         
         AR    R7,R1                                                            
         LR    R1,R7                                                            
         GOTO1 AFVAL                                                            
         BZ    DATAV34                                                          
         GOTO1 VSCANNER,DMCB,FLDH,(20,BLOCK1)                                   
         CLI   4(R1),0                                                          
         BE    EIIF                                                             
         LA    R6,BLOCK1           R6=A(SCAN BLOCK ENTRY)                       
         MVC   FCNT,4(R1)                                                       
         MVI   FNDX,1                                                           
*                                                                               
DATAV20  CLC   FNDX,FCNT                                                        
         BH    DATAV34                                                          
         CLI   1(R6),0                                                          
         BNE   DATAV22                                                          
         CLI   0(R6),3             VALIDATE USER-ID                             
         BL    EFTS                                                             
         CLI   0(R6),10                                                         
         BH    EFTL                                                             
         LA    R5,IOAREA2          SWITCH I/O AREAS                             
         ST    R5,AREC                                                          
         USING CTIREC,R5                                                        
         XC    CTIKEY,CTIKEY       BUILD ID KEY                                 
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID,12(R6)                                                    
         MVC   KEY,CTIKEY                                                       
         GOTO1 AREAD               CHECK RECORD FOUND/OK                        
         BZ    EIIO                                                             
         CLI   DMCB+8,0                                                         
         BNE   ERNF                                                             
         MVC   KEY,KEYSAVE                                                      
         LA    R4,IOAREA                                                        
         ST    R4,AREC                                                          
         MVC   TEMP(2),=X'200C'    BUILD & ADD ELEMENT                          
         MVC   TEMP+2(10),12(R6)                                                
         B     DATAV24                                                          
*                                                                               
DATAV22  CLI   0(R6),0             VALIDATE LIST-ID                             
         BE    EIIF                                                             
         ZIC   R1,0(R6)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R6),=C'LIST'   CHECK FOR VALID KEYWORD                      
         BNE   EIIF                                                             
         CLI   1(R6),6                                                          
         BH    EFTL                                                             
         LA    R5,IOAREA2          SWITCH I/O AREAS                             
         ST    R5,AREC                                                          
         USING CTWREC,R5                                                        
         XC    CTWKEY,CTWKEY       BUILD LIST KEY                               
         MVI   CTWKTYP,C'W'                                                     
         MVI   CTWKREC,C'I'                                                     
         MVC   CTWKID,22(R6)                                                    
         MVC   KEY,CTWKEY                                                       
         GOTO1 AREAD                                                            
         BZ    EIIO                CHECK RECORD FOUND/OK                        
         CLI   DMCB+8,0                                                         
         BNE   EIIF                                                             
         ST    R4,AREC                                                          
         MVC   KEY,KEYSAVE                                                      
         MVC   TEMP(2),=X'200C'    BUILD & ADD ELEMENT                          
         XC    TEMP+2(2),TEMP+2                                                 
         MVC   TEMP+4(8),22(R6)                                                 
*                                                                               
DATAV24  CLI   FLAG,0              FIRST LIST ENTRY (PRINCIPAL ID)              
         BNE   DATAV26                                                          
         OC    TEMP+2(2),TEMP+2    MUST BE AN ID (NOT A LIST)                   
         BZ    EIIF                                                             
         LA    R1,IOAREA2+(CTIDATA-CTIREC)                                      
         SR    R0,R0               FIND ALPHA-AGENCY ELEMENT                    
DATAV25  CLI   0(R1),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R1),X'06'                                                      
         BE    *+14                                                             
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     DATAV25                                                          
*                                  AGENCY ID MUST BE SAME AS LOGON ID           
         CLC   2(L'TWAAGY,R1),TWAAGY-TWAD(R2)                                   
         BNE   EIIF                                                             
         L     RF,AFACLIST                                                      
         L     RF,VCALLOV-SYSFACD(RF)                                           
         GOTO1 (RF),DMCB,0,X'D9000AFA',0                                        
         L     RF,0(R1)            RF=A(GETIDS)                                 
         GOTO1 (RF),DMCB,(C'C',IOAREA2),ATIA,VDATAMR                            
         CLI   0(R1),0                                                          
         BE    EIIF                                                             
         CLI   0(R1),X'FF'                                                      
         BE    ERNF                                                             
         MVI   FLAG,1                                                           
         B     DATAV32                                                          
*                                                                               
DATAV26  MVC   WORK(10),TEMP+2     OTHER LIST ENTRIES MUST BE IN FIRST          
         OC    WORK(2),WORK        LIST ENTRY'S COMPATIBLE ID LIST              
         BZ    *+16                                                             
         BAS   RE,VALID                                                         
         BE    EIIF                                                             
         B     DATAV32                                                          
         LA    R5,CTWDATA                                                       
         SR    R1,R1                                                            
*                                                                               
DATAV28  CLI   0(R5),0             IF ENTRY IS A LIST DO FOR ALL                
         BE    DATAV32             ENTRIES IN LIST RECORD                       
         CLI   0(R5),X'A4'                                                      
         BNE   DATAV30                                                          
         MVC   WORK(10),3(R5)                                                   
         BAS   RE,VALID                                                         
         BE    EIIF                                                             
DATAV30  IC    R1,1(R5)                                                         
         AR    R5,R1                                                            
         B     DATAV28                                                          
*                                  ADD ID ELEMENT TO TERM REC                   
DATAV32  GOTO1 VHELLO,DMCB,(C'P',=C'CTFILE '),AREC,TEMP,=C'ADD=CODE'            
         CLI   12(R1),0                                                         
         BE    *+12                                                             
         MVI   FERN,68             CHECK RECORD TOO BIG                         
         B     EXIT                                                             
*                                                                               
         ZIC   R1,FNDX             BUMP TO NEXT FIELD                           
         LA    R1,1(R1)                                                         
         STC   R1,FNDX                                                          
         LA    R6,32(R6)                                                        
         B     DATAV20                                                          
*                                                                               
DATAV34  ZIC   R1,0(R7)            BUMP TO NEXT TWA LINE                        
         AR    R7,R1                                                            
         BCT   R8,DATAV18                                                       
         B     DATAV3M                                                          
         EJECT                                                                  
* VALIDATE LIMIT ACCESS                                                         
*                                                                               
DATAV3M  XC    TEMP,TEMP           INITIALISE SYSTEM ELEMENT                    
         LA    R7,TEMP                                                          
         USING CTSYSD,R7                                                        
         MVC   CTSYSEL(2),=X'2110'                                              
         MVC   CTSYSNUM,SYSTEM                                                  
         MVC   CTSYSALL,XAUTH      PRESET ALL & PROGRAM VALUES                  
         MVC   CTSYSPGM,XAUTH                                                   
         MVC   CTSYSPGM+2(126),CTSYSPGM                                         
*                                                                               
         LA    R1,PERACCSH                                                      
         GOTO1 AFVAL                                                            
         BZ    DATAV3X                                                          
         CLI   SYSTEM,0                                                         
         BE    EIIF                                                             
         MVC   CTSYSLMT,FLD                                                     
         CLC   CTSYSLMT(2),=CL2'L=' TEST LIMIT ACCESS LIST                      
         BNE   DATAV3M1            FOR ALL SYSTEMS                              
         MVC   KEYSAVE,KEY                                                      
         LA    R5,IOAREA2          SWITCH I/O AREAS                             
         ST    R5,AREC                                                          
         USING SALMREC,R5                                                       
         XC    SALMKEY,SALMKEY     CHECK VALID LIMIT ACCESS LIST                
         MVI   SALMTYP,SALMTYPQ                                                 
         MVI   SALMSUB,SALMSUBQ                                                 
         MVC   SALMSYS,SYSTEM                                                   
         MVC   SALMAGY,TWAAGY-TWAD(R2)                                          
         MVC   SALMLID,CTSYSLMT+2                                               
         MVC   KEY,SALMKEY                                                      
         GOTO1 AREAD                                                            
         BZ    EIIO                CHECK RECORD FOUND/OK                        
         CLI   DMCB+8,0                                                         
         BNE   EIIF                                                             
         ST    R4,AREC                                                          
         MVC   KEY,KEYSAVE                                                      
         MVC   CTSYSLMT(2),=XL2'FFFF'                                           
         B     DATAV3X                                                          
*                                                                               
DATAV3M1 CLI   SYSTEM,14           TEST PERSONNEL SYSTEM                        
         BNE   DATAV3O                                                          
         XC    CTSYSLMT,CTSYSLMT                                                
         GOTO1 VSCANNER,DMCB,FLDH,(2,BLOCK1),C',=*-'                            
         XC    8(4,R1),8(R1)       CLEAR SPECIAL SCAN CHARACTERS                
         CLI   4(R1),0                                                          
         BE    EIIF                                                             
         CLI   4(R1),1             FORMAT=N(NN)-N(NN)                           
         BNE   EIIF                                                             
         CLI   BLOCK1,0            VALIDATE LOWER LIMIT                         
         BE    EIIF                                                             
         TM    BLOCK1+2,X'80'                                                   
         BZ    EFNN                                                             
         OC    BLOCK1+4(4),BLOCK1+4                                             
         BZ    EIIF                                                             
         OC    BLOCK1+4(3),BLOCK1+4                                             
         BNZ   EIIF                                                             
         MVC   CTSYSLMT+2(1),BLOCK1+7                                           
         MVC   CTSYSLMT+3(1),BLOCK1+7                                           
         CLI   BLOCK1+1,0          TEST UPPER LIMIT GIVEN                       
         BE    DATAV3X                                                          
         MVI   FNDX,2                                                           
         TM    BLOCK1+3,X'80'      VALIDATE UPPER LIMIT                         
         BZ    EFNN                                                             
         OC    BLOCK1+8(4),BLOCK1+8                                             
         BZ    EIIF                                                             
         OC    BLOCK1+8(3),BLOCK1+8                                             
         BNZ   EIIF                                                             
         MVC   CTSYSLMT+3(1),BLOCK1+11                                          
         CLC   CTSYSLMT+3(1),CTSYSLMT+2                                         
         BL    EIIF                                                             
         B     DATAV3X                                                          
DATAV3O  DS    0H                                                               
*&&UK                                                                           
         CLI   SYSTEM,4            TEST UK/MEDIA                                
         BNE   DATAV3X                                                          
         XC    CTSYSLMT,CTSYSLMT                                                
         GOTO1 VSCANNER,DMCB,FLDH,(5,BLOCK1),X'6B5E6BFF'                        
         CLI   4(R1),4             FORMAT=N(N),N(N),N(N),N(N)                   
         BNE   EIIF                                                             
         LA    RF,BLOCK1                                                        
         LA    R1,CTSYSLMT                                                      
         LA    R0,4                                                             
         LA    RE,1                                                             
DATAV3O2 STC   RE,FNDX                                                          
         CLI   0(RF),0                                                          
         BE    EIIF                                                             
         CLI   0(RF),2                                                          
         BH    EIIF                                                             
         TM    2(RF),X'80'                                                      
         BZ    EFNN                                                             
         MVC   0(1,R1),7(RF)                                                    
         LA    RF,L'BLOCK1(RF)                                                  
         LA    R1,1(R1)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,DATAV3O2                                                      
         MVI   FNDX,0                                                           
         B     DATAV3X                                                          
*&&                                                                             
*&&US                                                                           
         CLI   SYSTEM,3            TEST NETWORK                                 
         BE    *+12                                                             
         CLI   SYSTEM,2            TEST SPOT                                    
         BNE   DATAV3P                                                          
*                                                                               
         MVC   DMCB+4(4),=X'D9000A14'  CLPACK                                   
         L     RE,AFACLIST                                                      
         USING SYSFACD,RE                                                       
         L     RF,VCALLOV                                                       
         DROP  RE                                                               
         GOTO1 (RF),DMCB,0                                                      
         L     RF,0(R1)                                                         
         GOTO1 (RF),(R1),FLD,CTSYSLMT                                           
         CLI   0(R1),0                                                          
         BNE   EIIF                                                             
*                                                                               
DATAV3P  CLI   SYSTEM,6            TEST ACC                                     
         BNE   DATAV3X                                                          
         CLC   FLD(2),=C'T='       TEST FOR T=ULCC (DPS TALENT)                 
         BNE   DATAV3X                                                          
         CLI   FLDH+5,6                                                         
         BL    EFTS                                                             
         MVC   CTSYSLMT(2),FLD+2                                                
         GOTO1 VHEXIN,DMCB,FLD+4,CTSYSLMT+2,2                                   
         OC    12(4,R1),12(R1)                                                  
         BZ    EFNH                                                             
         MVC   CTSYSLMT+3(3),=C'TAL'                                            
         B     DATAV3X                                                          
*&&                                                                             
*                                                                               
DATAV3X  B     DATAV40                                                          
         EJECT                                                                  
* VALIDATE PROGRAM ACCESS LIST                                                  
*                                                                               
DATAV40  GOTO1 AFVAL,PERPRG1H                                                   
         BNZ   *+16                                                             
         CLI   SYSTEM,0                                                         
         BE    DATAEND                                                          
         B     EXIT                                                             
         CLI   SYSTEM,0                                                         
         BE    EIIF                                                             
*                                  TEST FOR DELETE                              
         CLC   FLD(6),=C'DELETE'                                                
         BNE   DATAV46                                                          
         CLI   ACTN,ADD                                                         
         BE    EIIF                                                             
         B     DATAEND                                                          
*                                                                               
DATAV46  LA    R6,PERPRG1H                                                      
         BAS   RE,GETSE                                                         
         LA    R8,2                                                             
*                                                                               
DATAV48  TM    1(R6),X'20'         SKIP PROTECTED FIELDS                        
         BZ    *+12                                                             
         SR    R1,R1                                                            
         IC    R1,0(R6)                                                         
         AR    R6,R1                                                            
         LR    R1,R6                                                            
         GOTO1 AFVAL                                                            
         BZ    DATAV59                                                          
         GOTO1 VSCANNER,DMCB,FLDH,(20,BLOCK1)                                   
         CLI   4(R1),0                                                          
         BE    EIIF                                                             
         LA    R5,BLOCK1           R5=A(SCAN BLOCK ENTRY)                       
         MVC   FCNT,4(R1)                                                       
         MVI   FNDX,1                                                           
*                                                                               
DATAV50  CLC   FNDX,FCNT                                                        
         BH    DATAV59                                                          
         CLI   0(R5),7             L'PART1                                      
         BH    EFTL                                                             
         CLI   0(R5),1             L'PART1                                      
         BL    EFTS                                                             
         CLI   1(R5),4             L'PART2                                      
         BH    EFTL                                                             
         BE    DATAV52                                                          
         CLI   1(R5),1             L'PART2                                      
         BNE   EIIF                                                             
         MVC   DUB(2),YAUTH        ONE CHR INPUT S/B Y OR N                     
         CLI   22(R5),C'Y'                                                      
         BE    DATAV54                                                          
         MVC   DUB(2),NAUTH                                                     
         CLI   22(R5),C'N'                                                      
         BE    DATAV54                                                          
         B     EIIF                                                             
*                                                                               
DATAV52  TM    3(R5),X'20'         IF NOT Y OR N MUST BE VALID HEX              
         BZ    EFNH                                                             
         GOTO1 VHEXIN,DMCB,22(R5),DUB,4                                         
         OC    DMCB+12(4),DMCB+12  DOUBLE CHECK FOR VALID HEX                   
         BZ    EFNH                                                             
*                                                                               
DATAV54  CLI   0(R5),3                                                          
         BNE   DATAV56                                                          
         CLC   12(3,R5),=C'ALL'                                                 
         BNE   DATAV56                                                          
         CLC   CTSYSALL,XAUTH      ALL VALUES ALREADY INPUT ?                   
         BNE   EDIF                                                             
         MVC   CTSYSALL,DUB                                                     
         B     DATAV58                                                          
*                                                                               
DATAV56  BAS   RE,GETPRGX          GET PROGRAM NUMBER                           
         CLI   PROGRAM,X'FF'                                                    
         BE    EIIF                                                             
         SR    R1,R1                                                            
         IC    R1,PROGRAM                                                       
         BCTR  R1,0                                                             
         SLL   R1,1                                                             
         LA    R1,CTSYSPGM(R1)     POINT TO PROGRAM AUTH                        
         CLC   0(2,R1),XAUTH       PRGM VALUE PREVIOUSLY INPUT ?                
         BNE   EDIF                                                             
         MVC   0(2,R1),DUB                                                      
*                                                                               
DATAV58  ZIC   R1,FNDX             BUMP TO NEXT FIELD                           
         LA    R1,1(R1)                                                         
         STC   R1,FNDX                                                          
         LA    R5,32(R5)                                                        
         B     DATAV50                                                          
*                                                                               
DATAV59  ZIC   R1,0(R6)            BUMP TO NEXT TWA LINE                        
         AR    R6,R1                                                            
         BCT   R8,DATAV48                                                       
DATAV60  CLC   CTSYSALL,XAUTH      SET ALL VALUE TO N IF N/I                    
         BNE   *+10                                                             
         MVC   CTSYSALL,NAUTH                                                   
         LA    R1,CTSYSPGM         SET PRG VALUES TO ALL VALUE IF N/I           
         LA    RE,64                                                            
*                                                                               
DATAV62  CLC   0(2,R1),XAUTH                                                    
         BNE   *+10                                                             
         MVC   0(2,R1),CTSYSALL                                                 
         LA    R1,2(R1)                                                         
         BCT   RE,DATAV62                                                       
         OI    CTSYSEL+5,X'80'                                                  
         CLC   CTSYSALL(128),CTSYSPGM                                           
         BE    DATAV63                                                          
         BAS   RE,CNVELM21                                                      
         B     DATAV63A                                                         
DATAV63  MVI   CTSYSLEN,16         SET SHORT IF NO PRGM OVERRIDES               
DATAV63A CLI   ACTN,ADD                                                         
         BE    DATAV64                                                          
*                                                                               
DATAV64  GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
         EJECT                                                                  
* ADD/WRITE RECORDS TO FILE                                                     
*                                                                               
DATAEND  CLI   ACTN,ADD                                                         
         BNE   DATAEND2                                                         
*                                  HANDLE ADD                                   
         MVC   KEYSAVE,KEY         ALLOCATE NEXT POINTER NUMBER                 
         LA    R4,IOAREA2                                                       
         XC    CT0KEY,CT0KEY       BUILD KEY OF NUMBER RECORD                   
         MVI   CT0KTYP,CT0KTEQU                                                 
         MVC   CT0KAGY,TWAAGY-TWAD(R2)                                          
         MVC   KEY,CT0KEY                                                       
         MVI   UPDATE,C'Y'                                                      
         ST    R4,AREC                                                          
         GOTO1 AREADHI                                                          
         XC    SECNUM,SECNUM                                                    
         CLC   CT0KEY(CT0KNUM-CT0KEY),KEY                                       
         BNE   *+10                                                             
         MVC   SECNUM,CT0KNUM      GET LAST ALLOCATED NUMBER                    
         SR    R1,R1               DECREMENT LAST BY 1 GIVING THIS              
         ICM   R1,3,SECNUM                                                      
         BCTR  R1,0                                                             
         STCM  R1,3,SECNUM                                                      
         B     DATAEND4                                                         
*                                  HANDLE CHANGE                                
DATAEND2 MVC   KEYSAVE,KEY         YES - DELETE OLD SECRET CODE RECORD          
         LA    R4,IOAREA2                                                       
         XC    CT0KEY,CT0KEY                                                    
         MVI   CT0KTYP,CT0KTEQU                                                 
         MVC   CT0KAGY,TWAAGY-TWAD(R2)                                          
         MVC   CT0KCODE,SECCODE2                                                
         MVC   KEY,CT0KEY                                                       
         ST    R4,AREC             READ OLD RECORD                              
         MVI   UPDATE,C'Y'                                                      
         GOTO1 AREAD                                                            
         CLI   DMCB+8,0                                                         
         BNE   EIIO                                                             
         CLC   SECCODE,SECCODE2    TEST IF SECRET CODE CHANGED                  
         BE    DATAEND4                                                         
         OI    CT0STAT,X'80'       SET DELETE BIT & WRITE BACK                  
         GOTO1 AWRITE                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                  NOW ADD/WRITE NEW RECORDS                    
DATAEND4 MVC   KEY,KEYSAVE         RESTORE KEY & A(I/O AREA)                    
         LA    R4,IOAREA                                                        
         ST    R4,AREC                                                          
         MVC   TEMP(2),=X'030C'    BUILD POINTER ELEMENTS                       
         MVC   TEMP+2(10),SECCODE                                               
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
         MVC   TEMP(2),=X'0304'                                                 
         MVC   TEMP+2(2),SECNUM                                                 
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
*                                  ADD/WRITE NAME RECORD                        
         L     RF,AADD                                                          
         CLI   ACTN,ADD                                                         
         BE    *+8                                                              
         L     RF,AWRITE                                                        
         MVC   KEY,CT0KEY                                                       
         BASR  RE,RF                                                            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 ADELEL              BUILD POINTER ELEMENTS                       
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
         MVC   TEMP(2),=X'0318'                                                 
         MVC   TEMP+2(22),CT0KOFFC                                              
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
*                                  ADD/WRITE CODE RECORD                        
         XC    CT0KEYS,CT0KEYS                                                  
         MVC   CT0KCODE,SECCODE                                                 
         MVC   KEY,CT0KEY                                                       
         L     RF,AADD                                                          
         CLI   ACTN,ADD                                                         
         BE    DATAEND6                                                         
         CLC   SECCODE,SECCODE2    ADD IF SECRET CODE CHANGED                   
         BNE   DATAEND6                                                         
         L     RF,AWRITE                                                        
DATAEND6 BASR  RE,RF                                                            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 ADELEL              BUILD POINTER ELEMENTS                       
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
         MVC   TEMP(2),=X'030C'                                                 
         MVC   TEMP+2(10),SECCODE                                               
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
*                                  ADD/WRITE NUMBER RECORD                      
         XC    CT0KEYS,CT0KEYS                                                  
         MVC   CT0KNUM,SECNUM                                                   
         MVC   KEY,CT0KEY                                                       
         L     RF,AADD                                                          
         CLI   ACTN,ADD                                                         
         BE    DATAEND8                                                         
         MVI   UPDATE,C'Y'                                                      
         LA    R1,IOAREA2                                                       
         ST    R1,AREC                                                          
         GOTO1 AREAD                                                            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         ST    R4,AREC                                                          
         L     RF,AWRITE                                                        
DATAEND8 BASR  RE,RF                                                            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                  SET NEXT TIME ACTION & EXIT                  
         MVI   NACTN,OKCHA                                                      
         LA    R1,BASACTNH                                                      
         ST    R1,FADR                                                          
         MVI   FERN,X'FF'                                                       
         B     EXIT                                                             
         EJECT                                                                  
* LOCATE SELIST ENTRY FOR SYSTEM                                                
*                                                                               
GETSE    NTR1                                                                   
         L     RE,AFACLIST                                                      
         USING SYSFACD,RE                                                       
         L     R5,VSELIST                                                       
         LH    R6,0(R5)                                                         
         L     R7,2(R5)                                                         
         LA    R5,6(R5)                                                         
         USING SELISTD,R5                                                       
*                                                                               
         CLC   SYSTEM,SEOVSYS                                                   
         BE    *+10                                                             
         BXLE  R5,R6,*-10                                                       
         DC    H'0'                DIE IF N/F                                   
*                                                                               
         ST    R5,ASE              SAVE A(SELIST ENTRY)                         
         MVC   APGM,SEPGMS         AND A(SEPGMS)                                
         B     EXIT                                                             
         DROP  RE,R5                                                            
         SPACE 1                                                                
* CHECK ID IS IN COMPATIBLE ID LIST OF MASTER ID                                
*                                                                               
VALID    L     RF,ATIA                                                          
VALID2   CLC   0(10,RF),WORK                                                    
         BE    VALIDX                                                           
         LA    RF,12(RF)                                                        
         CLI   0(RF),X'FF'                                                      
         BNE   VALID2                                                           
VALIDX   CLI   0(RF),X'FF'         CC=EQ IF NOT IN LIST                         
         BR    RE                                                               
         EJECT                                                                  
* CONVERT THE X'21' ELEMENT TO THE NEW FORMAT                                   
*                                                                               
CNVELM21 NTR1                                                                   
         MVC   BLOCK1(L'TEMP),TEMP COPY THE ELEMENT OVER                        
         LA    R7,BLOCK1           POINT FROM WHERE TO COPY                     
         USING CTSYSD,R7                                                        
         LA    R2,TEMP+CTSYSPGM-CTSYSD  WHERE TO COPY                           
         LA    R3,1                FIRST PROGRAM                                
         LA    R0,16               LENGTH IS HEADER FIRST                       
         LA    R6,CTSYSPGM                                                      
CNV21LP  CLC   0(2,R6),CTSYSALL    DEFAULT?                                     
         BE    CNV21NX             YES, SKIP TO NEXT ONE                        
         STC   R3,0(R2)            STORE THE PROGRAM NUMBER                     
         MVC   1(2,R2),0(R6)       AND ITS AUTHORIZATION CODE                   
         AH    R0,=H'3'            LENGTH IS CHANGED BY 3                       
         LA    R2,3(R2)            NEXT POSTION FOR NEXT PROGRAM                
CNV21NX  LA    R3,1(R3)            NEXT PROGRAM NUMBER                          
         LA    R6,2(R6)            NEXT AUTHORIZATION CODE                      
         CH    R3,=H'64'           DID WE DO ALL 64 PROGRAMS?                   
         BNH   CNV21LP             NO, CONTINUE UNTIL WE'RE DONE                
         LA    R7,TEMP             STORE THE NEW LENGTH OF THE ELEMENT          
         STC   R0,CTSYSLEN                                                      
         XIT1                                                                   
         DROP  R7                                                               
         EJECT                                                                  
* SET PROGRAM NAME FROM PROGRAM NUMBER                                          
*                                                                               
GETPRGN  NTR1                                                                   
         L     R5,APGM                                                          
         LH    R6,0(R5)                                                         
         L     R7,2(R5)                                                         
         LA    R5,6(R5)                                                         
         USING PGMLSTD,R5                                                       
GETPRGN2 CLC   PGMNUM,PROGRAM      MATCH ON PROGRAM NUMBER                      
         BE    GETPRGN4                                                         
         CLI   PGMALNUM,0                                                       
         BE    *+14                                                             
         CLC   PGMALNUM,PROGRAM    OR ACCESS OVERRIDE IF SET                    
         BE    GETPRGN4                                                         
         BXLE  R5,R6,GETPRGN2                                                   
         B     GETPRGNN                                                         
GETPRGN4 GOTO1 TSTAGYLA,(R5)       TEST FOR RESTRICTED AGENCY                   
         BNE   GETPRGNN            ACCESS LIST                                  
         B     GETPRGNY                                                         
GETPRGNN MVI   PROGRAM,X'FF'       SET PROGRAM=X'FF' IF N/F                     
         B     GETPRGNX                                                         
GETPRGNY MVC   PGNAME,PGMNAME      PROGRAM FOUND OK                             
GETPRGNX B     EXIT                                                             
         DROP  R5                                                               
         SPACE 1                                                                
* SET PROGRAM NUMBER FROM PROGRAM NAME                                          
* R5=A(SCANNER BLOCK ENTRY)                                                     
*                                                                               
GETPRGX  NTR1                                                                   
         LR    R3,R5                                                            
         SR    R1,R1                                                            
         IC    R1,0(R3)                                                         
         BCTR  R1,0                R1=L'COMPARE                                 
         L     R5,APGM                                                          
         LH    R6,0(R5)                                                         
         L     R7,2(R5)                                                         
         LA    R5,6(R5)                                                         
         USING PGMLSTD,R5                                                       
GETPRGX2 EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   PGMNAME(0),12(R3)                                                
         BE    GETPRGX4                                                         
         BXLE  R5,R6,GETPRGX2                                                   
         B     GETPRGXN                                                         
GETPRGX4 GOTO1 TSTAGYLA,(R5)       TEST FOR RESTRICTED AGENCY                   
         BNE   GETPRGXN            ACCESS LIST                                  
         MVC   PROGRAM,PGMNUM      SET PROGRAM NUMBER                           
         CLI   PGMALNUM,0                                                       
         BE    *+10                                                             
         MVC   PROGRAM,PGMALNUM    OR PROGRAM OVERRIDE IF SET                   
         B     GETPRGXX                                                         
GETPRGXN MVI   PROGRAM,X'FF'       SET PROGRAM=X'FF' IF N/F                     
         B     GETPRGXX                                                         
GETPRGXX B     EXIT                                                             
         DROP  R5                                                               
         SPACE 1                                                                
* TEST FOR RESTRICTED PROGRAM AGENCY ACCESS LIST                                
* R1=A(PGMLST)                                                                  
*                                                                               
         USING PGMLSTD,R1                                                       
TSTAGYLA NTR1                                                                   
         SR    RF,RF                                                            
         ICM   RF,7,PGMAGYLA       TEST FOR RESTRICTED AGENCY                   
         BZ    TALAY               ACCESS LIST ADDRESS                          
TALA6    CLC   0(2,RF),=CL2'  '                                                 
         BE    TALAN               END OF LIST                                  
         CLC   0(2,RF),TWAAGY-TWAD(R2)                                          
         BE    TALAY               CONNECTED AGENCY IN LIST                     
         LA    RF,2(RF)                                                         
         B     TALA6               BUMP TO NEXT AGENCY IN LIST                  
TALAY    SR    RC,RC                                                            
TALAN    LTR   RC,RC                                                            
         XIT1                                                                   
         DROP  R1                                                               
         EJECT                                                                  
ACTIVITY NTR1                                                                   
         L     R5,AREC                                                          
         CLI   28(R5),X'01'                                                     
         BNE   EXIT                                                             
         GOTO1 ABLDACT                                                          
         MVC   28(5,R5),TEMP       MOVE IN NEW ACTIVITY ELEMENT                 
         B     EXIT                                                             
         EJECT                                                                  
* CTLFMERRS                                                                     
       ++INCLUDE CTLFMERRS                                                      
         EJECT                                                                  
* LITERALS                                                                      
*                                                                               
         LTORG                                                                  
         SPACE 1                                                                
YAUTH    DC    X'000F'                                                          
NAUTH    DC    X'0000'                                                          
XAUTH    DC    X'FFFF'                                                          
         EJECT                                                                  
HEAD     DS    0X                                                               
         SPROG 1                                                                
         SPEC  H1,1,RUN                                                         
         SPEC  H1,71,PAGE                                                       
         SPEC  H1,35,C'PASSWORDS BY NAME'                                       
         SPEC  H2,01,C'TYPE NAME                   CODE  '                      
         SPEC  H2,33,C'       LAST CHG SYSTEMS           '                      
         SPEC  H2,81,C'STATUS'                                                  
*                                                                               
         SPROG 2                                                                
         SPEC  H1,1,RUN                                                         
         SPEC  H1,71,PAGE                                                       
         SPEC  H1,35,C'PASSWORDS BY CODE'                                       
         SPEC  H2,01,C'CODE       TYPE NAME              '                      
         SPEC  H2,40,C'LAST CHG SYSTEMS                  '                      
         SPEC  H2,81,C'STATUS'                                                  
HEADX    DC    AL1(0)                                                           
         EJECT                                                                  
       ++INCLUDE FASYSLST                                                       
         EJECT                                                                  
*                                                                               
* DSECT TO COVER TEMP W/S                                                       
*                                                                               
WORKD    DSECT                                                                  
VSCINKEY DS    V                                                                
VREPORT  DS    V                                                                
ASYSEL   DS    A                                                                
ASEPGMS  DS    A                                                                
ASE      DS    A                                                                
APGM     DS    A                                                                
*                                                                               
SYSTEM   DS    C                                                                
PROGRAM  DS    C                                                                
FLAG     DS    X                                                                
IDCNT    DS    X                                                                
PGCNT    DS    X                                                                
FCNT     DS    X                                                                
PGNAME   DS    CL8                                                              
LOFFC    DS    CL2                                                              
*                                                                               
SECCODE  DS    CL10                                                             
SECCODE2 DS    CL10                                                             
SECNUM   DS    XL2                                                              
*                                                                               
BLOCK1   DS    20CL32                                                           
BLOCK2   DS    20CL32                                                           
*                                                                               
IOAREA2  DS    1024C                                                            
*                                                                               
       ++INCLUDE FAREPBLK                                                       
         EJECT                                                                  
REPHS    DS    0CL132              ** REPORT HEAD LINES **                      
REPH1    DS    CL132                                                            
REPH2    DS    CL132                                                            
REPH3    DS    CL132                                                            
REPH4    DS    CL132                                                            
REPHN    EQU   (*-REPHS)/L'REPHS                                                
*                                                                               
REPPS    DS    0CL132              ** REPORT PRINT LINES **                     
REPP1    DS    CL132                                                            
REPPN    EQU   (*-REPPS)/L'REPPS                                                
*                                                                               
REPX     EQU   *                                                                
REPL     EQU   *-REPHS                                                          
*                                                                               
WORKX    EQU   *                                                                
         EJECT                                                                  
* CTLFMDSECT                                                                    
       ++INCLUDE CTLFMDSECT                                                     
LSYSTEM  DS    C                                                                
* SEACSFILE                                                                     
       ++INCLUDE SEACSFILE                                                      
* CTLFMACTNS                                                                    
       ++INCLUDE CTLFMACTNS                                                     
         EJECT                                                                  
* CTLFMTWA                                                                      
       ++INCLUDE CTLFMTWA                                                       
* CTLFME3D                                                                      
       ++INCLUDE CTLFME3D                                                       
         EJECT                                                                  
* FADSECTS                                                                      
       ++INCLUDE FADSECTS                                                       
         SPACE 1                                                                
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* FASYSLSTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FASYSLSTD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'024CTLFM1C   09/05/03'                                      
         END                                                                    
