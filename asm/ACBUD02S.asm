*          DATA SET ACBUD02S   AT LEVEL 006 AS OF 05/01/02                      
*                                                                               
*PHASE T61002A,*                                                                
         TITLE 'ACBUD02 - BUDGET PROGRAM - BUDGET TYPES'                        
ACBUD02  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**BUD2**                                                       
         SPACE 1                                                                
         USING GWS,R9                                                           
         USING TWAD,R8                                                          
         SPACE 1                                                                
         MVC   FLAG,SAVSTAT        SAVSTAT & FRMINITA ARE USED FOR              
         MVI   SAVSTAT,0           CONTINUATION STATUS & KEY                    
         MVI   NEXTMODE,TYPEMODE                                                
         CLI   ACTION,LIS                                                       
         BE    BT10                                                             
         CLI   ACTION,HLP                                                       
         BE    BT50                                                             
         B     BT80                                                             
         EJECT ,                                                                
* PROCESS LIST ACTION                                                           
         SPACE 1                                                                
BT10     LA    R3,BLILIN1H         CLEAR ANY USED LINES                         
         ZIC   R4,0(,R3)                                                        
         LA    R5,BLILINLH                                                      
*                                                                               
BT11     OC    8(78,R3),8(R3)                                                   
         BZ    *+14                                                             
         XC    8(78,R3),8(R3)                                                   
         OI    6(R3),TRANSMIT                                                   
         BXLE  R3,R4,BT11                                                       
*                                                                               
BT12     XC    KEY,KEY             PREPARE FOR READ/DISPLAY LOOP                
         LA    R7,KEY                                                           
*                                                                               
         USING ACKEYD,R7                                                        
*                                                                               
         MVI   ACBTKTYP,ACBTKTEQ                                                
         MVC   ACBTKCMP,COMPANY                                                 
         TM    FLAG,CONTINUE                                                    
         BNO   *+10                                                             
         MVC   KEY,FRMINITA                                                     
         MVI   COUNT,0                                                          
         L     R7,ABUDREC                                                       
         LA    R3,BLILIN1H                                                      
         SR    R3,R4                                                            
         GOTO1 ARDHI,ABUDREC                                                    
         L     RF,ASEQ                                                          
         B     *+6                                                              
*                                                                               
BT14     BASR  RE,RF                                                            
         TM    DMCB+8,2            SKIP DELETES                                 
         BO    BT14                                                             
         CLI   DMCB+8,0                                                         
         BNE   ERRXIT                                                           
         CLC   KEYSAVE(ACBTKCOD-ACBTKEY),ACBTKEY                                
         BNE   BT40                END                                          
         BXLE  R3,R4,*+8                                                        
         B     BT45                SCREEN FULL                                  
         MVI   COUNT,C'Y'                                                       
         LA    R6,ACRECORD         DISPLAY FLDS                                 
         MVC   8(L'ACBTKCOD,R3),ACBTKCOD                                        
         EDIT  ACBTKNO2,(3,22(R3)) NUMBER                                       
         SR    R0,R0                                                            
         OI    6(R3),TRANSMIT                                                   
*                                                                               
BT16     CLI   0(R6),0             ELEMENT SEARCH                               
         BE    BT14                                                             
         CLI   0(R6),X'1B'                                                      
         BE    BT18                                                             
         CLI   0(R6),X'20'                                                      
         BE    BT20                                                             
*                                                                               
BT17     IC    R0,1(,R6)                                                        
         AR    R6,R0                                                            
         B     BT16                                                             
*                                                                               
         USING ACBCD,R6                                                         
*                                                                               
BT18     DS    0H                  COL NAMES                                    
         MVC   28(10,R3),ACBCCOL1                                               
         MVC   39(10,R3),ACBCCOL2                                               
         B     BT17                                                             
*                                                                               
         USING ACNAMED,R6                                                       
*                                                                               
BT20     DS    0H                  FULL NAME                                    
         ZIC   RE,ACNMLEN                                                       
         SH    RE,=H'3'                                                         
         EX    RE,*+8                                                           
         B     BT14                                                             
         MVC   50(0,R3),ACNMNAME                                                
*                                                                               
BT40     LA    R1,VIRACTH          END                                          
         ST    R1,FADR                                                          
         MVC   MSG(21),=C'NO RECORDS TO DISPLAY'                                
         CLI   COUNT,0                                                          
         BE    OKEND                                                            
         MVC   MSG(37),=C'RECORDS DISPLAYED - ENTER NEXT ACTION'                
         B     OKEND                                                            
*                                                                               
BT45     LA    R1,VIRTABH          SCREEN FULL                                  
         ST    R1,FADR                                                          
         MVC   MSG(33),=C'HIT ENTER FOR CONTINUATION SCREEN'                    
         MVI   SAVSTAT,CONTINUE                                                 
         MVC   FRMINITA,ACBTKEY                                                 
         OI    VIRTABH+6,X'81'     MODIFY NEXT TIME                             
         B     OKEND                                                            
         EJECT ,                                                                
* PROCESS HELP ACTION                                                           
         SPACE 1                                                                
BT50     MVC   MSG(34),=C'HELP DISPLAYED - - ENTER NEXT ACTION'                 
         LA    R1,VIRACTH                                                       
         ST    R1,FADR                                                          
         B     OKEND                                                            
         EJECT ,                                                                
* PROCESS ALL OTHER ACTIONS                                                     
         SPACE 1                                                                
BT80     GOTO1 AFVAL,BTYCODEH      CHECK TYPE CODE                              
         BZ    ERRXIT                                                           
         XC    KEY,KEY                                                          
         LA    R7,KEY                                                           
*                                                                               
         USING ACKEYD,R7                                                        
*                                                                               
         MVI   ACBTKTYP,ACBTKTEQ                                                
         MVC   ACBTKCMP,COMPANY                                                 
         MVC   ACBTKCOD,FLD                                                     
         L     RF,ARDHIL                                                        
         CLI   ACTION,ADD                                                       
         BE    BT82                                                             
         CLI   ACTION,DIS                                                       
         BE    BT82                                                             
         CLI   ACTION,CHA                                                       
         BNE   BT84                                                             
         TM    BTYCODEH+4,VALPREV                                               
         BO    BT84                                                             
*                                                                               
BT82     L     RF,ARDHI                                                         
*                                                                               
BT84     GOTO1 (RF),ABUDREC        READ TYPE RECORD                             
         TM    DMCB+8,X'FD'                                                     
         BNZ   ERRXIT                                                           
         L     R7,ABUDREC                                                       
         MVC   KEYSAVE,ACBTKEY                                                  
         ZIC   R1,FLDH+5           SET NOT FOUND BIT IF CODE DOESN'T            
         LA    R1,(ACBTKCOD-ACBTKEY-1)(,R1)      MATCH                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   ACBTKEY(0),KEY                                                   
         BE    *+12                                                             
         OI    DMCB+8,X'10'                                                     
         B     BT86                                                             
         MVC   BTYCODE,ACBTKCOD                                                 
         OI    BTYCODEH+6,TRANSMIT                                              
*                                                                               
BT86     CLI   ACTION,ADD          BRANCH FOR ACTION                            
         BE    BT200               ADD                                          
         CLI   ACTION,REN                                                       
         BE    BT150               RENAME (DELETE OLD, ADD NEW)                 
         CLI   ACTION,RES                                                       
         BNE   BT100               DIS,CHA,DEL                                  
         MVI   FERN,NTDELETD                                                    
         CLI   DMCB+8,2                                                         
         BNE   ERRXIT                                                           
         B     BT102               UNDELETE                                     
         EJECT ,                                                                
* DISPLAY BUDGET TYPE FIELDS                                                    
         SPACE 1                                                                
BT100    MVI   FERN,NOTFOUND                                                    
         CLI   DMCB+8,0                                                         
         BNE   ERRXIT                                                           
*                                                                               
BT102    CLI   ACTION,DIS                                                       
         BE    *+12                                                             
         TM    BTYCODEH+4,VALPREV                                               
         BO    BT135               ALREADY DISPLAYED                            
         TWAXC BTYNAMEH                                                         
         XC    BTYCODN,BTYCODN                                                  
         OI    BTYCODNH+6,TRANSMIT                                              
         MVC   BTYCODN(7),=C'NUMBER='                                           
         LA    RF,BTYCODN+7                                                     
         EDIT  ACBTKNO2,(3,0(RF)),ALIGN=LEFT                                    
*                                                                               
BT103    MVC   CRD,SPACES          OPTIONS - ACSTATUS BITS                      
         SR    RF,RF                                                            
         LA    RE,OPTAB                                                         
         LA    R1,CRD                                                           
         MVI   FLAG,0                                                           
*                                                                               
BT103A   CLI   0(RE),X'FF'         END OF OPTIONS                               
         BE    BT103C                                                           
         CLC   FLAG,0(RE)          SYNONYM                                      
         BE    BT103B                                                           
         MVC   FLAG,0(RE)                                                       
         NC    FLAG,ACSTATUS                                                    
         BZ    BT103B                                                           
         MVC   0(20,R1),1(RE)                                                   
         LA    R1,20(,R1)                                                       
         LA    RF,1(,RF)                                                        
*                                                                               
BT103B   LA    RE,L'OPTAB(,RE)                                                  
         B     BT103A                                                           
*                                                                               
BT103C   LTR   RF,RF                                                            
         BZ    BT103X                                                           
         GOTO1 VUNSCAN,DMCB,((RF),CRD),BTYOPTH                                  
*                                                                               
BT103X   DS    0H                                                               
*                                                                               
         SR    R0,R0                                                            
         LA    R6,ACRECORD                                                      
*                                                                               
BT104    CLI   0(R6),0                                                          
         BE    BT130                                                            
         CLI   0(R6),ACBCELEQ                                                   
         BE    BT108                                                            
         CLI   0(R6),ACBVELEQ                                                   
         BE    BT112                                                            
         CLI   0(R6),X'20'                                                      
         BE    BT110                                                            
*                                                                               
BT106    IC    R0,1(,R6)                                                        
         AR    R6,R0                                                            
         B     BT104                                                            
*                                                                               
         USING ACBCD,R6                                                         
*                                                                               
BT108    DS    0H                  COL NAMES                                    
         MVC   BTYCOL1,ACBCCOL1                                                 
         MVC   BTYCOL2,ACBCCOL2                                                 
         B     BT106                                                            
*                                                                               
         USING ACNAMED,R6                                                       
*                                                                               
BT110    DS    0H                  FULL NAME                                    
         ZIC   RE,ACNMLEN                                                       
         SH    RE,=H'3'                                                         
         EX    RE,*+8                                                           
         B     BT106                                                            
         MVC   BTYNAME(0),ACNMNAME                                              
*                                                                               
         USING ACBVD,R6                                                         
*                                                                               
BT112    DS    0H                  VALIDATION RULES                             
         LA    R5,BTYACC1H                                                      
*                                                                               
BT114    MVC   8(2,R5),ACBVACUL                                                 
         MVI   10(R5),C'='                                                      
         MVC   11(1,R5),ACBVACLV                                                
         OI    11(R5),X'F0'                                                     
         IC    R0,0(,R5)                                                        
         AR    R5,R0                                                            
         LA    R4,8(,R5)                                                        
         LA    R1,ACBVCAUN         BXLE FOR SUBEL                               
         ZIC   R3,ACBVLEN                                                       
         AR    R3,R6                                                            
         BCTR  R3,0                                                             
         LA    R2,3                                                             
         B     BT118                                                            
*                                                                               
BT116    MVI   0(R4),C','                                                       
         LA    R4,1(,R4)                                                        
*                                                                               
BT118    CLI   0(R1),0             ALL                                          
         BNE   BT120                                                            
         MVC   0(3,R4),=C'ALL'                                                  
         LA    R4,3(,R4)                                                        
         B     BT124                                                            
*                                                                               
BT120    MVC   0(2,R4),0(R1)       +N(N)                                        
         CLI   0(R1),C'+'                                                       
         BNE   BT122                                                            
         OI    1(R4),X'F0'                                                      
         LA    R4,2(,R4)                                                        
         CLI   2(R1),0                                                          
         BE    BT124                                                            
         MVI   0(R4),C'('                                                       
         EDIT  (1,2(R1)),(1,1(R4))                                              
         MVI   2(R4),C')'                                                       
         LA    R4,3(,R4)                                                        
         B     BT124                                                            
*                                                                               
BT122    LA    R4,1(,R4)           U, UL, UL=N                                  
         CLI   1(R1),0                                                          
         BE    BT124                                                            
         LA    R4,1(,R4)                                                        
         CLI   2(R1),0                                                          
         BE    BT124                                                            
         MVI   0(R4),C'='                                                       
         MVC   1(1,R4),2(R1)                                                    
         OI    1(R4),X'F0'                                                      
         LA    R4,2(,R4)                                                        
*                                                                               
BT124    BXLE  R1,R2,BT116         BUMP TO NEXT SUBEL                           
         LR    R6,R1               END OF EL                                    
         CLI   0(R6),ACBVELEQ      IS NEXT SAME TYPE                            
         BNE   BT104               NO                                           
         IC    R0,0(,R5)           YES - BUMP TWA                               
         AR    R5,R0                                                            
         B     BT114                                                            
*                                                                               
BT130    OI    BTYCODEH+4,VALPREV  SET VALIDATED BIT                            
         CLI   ACTION,CHA          IF CHANGE EXIT FOR INPUT                     
         BNE   BT135                                                            
         MVC   MSG(36),=C'RECORD DISPLAYED - NOW ENTER CHANGES'                 
         LA    R1,BTYNAMEH                                                      
         ST    R1,FADR                                                          
         B     BT280                                                            
*                                                                               
BT135    CLI   ACTION,CHA                                                       
         BE    BT210                                                            
         CLI   ACTION,DIS                                                       
         BNE   BT137                                                            
         MVC   MSG(16),=C'RECORD DISPLAYED'                                     
         B     BT275                                                            
*                                                                               
BT137    CLI   ACTION,DEL                                                       
         BNE   BT139                                                            
         OI    ACSTATUS,X'80'                                                   
         MVC   MSG(14),=C'RECORD DELETED'                                       
         B     BT270                                                            
*                                                                               
BT139    CLI   ACTION,RES                                                       
         BNE   BT141                                                            
         NI    ACSTATUS,X'7F'                                                   
         MVC   MSG(15),=C'RECORD RESTORED'                                      
         B     BT270                                                            
*                                                                               
BT141    DC    H'0'                                                             
         EJECT ,                                                                
* RENAME                                                                        
         SPACE 1                                                                
BT150    TM    DMCB+8,X'10'        CHECK FOR DUP                                
         BO    BT151                                                            
         MVI   FERN,DUPLICT                                                     
         B     ERRXIT                                                           
*                                                                               
BT151    TM    FLAG,DISPLAY        RENAME - REC MUST BE DISPLAYED               
         BO    BT152                                                            
         LA    R1,VIRACTH                                                       
         ST    R1,FADR                                                          
         MVI   FERN,INVACTSQ                                                    
         B     ERRXIT                                                           
*                                                                               
BT152    MVC   KEY,FRMINITA        DELETE + ADD                                 
         LA    R0,2                                                             
         B     BT156                                                            
*                                                                               
BT154    MVC   KEYSAVE,ACBTKEY                                                  
         LA    R7,KEY                                                           
         MVC   ACBTKNO1,ACBTKNO2                                                
         XC    ACBTKNO2,ACBTKNO2                                                
         L     R7,ABUDREC                                                       
BT156    GOTO1 AREADL,ABUDREC                                                   
         BNE   ERRXIT                                                           
         OI    ACSTATUS,X'80'                                                   
         GOTO1 AWRITE                                                           
         BNE   ERRXIT                                                           
         MVC   ACBTKCOD,FLD                                                     
         NI    ACSTATUS,X'7F'                                                   
         GOTO1 AADD                                                             
         BNE   ERRXIT                                                           
         BCT   R0,BT154                                                         
         MVC   KEYSAVE,ACBTKEY                                                  
         MVC   MSG(14),=C'RECORD RENAMED'                                       
         B     BT275                                                            
         EJECT ,                                                                
* ADD OR CHANGE                                                                 
         SPACE 1                                                                
BT200    TM    DMCB+8,X'10'        CHECK FOR DUP ON ADD                         
         BO    BT205                                                            
         MVI   FERN,DUPLICT                                                     
         B     ERRXIT                                                           
*                                                                               
BT205    DS    0H                  ADD - BUILD RECORD FROM SCREEN               
         XC    ACKEYD(ACRECORD-ACKEYD),ACKEYD                                   
         MVI   ACBTKTYP,ACBTKTEQ                                                
         MVC   ACBTKCMP,COMPANY                                                 
         MVC   ACBTKCOD,FLD                                                     
*                                                                               
BT210    MVI   ACSTATUS,0          ADD OR CHANGE                                
         MVI   ACRECORD,0                                                       
         LA    R6,ACRECORD                                                      
*                                                                               
BT212    GOTO1 AFVAL,BTYNAMEH      NAME - SAVE ELEMENT IN WORK1                 
         BZ    ERRXIT                                                           
         ZIC   RE,FLDH+5                                                        
         LA    RE,2(,RE)                                                        
         MVI   WORK1,X'20'                                                      
         STC   RE,WORK1+1                                                       
         MVC   WORK1+2(L'BTYNAME),FLD                                           
*                                                                               
BT214    GOTO1 AFVAL,BTYCOL1H      COLUMN NAMES                                 
         BZ    ERRXIT                                                           
*                                                                               
         USING ACBCD,R6                                                         
*                                                                               
         MVI   ACBCEL,ACBCELEQ                                                  
         MVI   ACBCLEN,ACBCLNEQ                                                 
         MVC   ACBCCOL1,FLD                                                     
         MVC   ACBCCOL2,SPACES                                                  
         GOTO1 AFVAL,BTYCOL2H                                                   
         BZ    *+10                                                             
         MVC   ACBCCOL2,FLD                                                     
         LA    R6,ACBCLNEQ(,R6)                                                 
*                                                                               
BT216    GOTO1 AFVAL,BTYOPTH       OPTIONS - IE STATUS SETTINGS                 
         BZ    BT218                                                            
         GOTO1 VSCANNER,DMCB,FLDH,(3,TEMP)                                      
         ZIC   R0,4(,R1)                                                        
         MVI   FNDX,0                                                           
         CLI   4(R1),1                                                          
         BE    *+8                                                              
         MVI   FNDX,1                                                           
         LA    RF,TEMP                                                          
         MVI   FERN,INVALID                                                     
         SR    R1,R1                                                            
*                                                                               
BT216A   IC    R1,0(,RF)           PROCESS A SCANNER ENTRY                      
         SH    R1,=H'1'                                                         
         BM    ERRXIT              MUST BE LHS                                  
         LA    RE,OPTAB                                                         
*                                                                               
BT216B   CLI   0(RE),X'FF'         COMPARE LHS WITH OPTAB                       
         BE    ERRXIT              NO MATCH                                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   1(0,RE),12(RF)                                                   
         BNE   BT216C                                                           
         CLC   11(10,RE),SPACES    MATCHES                                      
         BE    BT216D              NO RHS REQUIRED                              
         CLC   11(10,RE),22(RF)                                                 
         BE    BT216D              FULL MATCH ON RHS IF REQUIRED                
*                                                                               
BT216C   LA    RE,L'OPTAB(,RE)                                                  
         B     BT216B                                                           
*                                                                               
BT216D   OC    ACSTATUS,0(RE)      SET STATUS                                   
         BCT   R0,*+8                                                           
         B     BT218               END                                          
         IC    R1,FNDX                                                          
         LA    R1,1(,R1)                                                        
         STC   R1,FNDX                                                          
         LA    RF,32(,RF)                                                       
         B     BT216A                                                           
*                                                                               
BT218    LA    R3,BTYACC1H         ACC/CAC VALIDATION RULES                     
         LA    R5,BTYACCLH                                                      
         MVI   COUNT,0                                                          
*                                                                               
BT220    GOTO1 AFVAL,(R3)          LOOP FOR A LINE                              
         BNZ   BT222                                                            
         CLI   COUNT,0                                                          
         BNE   BT250                                                            
         B     ERRXIT              AT LEAST ONE                                 
*                                                                               
BT222    MVI   FERN,INVALID                                                     
         MVC   XTRAMESS(10),=C'(UL=LEVEL)'                                      
         CLI   FLDH+5,4                                                         
         BNE   ERRXIT                                                           
         CLI   FLD+2,C'='                                                       
         BNE   ERRXIT                                                           
***      CLC   FLD(2),=C'SJ'       PROD LEDGER INVALID                          
***      BE    ERRXIT                                                           
         GOTO1 ACHECKUL,DMCB,FLD,ACCNTRL                                        
         BNE   ERRXIT                                                           
         MVI   FERN,DUPED          CHECK FOR DUPLICATES                         
         LA    R1,ACRECORD                                                      
         SR    R0,R0                                                            
*                                                                               
BT222A   CR    R1,R6                                                            
         BE    BT223                                                            
         CLI   0(R1),ACBVELEQ                                                   
         BNE   *+14                                                             
         CLC   ACBVACUL-ACBVD(2,R1),FLD                                         
         BE    ERRXIT                                                           
         IC    R0,1(,R1)                                                        
         AR    R1,R0                                                            
         B     BT222A                                                           
*                                                                               
BT223    LA    RF,FLD+3                                                         
         LA    R1,ACLLENS                                                       
         BAS   RE,CHECKLEV         CHECK LEVEL AT RF EXISTS FOR LEDGER          
         BZ    ERRXIT                                                           
*                                                                               
         USING ACBVD,R6                                                         
*                                                                               
         MVI   ACBVEL,ACBVELEQ     BUILD EL                                     
         MVC   ACBVACUL,FLD                                                     
         MVC   ACBVACLV,FLD+3                                                   
         XC    ACBVCAUN(6*3),ACBVCAUN                                           
         LA    RA,ACBVCAUN                                                      
*                                                                               
         USING ACBVCAUN,RA                                                      
*                                                                               
         MVC   XTRAMESS,SPACES                                                  
*                                                                               
BT224    ZIC   R0,0(,R3)           PROCESS CONTRA FLD                           
         AR    R3,R0                                                            
         GOTO1 AFVAL,(R3)                                                       
         BZ    ERRXIT                                                           
         GOTO1 VSCANNER,DMCB,FLDH,(6,TEMP)                                      
         MVC   FLAG1,4(R1)                                                      
         CLI   FLAG1,1                                                          
         BNH   *+8                                                              
         MVI   FNDX,1                                                           
         LA    R2,TEMP                                                          
         ZIC   R0,4(,R1)                                                        
*                                                                               
BT226    MVI   FERN,INVALID        ALL                                          
         CLC   12(3,R2),=C'ALL'                                                 
         BNE   BT228                                                            
         CLI   1(R2),0                                                          
         BNE   ERRXIT                                                           
         CLI   FLAG1,1                                                          
         BNZ   BT230               MUST BE ONLY ONE                             
         LA    RA,3(,RA)                                                        
         B     BT240                                                            
*                                                                               
BT228    CLI   12(R2),C'+'         +N(N)                                        
         BNE   BT236                                                            
         CLI   1(R2),0                                                          
         BNE   ERRXIT                                                           
         CLI   FLAG1,1                                                          
         BZ    *+12                MUST BE ONLY ONE                             
*                                                                               
BT230    MVI   FNDX,2                                                           
         B     ERRXIT                                                           
         MVC   ACBVCAUN(2),12(R2)                                               
         TM    13(R2),X'F0'                                                     
         MVI   FERN,INVNUM                                                      
         BNO   ERRXIT                                                           
         NI    ACBVCALE,X'0F'                                                   
         ZIC   R1,0(,R2)                                                        
         SH    R1,=H'2'                                                         
         BZ    BT232                                                            
         CLI   14(R2),C'('         (N) QUALIFIER                                
         MVI   FERN,NOPARNTH                                                    
         BNE   ERRXIT                                                           
         CLI   16(R2),C')'                                                      
         BNE   ERRXIT                                                           
         TM    15(R2),X'F0'                                                     
         MVI   FERN,INVNUM                                                      
         BNO   ERRXIT                                                           
         MVC   ACBVCALV,15(R2)                                                  
         NI    ACBVCALV,X'0F'                                                   
         BZ    ERRXIT                                                           
*                                                                               
BT232    LA    RA,3(,RA)                                                        
         B     BT240                                                            
*                                                                               
BT236    DS    0H                  OTHERWISE MUST BE UL OR UL=N                 
         GOTO1 ACHECKUL,DMCB,12(R2),CACNTRL                                     
         BNE   ERRXIT                                                           
         MVC   ACBVCAUN(2),12(R2)                                               
         CLI   0(R2),2                                                          
         BNE   ERRXIT                                                           
         CLI   1(R2),0                                                          
         BE    BT238                                                            
         LA    RF,22(,R2)          LEVEL                                        
         LA    R1,CALLENS                                                       
         BAS   RE,CHECKLEV         DOES IT EXIST FOR LEDGER                     
         BZ    ERRXIT                                                           
         MVC   ACBVCALV,22(R2)                                                  
*                                                                               
BT238    LA    RA,3(,RA)           BUMP SCANNER BLOCK & SUBEL POINTER           
         LA    R2,32(,R2)                                                       
         CLI   FNDX,0                                                           
         BZ    BT240               NEXT ONE IF ANY MUST BE A LEDGER             
         ZIC   R1,FNDX                                                          
         LA    R1,1(,R1)                                                        
         STC   R1,FNDX                                                          
         CLC   FNDX,FLAG1                                                       
         BNH   BT236                                                            
*                                                                               
BT240    SR    RA,R6               END OF SUBELS                                
         STC   RA,ACBVLEN                                                       
         AR    R6,RA                                                            
         MVI   COUNT,C'Y'                                                       
         ZIC   R4,0(,R3)                                                        
         BXLE  R3,R4,BT220                                                      
*                                                                               
BT250    MVC   0(L'WORK1,R6),WORK1 MOVE IN NAME EL AND COMPLETE REC             
         ZIC   R0,1(,R6)                                                        
         AR    R6,R0                                                            
         MVI   0(R6),0                                                          
         SR    R6,R7                                                            
         LA    R6,1(,R6)                                                        
         STH   R6,ACLENGTH                                                      
*                                                                               
BT252    CLI   ACTION,ADD          GET NEXT TYPE NUMBER FOR ADD                 
         BNE   BT260                                                            
         LA    R7,KEY                                                           
         XC    ACBTKEY,ACBTKEY                                                  
         MVI   ACBTKTYP,ACBTKTEQ                                                
         MVC   ACBTKCMP,COMPANY                                                 
         LA    R2,1                                                             
         LA    R1,AIOAREA2                                                      
         L     R3,0(,R1)                                                        
         L     RF,ARDHI                                                         
*                                                                               
BT254    STCM  R2,3,ACBTKNO1                                                    
         BASR  RE,RF                                                            
         TM    DMCB+8,2                                                         
         BO    *+12                                                             
         CLI   DMCB+8,0                                                         
         BNZ   ERRXIT                                                           
         CLC   ACBTKEY(3),0(R3)                                                 
         BNE   *+12                                                             
         LA    R2,1(,R2)                                                        
         B     BT254                                                            
*                                                                               
         MVC   ACBTKNO2,ACBTKNO1   BUILD KEY                                    
         XC    ACBTKNO1,ACBTKNO1                                                
         MVC   ACBTKCOD,BTYCODE                                                 
         OC    ACBTKCOD,SPACES                                                  
         L     R7,ABUDREC                                                       
         MVC   ACBTKEY,KEY                                                      
         GOTO1 AADD,ABUDREC        ADD 2 RECORDS                                
         BNE   ERRXIT                                                           
         MVC   KEYSAVE,KEY                                                      
         MVC   ACBTKNO1,ACBTKNO2                                                
         XC    ACBTKNO2,ACBTKNO2                                                
         BASR  RE,RF                                                            
         BNE   ERRXIT                                                           
         MVC   MSG(12),=C'RECORD ADDED'                                         
         B     BT275                                                            
*                                                                               
BT260    DS    0H                  ACTION CHANGE                                
         MVC   MSG(14),=C'RECORD CHANGED'                                       
         B     BT270                                                            
         EJECT ,                                                                
* FINAL FUNCTIONS FOR ALL ACTIONS                                               
         SPACE 1                                                                
BT270    GOTO1 AWRITE,ABUDREC      TWO WRITES FOR AMENDING ACTIONS              
         BNE   ERRXIT                                                           
         L     R7,ABUDREC                                                       
         MVC   ACBTKNO1,ACBTKNO2                                                
         XC    ACBTKNO2,ACBTKNO2                                                
         MVC   KEY,ACBTKEY                                                      
         GOTO1 AREADL,AIOAREA2                                                  
         BE    BT272                                                            
         CLI   ACTION,RES                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   DMCB+8,2            DELETED                                      
         BNE   *-6                                                              
*                                                                               
BT272    GOTO1 AWRITE,ABUDREC                                                   
         BNE   ERRXIT                                                           
*                                                                               
BT275    LA    R1,MSG+16           COMPLETE PARTIAL MESSAGE                     
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         MVC   2(19,R1),=C'- ENTER NEXT ACTION'                                 
         LA    R1,VIRACTH                                                       
         ST    R1,FADR                                                          
*                                                                               
BT280    CLI   ACTION,DEL          SET FLAGS FOR NEXT ACTION                    
         BE    OKEND                                                            
         OI    BTYCODEH+4,VALPREV  RECORD IS DISPLAYED                          
         OI    SAVSTAT,DISPLAY     OK FOR RENAME NEXT ACTION                    
         MVC   FRMINITA,KEYSAVE    SAVE KEY FOR RENAME                          
*                                                                               
OKEND    MVI   FERN,OK             OK COMPLETION                                
         B     EXIT                                                             
*                                                                               
OKXIT    SR    R8,R8               CC=EQU FOR OK EXITS FROM ROUTINES            
*                                                                               
ERRXIT   LTR   R8,R8                                                            
*                                                                               
EXIT     XIT1                                                                   
         EJECT ,                                                                
* CHECK LEDGER LEVEL AT (RF) AGAINST LENGTHS AT (R1)                            
*                                                                               
* RETURN CC=EQU IF NO SUCH LEVEL AND FERN SET                                   
* LEVEL IS IN EBCDIC ON INPUT, IN BINARY ON RETURN                              
         SPACE 1                                                                
CHECKLEV NI    0(RF),X'0F'                                                      
         ZIC   R0,0(,RF)                                                        
         AR    R1,R0                                                            
         BCTR  R1,0                                                             
         CLI   0(R1),0                                                          
         BNER  RE                                                               
         MVI   FERN,LEVNXIST                                                    
         BR    RE                                                               
         EJECT ,                                                                
         SPACE 1                                                                
         LTORG                                                                  
         EJECT ,                                                                
         SPACE 1                                                                
OPTAB    DS    0CL21               OPTION TABLE - ACSTATUS SETTINGS AND         
*                                  DISPLAY KEYWORDS (X OR X=Y)                  
         DC    AL1(THIRTEEN),CL20'13MONTH'                                      
         DC    AL1(THOUSAND),CL10'UNITS',CL10'1000'                             
         DC    AL1(THOUSAND),CL20'THOUSANDS'                                    
         DC    X'FF'                                                            
         EJECT ,                                                                
         SPACE 1                                                                
* ACBUDDSECT                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACBUDDSECT                                                     
         PRINT ON                                                               
         SPACE 3                                                                
* ACGENBOTH                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
         EJECT ,                                                                
         SPACE 1                                                                
TWAD     DSECT                                                                  
         ORG   VIRTABH                                                          
       ++INCLUDE ACBUDFED                                                       
         EJECT ,                                                                
         SPACE 1                                                                
         ORG   VIRTABH                                                          
       ++INCLUDE ACBUDFDD                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006ACBUD02S  05/01/02'                                      
         END                                                                    
