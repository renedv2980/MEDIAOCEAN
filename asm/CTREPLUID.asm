*          DATA SET CTREPLUID  AT LEVEL 036 AS OF 05/14/19                      
*PHASE CTREPLUA                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE SORTER                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE STXITER                                                                
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE BINSRCH2                                                               
*INCLUDE PDUMPER                                                                
*INCLUDE DMDMGRL                                                                
*INCLUDE XSORT                                                                  
*INCLUDE CARDS                                                                  
*                                                                               
***********************************************************************         
*  TTL:      CTREPLU: CONTROL FILE COMTENGEN REPORT                   *         
*  PURPOSE:  -PRODUCE REPORT OF ACTIVE AND UNKNOWN TERMIANL RECORDS   *         
*             SORTED BY LUID/PU.                                      *         
*            -PRODUCE REPORT OF IDLE AND CTFILE TERMIANL RECORDS      *         
*             SORTED BY LUID/NODE.                                    *         
*  NOTE:     ACTIVE  - EXIST IN BOTH CTFILE AND NETWORK               *         
*            UNKNOWN - EXIST IN NETWORK, BUT NOT DEFINED IN CTFILE    *         
*            IDLE    - EXIST IN CTFILE, BUT NOT USED IN NETWORK       *         
***********************************************************************         
CTREPLU  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,CTREPLU,=V(REGSAVE),R7                                         
*                                                                               
         ENTRY UTL                                                              
         ENTRY SSB                                                              
*                                                                               
         L     R8,=V(CPRINT)                                                    
         USING DPRINT,R8                                                        
*                                                                               
         GOTO1 =V(STXITER),DMCB,A(DUMPLIST)                                     
         B     MAIN                                                             
*                                                                               
DUMPLIST DS    0F                                                               
         DC    A(CTREPLU,65000)                                                 
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG   *                                                                
         EJECT                                                                  
MAIN     DS    0H                                                               
*                                                                               
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD                                 
         BAS   RE,READCT           READ CTFILE                                  
         BAS   RE,SETOPT           SCAN AND SET REPORT OPTIONS TIL DSC          
*                                                                               
         CLI   EOF,1               NO DATA SET TO READ?                         
         BE    M15                                                              
M10      BAS   RE,SETDCB           SET DATA CONTROL BLOCK FOR ONE DSC           
         BAS   RE,READDS           READ A DATA SET                              
         CLI   EOF,1               NO MORE DATA SET TO READ?                    
         BE    *+12                YES                                          
         CLI   DSEND,1             NO MORE DATA SET TO READ?                    
         BNE   M10                 NO - READ NEXT DS                            
*                                                                               
M15      TM    OPT1CT,REPORT       CTFILE REPORT OPT IS ON                      
         BZ    *+8                                                              
         BAS   RE,SORTCT           FEED CTFILE REC IN BST TO SORTER             
*                                                                               
         TM    OPT2ID,REPORT       IDLE REPORT OPT IS ON                        
         BZ    *+8                                                              
         BAS   RE,SORTIDLE         FEED IDLE REC IN BST TO SORTER               
*                                                                               
         TM    OPT4UN,REPORT       UNKNOWN REPORT OPT IS ON                     
         BZ    M40                                                              
*                                                                               
         BAS   RE,READUNK          READ UNKNOWN REC AND BUILD A BST             
M20      CLI   EOF,1               NO MORE DATA SET TO READ?                    
         BE    M30                 YES - EXIT                                   
         BAS   RE,SETDCBX          SET DCB FOR EXCLUDE UNKNOWN                  
         BAS   RE,READDS           READ A DATA SET X                            
         B     M20                                                              
M30      BAS   RE,SORTUNK          FEED UNKNOWNX REC BACK TO SORTER             
*                                                                               
M40      DS    0H                  PRINT ROUTINES                               
*                                                                               
         BAS   RE,BOX                                                           
*                                                                               
         TM    OPT1CT,REPORT       CTFILE REPORT OPT IS ON                      
         BZ    *+8                                                              
         BAS   RE,PRTCTFI          PRINT CTFILE REC FROM SORTER                 
*                                                                               
         TM    OPT2ID,REPORT       IDLE REPORT OPT IS ON                        
         BZ    *+8                                                              
         BAS   RE,PRTIDLE          PRINT IDLE REC FROM SORTER                   
*                                                                               
         TM    OPT3AC,REPORT       ACTIVE REPORT OPT IS ON                      
         BZ    *+8                                                              
         BAS   RE,PRTACTI          PRINT ACTIVE REC FROM SORTER                 
*                                                                               
         TM    OPT4UN,REPORT       UNKNOWN REPORT OPT IS ON                     
         BZ    *+8                                                              
         BAS   RE,PRTUNKX          PRINT UNKNOWN REC FROM SORTER                
*                                                                               
         GOTO1 =V(SORTER),DMCB,=C'END'                                          
         XBASE                                                                  
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*        READCT - READ ALL CTFILE TERMINAL REC AND BUILD BINSCHTAB    *         
***********************************************************************         
READCT   NTR1                                                                   
         SR    R9,R9               RESET REC COUNTER                            
         L     R3,ABST             ADDR. BINSCH TAB                             
         USING LUIDRECD,R3                                                      
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMOPEN'),=C'CONTROL',FLIST,AIO            
*                                                                               
         USING CTTREC,R2                                                        
         LA    R2,KEY                                                           
         XC    KEY,KEY             GET THE FIRST TERMINAL REC                   
         MVI   CTTKTYP,CTTKTYPQ                                                 
         MVI   CTTKTID,C'A'                                                     
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMRDHI'),=C'CTFILE',KEY,AIO               
*                                                                               
         L     R2,AIO                                                           
RCT10    CLI   CTTKTYP,CTTKTYPQ    STILL READING TERMINAL REC?                  
         BNE   RCTX                NO - DONE                                    
         OC    CTTKSPAR,CTTKSPAR   MUST BE BINARY ZERO                          
         BNZ   RCTX                NO - DONE                                    
*                                                                               
         MVC   LUID,CTTKTID        TERMINAL ID                                  
         MVI   STATUS,STBSTCTN     RESET THE STATUS BYTE                        
*                                                                               
         LR    R6,R2               AIO                                          
         MVI   ELCODE,CTTRMELQ     TERMINAL DEFN ELEMENT                        
         BAS   RE,GETEL                                                         
         BE    *+14                ELEMENT NOT FOUND                            
*                                                                               
         MVC   NODE,=C'---'        DEFAULT TO C'---' WHEN NO DEFN ELEM          
         B     RCT30                                                            
*                                                                               
         L     R4,ANODETBL                                                      
RCT20    CLI   0(R4),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                NODE NOT DEFINED IN NODETBL                  
*                                                                               
         USING CTTRMD,R6                                                        
         CLC   3(1,R4),CTTRMNDE    COMPARE NODE TO THE TABLE                    
         BNE   *+14                                                             
         MVC   NODE,0(R4)                                                       
         B     RCT30                                                            
*                                                                               
         LA    R4,NODETBLL(,R4)                                                 
         B     RCT20                                                            
         DROP  R6                                                               
*                                                                               
RCT30    LA    R3,L'LUIDREC(R3)                                                 
         LA    R9,1(R9)                                                         
*                                                                               
         MVC   KEY,0(R2)                                                        
         MVI   KEY+15,X'FF'        SKIP SAME LUID W/ DIFF PASSWORD              
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMRDHI'),=C'CTFILE',KEY,AIO               
         B     RCT10                                                            
*                                                                               
RCTX     ST    R9,CTCOUNT          STORE # OF CTFILE TERM. REC.                 
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        SETOPT - SET OPTIONS FOR REPORT UNTIL DATA SET CARD(DSC) READ*         
***********************************************************************         
SETOPT   NTR1                                                                   
*                                                                               
SO00     GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   =C'DS=',CARD        DATA SET FOUND?                              
         BE    SOX                                                              
*                                                                               
         CLC   =C'/*',CARD         END OF INPUT CARDS?                          
         BNE   *+12                                                             
         MVI   EOF,1                                                            
         B     SOX                                                              
*                                                                               
         CLC   =C'DSX=',CARD       EXCLUDING DATA SET FOUND?                    
         BNE   *+6                                                              
         DC    H'0'                ERROR DS MUST BE FOUND BEFORE DSX            
*                                                                               
         CLC   =C'REPORT=',CARD    REPORT OPTIONS?                              
         BNE   SO00                SKIP OVER COMMENTS                           
*                                                                               
*                                  CLEAR THE ENTIRE CARD BUFFER                 
         LA    R1,MAXLINEQ*L'CARDBUF                                            
         LA    R0,CARDBUF                                                       
         SR    RE,RE               DON'T CARE VALUE OF RE                       
         ICM   RF,15,=X'00000000'  PAD VALUE X'00'                              
         MVCL  R0,RE               PAD WITH BIN ZEROS                           
*                                                                               
*                                                                               
SO10     CLC   =C'CTFILE',CARD+7   CTFILE REPORT OPTIONS                        
         BNE   SO20                                                             
*                                                                               
         OI    OPT1CT,REPORT       TURN ON THE PRINT OUT BIT                    
*                                                                               
         MVC   CARDBUF,CARD                                                     
         CLI   CARD+71,C' '        CONTINUE ON THE NEXT LINE                    
         BE    SO10B                                                            
         LA    R9,MAXLINEQ-1       MAX LINES OF CONTINUATION                    
         LA    R2,CARDBUF+L'CARDBUF                                             
*                                  READ IN ALL CONTINUED LINES                  
SO10A    GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         MVC   0(L'CARDBUF,R2),CARD                                             
         CLI   CARD+71,C' '        CONTINUE ON THE NEXT LINE                    
         BE    SO10B                                                            
         LA    R2,L'CARDBUF(R2)                                                 
         BCT   R9,SO10A                                                         
*                                                                               
*                                  FIND AND SET CTFILE SORT OPTION              
SO10B    LA    R2,CARDBUF+13                                                    
SO10C    CLC   =C'SORT=',0(R2)                                                  
         BE    SO10D                                                            
*                                                                               
         LA    R2,1(R2)                                                         
         CLI   0(R2),X'00'                                                      
         BNE   SO10C                                                            
         B     SO10E                                                            
*                                                                               
SO10D    CLC   =C'NODE',5(R2)                                                   
         BNE   SO10E                                                            
         OI    OPT1CT,SORTNODE                                                  
*                                                                               
*                                  FIND AND SET CTFILE NODE FILTER LIST         
SO10E    LA    R2,CARDBUF+13                                                    
SO10F    CLC   =C'FILTER=',0(R2)                                                
         BE    SO10G                                                            
*                                                                               
         LA    R2,1(R2)                                                         
         CLI   0(R2),X'00'                                                      
         BNE   SO10F                                                            
         B     SO00                                                             
*                                                                               
SO10G    LA    R2,7(R2)            INPUT FILTER LIST                            
         L     R3,ACTFLST          STORAGE FILTER LIST                          
         SR    R9,R9               FILTER COUNTER                               
*                                                                               
SO10H    CLI   L'CTFLST(R2),C','   MORE FILTER TO READ IN                       
         BNE   SO10I                                                            
*                                                                               
         MVC   0(L'CTFLST,R3),0(R2)                                             
         LA    R9,1(R9)                                                         
         LA    R3,L'CTFLST(R3)                                                  
         LA    R2,L'CTFLST+1(R2)   NODE + ','                                   
         B     SO10H                                                            
*                                                                               
SO10I    MVC   0(L'CTFLST,R3),0(R2)                                             
         LA    R9,1(R9)                                                         
         STC   R9,OPT1CTFN                                                      
*                                                                               
         L     R3,ACTFLST                                                       
         CLC   =C'ALL',0(R3)                                                    
         BNE   SO00                                                             
         MVI   OPT1CTFN,0                                                       
         B     SO00                                                             
*                                                                               
*                                                                               
SO20     CLC   =C'IDLE',CARD+7                                                  
         BNE   SO30                                                             
*                                                                               
         OI    OPT2ID,REPORT       TURN ON THE PRINT OUT BIT                    
*                                                                               
         MVC   CARDBUF,CARD                                                     
         CLI   CARD+71,C' '        CONTINUE ON THE NEXT LINE                    
         BE    SO20B                                                            
         LA    R9,MAXLINEQ-1       MAX LINES OF CONTINUATION                    
         LA    R2,CARDBUF+L'CARDBUF                                             
*                                  READ IN ALL CONTINUED LINES                  
SO20A    GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         MVC   0(L'CARDBUF,R2),CARD                                             
         CLI   CARD+71,C' '        CONTINUE ON THE NEXT LINE                    
         BE    SO20B                                                            
         LA    R2,L'CARDBUF(R2)                                                 
         BCT   R9,SO20A                                                         
*                                                                               
*                                  FIND AND SET IDLE SORT OPTION                
SO20B    LA    R2,CARDBUF+12                                                    
SO20C    CLC   =C'SORT=',0(R2)                                                  
         BE    SO20D                                                            
*                                                                               
         LA    R2,1(R2)                                                         
         CLI   0(R2),X'00'                                                      
         BNE   SO20C                                                            
         B     SO20E                                                            
*                                                                               
SO20D    CLC   =C'NODE',5(R2)                                                   
         BNE   SO20E                                                            
         OI    OPT2ID,SORTNODE                                                  
*                                                                               
*                                  FIND AND SET IDLE NODE FILTER LIST           
SO20E    LA    R2,CARDBUF+12                                                    
SO20F    CLC   =C'FILTER=',0(R2)                                                
         BE    SO20G                                                            
*                                                                               
         LA    R2,1(R2)                                                         
         CLI   0(R2),X'00'                                                      
         BNE   SO20F                                                            
         B     SO00                                                             
*                                                                               
SO20G    LA    R2,7(R2)            INPUT FILTER LIST                            
         L     R3,AIDFLST          STORAGE FILTER LIST                          
         SR    R9,R9               FILTER COUNTER                               
*                                                                               
SO20H    CLI   L'IDFLST(R2),C','   MORE FILTER TO READ IN                       
         BNE   SO20I                                                            
*                                                                               
         MVC   0(L'IDFLST,R3),0(R2)                                             
         LA    R9,1(R9)                                                         
         LA    R3,L'IDFLST(R3)                                                  
         LA    R2,L'IDFLST+1(R2)                                                
         B     SO20H                                                            
*                                                                               
SO20I    MVC   0(L'IDFLST,R3),0(R2)                                             
         LA    R9,1(R9)                                                         
         STC   R9,OPT2IDFN                                                      
*                                                                               
         L     R3,AIDFLST                                                       
         CLC   =C'ALL',0(R3)                                                    
         BNE   SO00                                                             
         MVI   OPT2IDFN,0                                                       
         B     SO00                                                             
*                                                                               
*                                                                               
SO30     CLC   =C'ACTIVE',CARD+7                                                
         BNE   SO40                                                             
*                                                                               
         OI    OPT3AC,REPORT       TURN ON THE PRINT OUT BIT                    
*                                                                               
*                                  FIND AND SET ACTIVE SORT OPTION              
         LA    R2,CARD+13                                                       
SO30A    CLC   =C'SORT=',0(R2)                                                  
         BE    SO30B                                                            
*                                                                               
         LA    R2,1(R2)                                                         
         CLI   0(R2),X'00'                                                      
         BNE   SO30A                                                            
         B     SO00                                                             
*                                                                               
SO30B    CLC   =C'PU',5(R2)                                                     
         BNE   SO00                                                             
         OI    OPT3AC,SORTPU                                                    
         B     SO00                                                             
*                                                                               
*                                                                               
SO40     CLC   =C'UNKNOWN',CARD+7                                               
         BNE   SO00                                                             
*                                                                               
         OI    OPT4UN,REPORT       TURN ON THE PRINT OUT BIT                    
*                                                                               
*                                  FIND AND SET UNKNOWN SORT OPTION             
         LA    R2,CARD+14                                                       
SO40A    CLC   =C'SORT=',0(R2)                                                  
         BE    SO40B                                                            
*                                                                               
         LA    R2,1(R2)                                                         
         CLI   0(R2),X'00'                                                      
         BNE   SO40A                                                            
         B     SO00                                                             
*                                                                               
SO40B    CLC   =C'PU',5(R2)                                                     
         BNE   SO00                                                             
         OI    OPT4UN,SORTPU                                                    
         B     SO00                                                             
*                                                                               
SOX      B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        SETDCB - SET DATA CONTROL BLOCK INFO FOR DS GROUP            *         
***********************************************************************         
SETDCB   NTR1                                                                   
         MVI   FORMAT,C'0'         RESET FORMAT OF THE FILE                     
         MVC   FORMAT,CARD+13      READ IN FILE FORMAT FROM CARD                
         MVC   FILE(DCBQ),FILE2    RESET DCB                                    
         MVC   FILE+40(8),CARD+3   DDNAME OF DATA SET ABOUT TO BE READ          
*                                                                               
SD10     GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
*                                                                               
         CLC   =C'DS=',CARD        MORE DS YET TO READ?                         
         BE    SDX                 YES, EXIT                                    
*                                                                               
         CLC   =C'DSX=',CARD       START THE EXCLUDE DS SECTION?                
         BNE   *+12                NO, CONTINUE                                 
         MVI   DSEND,1                                                          
         B     SDX                                                              
*                                                                               
         CLC   =C'/*',CARD         END OF INPUT CARDS?                          
         BNE   SD10                SKIP COMMENTS                                
         MVI   EOF,1                                                            
*                                                                               
SDX      B     EXIT                                                             
***********************************************************************         
*        SETDCBX - SET DATA CONTROL BLOCK INFO FOR DSX GROUP          *         
***********************************************************************         
SETDCBX  NTR1                                                                   
         MVC   FILE(DCBQ),FILE2    RESET DCB                                    
         MVC   FILE+40(8),CARD+4   DDNAME OF DATA SET ABOUT TO BE READ          
         MVI   FORMAT,C'X'         FORMAT OF THE FILE TO BE EXCLUDED            
*                                                                               
SDX10    GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
*                                                                               
         CLC   =C'DSX=',CARD       MORE DSX YET TO READ?                        
         BE    SDXX                YES, EXIT                                    
*                                                                               
         CLC   =C'/*',CARD         END OF INPUT CARDS?                          
         BNE   SDX10               SKIP COMMENTS                                
         MVI   EOF,1                                                            
*                                                                               
SDXX     B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        READDS - READ A DATA SET ACCORDING TO THE FILE FORMAT        *         
***********************************************************************         
READDS   NTR1                                                                   
         OPEN  FILE                                                             
*                                                                               
         CLI   FORMAT,C'1'         CONTEM                                       
         BE    RDF1B                                                            
*                                                                               
         CLI   FORMAT,C'2'         X3172 AND OTHER                              
         BE    RDF2B                                                            
*                                                                               
         CLI   FORMAT,C'X'         REC TO BE EXCLUDE FROM UNKNOWN REP           
         BE    RDFXB                                                            
*                                                                               
         DC    H'0'                UNKNOWN FORMAT                               
*                                                                               
RDF1B    DS    0H                                                               
RF110    GET   FILE,BUFFER                                                      
         CLC   =C'.DDSSTART ',BUFFER                                            
         BE    RF120                                                            
         CLC   =C'*DDSSTART ',BUFFER                                            
         BE    RF120                                                            
         B     RF110                                                            
*                                                                               
RF120    GET   FILE,BUFFER                                                      
         CLC   =C'.DDSEND ',BUFFER                                              
         BE    EODS                                                             
         CLC   =C'*DDSEND ',BUFFER                                              
         BE    EODS                                                             
*                                                                               
         CLI   BUFFER,C'.'         IGNORE COMMENT AND CONT. LINES               
         BE    RF120                                                            
         CLI   BUFFER,C'*'                                                      
         BE    RF120                                                            
         CLI   BUFFER,C' '                                                      
         BE    RF120                                                            
*                                                                               
         BAS   RE,CKMARCO                                                       
         B     RF120                                                            
*                                                                               
RDF2B    DS    0H                                                               
         XC    REC,REC                                                          
RF210    GET   FILE,BUFFER                                                      
*                                                                               
         CLI   BUFFER,C'*'                                                      
         BE    RF210                                                            
         CLI   BUFFER,C' '                                                      
         BE    RF210                                                            
*                                                                               
         BAS   RE,CKMARCO                                                       
         B     RF210                                                            
*                                                                               
RDFXB    DS    0H                                                               
RFX10    GET   FILE,BUFFER                                                      
*                                                                               
         CLI   BUFFER,C'*'                                                      
         BE    RFX10                                                            
         CLI   BUFFER,C' '                                                      
         BE    RFX10                                                            
*                                                                               
         BAS   RE,CKMARCOX                                                      
         B     RFX10                                                            
*                                                                               
EODS     CLOSE FILE                                                             
RDSX     B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        CKMARCOX - CHECK FOR LU/CDRSC/APPL MACROS                    *         
*                 - COMPARE THE LUID TO UNKNOWN REC IN BST, MARK THE  *         
*                   UNKNOWN REC TO BE EXCLUDED WHEN THERE IS A MATCH  *         
***********************************************************************         
CKMARCOX NTR1                                                                   
*                                                                               
         USING LUIDRECD,R3                                                      
         LA    R3,REC                                                           
*                                                                               
         LA    R2,BUFFER+9         SKIP OVER THE LUID                           
CMX10    CLI   0(R2),C' '          SCAN PAST ALL BLANKS                         
         BNE   *+12                                                             
         LA    R2,1(R2)                                                         
         B     CMX10                                                            
*                                                                               
         CLC   =C'LU',0(R2)                                                     
         BE    CMX20                                                            
         CLC   =C'CDRSC',0(R2)                                                  
         BE    CMX20                                                            
         CLC   =C'APPL ',0(R2)                                                  
         BNE   CMXX                                                             
*                                                                               
CMX20    MVC   LUID,BUFFER                                                      
         GOTO1 =V(BINSRCH),DMCB,LUIDREC,ABST,UNCOUNT,25,(1,8),UNCOUNT           
         TM    0(R1),X'01'         REC NOT FOUND?                               
         BO    CMXX                YES - EXIT                                   
*                                                                               
         L     RE,0(R1)            A(EXCLUDED UNKNOWN REC) IN BST               
         CLI   0(RE),STBSTUNY      ALREADY MARK EXCLUDED?                       
         BE    CMXX                                                             
*                                                                               
         MVI   0(RE),STBSTUNY      MARK EXCLUDED UNKNOWN REC IN BST             
         L     RF,UNCOUNTX         INC EXCLUDED UNKNOWN REC COUNTER             
         LA    RF,1(RF)                                                         
         ST    RF,UNCOUNTX                                                      
*                                                                               
CMXX     B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*        CKMARCO - CHECK FOR LINE, CLUSTER/PU AND                     *         
*                  TERM/LU/CDRSC/APPL MACROS                          *         
*                - A LUID REC IS CONSTRUCTED AND FED TO SORTER WHEN   *         
*                  TERMINAL/LU MACRO IS READ                          *         
***********************************************************************         
CKMARCO  NTR1                                                                   
         USING LUIDRECD,R3                                                      
         LA    R3,REC                                                           
*                                                                               
         LA    R2,BUFFER+9         SKIP OVER THE LUID                           
CM10     CLI   0(R2),C' '          SCAN PAST ALL BLANKS                         
         BNE   CM20                                                             
         LA    R2,1(R2)                                                         
         B     CM10                                                             
*                                                                               
*                                                                               
CM20     CLC   =C'LINE',0(R2)                                                   
         BNE   CM30                                                             
*                                                                               
         MVC   TLINE,BUFFER                                                     
         GOTO1 =V(HEXIN),DMCB,BUFFER+5,TLINE+5,2                                
         OC    DMCB+12,DMCB+12     INVALID INPUT?                               
         BNZ   *+14                                                             
         MVC   TLINE,BUFFER        RESTORE THE NON-HEXADECIMAL CHAR             
         B     *+8                                                              
         MVI   TLINE+6,0                                                        
         B     CMX                                                              
*                                                                               
*                                                                               
CM30     CLC   =C'PU',0(R2)                                                     
         BE    *+14                                                             
         CLC   =C'CLUSTER',0(R2)                                                
         BNE   CM40                                                             
*                                                                               
         MVC   PU,BUFFER                                                        
         GOTO1 =V(HEXIN),DMCB,BUFFER+5,PU+5,2                                   
         OC    DMCB+12,DMCB+12     INVALID INPUT?                               
         BNZ   *+14                                                             
         MVC   PU,BUFFER           RESTORE THE NON-HEXADECIMAL CHAR             
         B     *+8                                                              
         MVI   PU+6,0                                                           
         B     CMX                                                              
*                                                                               
*                                                                               
CM40     CLC   =C'LU',0(R2)                                                     
         BE    CM40A                                                            
         CLC   =C'CDRSC',0(R2)                                                  
         BE    CM40A                                                            
         CLC   =C'APPL ',0(R2)                                                  
         BE    CM40A                                                            
         CLC   =C'TERMINAL',0(R2)                                               
         BNE   CMX                                                              
*                                                                               
CM40A    MVC   LUID,BUFFER                                                      
         GOTO1 =V(BINSRCH),DMCB,LUIDREC,ABST,CTCOUNT,25,(1,8),CTCOUNT           
         TM    0(R1),X'01'         REC NOT FOUND?                               
         BO    CM40C               YES - UNKNOWN REC                            
*                                                                               
         L     RE,0(R1)            A(ACTIVE CTFILE REC) IN BST                  
         MVI   0(RE),STBSTCTY      MARK ACTIVE CTFILE REC IN BST                
*                                                                               
         TM    OPT3AC,REPORT       ACTIVE REPORT OPT IS ON                      
         BZ    CMX                                                              
*                                                                               
         GOTO1 =V(HEXIN),DMCB,BUFFER+5,LUID+5,2                                 
         OC    DMCB+12,DMCB+12     INVALID INPUT?                               
         BNZ   *+14                                                             
         MVC   LUID,BUFFER         RESTORE THE NON-HEXADECIMAL CHAR             
         B     *+8                                                              
         MVI   LUID+6,0                                                         
*                                                                               
         MVI   STATUS,STAAC        MARK ACTIVE REC FEEDING TO SORTER            
         TM    OPT3AC,SORTPU       SORTED BY PU, NOT LUID                       
         BZ    CM40B                                                            
*                                  SWAP LUID AND PU IN REC                      
         MVC   BUFFER(L'LUIDREC),LUIDREC                                        
         MVC   PUP,BUFFER+PU-LUIDREC                                            
         MVC   LUIDP,BUFFER+LUID-LUIDREC                                        
         GOTO1 =V(SORTER),DMCB,=C'PUT',LUIDREC                                  
         MVC   REC,BUFFER           RESTORE PU                                  
         B     CM40B1                                                           
*                                                                               
CM40B    GOTO1 =V(SORTER),DMCB,=C'PUT',LUIDREC                                  
*                                                                               
CM40B1   L     RE,ACCOUNT          INC ACTIVE REC COUNTER                       
         LA    RE,1(RE)                                                         
         ST    RE,ACCOUNT                                                       
         B     CMX                                                              
*                                                                               
CM40C    TM    OPT4UN,REPORT       UNKNOWN REPORT OPT IS ON                     
         BZ    CMX                                                              
*                                  IGNORE SPECIAL LUID FOR UNKNOWN REC          
         CLC   =C'DD',LUID         DD LINES                                     
         BE    CMX                                                              
         CLC   =C'MHER',LUID       OR MHER                                      
         BE    CMX                                                              
         CLC   =C'HDTO',LUID       OR HDTO                                      
         BE    CMX                                                              
         CLC   =C'SMTA',LUID       OR SMTA                                      
         BE    CMX                                                              
         CLC   =C'JW',LUID         OR JW***N0* WHERE N > 31                     
         BNE   CM40D                                                            
         GOTO1 =V(HEXIN),DMCB,BUFFER+5,BYTE,2                                   
         CLI   BYTE,X'31'                                                       
         BH    CMX                                                              
*                                                                               
CM40D    MVI   STATUS,STAUN        MARK UNKNOWN REC FEEDING TO SORTER           
         GOTO1 =V(SORTER),DMCB,=C'PUT',LUIDREC                                  
*                                                                               
         L     RE,UNCOUNT          INC UNKNOWN REC COUNTER                      
         LA    RE,1(RE)                                                         
         ST    RE,UNCOUNT                                                       
*                                                                               
*                                                                               
CMX      B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*        SORTCT - FEED ALL CTFILE REC IN BST INTO THE SORTER          *         
***********************************************************************         
SORTCT   NTR1                                                                   
         L     R9,CTCOUNT          # OF CTFILE IN BST                           
         LTR   R9,R9                                                            
         BZ    SCTX                                                             
*                                                                               
         USING LUIDRECD,R3                                                      
         L     R3,ABST                                                          
*                                                                               
         TM    OPT1CT,SORTNODE     SORTED BY NODE, NOT LUID                     
         BZ    SCT20                                                            
*                                                                               
SCT10    MVC   REC,LUIDREC                                                      
         GOTO1 =V(HEXIN),DMCB,LUID-LUIDREC+REC+5,LUID+5,2                       
         OC    DMCB+12,DMCB+12     INVALID INPUT?                               
         BNZ   *+14                                                             
         MVC   LUIDREC,REC         RESTORE THE NON-HEXADECIMAL CHAR             
         B     *+8                                                              
         MVI   LUID+6,0                                                         
*                                  SWAP NODE AND LUID                           
         MVC   NODEN-LUIDREC+REC,NODE                                           
         MVC   LUIDN-LUIDREC+REC,LUID                                           
*                                                                               
         MVI   STATUS-LUIDREC+REC,STACT   MARK CTFILE REC FED TO SORTER         
         GOTO1 =V(SORTER),DMCB,=C'PUT',REC                                      
*                                                                               
         LA    R3,L'LUIDREC(R3)    NEXT REC                                     
         BCT   R9,SCT10                                                         
         B     SCTX                                                             
*                                                                               
SCT20    MVC   REC,LUIDREC                                                      
         GOTO1 =V(HEXIN),DMCB,LUID+5,LUID-LUIDREC+REC+5,2                       
         OC    DMCB+12,DMCB+12     INVALID INPUT?                               
         BNZ   *+14                                                             
         MVC   REC,LUIDREC         RESTORE THE NON-HEXADECIMAL CHAR             
         B     *+8                                                              
         MVI   LUID-LUIDREC+REC+6,0                                             
*                                                                               
         MVI   STATUS-LUIDREC+REC,STACT   MARK CTFILE REC FED TO SORTER         
         GOTO1 =V(SORTER),DMCB,=C'PUT',REC                                      
*                                                                               
         LA    R3,L'LUIDREC(R3)    NEXT REC                                     
         BCT   R9,SCT20                                                         
*                                                                               
SCTX     B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        SORTIDLE - FEED ALL IDLE REC IN BST INTO THE SORTER          *         
***********************************************************************         
SORTIDLE NTR1                                                                   
         L     R9,CTCOUNT          # OF CTFILE IN BST                           
         LTR   R9,R9                                                            
         BZ    SIX                                                              
*                                                                               
         USING LUIDRECD,R3                                                      
         L     R3,ABST                                                          
         SR    R5,R5               IDLE REC COUNTER                             
*                                                                               
         TM    OPT2ID,SORTNODE     SORTED BY NODE, NOT LUID                     
         BZ    SI20                                                             
*                                                                               
SI10     CLI   STATUS,STBSTCTN     CHECK IF IDLE REC?                           
         BNE   SI10A               NO - NEXT REC                                
*                                                                               
         MVC   REC,LUIDREC                                                      
         GOTO1 =V(HEXIN),DMCB,LUID-LUIDREC+REC+5,LUID+5,2                       
         OC    DMCB+12,DMCB+12     INVALID INPUT?                               
         BNZ   *+14                                                             
         MVC   LUIDREC,REC         RESTORE THE NON-HEXADECIMAL CHAR             
         B     *+8                                                              
         MVI   LUID+6,0                                                         
*                                  SWAP NODE AND LUID                           
         MVC   NODEN-LUIDREC+REC,NODE                                           
         MVC   LUIDN-LUIDREC+REC,LUID                                           
*                                                                               
         MVI   STATUS-LUIDREC+REC,STAID   MARK IDLE REC FED TO SORTER           
         GOTO1 =V(SORTER),DMCB,=C'PUT',REC                                      
         LA    R5,1(R5)            INC IDLE REC COUNTER                         
*                                                                               
SI10A    LA    R3,L'LUIDREC(R3)    NEXT REC                                     
         BCT   R9,SI10                                                          
         B     SIX                                                              
*                                                                               
SI20     CLI   STATUS,STBSTCTN     CHECK IF IDLE REC?                           
         BNE   SI20A               NO - NEXT REC                                
*                                                                               
         MVC   REC,LUIDREC                                                      
         GOTO1 =V(HEXIN),DMCB,LUID+5,LUID-LUIDREC+REC+5,2                       
         OC    DMCB+12,DMCB+12     INVALID INPUT?                               
         BNZ   *+14                                                             
         MVC   REC,LUIDREC         RESTORE THE NON-HEXADECIMAL CHAR             
         B     *+8                                                              
         MVI   LUID-LUIDREC+REC+6,0                                             
*                                                                               
         MVI   STATUS-LUIDREC+REC,STAID   MARK IDLE REC FED TO SORTER           
         GOTO1 =V(SORTER),DMCB,=C'PUT',REC                                      
         LA    R5,1(R5)            INC IDLE REC COUNTER                         
*                                                                               
SI20A    LA    R3,L'LUIDREC(R3)    NEXT REC                                     
         BCT   R9,SI20                                                          
*                                                                               
SIX      ST    R5,IDCOUNT          STORE IDLE REC COUNTER                       
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        READUNK - READ UNKNOWN REC FROM SORTER AND BUILD A BST       *         
***********************************************************************         
READUNK  NTR1                                                                   
         L     R9,UNCOUNT          UNKNOWN REC COUNTER                          
         LTR   R9,R9                                                            
         BZ    RUX                                                              
*                                                                               
         SR    R5,R5               # OF REPEATED UNKNOWN REC                    
         USING LUIDRECD,R3                                                      
         L     R3,ABST             ADDR. OF BST                                 
*                                                                               
RU10     GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   R4,15,4(R1)         LOAD A(REC)                                  
         BNZ   *+6                                                              
         DC    H'0'                MORE REC YET TO READ, NO WAY FAIL            
*                                                                               
         CLC   SVLUREC,LUIDREC     SKIP REC WITH SAME UNKNOWN REC               
         BNE   *+12                                                             
         LA    R5,1(R5)            INC # OF REPEATED UNKNOWN REC                
         B     RU10                                                             
*                                                                               
         MVC   SVLUREC,LUIDREC     SAVE THE CURRENT UNKNOWN REC                 
         MVC   LUIDREC,0(R4)       MOVE UNKNOWN REC TO BST                      
         MVI   STATUS,STBSTUNN     RESET THE STATUS BYTE                        
         LA    R3,L'LUIDREC(R3)                                                 
         BCT   R9,RU10                                                          
*                                                                               
         L     R9,UNCOUNT          UPDATE THE # OF UNKNOWN REC                  
         SR    R9,R5                                                            
         ST    R9,UNCOUNT                                                       
*                                                                               
RUX      B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        SORTUNK  - FEED UNKNOWN REC IN BST INTO THE SORTER,          *         
*                   EXCEPT THOSE ARE MARKED EXCLUDED.                 *         
***********************************************************************         
SORTUNK  NTR1                                                                   
*                                  SORT UNK REC BY STATUS, XCLUDE/NOT           
         GOTO1 =V(XSORT),DMCB,ABST,UNCOUNT,L'LUIDREC,L'STATUS,0                 
*                                                                               
         L     R9,UNCOUNT          # OF UNKNOWN REC IN BST                      
         S     R9,UNCOUNTX         # OF UNKNOWN REC AFTER EXCLUSION             
         BZ    SUX                                                              
*                                                                               
         LR    R5,R9                                                            
         USING LUIDRECD,R3                                                      
         L     R3,ABST             ADDR. OF BST                                 
*                                                                               
         TM    OPT4UN,SORTPU       SORTED BY PU, NOT LUID                       
         BZ    SU20                                                             
*                                                                               
SU10     MVC   REC,LUIDREC         SWAP NODE AND LUID                           
         GOTO1 =V(HEXIN),DMCB,LUID+5,LUID-LUIDREC+REC+5,2                       
         OC    DMCB+12,DMCB+12     INVALID INPUT?                               
         BNZ   *+14                                                             
         MVC   REC,LUIDREC         RESTORE THE NON-HEXADECIMAL CHAR             
         B     *+8                                                              
         MVI   LUID-LUIDREC+REC+6,0                                             
         MVC   PUP,PU-LUIDREC+REC                                               
         MVC   LUIDP,LUID-LUIDREC+REC                                           
*                                                                               
SU10A    LA    R3,L'LUIDREC(R3)    NEXT REC                                     
         BCT   R9,SU10                                                          
         B     SU30                                                             
*                                                                               
SU20     MVC   REC,LUIDREC                                                      
         GOTO1 =V(HEXIN),DMCB,LUID-LUIDREC+REC+5,LUID+5,2                       
         OC    DMCB+12,DMCB+12     INVALID INPUT?                               
         BNZ   *+14                                                             
         MVC   LUIDREC,REC         RESTORE THE NON-HEXADECIMAL CHAR             
         B     *+8                                                              
         MVI   LUID+6,0                                                         
*                                                                               
SU20A    LA    R3,L'LUIDREC(R3)    NEXT REC                                     
         BCT   R9,SU20                                                          
*                                  SORT UNKNOWN REC AFTER EXCLUSION             
SU30     GOTO1 =V(XSORT),DMCB,ABST,(R5),L'LUIDREC,L'LUIDREC-L'STATUS,1          
SUX      B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        PRTCTFI - PRINT OUT THE CTFILE TERMINAL REC                  *         
***********************************************************************         
PRTCTFI  NTR1                                                                   
         L     R9,CTCOUNT                                                       
         LTR   R9,R9                                                            
         BZ    PCTX                                                             
*                                                                               
         MVC   TITLE(L'T1),T1                                                   
         TM    OPT1CT,SORTNODE                                                  
         BO    *+12                                                             
         LA    RE,H1                                                            
         B     *+8                                                              
         LA    RE,H2                                                            
         ST    RE,HEADING                                                       
*                                                                               
         SR    R4,R4               # REC WILL BE PRINTED                        
         SR    R5,R5                                                            
         L     R2,APRNT                                                         
         USING PCIRECD,R2                                                       
         USING LUIDRECD,R3                                                      
*                                                                               
PCT10    GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   R3,15,4(R1)         LOAD A(REC)                                  
         BNZ   PCT20                                                            
         DC    H'0'                MORE REC YET TO READ, NO WAY FAIL            
*                                                                               
PCT20    TM    OPT1CT,SORTNODE                                                  
         BZ    PCT40                                                            
*                                                                               
         CLI   LUIDN+6,X'00'                                                    
         BNE   PCT30                                                            
         GOTO1 =V(HEXOUT),DMCB,LUIDN+5,BUFFER,1,=C'TOG'                         
         MVC   LUIDN+5(2),BUFFER                                                
*                                                                               
PCT30    XC    PCINODEN,PCINODEN                                                
         MVC   PCILUIDN,LUIDN                                                   
         MVC   PCINDNC3,NODEN                                                   
*                                                                               
         ZICM  R0,OPT1CTFN,1       # FILTERS                                    
         BZ    PCT60               SKIP FILTERING WHEN FILTER#=0                
         L     RF,ACTFLST          INPUT FILTER LIST                            
*                                                                               
PCT35    CLC   NODEN,0(RF)                                                      
         BE    PCT60               PRINT REC WHEN MATCH                         
         LA    RF,L'CTFLST(RF)                                                  
         BCT   R0,PCT35                                                         
         B     PCT70                                                            
*                                                                               
PCT40    CLI   LUID+6,X'00'                                                     
         BNE   PCT50                                                            
         GOTO1 =V(HEXOUT),DMCB,LUID+5,BUFFER,1,=C'TOG'                          
         MVC   LUID+5(2),BUFFER                                                 
*                                                                               
PCT50    XC    PCINODE,PCINODE                                                  
         MVC   PCILUID,LUID                                                     
         MVC   PCINDC3,NODE                                                     
*                                                                               
         ZICM  R0,OPT1CTFN,1       # FILTERS                                    
         BZ    PCT60               SKIP FILTERING WHEN FILTER#=0                
         L     RF,ACTFLST          INPUT FILTER LIST                            
*                                                                               
PCT55    CLC   NODE,0(RF)                                                       
         BE    PCT60               PRINT REC WHEN MATCH                         
         LA    RF,L'CTFLST(RF)                                                  
         BCT   R0,PCT55                                                         
         B     PCT70                                                            
*                                                                               
PCT60    LA    R2,PCIRECQ(R2)                                                   
         LA    R4,1(R4)            INC COUNTER OF #REC WILL BE PRINTED          
         LA    R5,1(R5)            INC COUNTER                                  
         CH    R5,=Y(ROW*COL/CICELLQ)  MAX # REC PRINTED ON A PAGE?             
         BL    PCT70                                                            
*                                                                               
         BAS   RE,PRTPAGE1                                                      
         SR    R5,R5                                                            
         L     R2,APRNT                                                         
*                                                                               
PCT70    BCT   R9,PCT10                                                         
*                                                                               
         LTR   R5,R5                                                            
         BZ    PCTX                                                             
*                                                                               
         LH    R1,=Y(ROW*COL/CICELLQ)                                           
         SR    R1,R5                                                            
         MH    R1,=Y(PCIRECQ)      TARGET LENGTH                                
         LR    R0,R2               TARGET ADDR.                                 
         SR    RE,RE               DON'T CARE ABOUT THE VALUE OF RE             
         ICM   RF,15,=X'40000000'                                               
         MVCL  R0,RE               PAD REST OF APRNT WITH BLANKS                
         BAS   RE,PRTPAGE1                                                      
*                                                                               
PCTX     MVC   P(18),=CL18'CTFILE REC # ='                                      
         EDIT  (R4),(8,P+18)                                                    
         GOTO1 =V(PRINTER)                                                      
         ZAP   LINE,=P'75'         FORCE NEW PAGE                               
         B     EXIT                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
*        PRTIDLE - PRINT OUT THE IDLE TERMINAL REC                    *         
***********************************************************************         
PRTIDLE  NTR1                                                                   
         L     R9,IDCOUNT                                                       
         LTR   R9,R9                                                            
         BZ    PIDX                                                             
*                                                                               
         MVC   TITLE(L'T2),T2                                                   
         TM    OPT2ID,SORTNODE                                                  
         BO    *+12                                                             
         LA    RE,H1                                                            
         B     *+8                                                              
         LA    RE,H2                                                            
         ST    RE,HEADING                                                       
*                                                                               
         SR    R4,R4               # REC WILL BE PRINTED                        
         SR    R5,R5                                                            
         L     R2,APRNT                                                         
         USING PCIRECD,R2                                                       
         USING LUIDRECD,R3                                                      
*                                                                               
PID10    GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   R3,15,4(R1)         LOAD A(REC)                                  
         BNZ   PID20                                                            
         DC    H'0'                MORE REC YET TO READ, NO WAY FAIL            
*                                                                               
PID20    TM    OPT2ID,SORTNODE                                                  
         BZ    PID40                                                            
*                                                                               
         CLI   LUIDN+6,X'00'                                                    
         BNE   PID30                                                            
         GOTO1 =V(HEXOUT),DMCB,LUIDN+5,BUFFER,1,=C'TOG'                         
         MVC   LUIDN+5(2),BUFFER                                                
*                                                                               
PID30    XC    PCINODEN,PCINODEN                                                
         MVC   PCILUIDN,LUIDN                                                   
         MVC   PCINDNC3,NODEN                                                   
*                                                                               
         ZICM  R0,OPT2IDFN,1       # FILTERS                                    
         BZ    PID60               SKIP FILTERING WHEN FILTER#=0                
         L     RF,AIDFLST          INPUT FILTER LIST                            
*                                                                               
PID35    CLC   NODEN,0(RF)                                                      
         BE    PID60               PRINT REC WHEN MATCH                         
         LA    RF,L'IDFLST(RF)                                                  
         BCT   R0,PID35                                                         
         B     PID70                                                            
*                                                                               
PID40    CLI   LUID+6,X'00'                                                     
         BNE   PID50                                                            
         GOTO1 =V(HEXOUT),DMCB,LUID+5,BUFFER,1,=C'TOG'                          
         MVC   LUID+5(2),BUFFER                                                 
*                                                                               
PID50    XC    PCINODE,PCINODE                                                  
         MVC   PCILUID,LUID                                                     
         MVC   PCINDC3,NODE                                                     
*                                                                               
         ZICM  R0,OPT2IDFN,1       # FILTERS                                    
         BZ    PID60               SKIP FILTERING WHEN FILTER#=0                
         L     RF,AIDFLST          INPUT FILTER LIST                            
*                                                                               
PID55    CLC   NODE,0(RF)                                                       
         BE    PID60               PRINT REC WHEN MATCH                         
         LA    RF,L'IDFLST(RF)                                                  
         BCT   R0,PID55                                                         
         B     PID70                                                            
*                                                                               
PID60    LA    R2,PCIRECQ(R2)                                                   
         LA    R4,1(R4)            INC COUNTER OF #REC WILL BE PRINTED          
         LA    R5,1(R5)            INC COUNTER                                  
         CH    R5,=Y(ROW*COL/CICELLQ)  MAX # REC PRINTED ON A PAGE?             
         BL    PID70                                                            
*                                                                               
         BAS   RE,PRTPAGE1                                                      
         SR    R5,R5                                                            
         L     R2,APRNT                                                         
*                                                                               
PID70    BCT   R9,PID10                                                         
*                                                                               
         LTR   R5,R5                                                            
         BZ    PIDX                                                             
*                                                                               
         LH    R1,=Y(ROW*COL/CICELLQ)                                           
         SR    R1,R5                                                            
         MH    R1,=Y(PCIRECQ)      TARGET LENGTH                                
         LR    R0,R2               TARGET ADDR.                                 
         SR    RE,RE               DON'T CARE ABOUT THE VALUE OF RE             
         ICM   RF,15,=X'40000000'                                               
         MVCL  R0,RE               PAD REST OF APRNT WITH BLANKS                
         BAS   RE,PRTPAGE1                                                      
*                                                                               
PIDX     MVC   P(18),=CL18'IDLE REC # ='                                        
         EDIT  (R4),(8,P+18)                                                    
         GOTO1 =V(PRINTER)                                                      
         ZAP   LINE,=P'75'         FORCE NEW PAGE                               
         B     EXIT                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
*        PRTPAGE1 - PRINT A PAGE OF CTFILE/IDLE REC                   *         
***********************************************************************         
PRTPAGE1 NTR1                                                                   
         USING PRTLINED,R4                                                      
         LA    R4,P                                                             
         L     R2,APRNT                                                         
         LA    R9,ROW                                                           
*                                                                               
         L     RE,HEADING                                                       
         MVC   P(L'H1),0(RE)                                                    
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(PRINTER)                                                      
*                                                                               
PP110    LR    R3,R2                                                            
         MVC   PRTCELL1,0(R3)                                                   
         MVC   PRTCELL2,8(R3)                                                   
         A     R3,=A(ROW*PCIRECQ)                                               
         MVC   PRTCELL3,0(R3)                                                   
         MVC   PRTCELL4,8(R3)                                                   
         A     R3,=A(ROW*PCIRECQ)                                               
         MVC   PRTCELL5,0(R3)                                                   
         MVC   PRTCELL6,8(R3)                                                   
         A     R3,=A(ROW*PCIRECQ)                                               
         MVC   PRTCELL7,0(R3)                                                   
         MVC   PRTCELL8,8(R3)                                                   
         A     R3,=A(ROW*PCIRECQ)                                               
         MVC   PRTCELL9,0(R3)                                                   
         MVC   PRTCELLA,8(R3)                                                   
         A     R3,=A(ROW*PCIRECQ)                                               
         MVC   PRTCELLB,0(R3)                                                   
         MVC   PRTCELLC,8(R3)                                                   
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         LA    R2,PCIRECQ(R2)                                                   
         BCT   R9,PP110                                                         
*                                                                               
         ZAP   LINE,=P'75'         FORCE NEW PAGE                               
*                                                                               
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*        PRTACTI - PRINT OUT THE ACTIVE TERMINAL REC                  *         
***********************************************************************         
PRTACTI  NTR1                                                                   
         L     R9,ACCOUNT                                                       
         LTR   R9,R9                                                            
         BZ    PACX                                                             
*                                                                               
         MVC   TITLE(L'T3),T3                                                   
         TM    OPT3AC,SORTPU                                                    
         BO    *+12                                                             
         LA    RE,H3                                                            
         B     *+8                                                              
         LA    RE,H4                                                            
         ST    RE,HEADING                                                       
*                                                                               
         SR    R4,R4               # REC WILL BE PRINTED                        
         SR    R5,R5                                                            
         L     R2,APRNT                                                         
         USING PAURECD,R2                                                       
         USING LUIDRECD,R3                                                      
*                                                                               
PAC10    GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   R3,15,4(R1)         LOAD A(REC)                                  
         BNZ   *+6                                                              
         DC    H'0'                MORE REC YET TO READ, NO WAY FAIL            
*                                                                               
         CLC   SVLUREC,LUIDREC     SKIP REPEATED REC                            
         BNE   PAC20                                                            
         BCT   R9,PAC10                                                         
         B     PAC70                                                            
*                                                                               
PAC20    MVC   SVLUREC,LUIDREC     SAVE THE CURRENT LUID REC W/NODE             
*                                                                               
         CLI   LUID+6,X'00'                                                     
         BNE   PAC30                                                            
         GOTO1 =V(HEXOUT),DMCB,LUID+5,BUFFER,1,=C'TOG'                          
         MVC   LUID+5(2),BUFFER                                                 
*                                                                               
PAC30    CLI   PU+6,X'00'                                                       
         BNE   PAC40                                                            
         GOTO1 =V(HEXOUT),DMCB,PU+5,BUFFER,1,=C'TOG'                            
         MVC   PU+5(2),BUFFER                                                   
*                                                                               
PAC40    OC    TLINE,TLINE         NO LINE LABEL?                               
         BZ    PAC50                                                            
         CLI   TLINE+6,X'00'                                                    
         BNE   PAC50                                                            
         GOTO1 =V(HEXOUT),DMCB,TLINE+5,BUFFER,1,=C'TOG'                         
         MVC   TLINE+5(2),BUFFER                                                
*                                                                               
PAC50    MVC   PAUREC,LUIDREC+L'STATUS                                          
*                                                                               
         LA    R2,PAURECQ(R2)                                                   
         LA    R4,1(R4)            INC COUNTER OF #REC WILL BE PRINTED          
         LA    R5,1(R5)            INC COUNTER                                  
         CH    R5,=Y(ROW*COL/AUCELLQ)  MAX # REC PRINTED ON A PAGE?             
         BL    PAC60                                                            
*                                                                               
         BAS   RE,PRTPAGE2                                                      
         SR    R5,R5                                                            
         L     R2,APRNT                                                         
*                                                                               
PAC60    BCT   R9,PAC10                                                         
*                                                                               
PAC70    LTR   R5,R5                                                            
         BZ    PACX                                                             
*                                                                               
         LH    R1,=Y(ROW*COL/AUCELLQ)                                           
         SR    R1,R5                                                            
         MH    R1,=Y(PAURECQ)      TARGET LENGTH                                
         LR    R0,R2               TARGET ADDR.                                 
         SR    RE,RE               DON'T CARE ABOUT THE VALUE OF RE             
         ICM   RF,15,=X'40000000'                                               
         MVCL  R0,RE               PAD REST OF APRNT WITH BLANKS                
         BAS   RE,PRTPAGE2                                                      
*                                                                               
PACX     MVC   P(18),=CL18'ACTIVE REC # ='                                      
         EDIT  (R4),(8,P+18)                                                    
         GOTO1 =V(PRINTER)                                                      
         ZAP   LINE,=P'75'         FORCE NEW PAGE                               
         B     EXIT                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
*        PRTUNKX - PRINT OUT THE UNKNOWN TERMINAL REC AFTER EXCLUSION *         
***********************************************************************         
PRTUNKX  NTR1                                                                   
         L     R9,UNCOUNT                                                       
         S     R9,UNCOUNTX                                                      
         BZ    PUNX                                                             
*                                                                               
         MVC   TITLE(L'T4),T4                                                   
         TM    OPT4UN,SORTPU                                                    
         BO    *+12                                                             
         LA    RE,H3                                                            
         B     *+8                                                              
         LA    RE,H4                                                            
         ST    RE,HEADING                                                       
*                                                                               
         SR    R4,R4               # REC WILL BE PRINTED                        
         SR    R5,R5                                                            
         L     R2,APRNT                                                         
         USING PAURECD,R2                                                       
         L     R3,ABST                                                          
         USING LUIDRECD,R3                                                      
*                                                                               
PUN10    CLI   LUID+6,X'00'                                                     
         BNE   PUN20                                                            
         GOTO1 =V(HEXOUT),DMCB,LUID+5,BUFFER,1,=C'TOG'                          
         MVC   LUID+5(2),BUFFER                                                 
*                                                                               
PUN20    CLI   PU+6,X'00'                                                       
         BNE   PUN30                                                            
         GOTO1 =V(HEXOUT),DMCB,PU+5,BUFFER,1,=C'TOG'                            
         MVC   PU+5(2),BUFFER                                                   
*                                                                               
PUN30    OC    TLINE,TLINE         NO LINE LABEL?                               
         BZ    PUN40                                                            
         CLI   TLINE+6,X'00'                                                    
         BNE   PUN40                                                            
         GOTO1 =V(HEXOUT),DMCB,TLINE+5,BUFFER,1,=C'TOG'                         
         MVC   TLINE+5(2),BUFFER                                                
*                                                                               
PUN40    MVC   PAUREC,LUIDREC+L'STATUS                                          
*                                                                               
         LA    R2,PAURECQ(R2)                                                   
         LA    R3,L'LUIDREC(R3)                                                 
         LA    R4,1(R4)            INC COUNTER OF #REC WILL BE PRINTED          
         LA    R5,1(R5)            INC COUNTER                                  
         CH    R5,=Y(ROW*COL/AUCELLQ)  MAX # REC PRINTED ON A PAGE?             
         BL    PUN50                                                            
*                                                                               
         BAS   RE,PRTPAGE2                                                      
         SR    R5,R5                                                            
         L     R2,APRNT                                                         
*                                                                               
PUN50    BCT   R9,PUN10                                                         
*                                                                               
         LTR   R5,R5                                                            
         BZ    PUNX                                                             
*                                                                               
         LH    R1,=Y(ROW*COL/AUCELLQ)                                           
         SR    R1,R5                                                            
         MH    R1,=Y(PAURECQ)      TARGET LENGTH                                
         LR    R0,R2               TARGET ADDR.                                 
         SR    RE,RE               DON'T CARE ABOUT THE VALUE OF RE             
         ICM   RF,15,=X'40000000'                                               
         MVCL  R0,RE               PAD REST OF APRNT WITH BLANKS                
         BAS   RE,PRTPAGE2                                                      
*                                                                               
PUNX     MVC   P(18),=CL18'UNKNOWN REC # ='                                     
         EDIT  (R4),(8,P+18)                                                    
         GOTO1 =V(PRINTER)                                                      
         ZAP   LINE,=P'75'         FORCE NEW PAGE                               
         B     EXIT                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
*        PRTPAGE2 - PRINT A PAGE OF ACTIVE/UNKNOWN REC                *         
***********************************************************************         
PRTPAGE2 NTR1                                                                   
         USING PRTLINED,R4                                                      
         LA    R4,P                                                             
         L     R2,APRNT                                                         
         LA    R9,ROW                                                           
*                                                                               
         L     RE,HEADING                                                       
         MVC   P(L'H1),0(RE)                                                    
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(PRINTER)                                                      
*                                                                               
PP210    LR    R3,R2                                                            
         MVC   PRTCELL1,0(R3)                                                   
         MVC   PRTCELL2,8(R3)                                                   
         MVC   PRTCELL3,16(R3)                                                  
         A     R3,=A(ROW*PAURECQ)                                               
         MVC   PRTCELL4,0(R3)                                                   
         MVC   PRTCELL5,8(R3)                                                   
         MVC   PRTCELL6,16(R3)                                                  
         A     R3,=A(ROW*PAURECQ)                                               
         MVC   PRTCELL7,0(R3)                                                   
         MVC   PRTCELL8,8(R3)                                                   
         MVC   PRTCELL9,16(R3)                                                  
         A     R3,=A(ROW*PAURECQ)                                               
         MVC   PRTCELLA,0(R3)                                                   
         MVC   PRTCELLB,8(R3)                                                   
         MVC   PRTCELLC,16(R3)                                                  
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         LA    R2,PAURECQ(R2)                                                   
         BCT   R9,PP210                                                         
*                                                                               
         ZAP   LINE,=P'75'         FORCE NEW PAGE                               
*                                                                               
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*        BOX: SET UP BOXES FOR TERMINAL RECORDS                       *         
***********************************************************************         
BOX      NTR1                                                                   
         L     R4,=V(BOXAREA)      A(BOX DSECT)                                 
         USING BOXD,R4                                                          
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+2,C'T'                                                   
         MVI   BOXROWS+4,C'M'      UNDER HEADINGS                               
         MVI   BOXROWS+58,C'B'                                                  
         MVC   BOXCOLS,SPACES                                                   
*                                                                               
         MVI   BOXCOLS,C'L'                                                     
         MVI   BOXCOLS+09,C'C'                                                  
         MVI   BOXCOLS+18,C'C'                                                  
         MVI   BOXCOLS+27,C'C'                                                  
*                                                                               
         MVI   BOXCOLS+36,C'C'                                                  
         MVI   BOXCOLS+45,C'C'                                                  
         MVI   BOXCOLS+54,C'C'                                                  
*                                                                               
         MVI   BOXCOLS+63,C'C'                                                  
         MVI   BOXCOLS+72,C'C'                                                  
         MVI   BOXCOLS+81,C'C'                                                  
*                                                                               
         MVI   BOXCOLS+90,C'C'                                                  
         MVI   BOXCOLS+99,C'C'                                                  
         MVI   BOXCOLS+108,C'R'                                                 
*                                                                               
         MVI   BOXWT,1                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
         LTORG                                                                  
***********************************************************************         
         GETEL R6,DATADISP,ELCODE                                               
ELCODE   DS    X                                                                
DATADISP DC    Y(CTTDATA-CTTREC)   DISPLACEMENT OF FIRST ELEMENT IN REC         
***********************************************************************         
         DS    0D                                                               
* ++INCLUDE FASSBOFF                                                            
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
         ORG   SSOOFF                                                           
SSB      DC    XL(SSOOFFX-SSOOFF)'00'                                           
         ORG   SSOXTND                                                          
         DC    X'FF'               SET EXTENDED OFFLINE SSB                     
         ORG                                                                    
SSBL     EQU   *-SSB                                                            
UTL      DC    F'0',X'0A'                                                       
***********************************************************************         
DUB      DS    D                                                                
DMCB     DS    6F                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,25,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=25'                                    
FLIST    DC    CL8'NCTFILE',C'X'                                                
KEY      DS    CL25                KEY INTO CTFILE                              
AIO      DC    A(IO)                                                            
ABST     DC    A(BST)                                                           
ANODETBL DC    A(NODETBL)                                                       
CTCOUNT  DC    A(0)                CTFILE REC COUNTER                           
IDCOUNT  DC    F'0'                IDLE REC COUNTER                             
ACCOUNT  DC    F'0'                ACTIVE REC COUNTER                           
UNCOUNT  DC    A(0)                UNKNOWN REC COUNTER                          
UNCOUNTX DC    A(0)                EXCLUDED UNKNOWN REC COUNTER                 
BYTE     DS    X                                                                
WORK     DS    CL17                                                             
***********************************************************************         
DSEND    DC    X'00'               END OF DS SECTION IN JCT PARAMS              
EOF      DC    X'00'               END OF JCL PARAMS                            
CARD     DS    CL80                                                             
MAXLINEQ EQU   5                   MAX LINES OF A OPTION INPUT CARD             
CARDBUF  DS    (MAXLINEQ)CL71                                                   
ACTFLST  DC    A(CTFLST)           CTFILE REPORT NODE FILTER LIST               
AIDFLST  DC    A(IDFLST)           IDLE REPORT NODE FILTER LIST                 
OPT1CTFN DC    X'00'               CTFILE REPORT NODE FILTER COUNTER            
OPT2IDFN DC    X'00'               IDLE REPORT NODE FILTER COUNTER              
OPT1CT   DC    X'00'               CTFILE REPORT OPTIONS                        
OPT2ID   DC    X'00'               IDLE REPORT OPTIONS                          
OPT3AC   DC    X'00'               ACTIVE REPORT OPTIONS                        
OPT4UN   DC    X'00'               UNKNOWN REPORT OPTIONS                       
* FOR ALL REPORT OPTIONS                                                        
REPORT   EQU   X'80'               PRINT OUT(1) OR NOT(0)                       
SORTPU   EQU   X'40'               SORT BY PU(1) OR LUID(0)                     
SORTNODE EQU   X'40'               SORT BY NODE(1) OR LUID(0)                   
***********************************************************************         
FORMAT   DS    C                                                                
FILE     DCB   DDNAME=X,DSORG=PS,MACRF=GM,EODAD=EODS                            
FILE2    DCB   DDNAME=X,DSORG=PS,MACRF=GM,EODAD=EODS                            
DCBQ     EQU   *-FILE2                                                          
BUFFER   DS    CL80                                                             
REC      DS    CL25                                                             
SVLUREC  DS    CL25                                                             
***********************************************************************         
APRNT    DC    A(PRTAREA)          TEMP STORAGE OF REC'S ABOUT TO PRINT         
ROW      EQU   45                  MAX # OF ROW IN A PAGE                       
COL      EQU   12                  MAX # OF COL IN A PAGE                       
CICELLQ  EQU   2                   # CELLS NEEDED TO PRT CT/IDLE REC            
AUCELLQ  EQU   3                   # CELLS NEEDED TO PRT ACT/UNKN REC           
***********************************************************************         
T1       DC    CL25'TERMINAL RECORD IN CTFILE'                                  
T2       DC    CL25'IDLE TERMINAL ID'                                           
T3       DC    CL25'ACTIVE TERMINAL ID'                                         
T4       DC    CL25'UNKNOWN TERMINAL ID'                                        
HEADING  DS    A                                                                
H1       DC    CL106'   LUID     NODE     LUID     NODE     LUID     NO+        
               DE     LUID     NODE     LUID     NODE     LUID     NODE+        
               '                                                                
H2       DC    CL106'   NODE     LUID     NODE     LUID     NODE     LU+        
               ID     NODE     LUID     NODE     LUID     NODE     LUID+        
               '                                                                
H3       DC    CL106'   LUID   PU/CLUST   LINE     LUID   PU/CLUST   LI+        
               NE     LUID   PU/CLUST   LINE     LUID   PU/CLUST   LINE+        
               '                                                                
H4       DC    CL106' PU/CLUST   LUID     LINE   PU/CLUST   LUID     LI+        
               NE   PU/CLUST   LUID     LINE   PU/CLUST   LUID     LINE+        
               '                                                                
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* THE FOLLOWING BOOK IS IN CTGEN0D, SO RELINK THAT PHASE TOO.                   
* NODETBL                                                                       
***********************************************************************         
       ++INCLUDE CTREPLUTAB                                                     
                                                                                
***********************************************************************         
MAXFNQ   EQU   100                 MAX # OF NODE FILTERS                        
CTFLST   DS    (MAXFNQ)CL3         CTFILE REPORT NODE FILTER LIST               
IDFLST   DS    (MAXFNQ)CL3         IDLE REPORT NODE FILTER LIST                 
IO       DS    2000C                                                            
PRTAREA  DS    (ROW*COL)CL(L'LUID) TEMP STORAGE OF REC'S ABOUT TO PRINT         
BST      DS    (MAXBSTQ)CL(L'LUIDREC)  BINSCH TABLE                             
MAXBSTQ  EQU   100000                                                           
                                                                                
***********************************************************************         
* LUID RECORD DSECT                                                             
***********************************************************************         
LUIDRECD DSECT                     LUID RECORD DSECT                            
LUIDREC  DS    0CL25                                                            
STATUS   DS    X                                                                
LUID     DS    CL8                                                              
PU       DS    CL8                                                              
TLINE    DS    CL8                                                              
         ORG   PU                                                               
NODE     DS    CL3                                                              
SPARE    DS    CL13                                                             
         ORG   LUID                                                             
PUP      DS    CL8                                                              
LUIDP    DS    CL8                                                              
         ORG   LUID                                                             
NODEN    DS    CL3                                                              
LUIDN    DS    CL8                                                              
* VALUE FOR STATUS FOR SORTER                                                   
STACT    EQU   X'01'               CTFILE                                       
STAID    EQU   X'02'               IDLE                                         
STAAC    EQU   X'03'               ACTIVE                                       
STAUN    EQU   X'00'               UNKNOWN                                      
* VALUE FOR STATUS IN BST FOR CTFILE REC                                        
STBSTCTY EQU   X'01'               THIS REC IS ACTIVE                           
STBSTCTN EQU   X'00'               THIS REC IS IDLE                             
* VALUE FOR STATUS IN BST FOR UNKNOWN REC                                       
STBSTUNY EQU   X'01'               THIS REC IS EXCLUDED                         
STBSTUNN EQU   X'00'               THIS REC WILL BE PRINTED                     
                                                                                
***********************************************************************         
* DSECT FOR CT/IDLE REC IN PRINT AREA                                           
***********************************************************************         
PCIRECD  DSECT                                                                  
PCILUID  DS    CL8                                                              
PCINODE  DS    0CL8                                                             
         DS    2C                                                               
PCINDC3  DS    CL3                                                              
         DS    3C                                                               
         ORG   PCILUID                                                          
PCINODEN DS    0CL8                                                             
         DS    2C                                                               
PCINDNC3 DS    CL3                                                              
         DS    3C                                                               
PCILUIDN DS    CL8                                                              
PCIRECQ  EQU   *-PCIRECD                                                        
                                                                                
***********************************************************************         
* DSECT FOR ACT/UNK REC IN PRINT AREA                                           
***********************************************************************         
PAURECD  DSECT                                                                  
PAUREC   DS    0CL24                                                            
PAULUID  DS    CL8                                                              
PAUPU    DS    CL8                                                              
PAULINE  DS    CL8                                                              
         ORG   PAULUID                                                          
PAUPUP   DS    CL8                                                              
PAULUIDP DS    CL8                                                              
         ORG                                                                    
PAURECQ  EQU   *-PAURECD                                                        
                                                                                
***********************************************************************         
* DSECT FOR THE PRINT LINE                                                      
***********************************************************************         
PRTLINED DSECT                                                                  
         DS    C                                                                
PRTCELL1 DS    CL8                                                              
         DS    C                                                                
PRTCELL2 DS    CL8                                                              
         DS    C                                                                
PRTCELL3 DS    CL8                                                              
         DS    C                                                                
PRTCELL4 DS    CL8                                                              
         DS    C                                                                
PRTCELL5 DS    CL8                                                              
         DS    C                                                                
PRTCELL6 DS    CL8                                                              
         DS    C                                                                
PRTCELL7 DS    CL8                                                              
         DS    C                                                                
PRTCELL8 DS    CL8                                                              
         DS    C                                                                
PRTCELL9 DS    CL8                                                              
         DS    C                                                                
PRTCELLA DS    CL8                                                              
         DS    C                                                                
PRTCELLB DS    CL8                                                              
         DS    C                                                                
PRTCELLC DS    CL8                                                              
*                                                                               
***********************************************************************         
*DDDPRINT                                                                       
*DDBIGBOX                                                                       
*CTGENFILE                                                                      
*                                                                               
*        PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'036CTREPLUID 05/14/19'                                      
         END                                                                    
