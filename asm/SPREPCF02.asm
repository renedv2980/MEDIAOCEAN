*          DATA SET SPREPCF02  AT LEVEL 018 AS OF 06/09/98                      
*PHASE SPCF02A                                                                  
*INCLUDE UNTIME                                                                 
         TITLE 'SPCF02 - WORKER FILE BUYS IN ERROR'                             
SPCF02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPCF02,R8,RR=R2                                                
                                                                                
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
                                                                                
         CLI   MODE,REQFRST                                                     
         BE    CF10                                                             
         B     EXIT                                                             
                                                                                
CF10     DS    0H                                                               
         L     RE,UTL                                                           
         MVC   SVSPUTL,4(RE)                                                    
         MVI   4(RE),X'0A'                                                      
         GOTO1 DATAMGR,DMCB,(0,=C'DMOPEN'),=C'CONTROL',                +        
               =C'NGENDIR NGENFIL X',WKBUFF,0                                   
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         LA    R4,P2                                                            
         USING PRINTD,R4                                                        
         XC    WRKRINDX,WRKRINDX                                                
         LA    R2,WRKRINDX                                                      
         USING UKRECD,R2                                                        
         MVC   UKUSRID,RCORIGID                                                 
         MVC   UKSYSPRG(3),=C'CCX'                                              
                                                                                
CF20     DS    0H                                                               
         NI    FLAG,X'FF'-TMSTMP                                                
         LA    R2,WRKRINDX                                                      
*****    OI    UKFLAG,UKFLTMP                                                   
         GOTO1 DATAMGR,DMCB,DINDEX,WRKFILE,WRKRINDX,IO,AWKBUFF                  
         CLI   8(R1),0                                                          
         BE    CF30                                                             
         CLI   8(R1),X'90'         EOF?                                         
         BE    CFX                 YES                                          
         DC    H'0'                                                             
                                                                                
CF30     DS    0H                                                               
         CLC   UKUSRID,RCORIGID                                                 
         BNE   CF20                                                             
         CLC   UKSYSPRG(3),=C'CCX'                                              
         BNE   CF20                                                             
         CLI   UKSTAT,X'40'        CHECK STATUS HOLD                            
         BNE   CF20                                                             
         DROP  R2                                                               
         OI    FLAG,NEWRKR         PROCESSING A NEW WRKR FILE W/O ERROR         
                                                                                
CF40     DS    0H                                                               
         LA    R0,IO               SET 'TO' ADDRESS                             
         LA    R1,L'IO             SET 'TO' LENGTH                              
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         GOTO1 DATAMGR,DMCB,DREAD,WRKFILE,WRKRINDX,IO,AWKBUFF                   
         CLI   8(R1),0                                                          
         BE    CF50                                                             
         CLI   8(R1),X'90'         EOF?                                         
         BE    CF160               YES                                          
         DC    H'0'                                                             
                                                                                
CF50     DS    0H                                                               
         TM    FLAG,TMSTMP         ALREADY GOT TIME STAMP?                      
         BO    CF60                                                             
         MVC   SVTMSTMP,IO+59+4    TIME STAMP                                   
         OI    FLAG,TMSTMP                                                      
         B     CF40                                                             
                                                                                
CF60     LA    R3,IO+14                                                         
         USING SHDRD,R3                                                         
         CLC   =C'HDR*',SHDRTYPE   CHECK IF HDR RECORD                          
         BNE   CF70                                                             
         MVC   SAVEHDR,SHDRSTRT                                                 
         OC    SHDRERNO,SHDRERNO   CHECK IF MARKED WITH ERROR                   
         BZ    CF40                                                             
         OI    FLAG,FNDERR         AN ERROR HAS BEEN FOUND                      
         MVC   ERRNO,SHDRERNO      YES, SAVE ERROR                              
         B     CF90                                                             
         DROP  R3                                                               
                                                                                
         USING SBUYD,R3                                                         
CF70     CLC   =C'BUY*',SBUYTYPE   CHECK IF BUY RECORD                          
         BNE   CF80                                                             
         MVC   SAVEBUY,SBUYSTRT                                                 
         B     CF40                                                             
                                                                                
         DROP  R3                                                               
         USING SEBYD,R3                                                         
                                                                                
CF80     CLC   =C'EBY*',SEBYTYPE   CHECK IF EBY RECORD                          
         BNE   CF40                                                             
         OC    SEBYODAT(2),SEBYODAT  CHECK IF THERE'S AN ERROR                  
         BZ    CF40                                                             
         MVC   ERRNO,SEBYODAT      YES, SAVE ERROR NUMBER                       
         DROP  R3                                                               
                                                                                
CF90     DS    0H                                                               
         TM    FLAG,NEWRKR                                                      
         BNO   CF100                                                            
         NI    FLAG,X'FF'-NEWRKR   FOUND 1ST ERROR SO PRINT WRKR INFO           
         LA    R2,WRKRINDX                                                      
         USING UKRECD,R2                                                        
         MVC   P2(16),=C'WORKER FILE ID ='                                      
         MVC   P2+25(4),UKSYSPRG                                                
         GOTO1 HEXOUT,DMCB,UKDAY,P2+29,1,=C'TOG'                                
         MVC   P2+31(1),UKCLASS                                                 
         MVI   P2+32,C','                                                       
         EDIT  UKFILENO,(5,P2+33),0,ALIGN=LEFT                                  
         MVC   P2+40(L'SVTMSTMP),SVTMSTMP    TIME STAMP                         
         GOTO1 REPORT                                                           
         DROP  R2                                                               
                                                                                
CF100    MVC   P,SPACES                                                         
         LA    R3,SAVEHDR                                                       
         USING SHDRSTRT,R3                                                      
         TM    ERRNO,X'80'         CHECK IF ERROR MESSAGE FROM TABLE            
         BNO   CF120                                                            
                                                                                
         LA    RE,ERRTAB                                                        
CF110    CLI   0(RE),X'FF'                                                      
         BE    CF130                                                            
         CLC   ERRNO+1(1),0(RE)                                                 
         BE    *+12                                                             
         LA    RE,41(RE)                                                        
         B     CF110                                                            
         MVC   P1(40),1(RE)                                                     
         B     CF130                                                            
                                                                                
CF120    LA    R5,GTKEY            DEFINE MESSAGE KEY                           
         USING GMSGD,R5                                                         
         XC    GMKEY,GMKEY                                                      
         MVI   GMKREC,GMKRECQ                                                   
         MVI   GMKSYS,3                                                         
         MVI   GMKTYP,GMKTERR                                                   
         MVC   GMKMSG,ERRNO                                                     
         MVI   GMKLANG,X'FF'                                                    
                                                                                
         MVC   GTKEYSVE,GMKEY                                                   
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'GENDIR',GTKEYSVE,GMKEY,0              
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         LA    R1,WKBUFF                                                        
         CLC   GMKEY,GTKEYSVE                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'GENFIL',GMKEY+36,WKBUFF,     +        
               IOWORK                                                           
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R5                                                               
                                                                                
         LA    R5,WKBUFF                                                        
         USING GMSGEL,R5                                                        
         MVI   ELCODE,GMSGELC                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         ZIC   R1,GMSGELL                                                       
         SH    R1,=Y(GMSGFXDL+1)   LENGTH OF MESSAGE -1                         
         EX    R1,*+4                                                           
         MVC   P1(0),GMSGTXT                                                    
         DROP  R5                                                               
*                                                                               
CF130    MVC   PRSEQ,IO+4+4        SEQ NO                                       
         MVC   PRMED,SHDRMED       MEDIA                                        
         MVC   PRCLT,SHDRCLT       CLIENT                                       
         MVC   PRPRD,SHDRPRD       PRODUCT                                      
         MVC   PREST,SHDREST       ESTIMATE                                     
*                                                                               
         CLC   ERRNO,=X'8013'      ERROR WITH HDR DATES                         
         BNE   CF133                                                            
         MVC   PRPERSTA(L'SHDRSTPD),SHDRSTPD                                    
         MVC   PRPEREND(L'SHDRENPD),SHDRENPD                                    
         B     CF138                                                            
CF133    GOTO1 DATCON,DMCB,(0,SHDRSTPD),(5,PRPERSTA)   PERIOD START             
         MVI   PRPEREND-1,C'-'                                                  
         GOTO1 DATCON,DMCB,(0,SHDRENPD),(5,PRPEREND)   PERIOD END               
*                                                                               
CF138    TM    FLAG,FNDERR         FOUND ERROR IN HEADER?                       
         BO    CF150                                                            
         DROP  R3                                                               
                                                                                
         USING SBUYSTRT,R3                                                      
         LA    R3,SAVEBUY                                                       
         MVC   PRSTA,SBUYSTA       STATION                                      
                                                                                
         CLI   SBUYDEL,C'Y'                                                     
         BE    CF150                                                            
         LA    R2,SBUYROT          ROTATION                                     
         LA    R5,PRROTATN                                                      
         LA    R6,DAYS                                                          
         LA    R7,7                                                             
                                                                                
CF140    MVI   0(R5),C'.'                                                       
         CLI   0(R2),C'Y'                                                       
         BNE   *+10                                                             
         MVC   0(1,R5),0(R6)                                                    
         LA    R1,1                                                             
         AR    R2,R1                                                            
         AR    R5,R1                                                            
         AR    R6,R1                                                            
         BCT   R7,CF140                                                         
                                                                                
         PACK  DUB,SBUYSTIM         START TIME                                  
         CVB   R1,DUB                                                           
         STCM  R1,3,WORK                                                        
         PACK  DUB,SBUYETIM         END TIME                                    
         CVB   R1,DUB                                                           
         STCM  R1,3,WORK+2                                                      
         GOTO1 =V(UNTIME),DMCB,(0,WORK),PRSTTIME                                
                                                                                
         MVC   PRDAYPRT,SBUYDPT    DAYPART                                      
                                                                                
         PACK  DUB,SBUYSLEN        SPOT LENGTH                                  
         CVB   R2,DUB                                                           
         EDIT  (R2),PRSPTLEN                                                    
                                                                                
         MVC   PRPRGMNM,SBUYPROG   PROGRAM NAME                                 
                                                                                
         PACK  DUB,SBUYCOST        SPOT COST                                    
         CVB   R2,DUB                                                           
         EDIT  (R2),PRCOST,2                                                    
                                                                                
CF150    DS    0H                                                               
         GOTO1 REPORT                                                           
         NI    FLAG,X'FF'-FNDERR                                                
         B     CF40                                                             
         DROP  R3                                                               
                                                                                
CF160    GOTO1 DATAMGR,DMCB,DCLOSE,WRKFILE,0,IO,AWKBUFF                         
         GOTO1 DATAMGR,DMCB,=C'SENT',WRKFILE,WRKRINDX,IO,AWKBUFF                
         B     CF20                                                             
                                                                                
CFX      MVC   4(1,RE),SVSPUTL                                                  
         GOTO1 AENDREQ                                                          
         DROP  R4                                                               
EXIT     XIT1                                                                   
                                                                                
         GETEL R5,42,ELCODE                                                     
         EJECT                                                                  
         LTORG                                                                  
CTFLIST  DS    0F                                                               
         DC    CL8'NGENDIR'                                                     
         DC    CL8'NGENFIL'                                                     
         DC    CL8'NCTFILE'                                                     
         DC    C'X'                                                             
DINDEX   DC    CL8'INDEX'                                                       
DREAD    DC    CL8'READ'                                                        
DCLOSE   DC    CL8'CLOSE'                                                       
WRKF     DC    CL8'WRKF'                                                        
WRKFILE  DC    CL8'WRKFILE'                                                     
DAYS     DC    CL7'MTWTFSS'                                                     
AWKBUFF  DC    A(WKBUFF)                                                        
*                                                                               
ERRTAB   DC    X'01',CL40'COMMENT COUNT ERROR'                                  
         DC    X'02',CL40'DELETE ERROR'                                         
         DC    X'03',CL40'PRODUCT NOT IN CLIENT TABLE'                          
         DC    X'04',CL40'HDR OBJ NOT FOUND'                                    
         DC    X'05',CL40'MISSING DEMOS'                                        
         DC    X'06',CL40'INVALID DEMO NAMES'                                   
         DC    X'07',CL40'INVOICE DATA FOUND'                                   
         DC    X'08',CL40'NO UPLOAD DEMOS IN ESTHDR'                            
         DC    X'09',CL40'ACN VALIDATION ERROR'                                 
         DC    X'0A',CL40'BUY TO DELETE NOT FOUND'                              
         DC    X'0B',CL40'TOO MANY ROT* OBJECTS'                                
         DC    X'0C',CL40'INVALID PRODUCT IN ROTATION OBJECT'                   
         DC    X'0D',CL40'ROTATION OBJECTS OUT OF ORDER'                        
         DC    X'0E',CL40'CANNOT DELETE PAID/MISSED/MATCHED ROT'                
         DC    X'0F',CL40'ROTATION BEFORE START DATE'                           
         DC    X'10',CL40'SKED OBJ NOT FOUND'                                   
         DC    X'11',CL40'START DATE BEFORE LOCK DATE'                          
         DC    X'12',CL40'AUTOI5 EXISTS, CHANGE LOCK DATE'                      
         DC    X'13',CL40'INVALID DATE IN HDR OBJECT'                           
         DC    X'FF'                                                            
IOWORK   DS    12D                 IO WORK AREA                                 
ELCODE   DS    X                                                                
SVSPUTL  DS    X                                                                
FLAG     DS    X                                                                
NEWRKR   EQU   X'80'               FIRST ERROR HAS BEEN FOUND                   
FNDERR   EQU   X'40'               ERROR HAS BEEN FOUND                         
TMSTMP   EQU   X'20'               FOUND TIME STAMP                             
ERRNO    DS    XL2                 ERROR NUMBER                                 
SVTMSTMP DS    CL13                SAVE TIME STAMP ON UPLOAD                    
WRKRINDX DS    CL42                                                             
GTKEY    DS    XL44                                                             
GTKEYSVE DS    XL44                                                             
IO       DS    XL255               IO AREA                                      
SAVEHDR  DS    CL(SHDRRLNQ)                                                     
SAVEBUY  DS    CL(SBUYRLNQ)                                                     
WKBUFF   DS    14336C                                                           
                                                                                
PRINTD   DSECT                                                                  
PRSEQ    DS    CL6                                                              
         DS    CL3                                                              
PRMED    DS    CL1                                                              
         DS    CL5                                                              
PRCLT    DS    CL3                                                              
         DS    CL3                                                              
PRPRD    DS    CL3                                                              
         DS    CL3                                                              
PREST    DS    CL3                                                              
         DS    CL3                                                              
PRSTA    DS    CL(L'SBUYSTA)                                                    
         DS    CL2                                                              
PRPERSTA DS    CL8                                                              
         DS    C                                                                
PRPEREND DS    CL8                                                              
         DS    CL3                                                              
PRROTATN DS    CL(L'SBUYROT)                                                    
         DS    CL3                                                              
PRSTTIME DS    CL11                                                             
         DS    CL3                                                              
PRDAYPRT DS    CL(L'SBUYDPT)                                                    
         DS    CL7                                                              
PRSPTLEN DS    CL(L'SBUYSLEN)                                                   
         DS    CL2                                                              
PRPRGMNM DS    CL(L'SBUYPROG)                                                   
         DS    CL2                                                              
PRCOST   DS    CL10                                                             
*                                                                               
         EJECT                                                                  
                                                                                
         PRINT OFF                                                              
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE GEGENMSG                                                       
       ++INCLUDE SPTUPLOADD                                                     
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE DMWRKFL                                                        
       ++INCLUDE DMWRKFK                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'018SPREPCF02 06/09/98'                                      
         END                                                                    
