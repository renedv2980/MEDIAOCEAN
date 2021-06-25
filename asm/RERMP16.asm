*          DATA SET RERMP16    AT LEVEL 025 AS OF 04/13/09                      
*          DATA SET RERMP16    AT LEVEL 219 AS OF 12/11/95                      
*PHASE T81016C,+0                                                               
*INCLUDE INVDAY                                                                 
*INCLUDE REBKLST                                                                
         TITLE 'T81016 - REPPAK FILE MAINT - SWAP INV NAME'                     
********************************************************************            
*  HISTORY OF CHANGES:                                             *            
*  AUG31/01 (BU ) --- DON'T ABORT ON 'NO PASSIVE PTR FOUND'        *            
*                                                                  *            
*  APR26/02 (HQ ) --- FIX SWAP BUG, OVERLAPPING SWAP NOT ALLOWED   *            
*                                                                  *            
*  MAR00/09 (BOB) --- NEW INVENTORY RECORD KEY                     *            
*                                                                  *            
*                                                                  *            
*                                                                  *            
********************************************************************            
T81016   CSECT                                                                  
         PRINT GEN                                                              
         NMOD1 0,T81016,RR=R5                                                   
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         USING T81016+4096,R9                                                   
*                                                                               
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R8,ASYSD                                                         
         USING SYSD,R8                                                          
         L     R7,ASPOOLD                                                       
         USING SPOOLD,R7                                                        
         ST    R5,RELO                                                          
*                                                                               
         MVI   IOOPT,C'Y'          CONTROL MY OWN ADDREC AND PUTREC             
*  MOVE PROFILE TO LOCAL WORKING STORAGE                                        
         LR    R3,RA                                                            
         AH    R3,=AL2(SFMPROFS-CONHEADH)                                       
         USING SVDSECT,R3                                                       
         MVC   RMPPROFS,SVPGPBIT                                                
*-- ONLY FOR SELF DEFINED INVENTORY SETTING                                     
         LA    R2,CONRECH                                                       
         MVI   ERROR,INVALID                                                    
         TM    RMPPROFS,X'80'      SELF DEFINED                                 
         BZ    ERREND              BIT ON FIELD REQUIRED                        
         DROP  R3                                                               
         MVC   AIO,AIO1                                                         
*                                                                               
         OI    CONSERVH+6,X'81'    SCREEN IS ALWAYS MODIFIED                    
         MVC   MYSCRNUM,TWASCR     SET SCREEN NUMBER                            
*                                                                               
         EJECT                                                                  
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VKEY                                                             
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VREC                                                             
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LIST                                                             
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DKEY                                                             
*                                                                               
         B     EXIT                                                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              VALIDATE KEY ROUTINE                            *                
****************************************************************                
****************************************************************                
         SPACE                                                                  
VKEY     DS    0H                                                               
*              INIT WORK AREA                                                   
         XC    KEY,KEY                                                          
         XC    STAHLD(15),STAHLD                                                
*              VALIDATE THE STATION                                             
         LA    R2,INVSSTAH                                                      
         GOTO1 ANY                                                              
         MVI   ERROR,INVALID                                                    
         CLI   5(R2),0             REQUIRED                                     
         BE    ERREND                                                           
         GOTO1 VALISTA                                                          
         MVC   STAHLD,WORK                                                      
         MVI   STAHLD+4,C'T'                                                    
         CLI   WORK+4,C' '                                                      
         BE    *+10                                                             
         MVC   STAHLD+4(1),WORK+4                                               
         CLI   WORK+40,C' '                                                     
         BE    *+10                                                             
         MVC   STAHLD+4(1),WORK+40 CHECK SATTELITE                              
         MVC   CSTAT,STAHLD                                                     
         MVC   CCOSCRST,8(R2)                                                   
         SPACE 1                                                                
*                                                                               
*              INVOICE NUMBER                                                   
*                                                                               
         XC    INVHLD,INVHLD                                                    
         MVI   ERROR,INVALID                                                    
         LA    R2,INVINVH                                                       
         CLI   ACTNUM,ACTCHA                                                    
         BE    VK140                                                            
         B     VK200                                                            
*                                                                               
VK140    CLI   5(R2),0             REQUIRED LOGIC (CHA,DIS)                     
         BE    ERREND                                                           
         CLI   5(R2),4             MAX LENGTH IS 4                              
         BH    ERREND                                                           
         MVC   INVHLD(4),8(R2)                                                  
         OC    INVHLD(4),=4X'40'                                                
         B     VK440                                                            
*                                                                               
VK200    CLI   5(R2),0             OPTIONAL LOGIC (LIST)                        
         BE    VK440                                                            
         B     VK140                                                            
*                                                                               
*              EFFECTIVE DATE                                                   
*  OPTIONAL                                                                     
VK440    LA    R2,INVEFFH                                                       
*                                                                               
         CLI   5(R2),0                                                          
         BE    VK560                                                            
*                                                                               
         MVI   ERROR,INVALID                                                    
*                                                                               
         XC    DTEHLD,DTEHLD                                                    
         XC    DTEHLD2(4),DTEHLD2                                               
*                                                                               
         LA    R2,INVEFFH          EDIT DATES                                   
         GOTO1 ANY                                                              
         GOTO1 SCANNER,DMCB,(R2),(2,WORK2),C',=,-'                              
         MVI   ERROR,INVALID                                                    
         CLI   DMCB+4,1                                                         
         BNE   ERREND              THEY INPUT A ,                               
         SPACE 1                                                                
         GOTO1 DATVAL,DMCB,(0,WORK2+12),WORK      START DATE                    
         OC    DMCB(4),DMCB                                                     
         BZ    ERREND                                                           
         GOTO1 DATCON,DMCB,(0,WORK),(3,DTEHLD)                                  
         GOTO1 DATCON,DMCB,(0,WORK),(2,DTEHLD2)                                 
         CLI   WORK2+1,0           NO END                                       
         BE    VK560                                                            
         MVC   DTEHLD2+2(2),WORK2+10                                            
         CLI   WORK2+1,1                                                        
         BNE   *+16                SHOULD BE A DATE                             
         TM    WORK2+3,X'80'       TEST FOR 1 POSITION NUMERIC                  
         BO    VK560                                                            
         B     ERREND                                                           
         SPACE 1                                                                
         GOTO1 DATVAL,DMCB,(0,WORK2+22),WORK                                    
         OC    DMCB(4),DMCB                                                     
         BZ    ERREND                                                           
         GOTO1 DATCON,DMCB,(0,WORK),(2,DTEHLD2+2)                               
         CLC   DTEHLD2(2),DTEHLD2+2  START CANNOT BE > THEN END                 
         BH    ERREND                                                           
         SPACE                                                                  
*                                                                               
*                                                                               
VK560    MVC   AIO,AIO1                                                         
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING REINVREC,R6                                                      
*                                                                               
         MVC   CCONKSTA,STAHLD                                                  
         MVC   CCONINV,INVHLD                                                   
         MVC   CCONEFF,INVEFF                                                   
*                                                                               
         MVI   RINVKTYP,RINVKTYQ                                                
         MVC   RINVKREP,AGENCY                                                  
         MVC   RINVKSTA,STAHLD                                                  
         MVC   RINVKINV,INVHLD                                                  
         MVC   RINVKSTD,DTEHLD                                                  
         MVC   SAVEKEY,KEY                                                      
*                                                                               
         LA    R2,INVSSTAH                                                      
         GOTO1 GETINV                                                           
*                                                                               
VKXIT    CLI   ACTNUM,ACTCHA                                                    
         BE    DKEY                                                             
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              DISPLAY KEY ROUTINE                             *                
****************************************************************                
****************************************************************                
         SPACE                                                                  
DKEY     DS    0H                                                               
         L     R6,AIO                                                           
         USING REINVREC,R6                                                      
*                                                                               
         LA    R2,INVSSTAH         STATION                                      
         MVC   8(4,R2),RINVKSTA                                                 
         OI    6(R2),X'80'         TRANSMIT                                     
*                                                                               
         LA    R2,INVINVH          INVENTORY                                    
         MVC   8(4,R2),RINVKINV                                                 
         OI    6(R2),X'80'         TRANSMIT                                     
*                                                                               
         LA    R2,INVEFF           EFFECTIVE DATE                               
         GOTO1 DATCON,DMCB,(3,RINVKSTD),(5,0(R2))                               
         OI    INVEFFH+6,X'80'     TRANSMIT                                     
*                                                                               
         MVC   CCONKSTA,RINVKSTA                                                
         MVC   SAVESTD,RINVKSTD    SAVE EFFECTIVE DATE                          
         MVC   CCONINV,RINVKINV                                                 
         GOTO1 DATCON,DMCB,(3,RINVKSTD),(5,CCONEFF)                             
*                                                                               
DKXIT    B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              LIST ROUTINE IN OVERFLOW AREA                   *                
****************************************************************                
****************************************************************                
         SPACE                                                                  
LIST     DS    0H                                                               
         OC    KEY(RINVKSTA-RINVKEY),KEY                                        
         BNZ   LR100                                                            
*                                                                               
         SPACE                                                                  
         LA    R6,KEY                                                           
         USING RINVKEY,R6                                                       
         XC    KEY,KEY                                                          
         MVC   KEY(RINVKSPR-RINVKEY),SAVEKEY                                    
LR100    GOTO1 HIGH                                                             
         B     LR220                                                            
*                                                                               
LR200    GOTO1 SEQ                                                              
LR220    LA    R6,KEY                                                           
         CLC   SAVEKEY(RINVKINV-RINVKEY),KEY     SAME STATION                   
         BNE   LREXT                                                            
         CLI   RINVKRTP,X'00'      INVENTORY HEADER                             
         BNE   LR200                                                            
         CLI   RINVKINV+3,0        CHECK OLD RECORD FORMAT                      
         BE    LR200                                                            
         MVC   SAVEKEY,KEY                                                      
         L     R6,AIO                                                           
         GOTO1 GETREC                                                           
*--DATE FILTER                                                                  
         CLI   DTEHLD2,0                                                        
         BE    LR260                                                            
         CLI   RINVPEFF+2,0        DATE RANGE                                   
         BE    LR240               NO SINGLE DATE                               
         CLC   DTEHLD2,RINVPEFF                                                 
         BL    LR200                                                            
         CLC   DTEHLD2,RINVPEFF+2                                               
         BH    LR200                                                            
         B     LR260                                                            
*--SINGLE DATE EDIT                                                             
LR240    CLC   DTEHLD2,RINVPEFF                                                 
         BH    LR200                                                            
*                                                                               
LR260    LA    R5,LISTAR                                                        
         USING LLINED,R5                                                        
         XC    LISTAR,LISTAR                                                    
*                                                                               
         LA    R2,INVINVH          INVENTORY                                    
         MVC   LINVNUM(4),RINVKINV                                              
*                                                                               
         LA    R3,RINVPEFF         EFFECTIVE DATES                              
         LA    R4,LEFFDTE                                                       
         GOTO1 DATCON,DMCB,(2,0(R3)),(5,0(R4))                                  
         CLI   2(R3),0                                                          
         BE    LR280                                                            
         MVI   8(R4),C'-'                                                       
         GOTO1 DATCON,DMCB,(2,2(R3)),(5,9(R4))                                  
*                                                                               
LR280    MVC   LDPT,RINVDP         DAYPART                                      
*                                                                               
*  DAY/TIME                                                                     
         USING RIDTELEM,R4                                                      
         GOTO1 HELLO,DMCB,(C'G',REPFILE),(X'02',(R6)),0                         
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,12(R1)                                                        
*                                                                               
         GOTO1 UNDAY,DMCB,RIDTDAY,LDYTIME           DAY                         
         LA    RE,17                                                            
         LA    R3,LDYTIME                                                       
*                                                                               
LR320    CLI   0(R3),X'40'                                                      
         BNH   LR340                                                            
         LA    R3,1(R3)                                                         
         BCT   RE,LR320                                                         
         DC    H'0'                                                             
LR340    MVI   0(R3),C'/'                                                       
         GOTO1 UNTIME,DMCB,RIDTTIME,(0,1(R3))       TIME                        
         DROP  R4                                                               
*                                                                               
*  PROGRAM                                                                      
         USING RIPGELEM,R4                                                      
         GOTO1 HELLO,DMCB,(C'G',REPFILE),(X'03',(R6)),0                         
         CLI   12(R1),0                                                         
         BNE   LR500                                                            
         L     R4,12(R1)                                                        
         ZIC   R1,RIPGLEN                                                       
         S     R1,=F'2'                                                         
         C     R1,=F'20'           MAX OUTPUT SIZE                              
         BNH   *+8                                                              
         LA    R1,20                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     LR500                                                            
         MVC   LPROGRM(0),RIPGNAME                                              
*                                                                               
         SPACE                                                                  
LR500    GOTO1 LISTMON                                                          
         B     LR200               GOTO READ SEQ                                
*                                                                               
LREXT    DS    0H                                                               
         B     EXIT                                                             
         DROP  R4,R5,R6                                                         
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              VALIDATE RECORD ROUTINE                         *                
****************************************************************                
****************************************************************                
         SPACE                                                                  
VREC     DS    0H                                                               
         GOTO1 CHKLOCK                                                          
*                                                                               
         MVI   ERROR,INVALID                                                    
         LA    R2,INVCODEH                                                      
         CLI   5(R2),2                                                          
         BL    ERREND                                                           
*                                                                               
         LA    R3,INVINVH                                                       
         OC    8(4,R3),=4X'40'                                                  
         OC    8(4,R2),=4X'40'                                                  
         CLC   8(4,R2),8(R3)       HAS INVENTORY NUMBER CHANGED                 
         BE    VREX                                                             
*                                                                               
*-- CREATE NEW RECORD IN AIO2                                                   
         USING REINVREC,RF                                                      
         L     RE,AIO1                                                          
         L     RF,AIO2                                                          
         LA    R1,2000                                                          
         MOVE  ((RF),(R1)),(RE)                                                 
         L     RF,AIO2             RESET THE POINTER                            
         MVC   DTEHLD3(L'RINVPEFF),RINVPEFF  SAVE EFFECTIVE DATE                
         MVC   RINVKINV,INVCODE    UPDATE CODE                                  
         MVC   SAVEKEY2(27),REINVREC                                            
         L     RE,AIO1                                                          
         MVC   SAVEKEY4(27),0(RE)                                               
         TM    RMPPROFS,X'80'      SELF DEFINED                                 
         BZ    *+8                 NO DONT SET BIT                              
         OI    RINVSTAT,X'80'                                                   
*                                                                               
         BAS   RE,MAINTREC                                                      
*                                                                               
VREX     B     EXIT                                                             
         DROP  RF                                                               
*                                                                               
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              MODIFY INVENTORY KEYS                                            
****************************************************************                
****************************************************************                
* SUB-ROUTINE TO DO FILE AND DIRECTORY MAINTENENCE                              
*                                                                               
MAINTREC NTR1                                                                   
*                                                                               
*  MODIFIY KEYS ON A CHANGE                                                     
*                                                                               
         CLI   ACTNUM,ACTCHA                                                    
         BE    MNT220                                                           
         CLI   ACTNUM,ACTSEL                                                    
         BNE   EXIT                                                             
*                                                                               
MNT220   L     R4,AIO1             ORIGINAL RECORD ***************              
         L     R6,AIO2             NEW RECORD      ***************              
* KEY CHANGE DELETE OLD KEYS CREATE NEW KEYS                                    
MNT300   LA    RE,21                                                            
         STC   RE,CHNGLEN                                                       
*                                                                               
         MVC   KEY(27),0(R6)       MOVE NEW INVENTORY SCREEN                    
         GOTO1 HIGH                                                             
         CLC   KEY,KEYSAVE                                                      
         BNE   MNT360                                                           
         LA    R2,INVCODEH                                                      
         MVI   ERROR,DUPLICAT                                                   
         B     ERREND                                                           
*                                                                               
MNT360   MVC   SAVEKEY,KEYSAVE     SAVE NEW KEY                                 
         MVC   AIO,AIO2            POINT TO NEW RECORD                          
         BAS   RE,FLADD            WRITE RECORD OUT                             
*                                                                               
MNT600   MVC   KEY(27),SAVEKEY2    GET NEW HEADER DISK ADD                      
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   BSVDA,KEY+28                                                     
*                                                                               
         MVC   KEY(27),SAVEKEY4    REMOVE OLD PASSIVE POINTER                   
         OI    DMINBTS,X'08'                                                    
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(27),KEY                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   AIO,AIO1                                                         
         L     R4,AIO                                                           
         GOTO1 GETREC                                                           
         GOTO1 INVPTR,DMCB,0(R4),WORK2                                          
         GOTO1 DELPT,DMCB,WORK2                                                 
*                                                                               
         MVC   KEY(27),SAVEKEY2    ADD NEW PASSIVE POINTERS                     
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(27),KEY                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   AIO,AIO2                                                         
         L     R6,AIO                                                           
         GOTO1 GETREC                                                           
         GOTO1 INVPTR,DMCB,0(R6),WORK2                                          
         GOTO1 NWPT,DMCB,WORK2                                                  
         B     EXIT                                                             
*                                                                               
COMPKEYS CLC   KEY(0),KEYSAVE                                                   
MOVEKEYS MVC   KEY(0),SAVEKEY                                                   
         EJECT                                                                  
*              ROUTINE TO ADD PASSIVE POINTERS                                  
         SPACE 1                                                                
*              PARAM 1   BYTES 1-3 A(LIST OF POINTERS)                          
         SPACE 1                                                                
NWPT     NTR1                                                                   
         L     R2,0(R1)                                                         
NWPT1    CLI   0(R2),0                                                          
         BE    EXIT                END OF LIST                                  
         MVC   KEY(27),0(R2)                                                    
         OI    DMINBTS,X'08'                                                    
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'F7'                                                    
         CLC   KEYSAVE(27),KEY                                                  
         BE    NWPT3                                                            
         MVC   KEY(28),0(R2)                                                    
         MVC   KEY+28(4),BSVDA                                                  
         BAS   RE,MYDIRADD                                                      
         B     NWPT4                                                            
         SPACE 1                                                                
NWPT3    MVC   KEY(28),0(R2)                                                    
         MVC   KEY+28(4),BSVDA                                                  
         BAS   RE,MYDIRWRT                                                      
         SPACE 1                                                                
NWPT4    LA    R2,32(R2)                                                        
         B     NWPT1                                                            
         SPACE 2                                                                
*              ROUTINE TO DELETE POINTERS                                       
         SPACE 1                                                                
DELPT    NTR1                                                                   
         L     R2,0(R1)                                                         
DELPT1   CLI   0(R2),0                                                          
         BE    EXIT                                                             
         MVC   KEY(27),0(R2)                                                    
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(27),KEY                                                  
         BNE   DELPT4              PASSIVE NOT FOUND: SKIP                      
*                                                                               
*   FORMERLY, DIED ON 'NO PASSIVE POINTER FOUND'                                
*                                                                               
****     BE    *+6                                                              
****     DC    H'0'                                                             
         OI    KEY+27,X'80'                                                     
         BAS   RE,MYDIRWRT                                                      
         SPACE 1                                                                
DELPT4   LA    R2,32(R2)                                                        
         B     DELPT1                                                           
         EJECT                                                                  
         SPACE 2                                                                
*              ROUTINE TO RESTORE POINTERS                                      
         SPACE 1                                                                
RSTPT    NTR1                                                                   
         L     R2,0(R1)                                                         
RSTPT1   CLI   0(R2),0                                                          
         BE    EXIT                                                             
         MVC   KEY(27),0(R2)                                                    
         OI    DMINBTS,X'08'                                                    
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'F7'                                                    
         CLC   KEYSAVE(27),KEY                                                  
         BNE   RSTPT4                                                           
         NI    KEY+27,X'7F'                                                     
         BAS   RE,MYDIRWRT                                                      
         SPACE 1                                                                
RSTPT4   LA    R2,32(R2)                                                        
         B     RSTPT1                                                           
         EJECT                                                                  
*              CREATE NEW PASSIVE POINTER                                       
         SPACE 1                                                                
*              PARAM 1   BYTES 1-3 A(INVENTORY RECORD)                          
*              PARAM 2   BYTES 1-3 A(200 BYTE OUTPUT AREA)                      
         SPACE 1                                                                
INVPTR   NTR1                                                                   
         L     R2,0(R1)                                                         
         USING RINVREC,R2                                                       
         L     R4,4(R1)                                                         
         USING RIDPKEY,R4                                                       
         XC    0(200,R4),0(R4)                                                  
         LA    R6,6                                                             
         LA    R3,RINVDP                                                        
         SPACE 1                                                                
INVPTR1  MVI   RIDPKTYP,RIDPKTYQ                                                
         MVC   RIDPKREP,RINVKREP                                                
         MVC   RIDPKSTA,RINVKSTA                                                
         MVC   RIDPKDPT,0(R3)                                                   
         MVC   RIDPKINV,RINVKINV                                                
         MVC   RIDPKSTD,RINVKSTD                                                
*                                                                               
*  IF SELF ASSIGNED GET NEXT DAYPART                                            
*  ONLY COMPUTER GENERATED NUMBERS GET THE DAY,QTR HOUR                         
*  AND THE LENGTH FILLED IN.                                                    
*                                                                               
         TM    RINVSTAT,X'80'                                                   
         BO    INVPTR20            BIT ON SELF ASSIGNED                         
*                                                                               
         MVC   RIDPKDAY,RINVOINV+1   MOVE DAY CODE,                             
         MVC   RIDPKQTR,RINVOINV     QUARTER HOUR,                              
         MVC   RIDPKLEN,RINVOINV+2   AND PROGRAM LENGTH TO KEY                  
         SPACE                                                                  
INVPTR20 LA    R3,1(R3)            NEXT DAYPART CODE                            
         CLI   0(R3),X'40'                                                      
         BNH   INVPTX                                                           
         LA    R4,32(R4)                                                        
         BCT   R6,INVPTR1          DO NEXT POINTER                              
*                                                                               
INVPTX   B     EXIT                                                             
*                                                                               
         DROP  R2,R4                                                            
*                                                                               
*  THESE DAYPARTS GET A DAY CODE, QUARTER HOUR, AND PROGRAM LENGTH              
DAYCOD   DC    C'MDKNPOUXYWZ',X'FF'                                             
         SPACE 1                                                                
*  THESE DAYPARTS GET EFFECTIVE DATE, QUARTER HOUR, AND PROGRAM LENGTH          
EFFDAT   DC    C'VSJ',X'FF'                                                     
         SPACE 1                                                                
*  THESE DAYPARTS ONLY GET INVENTORY NUMBER AND START DATE                      
*       ERATLF - THEY ARE THE FRINGE "SUB-DAYPARTS"                             
* (W-WEEKEND IS NOT TREATED AS FRINGE FOR PASSIVE POINTERS, BUT                 
*    IS GROUPED WITH FRINGE EVERYWHERE ELSE)                                    
         EJECT                                                                  
*              CHANGE OLD RECORDS IF KEY CHANGE                                 
*   R4 = ADDRESS OF THE OLD RECORD (AIO1)                                       
*   R6 = ADDRESS OF THE NEW RECORD (AIO2)                                       
         SPACE 1                                                                
CHAOLD   NTR1                                                                   
         CLI   ACTNUM,ACTCHA                                                    
         BE    CHAOLDC                                                          
         CLI   ACTNUM,ACTSEL                                                    
         BNE   EXIT                                                             
CHAOLDC  MVC   KEY(27),0(R4)       OLD HEADER KEY                               
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(27),KEY                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
         OI    KEY+27,X'80'                                                     
         BAS   RE,MYDIRWRT         DELETE OLD HEADER POINTER                    
         B     CHAOLDP                                                          
         SPACE 1                                                                
CHAOLDN  CLC   KEYSAVE(22),KEY     IS THIS AN OLD BOOK OR TEXT                  
         BNE   EXIT                NO, I AM FINISHED                            
         SPACE 1                                                                
         L     R2,AIO3                                                          
         ST    R2,AIO                                                           
         GOTO1 GETREC                                                           
         OI    KEY+27,X'80'        DELETE OLD POINTER                           
         BAS   RE,MYDIRWRT                                                      
         SPACE 1                                                                
         MVC   0(24,R2),0(R6)      NEW KEY                                      
*  GET FIRST X'CE' ELEMENT AND SAVE THE FROM BOOK (FROM TRACK RECORD)           
         GOTO1 HELLO,DMCB,(C'G',REPFILE),(X'CE',AIO3),0                         
         CLI   12(R1),0                                                         
         BNE   CHAOLDO             NO X'CE' ELEM WRITE RECORD OUT               
         L     R5,12(R1)                                                        
         MVC   INVSRC(3),7(R5)                                                  
*  DELETE ALL THE X'CE' ELEMENTS (FROM THE TRACK RECORD)                        
         GOTO1 HELLO,DMCB,(C'D',REPFILE),(X'CE',AIO3),0                         
*  GET FIRST X'02' DAY TIME ELEMENT (FROM THE INV HEADER)                       
         GOTO1 HELLO,DMCB,(C'G',REPFILE),(X'02',AIO2),0                         
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R5,12(R1)                                                        
*  BUILD A X'CE' ELEMENT                                                        
CHAOLDM  XC    WORK,WORK                                                        
         MVC   WORK(2),=X'CE0A'                                                 
         MVC   WORK+2(5),2(R5)     DAY TIME FROM X'02' ELEM                     
         MVC   WORK+7(3),INVSRC    FROM BOOK                                    
         GOTO1 HELLO,DMCB,(C'P',REPFILE),(0,AIO3),WORK,0                        
*  GET NEXT DAY TIME ELEMENT                                                    
         ZIC   RE,1(R5)                                                         
         AR    R5,RE                                                            
         CLI   0(R5),X'02'                                                      
         BE    CHAOLDM                                                          
*                                                                               
CHAOLDO  BAS   RE,FLADD            ADD IT                                       
         SPACE 1                                                                
CHAOLDP  MVC   KEY(27),0(R4)       ITS DELETED SO I'LL GET NEXT                 
         GOTO1 HIGH                                                             
         B     CHAOLDN                                                          
         EJECT                                                                  
*              ADD THE RECORD TO FILE                                           
         EJECT                                                                  
*                                                                               
FLADD    NTR1                                                                   
         L     R6,AIO                                                           
         USING REINVREC,R6                                                      
         XC    KEY,KEY                                                          
         MVC   KEY(RINVKSTD-RINVKEY),0(R6)                                      
*                                                                               
         LA    R5,WORK                                                          
         USING RINVAEL,R5          BUILD A NEW ACTIVITY ELT                     
         XC    WORK,WORK                                                        
         MVC   RINVACOD(2),=X'EF0C'                                             
         GOTO1 DATCON,DMCB,(5,DUB),(3,RINVAFST)                                 
         MVC   RINVALST,RINVAFST                                                
         MVI   RINVAWHY,C'A'                                                    
         OI    DMINBTS,X'08'       PASS DELETES                                 
*                                                                               
         GOTO1 HIGH                                                             
         CLC   SAVEKEY2(RINVKSTD-RINVKEY),KEY                                   
         BE    FLP010              INV WITH SAME # EXISTS                       
         B     FLP050              DIRECTLY SWAP TO A NEW INV NUMBER            
GETNEXT  GOTO1 SEQ                                                              
         CLC   SAVEKEY(RINVKSTD-RINVKEY),KEY                                    
         BNE   FLP010X                                                          
INVKEYD  USING RINVKEY,KEY                                                      
         CLI   INVKEYD.RINVKRTP,0  CHK FOR HEADER ONLY                          
         BNE   GETNEXT                                                          
*                                                                               
FLP010   EQU   *                   CHECK FOR OVERLAP                            
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
*                                                                               
         LA    R2,INVCODEH                                                      
         OC    RINVPEFF+2(2),RINVPEFF+2  IS THERE AN END DATE?                  
         BNZ   FLP020                    YES                                    
         OC    DTEHLDE3,DTEHLDE3                                                
         BNZ   *+8                                                              
         BAS   RE,OVERERR                                                       
         CLC   DTEHLDE3(2),RINVPEFF      NO                                     
         BL    GETNEXT                   ARE THEY OVERLAPPING?                  
         BAS   RE,OVERERR                                                       
*                                                                               
FLP020   CLC   DTEHLD3,RINVPEFF+2  START DATE > OLD END DATE?                   
         BH    GETNEXT             YES, OK                                      
         OC    DTEHLDE3,DTEHLDE3     IS THERE A NEW END DATE?                   
         BNZ   *+8                 YES                                          
         BAS   RE,OVERERR                                                       
         CLC   DTEHLDE3,RINVPEFF   NEW END DATE < OLD START DATE?               
         BL    GETNEXT             OK                                           
         BAS   RE,OVERERR                                                       
FLP010X  EQU   *                   NO ERROR, ADD REC                            
         B     FLP050                                                           
                                                                                
FLP050   EQU   *                                                                
*  ADD NEW REC,NEW KEY IN AIO2 TO FILE                                          
*  GET X'EF' ELEMENT AND UPDATE THE CHANGE DATE                                 
*                                                                               
         GOTO1 HELLO,DMCB,(C'G',REPFILE),(X'EF',AIO2),0                         
         CLI   12(R1),0                                                         
         BNE   FLP100              NO X'EF' ELEM BUILD ONE                      
         L     R5,12(R1)                                                        
         GOTO1 DATCON,DMCB,(5,DUB),(3,RINVALST)                                 
         MVI   RINVAWHY,C'C'                                                    
         B     FLP200                                                           
         SPACE 1                                                                
FLP100   GOTO1 HELLO,DMCB,(C'P',REPFILE),(0,AIO2),WORK,=C'ADD=CODE'             
         SPACE 1                                                                
FLP200   EQU   *                                                                
         L     R6,AIO2                                                          
         MVC   KEY(27),0(R6)                                                    
         MVC   AIO,AIO2                                                         
         BAS   RE,MYFILADD         WRITE BACK THE NEW                           
         NI    DMINBTS,X'F7'                                                    
         MVC   BSVDA,KEY+28                                                     
*                                                                               
         DROP  R5,R6                                                            
* DO NOT DELETE REC IN AIO1 ON FILE                                             
* DELETE KEY IN AIO1 IN FILE                                                    
*                                                                               
         L     R1,AIO1                                                          
         MVC   KEY(27),0(R1)                                                    
         GOTO1 HIGH                                                             
         MVI   KEY+27,X'80'                                                     
         BAS   RE,MYDIRWRT                                                      
* GET NEXT KEY                                                                  
         GOTO1 SEQ                                                              
         CLC   KEYSAVE(21),KEY     IS THIS THE SAME INV?                        
         BNE   FLPPUT2X            NO, DONE                                     
INVKEYD  USING RINVKEY,KEY                                                      
         CLI   INVKEYD.RINVKRTP,0  IS THIS A HEADER?                            
         BE    FLPPUT2X            YES, DONE                                    
         L     R4,AIO1                                                          
         ST    R4,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         USING REINVREC,RF                                                      
         L     RE,AIO1                                                          
         L     RF,AIO2                                                          
         LA    R1,2000                                                          
         MOVE  ((RF),(R1)),(RE)                                                 
         L     RF,AIO2             RESET THE POINTER                            
         MVC   RINVKINV,INVCODE    UPDATE CODE                                  
*        MVC   SAVEKEY2(27),REINVREC                                            
         B     FLP050                                                           
         EJECT                                                                  
FLPPUT2X XIT1                                                                   
*                                                                               
OVERERR  DS    0H                                                               
         MVI   ERROR,OVERLAP                                                    
         TM    KEY+27,X'80'                                                     
         BZ    ERREND                                                           
         MVI   ERROR,X'FE'                                                      
         MVI   GENSTAT2,USMYOK                                                  
         MVC   CONHEAD(L'OVERLAPD),OVERLAPD                                     
         GOTO1 ERREX2                                                           
***********************************************************************         
* DATAMGR INTERFACE                                                             
***********************************************************************         
MYFILADD NTR1                                                                   
         GOTO1 DATAMGR,DMCB,=C'ADDREC',=C'REPFILE ',KEY+28,AIO,DMWORK           
         BAS   RE,DMCHECK                                                       
         MVC   BSVDA,KEY+28     SAVE DISK ADDRESS                               
         B     YES                                                              
*                                                                               
MYFILWRT NTR1                                                                   
         GOTO1 DATAMGR,DMCB,=C'PUTREC',=C'REPFILE ',KEY+28,AIO,DMWORK           
         BAS   RE,DMCHECK                                                       
         B     YES                                                              
*                                                                               
MYDIRWRT NTR1                                                                   
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'REPDIR  ',KEY,KEY                      
         BAS   RE,DMCHECK                                                       
         B     YES                                                              
*                                                                               
MYDIRADD NTR1                                                                   
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'REPDIR  ',KEY,KEY                      
         BAS   RE,DMCHECK                                                       
         B     YES                                                              
*                                                                               
DMCHECK  CLI   8(R1),0                                                          
         BER   RE                                                               
         TM    8(R1),X'90'                                                      
         BM    NO                                                               
         DC    H'0'                                                             
         SPACE 1                                                                
YES      SR    R1,R1                                                            
         B     *+8                                                              
NO       LA    R1,1                                                             
         LTR   R1,R1                                                            
         SPACE 1                                                                
XIT      XIT1  REGS=(R0,R1)                                                     
         EJECT                                                                  
*  BUMP TO NEXT SCREEN FIELD                                                    
NEXTFLD  ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         BR    RE                                                               
         EJECT                                                                  
ERREND   GOTO1 ERREX                                                            
*                                                                               
RELO     DS    A                                                                
REPFILE  DC    CL8'REPFILE'                                                     
OVERLAPD DC    CL44'Error: overlapping date with deleted records'               
         LTORG                                                                  
         EJECT                                                                  
PLINED   DSECT                                                                  
PRINVNUM DS    CL4                                                              
         DS    CL3                                                              
PREFFDTE DS    CL17                                                             
         DS    CL3                                                              
PRPRGNM  DS    CL27                                                             
         DS    CL3                                                              
PRDYTIME DS    CL20                                                             
         DS    CL3                                                              
PRDAYPT  DS    CL7                                                              
         DS    CL3                                                              
PRAVDAY  DS    CL11                                                             
         DS    CL3                                                              
PRAVTIME DS    CL11                                                             
         SPACE 2                                                                
LLINED   DSECT                                                                  
LINVNUM  DS    CL6                                                              
         DS    CL1                                                              
LEFFDTE  DS    CL17                                                             
         DS    CL1                                                              
LPROGRM  DS    CL20                                                             
         DS    CL1                                                              
LDPT     DS    CL6                                                              
         DS    CL1                                                              
LDYTIME  DS    CL18                                                             
         EJECT                                                                  
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* RERMPFFD                                                                      
* DDGENTWA                                                                      
* RERMPWTWA                                                                     
* RERMPD7D                                                                      
* REGENMKT                                                                      
* REGENREP(A)                                                                   
* RERMPWORKD                                                                    
*        PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE RERMPFFD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE RERMPE3D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE RERMPE4D                                                       
         EJECT                                                                  
       ++INCLUDE RERMPWTWA                                                      
         EJECT                                                                  
REINVREC DSECT                                                                  
       ++INCLUDE REGENINVA                                                      
         EJECT                                                                  
       ++INCLUDE RERMPWORKD                                                     
         SPACE 3                                                                
         ORG   SYSSPARE                                                         
*                                                                               
*              WORK AREA                                                        
*                                                                               
*  ALL FIELDS DEFINED ABOVE THE DOUBLE LINE OF ASTERIKS                         
*  MUST ALSO BE DEFINED IN THE RERMP30 PHASE.                                   
*                                                                               
INVLIST  DS    F                   POINTER TO INVENTORY INFO                    
INVDYTIM DS    CL60                EXTENDED DAY TIME DEMO TABLE                 
*                                                                               
INVMED   DS    CL1                 MEDIA                                        
INVSTAT  DS    CL5                 STATION                                      
INVMKT   DS    CL2                 MARKET                                       
INVSRC   DS    CL1                 SOURCE                                       
INVFBK   DS    CL2                 FROM BOOK                                    
INVTYP   DS    CL1                 I OR P                                       
INVEFDT  DS    CL2                 EFFECTIVE DATE - COMPRESSED                  
INVNO    DS    CL1                 NUMBER IN INVENTORY LIST                     
INVBAD   DS    CL1                 0=NO ERROR, N=NUMBER OF BAD ITEM             
TOTWGHT  DS    CL1                 TOTAL NUMBER QTR HOURS                       
INVTOBK  DS    CL15                TO BOOK CODES                                
*                                                                               
INVIND   DS    CL1                 INVENTORY TYPE INDICATOR                     
INVDAYS  DS    CL1                 1=MON, 7=SUN                                 
INVTIM   DS    CL4                 MILITARY TIME                                
INVCDE   DS    CL2                 PROGRAM CODE                                 
INVCDCTL DS    B                   CONTROL BITS FOR PROGRAM CODE                
INVBTYPE DS    C                   BOOK TYPE (USER INPUT, APPLIES TO            
*                                  DEMO FILE TRANSFERS)                         
INVFRBT  DS    C                   BOOK TYPE (ON INV TO INV TRANSFER            
*                                                                               
TRBKLIST DS    CL60                BOOK ENTRIES BUILT BY REBKLST                
         SPACE                                                                  
TRBKCNT  DS    X                   COUNT OF BOOK ENTRIES                        
TRMODE   DS    C                   COMMUNICATION TO BUFFER ROUTINE              
TRWTOV   DS    C                   USER WEIGHTING OVERRIDE (Y/N)                
TRHOOKSW DS    C                   HOOK ENTERED FOR DEMAND CALL (Y/N)           
TRSVKEY  DS    CL27                                                             
TRFNOVER DS    C                   Y=SUPPRESS TIME PERIOD FOOTNOTING            
TRAPAGE  DS    A                   A(2304 BYTE PAGE)                            
TRPAGE   DS    X                   PAGES WRITTEN TO TWA                         
TRRECS   DS    X                   RECORDS GENERATED DURING LINE EDIT           
         SPACE 1                                                                
DEMEDIA  DS    CL1                 FROM MEDIA                                   
DEMSTA   DS    CL5                      STATION                                 
DEMRKT   DS    CL2                      MARKET FOR DEMOS                        
*                                                                               
HALF2    DS    H                                                                
BYTE2    DS    CL1                                                              
BYTE3    DS    CL1                                                              
BYTE4    DS    CL1                                                              
*****************************************************                           
*****************************************************                           
STAHLD   DS    CL5                 STATION HOLD AREA                            
INVHLD   DS    CL4                 INVENTORY HOLD AREA                          
DTEHLD   DS    CL3                 DATE HOLD AREA                               
DTEHLD2  DS    CL2                 2 BYTE DATE HOLD AREA                        
DTEHLDE2 DS    CL2                 2 BYTE END DATE HOLD                         
*                                                                               
DTEHLD3  DS    CL2                 2 BYTE DATE HOLD AREA                        
DTEHLDE3 DS    CL2                 2 BYTE END DATE HOLD                         
*                                                                               
DATEDEB  DS    CL2                 DAY TO SUBTRACT FROM LAST INV                
*                                                                               
*  PRINT ELEMENT ADDRESS STORAGE LOCATIONS                                      
DYTMPTR  DS    F                   DAY/TIME ELEMENT                             
PROGPTR  DS    F                   PROGRAM ELEMENT                              
AVPRPTR  DS    F                   AVAIL PROGRAM ELEMENT                        
OVFLSW   DS    CL1                 TOO MANY LINES TO PRINT                      
*                                                                               
WORK2    DS    CL200               EXTRA WORK AREA                              
SAVEKEY  DS    CL27                                                             
SAVEKEY2 DS    CL27                                                             
SAVEKEY4 DS    CL27                OLD REC KEY SAVE                             
SAVESTD  DS    XL3                 SAVE EFFECTIVE DATE                          
CHNGLEN  DS    CL1                                                              
*                                                                               
FIRSTSW  DS    CL1                                                              
DAYINP   DS    CL1                                                              
RMPPROFS DS    CL8                 PROFILE SETTINGS                             
BSVDA    DS    CL4                 SAVED DISK ADDRESS                           
         EJECT                                                                  
* INVENTORY LIST ENTRY DSECT                                                    
*                                                                               
INVLD    DSECT                                                                  
INVLREC  DS    0CL10                                                            
INVLFLE  DS    CL1                 P=PAV, I=INVENTORY                           
INVLTYP  DS    CL1                 X'80'  INVENTORY NUMBER                      
*                                  X'40'  FIRST IN DAY/TIME EXP.                
*                                  X'20'  LAST IN DAY/TIME EXP.                 
*                                  X'08'  ADD EXPRESSION                        
INVLWT   DS    CL1                 WEIGHT (BINARY)                              
INVLDATA DS    0CL6                                                             
INVLSTIM DS    CL2                 START TIME                                   
INVLETIM DS    CL2                 END TIME                                     
INVLDAY  DS    CL1                 DAY                                          
         DS    CL1                 SPARE                                        
         ORG   INVLDATA                                                         
INVLNUMB DS    CL3                 NUMBER                                       
INVLDATE DS    CL3                 START DATE (Y/M/D BINARY)                    
         DS    CL1                 SPARE                                        
         SPACE 2                                                                
         EJECT                                                                  
RINVD    DSECT                                                                  
       ++INCLUDE REGENAVL                                                       
       ++INCLUDE DDCOMFACS                                                      
         SPACE 5                                                                
* SAVED STORAGE IN TWA0 FOR NESFM00 STARTS HERE                                 
T810FFD  DSECT                                                                  
SAVAREAL EQU   ((CONHEAD+3520)-(T810FFD+3072))                                  
         ORG   CONHEAD+3520-SAVAREAL                                            
SAVAREA  DS    0C                                                               
SVLIST   DS    CL268               CALL ROUTINE STACK POINTER                   
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'025RERMP16   04/13/09'                                      
         END                                                                    
**PAN#1  CSECT                                                                  
         END                                                                    
