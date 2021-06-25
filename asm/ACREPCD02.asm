*          DATA SET ACREPCD02  AT LEVEL 002 AS OF 10/06/03                      
*PHASE ACCD02A,+0                                                               
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
         TITLE 'COSTING DUMP RECOVERY PROGRAM'                                  
ACCD02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*ACCD02*,R9                                                    
*                                                                               
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING CD02D,RC                                                         
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BNE   XIT                                                              
*                                                                               
         OPEN  (COSTRCV,(INPUT))                                                
         LTR   RF,RF               SUCCESSFUL OPEN?                             
         BZ    *+6                 YES - CONTINUE                               
         DC    H'0'                NO  - DEATH                                  
*                                                                               
         EJECT                                                                  
*--------------------------------------                                         
*        READ A COSTING RECOVERY RECORD                                         
*--------------------------------------                                         
*                                                                               
*                                                                               
GETR00   GET   COSTRCV,RCVIO                                                    
         LA    R5,RCVIO            ADDR OF THE RECOVERY RECORD                  
         USING CSTRCVD,R5                                                       
         LA    R6,CSTRECRD         THE HISTORY RECORD (PAST THE HEADER)         
         USING CACRECD,R6                                                       
         CLI   0(R6),PLDKTYPQ      X'18' - DIRECT TIME OR P&L DIR               
         BE    GETR100                                                          
*                                                                               
         CLI   CSTRECTY,CSTRCPYQ   COPY OF A RECORD?                            
         BE    GETR10              YES - READ NEW AND REWRITE COPY              
         CLI   CSTRECTY,CSTRADDQ   ADDED A NEW RECORD?                          
         BE    GETR20              YES - ZERO BUCKET                            
         DC    H'0'                UNKNOWN                                      
*                                                                               
GETR10   LA    RE,IO2              RECEIVING FIELD                              
         LH    RF,=S(L'IO2)        RECEIVING FIELD LENGTH                       
         XCEF                                                                   
         LA    RF,IO2              RECEIVING FIELD                              
         LH    R1,CACRLEN          LENGTH                                       
         LA    RE,CACKEY           SENDING FIELD                                
         MOVE  ((RF),(R1)),(RE)                                                 
*                                                                               
GETR20   MVC   THISKEY,CACKEY      COPY AND ADD COMES HERE                      
         XC    DIRREC,DIRREC                                                    
         OI    DMINBTS,PASSDEL      PASS BACK DELETES AS WELL                   
         GOTO1 DATAMGR,DMCB,(DMINBTS,DMREAD),DIR,THISKEY,DIRREC                 
         NI    DMINBTS,ALL-PASSDEL                                              
         TM    8(R1),X'10'         DID I FIND THE RECORD?                       
         BNO   *+6                                                              
         DC    H'0'                NOT ON FILE                                  
         LA    R6,DIRREC           THE DIRECTORY RECORD JUST READ               
         CLI   CSTRECTY,CSTRADDQ   IS IT AN ADD?                                
         BNE   *+8                                                              
         BAS   RE,DMPGET                                                        
*                                                                               
         MVC   DA,CACKDA           DISK ADDRESS                                 
         OI    DMINBTS,PASSDEL      PASS BACK DELETES AS WELL                   
         GOTO1 DATAMGR,DMCB,(DMINBTS,GETREC),MST,DA,IO1,DMWORK                  
         NI    DMINBTS,ALL-PASSDEL                                              
         TM    8(R1),X'10'         DID I FIND THE RECORD?                       
         BNO   *+6                                                              
         DC    H'0'                NOT ON FILE                                  
         LA    R6,IO1              THE HISTORY RECORD JUST READ                 
         BAS   RE,DMPGET                                                        
         LA    R6,CACRECD+(CACRFST-CACRECD)                                     
         BAS   RE,FIXCAC           ZERO OUT 45 BUCKET AMOUNT (MAYBE)            
*                                                                               
         CLI   CSTRECTY,CSTRADDQ   ADD?                                         
         BNE   GETR50              NO  - IT'S A COPY                            
         LA    R6,IO1              POINT BACK TO BEGINNING OF REC               
         TM    BIT,HISTREC         CONTRA HISTORY REC? DON'T DELETE             
         BO    GETR70                                                           
         TM    BIT,CACHREC         IF CONTRA HEADER REC DON'T DELETE            
         BO    GETR70                                                           
         LA    R6,DIRREC                                                        
         OI    CACKSTAT,DELETE     YES - MARK DIRECT ENTRY DELETED              
         LA    R6,IO1                                                           
         OI    CACRSTAT,DELETE     YES - MARK MASTER DELETED                    
         B     GETR70              GO WRITE IT BACK                             
*                                                                               
GETR50   CLC   IO1(L'CACKEY),IO2   COPY                                         
         BE    *+6                 MAKE SURE SAME KEY                           
         DC    H'0'                                                             
         LA    R6,IO2              RESTORE THE OLD HISTORY RECORD               
*                                                                               
GETR70   CLI   RCWRITE,C'N'                                                     
         BE    GETR80                                                           
         GOTO1 DATAMGR,DMCB,PUTREC,MST,DA,(R6),DMWORK                           
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                DIED WRITING A RECORD                        
         TM    CACRSTAT,DELETE     MASTER MARKED DELETED                        
         BNO   GETR80              IF YES -WRITE BACK DELETED DIR REC           
         LA    R1,DIRREC                                                        
         OI    (CACKSTAT-CACRECD)(R1),DELETE                                    
         GOTO1 DATAMGR,DMCB,DMWRT,DIR,DIRREC,DIRREC                             
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                DIED WRITING A RECORD                        
*                                                                               
GETR80   BAS   RE,DMPPUT           (R6) POINTS TO RECORD TO DUMP                
         CLI   CSTRECTY,CSTRADDQ   AN ADD?                                      
         BNE   *+12                NO - DONE ,GET NEXT RECORD                   
         LA    R6,DIRREC           YES - DUMP DELETED DIRECTORY REC             
         BAS   RE,DMPPUT           (R6) POINTS TO RECORD TO DUMP                
         B     GETR00              READ NEXT RECORD                             
*                                                                               
*                                                                               
*                                  P&L RECORDS                                  
         USING PLDRECD,R6                                                       
GETR100  LA    R6,CSTRECRD         THE P&L RECORD (PAST THE HEADER)             
         MVC   THISKEY,PLDKEY                                                   
         XC    DIRREC,DIRREC                                                    
         OI    DMINBTS,PASSDEL      PASS BACK DELETES AS WELL                   
         ZAP   PKDUB,=P'0'                                                      
         CLI   CSTRECTY,CSTRADDQ   IS IT AN ADD?                                
         BE    *+10                                                             
         ZAP   PKDUB,PLDKAMT                                                    
         GOTO1 DATAMGR,DMCB,(DMINBTS,DMREAD),DIR,THISKEY,DIRREC                 
         NI    DMINBTS,ALL-PASSDEL                                              
         TM    8(R1),X'10'         DID I FIND THE RECORD?                       
         BNO   *+6                                                              
         DC    H'0'                NOT ON FILE                                  
         LA    R6,DIRREC           THE DIRECTORY RECORD JUST READ               
         BAS   RE,DMPGET                                                        
         ZAP   PLDKAMT,PKDUB       SET BUCKETS-EITHER 0 OR OLD AMT              
*                                                                               
         CLI   RCWRITE,C'N'                                                     
         BE    GETR102                                                          
         GOTO1 DATAMGR,DMCB,DMWRT,DIR,DIRREC,DIRREC                             
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                DIED WRITING A RECORD                        
GETR102  LA    R6,DIRREC           THE DIRECTORY RECORD JUST WRITTEN            
         BAS   RE,DMPPUT                                                        
         B     GETR00              READ NEXT RECORD                             
*                                                                               
LAST     DS    0H                                                               
         CLOSE (COSTRCV)                                                        
         LTR   RF,RF               SUCCESSFUL CLOSE?                            
         BZ    *+6                 YES - GOODBYE                                
         DC    H'0'                NO  - DEATH                                  
XIT      XIT1                                                                   
         EJECT                                                                  
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*         
* ZERO OUT BUCKET AMOUNTS IN 45 ELEMENT OF CONTRA HISTORY (IF IT IS A           
* CONTRA HISTORY REC) AND CHECK FOR CONTRA HEADER RECORD                        
* R6 POINTS TO RECORD                                                           
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*         
*                                                                               
FIXCAC   NTR1                                                                   
*                                                                               
         NI    BIT,X'FF'-HISTREC                                                
         NI    BIT,X'FF'-CACHREC                                                
FIXC05   CLI   0(R6),0                                                          
         BE    FIXX                MUST NOT BE THE CONTRA HISTORY REC           
         CLI   0(R6),X'43'         CONTRA ACCOUNT HEADER ELEM                   
         BE    FIXC15                                                           
         CLI   0(R6),X'45'         BUCKET ELEMENT                               
         BE    FIXC20                                                           
FIXC10   ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     FIXC05                                                           
*                                                                               
FIXC15   OI    BIT,CACHREC         MARK AS C/A HEADER REC                       
         B     FIXX                AND DON'T DELETE                             
*                                                                               
         USING BUKELD,R6                                                        
FIXC20   OI    BIT,HISTREC                                                      
         CLC   BUKMOS,CSTBMOS      SAME BUCKET MONTH?                           
         BNE   FIXC10                                                           
         ZAP   BUKDR,=P'0'         ZERO OUT THE BUCKET AMOUNT                   
         ZAP   BUKCR,=P'0'                                                      
         B     FIXC10                                                           
*                                                                               
FIXX     B     XIT                                                              
         DROP  R6                                                               
*                                                                               
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*         
DMPGET   NTR1                                                                   
         AP    DUMPCNT,=P'1'                                                    
         ZAP   DUB,DUMPCNT                                                      
         DP    DUB,EVERY                                                        
         CP    DUB+4(4),=P'0'                                                   
         BNE   XIT                                                              
         AP    PDUMP,=P'1'                                                      
         CP    PDUMP,MAXDUMP                                                    
         BH    XIT                                                              
         LA    R4,=C'GET'                                                       
         B     DUMP                                                             
         SPACE 1                                                                
DMPPUT   NTR1                                                                   
         USING CACRECD,R6                                                       
         CP    PDUMP,MAXDUMP                                                    
         BH    XIT                                                              
         ZAP   DUB,DUMPCNT                                                      
         DP    DUB,EVERY                                                        
         CP    DUB+4(4),=P'0'                                                   
         BNE   XIT                                                              
         LA    R4,=C'PUT'                                                       
         SPACE 1                                                                
DUMP     LR    R3,R6                                                            
         SR    R8,R8                                                            
         ICM   R8,3,CACRLEN                                                     
         C     R3,=A(DIRREC)    IF DIRECTORY RECORD USE DIRREC LENGTH           
         BNE   *+8                                                              
         LH    R8,=Y(L'DIRREC)                                                  
         GOTO1 PRNTBL,DMCB,(3,(R4)),(R3),C'DUMP',(R8),=C'2D'                    
         B     XIT                                                              
         EJECT                                                                  
PRNTBL   DC    V(PRNTBL)                                                        
         SPACE 1                                                                
ACCFIL   DC    CL8'ACCOUNT '                                                    
DIR      DC    CL6'ACCDIR'                                                      
MST      DC    CL6'ACCMST'                                                      
GETREC   DC    CL8'GETREC  '                                                    
ADDREC   DC    CL8'ADDREC  '                                                    
PUTREC   DC    CL8'PUTREC  '                                                    
         SPACE 1                                                                
DUMPCNT  DC    PL4'0'                                                           
EVERY    DC    PL4'1'                                                           
PDUMP    DC    PL4'0'                                                           
MAXDUMP  DC    PL4'1000'                                                        
*                                                                               
IO1      DS    CL(IOSIZE)           I/O AREA 1                                  
IO2      DS    CL(IOSIZE)           I/O AREA 2                                  
DIRREC   DS    CL(CACRFST-CACRECD)  I/O AREA FOR DIRECTORY READ                 
RCVIO    DS    CL(RCVSIZE)          RECOVERY I/O AREA                           
DA       DS    XL4                  DISK ADDRESS                                
*                                                                               
*                                                                               
*                                                                               
*                                                                               
COSTRCV  DCB   DDNAME=COSTRCV,DSORG=PS,MACRF=(GM),EODAD=LAST,          *        
               RECFM=VB,LRECL=RCVSIZE,BLKSIZE=RCVSIZE+4                         
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
CD02D    DSECT                                                                  
THISKEY  DS    CL(L'CACKEY)                                                     
PKDUB    DS    PL8                                                              
BIT      DS    XL1                                                              
HISTREC  EQU   X'80'               REC READ IS A CONTRA HISTORY REC             
CACHREC  EQU   X'40'               REC READ IS A CONTRA HEADER REC              
         EJECT                                                                  
**************EQUATES*****************                                          
*                                                                               
*                                                                               
*                                                                               
DELETE   EQU   X'80'                                                            
PASSDEL  EQU   X'08'               PASS BACK DELETED RECORDS(DATAMGR)           
ALL      EQU   X'FF'                                                            
NOTFOUND EQU   X'10'                                                            
*                                                                               
*                                                                               
*                                                                               
*  ACREPWORKD                                                                   
*  ACGENFILE                                                                    
*  ACGENMODES                                                                   
*  ACMASTD                                                                      
*  ACCSTRCVD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
       ++INCLUDE ACCSTRCVD                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002ACREPCD02 10/06/03'                                      
         END                                                                    
