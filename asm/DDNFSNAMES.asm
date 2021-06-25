*          DATA SET DDNFSNAMES AT LEVEL 011 AS OF 09/03/15                      
*PROCESS USING(WARN(15))                                                        
*PHASE NFSNAMSA                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE CARDS                                                                  
*INCLUDE QSORT                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
NFSNAMES CSECT                                                                  
         TITLE 'GET THE FILENAMES IN AN NFS-MOUNTED FOLDER'                     
         PRINT NOGEN                                                            
         NBASE 0,NFSNAMES,=V(REGSAVE)                                           
*                                                                               
         USING DPRINT,RA                                                        
         L     RA,=V(CPRINT)                                                    
*                                                                               
         MVC   P(16),=C'PARAMETER CARDS:'                                       
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         GOTO1 =V(CARDS),DMCB,CARD,=C'RE10'  ***NOTE P2***                      
         CLI   4(R1),0             WAS THE OPEN SUCCESSFUL?                     
         BE    PRINTCRD            YES                                          
         MVC   P(20),=C'** NO SYSIN FOUND **'                                   
         GOTO1 =V(PRINTER)                                                      
         B     CARDEOF                                                          
*                                                                               
PRINTCRD DS    0H                                                               
         MVC   P(L'CARD),CARD      PRINT EACH PARAMETER CARD                    
         GOTO1 =V(PRINTER)                                                      
         CLC   =C'/*',CARD         EOF?                                         
         BE    CARDEOF             YES                                          
*                                                                               
         CLI   CARD,C'*'           COMMENT?                                     
         BE    GETCARD             YES                                          
*                                                                               
         CLC   =C'SORTED=',CARD    SORT THE PATHNAME FILE?                      
         BNE   CHKDIR                                                           
         CLI   CARD+7,C'N'                                                      
         BE    GETCARD             NO IS THE DEFAULT                            
         CLI   CARD+7,C'Y'                                                      
         BE    *+6                                                              
         DC    H'0'                INVALID OPTION ON CONTROL CARD               
         MVI   SORTFLAG,C'Y'                                                    
         B     GETCARD                                                          
*                                                                               
CHKDIR   DS    0H                                                               
         CLC   =C'DIRECTORY=',CARD OVERRIDE THE WORKING DIRECTORY?              
         BNE   CHKEXTF                                                          
         LA    RE,CARD+10          YES: POINT RE TO DIRECTORY NAME              
         LA    RF,PATHNAME         PUT THE NAME HERE                            
         SR    R0,R0               COUNT THE CHARACTERS                         
NXTCHR   MVC   0(1,RF),0(RE)                                                    
         AHI   R0,1                                                             
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         CLI   0(RE),C' '                                                       
         BNE   NXTCHR                                                           
         ST    R0,PATHNAME_LENGTH  SAVE THE LENGTH                              
         B     GETCARD                                                          
*                                                                               
CHKEXTF  DS    0H                                                               
         CLC   =C'EXTFILT=',CARD   FILENAME EXTENSION FILTER?                   
         BNE   CHKDD                                                            
         MVC   EXTFILT,CARD+8      YES: ASSUME IT'S A POSITIVE FILTER           
         CLI   CARD+8,C'-'         MINUS SIGN = NEGATIVE FILTER                 
         BNE   *+14                                                             
         MVC   EXTFILT,CARD+9      SAVE NEGATIVE EXTENSION FILTER               
         MVI   EXTFILT_TYPE,C'-'                                                
         OC    EXTFILT,SPACES      CONVERT TO UPPERCASE                         
         B     GETCARD                                                          
*                                                                               
CHKDD    DS    0H                                                               
         CLC   =C'DDSTMTS=',CARD   GENERATE FILE OF DD STATEMENTS?              
         BE    *+6                                                              
         DC    H'0'                INVALID CONTROL CARD                         
         CLI   CARD+8,C'N'                                                      
         BE    GETCARD             NO IS THE DEFAULT                            
         CLI   CARD+8,C'Y'                                                      
         BE    *+6                                                              
         DC    H'0'                INVALID OPTION ON CONTROL CARD               
         MVI   DDSTFLAG,C'Y'                                                    
*                                                                               
GETCARD  DS    0H                                                               
         GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         B     PRINTCRD                                                         
*                                                                               
CARDEOF  DS    0H                                                               
         LHI   R2,MAXNAMES         MAX NUMBER OF PATHNAMES                      
         MHI   R2,L'FILENAME+4     X L'PATHNAME (VARIABLE-LENGTH REC)           
         LA    R2,8(R2)            FOR EYE-CATCHER(8)                           
         STORAGE OBTAIN,LENGTH=(R2) RETURNS A(STORAGE) IN R1                    
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                UNSUCCESSFUL STORAGE OBTAIN                  
         MVC   0(8,R1),=C'*PATHTAB'  EYE-CATCHER                                
         LA    R1,8(R1)            BUMP PAST EYE-CATCHER                        
         ST    R1,APATHTAB         A(PATHNAME TABLE)                            
         LR    R6,R1                                                            
*                                                                               
BPX1GCW  EQU   116                 GETCWD                                       
BPX1CHD  EQU   56                  CHDIR                                        
BPX1OPD  EQU   152                 OPENDIR                                      
BPX1RDD  EQU   168                 READDIR                                      
*                                                                               
         SAM31                     SWITCH TO 31-BIT MODE                        
*                                                                               
         L     R4,CVTPTR(,0)       CVT - COMMON VECTOR TABLE                    
         L     R4,CVTCSRT-CVT(,R4) CSRTABLE (CALLABLE SERVICE REQUEST)          
         L     R4,24(R4)           CSR SLOT                                     
*                                                                               
         CLC   PATHNAME,SPACES     WAS THE DIRECTORY OVERRIDDEN?                
         BE    *+12                                                             
         L     RF,PATHNAME_LENGTH  YES: PUT L'DIRECTORY IN RF                   
         B     MAIN10              NAME IS IN FIELD "PATHNAME"                  
*                                                                               
         MVC   BUFFER_LENGTH,=A(L'PATHNAME)                                     
         L     RF,BPX1GCW(R4)      ADDRESS OF THE SERVICE GETCWD                
         CALL  (15),                 GET PATHNAME OF WORKING DIRECTORY +        
               (BUFFER_LENGTH,       INPUT: PATHNAME BUFFER LENGTH     +        
               PATHNAME,             OUTPUT: PATHNAME                  +        
               RETURN_VALUE,         RETURN VALUE: -1 OR L'PATHNAME    +        
               RETURN_CODE,                                            +        
               REASON_CODE),                                           +        
               VL,MF=(E,PLIST)                                                  
         ICM   RF,15,RETURN_VALUE                                               
         BP    MAIN10              RF = PATHNAME LENGTH                         
         L     R7,RETURN_CODE                                                   
         L     R8,REASON_CODE                                                   
         ABEND 1,DUMP                                                           
*                                                                               
MAIN10   DS    0H                                                               
         LR    R5,RF                                                            
         MVC   P(20),=C'WORKING DIRECTORY = '                                   
         BCTR  RF,0                                                             
         EXRL  RF,*+10                                                          
         B     *+10                                                             
         MVC   P+20(0),PATHNAME                                                 
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         LA    RE,PATHNAME(R5)     POINT TO END OF PATHNAME                     
         MVC   0(4,RE),=C'/nfs'    APPEND "/NFS" (LOWER-CASE)                   
*                                                                               
         AHI   R5,4                L' "/nfs"                                    
         ST    R5,PATHNAME_LENGTH                                               
*                                                                               
         L     RF,BPX1CHD(R4)      ADDRESS OF THE SERVICE CHDIR                 
         CALL  (15),                 CHANGE THE WORKING DIRECTORY      +        
               (PATHNAME_LENGTH,     INPUT: PATHNAME LENGTH            +        
               PATHNAME,             INPUT: PATHNAME                   +        
               RETURN_VALUE,         RETURN VALUE: -1 OR 0             +        
               RETURN_CODE,                                            +        
               REASON_CODE),                                           +        
               VL,MF=(E,PLIST)                                                  
         ICM   RF,15,RETURN_VALUE                                               
         BZ    MAIN20                                                           
         L     R7,RETURN_CODE                                                   
         L     R8,REASON_CODE                                                   
         ABEND 2,DUMP                                                           
*                                                                               
MAIN20   DS    0H                                                               
         MVC   P(20),=C'WORKING DIRECTORY = '                                   
         L     RF,PATHNAME_LENGTH                                               
         BCTR  RF,0                                                             
         EXRL  RF,*+10                                                          
         B     *+10                                                             
         MVC   P+20(0),PATHNAME                                                 
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         L     RF,BPX1OPD(R4)      ADDRESS OF THE SERVICE OPENDIR               
         CALL  (15),                 OPEN THE DIRECTORY                +        
               (PATHNAME_LENGTH,     INPUT: DIRECTORY NAME LENGTH      +        
               PATHNAME,             INPUT: DIRECTORY NAME             +        
               RETURN_VALUE,         RETURN VALUE: -1 OR FILE DESCRIPT +        
               RETURN_CODE,                                            +        
               REASON_CODE),                                           +        
               VL,MF=(E,PLIST)                                                  
         L     RF,RETURN_VALUE                                                  
         CHI   RF,-1                                                            
         BNE   MAIN30                                                           
         L     R7,RETURN_CODE                                                   
         L     R8,REASON_CODE                                                   
         ABEND 3,DUMP                                                           
*                                                                               
MAIN30   DS    0H                                                               
         ST    RF,DIRECTORY_FILE_DESCRIPTOR                                     
         LA    RF,BUFFER                                                        
         ST    RF,BUFFER_ADDRESS                                                
         MVC   BUFFER_LENGTH,=A(BUFFER_LENGTH_Q)                                
*                                                                               
         MVC   P(29),=C'DIRECTORY OPENED SUCCESSFULLY'                          
         GOTO1 =V(PRINTER)                                                      
         MVI   P,0                                                              
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         CLI   SORTFLAG,C'Y'                                                    
         BNE   MAIN35                                                           
         MVC   P(36),=C'*** PATHNAME LIST WILL BE SORTED ***'                   
         GOTO1 =V(PRINTER)                                                      
         MVC   P(32),=C'UNSORTED PATHNAME TRACE FOLLOWS:'                       
         GOTO1 =V(PRINTER)                                                      
         MVI   P,0                 SKIP A LINE                                  
         GOTO1 =V(PRINTER)                                                      
*                                                                               
MAIN35   DS    0H                                                               
         L     RF,BPX1RDD(R4)      ADDRESS OF THE SERVICE READDIR               
         CALL  (15),                 READ AN ENTRY FROM A DIRECTORY    +        
               (DIRECTORY_FILE_DESCRIPTOR,  DIRECTORY FILE DESCRIPTOR  +        
               BUFFER_ADDRESS,              INPUT: BUFFER ADDRESS      +        
               BUFFER_ALET,                 INPUT: ALWAYS ZERO         +        
               BUFFER_LENGTH,               INPUT: BUFFER LENGTH       +        
               RETURN_VALUE,                RETURN: -1 OR # OF ENTRIES +        
               RETURN_CODE,                                            +        
               REASON_CODE),                                           +        
               VL,MF=(E,PLIST)                                                  
         L     RF,RETURN_VALUE                                                  
         CHI   RF,-1                                                            
         BNE   MAIN40                                                           
         L     R7,RETURN_CODE                                                   
         L     R8,REASON_CODE                                                   
         ABEND 4,DUMP                                                           
*                                                                               
MAIN40   DS    0H                                                               
         LTR   R2,RF               # OF DIRECTORY ENTRIES IN BUFFER             
         BZ    MAIN70              NO MORE DIRECTORY ENTRIES                    
*                                                                               
         LA    R5,BUFFER           R5 -> BPX1RDD BUFFER AND 1ST DIRE            
         USING DIRE,R5                                                          
*                                                                               
MAIN50   DS    0H                                                               
         OC    DIRENTNAML,DIRENTNAML DIRECTORY ENTRY LENGTH IS PRESENT?         
         BZ    MAIN60              NO                                           
         CLC   DIRENTNAML,=H'2'                                                 
         BH    *+12                                                             
         CLI   DIRENTNAME,C'.'     FIRST TWO FILES ARE "." AND ".."             
         BE    MAIN60              IGNORE THESE                                 
*                                                                               
         XC    FILENAME,FILENAME   PREPARE OUTPUT RECORD                        
         L     R1,PATHNAME_LENGTH                                               
         LR    R3,R1               SAVE L'PATHNAME                              
         BCTR  R1,0                                                             
         EXRL  R1,*+10                                                          
         B     *+10                                                             
         MVC   FILENAME(0),PATHNAME                                             
         LA    RF,FILENAME+1(R1)                                                
         MVI   0(RF),C'/'                                                       
         SR    R1,R1                                                            
         ICM   R1,3,DIRENTNAML     R1 = L'FILENAME                              
         LA    R3,1(R1,R3)                                                      
         BCTR  R1,0                1 FOR EXRL INSTRUCTION                       
         EXRL  R1,*+10                                                          
         B     *+10                                                             
         MVC   1(0,RF),DIRENTNAME                                               
         AHI   R3,4                L'RDW                                        
         STCM  R3,3,FILENAME_RDW   PUT VARIABLE RECLEN IN RDW                   
*                                                                               
         CLC   EXTFILT,SPACES      EXTENSION FILTER GIVEN?                      
         BE    MAIN55              NO: KEEP THE FILE                            
         ICM   R1,3,DIRENTNAML     R1 = L'FILENAME                              
         LA    R0,DIRENTNAME       SAVE A(FILENAME)                             
         LA    R1,DIRENTNAME(R1)   BUMP TO END OF NAME                          
         LR    RF,R1                                                            
*                                                                               
         BCTR  R1,0                                                             
         CR    R1,R0               DID WE HIT THE BEGINNING?                    
         BE    MAIN53              YES: NO EXTENSION PRESENT. SKIP FILE         
         CLI   0(R1),C'.'          START OF EXTENSION?                          
         BNE   *-12                NO: BACK UP AND KEEP LOOKING                 
*                                                                               
         SR    RF,R1               RF = L'EXTENSION PLUS '.'                    
         CHI   RF,L'EXTFILT+1      IS THE EXTENSION TOO LONG?                   
         BH    MAIN53              YES: SKIP THE FILE                           
*                                                                               
         LA    RF,WORK                                                          
         MVC   WORK(L'EXTFILT),SPACES                                           
         LHI   R0,L'EXTFILT                                                     
         MVC   0(1,RF),1(R1)       YES: PICK UP THE EXTENSION                   
         LA    RF,1(RF)                                                         
         LA    R1,1(R1)                                                         
         BCT   R0,*-14                                                          
         OC    WORK(L'EXTFILT),SPACES      CONVERT TO UPPERCASE                 
         CLI   EXTFILT_TYPE,C'-'   NEGATIVE FILTER?                             
         BNE   *+18                NO: IT'S A POSITIVE FILTER                   
         CLC   EXTFILT,WORK        MATCH ON NEGATIVE EXTENSION FILTER?          
         BE    MAIN53              YES: SKIP IT                                 
         B     MAIN55              KEEP IT                                      
         CLC   EXTFILT,WORK        MATCH ON EXTENSION FILTER?                   
         BE    MAIN55              YES: KEEP IT                                 
*                                                                               
MAIN53   DS    0H                                                               
         MVC   P+67(11),=C'**SKIPPED**'                                         
         B     MAIN58              SKIP FILE                                    
*                                                                               
MAIN55   DS    0H                                                               
         MVC   0(L'FILENAME+4,R6),FILENAME_RDW                                  
         LA    R6,L'FILENAME+4(R6) BUMP TO NEXT AVAILABLE SLOT                  
         L     R1,FILCOUNT         INCREMENT FILE COUNTER                       
         AHI   R1,1                                                             
         ST    R1,FILCOUNT                                                      
         CLC   FILCOUNT,=AL4(MAXNAMES)                                          
         BNH   *+6                                                              
         DC    H'0'                TABLE FULL: INCREASE MAXNAMES                
*                                                                               
MAIN58   DS    0H                                                               
         SHI   R3,4+1              L'RDW PLUS 1 FOR EXRL INSTRUCTION            
         EXRL  R3,*+10                                                          
         B     *+10                                                             
         MVC   P(0),FILENAME                                                    
         GOTO1 =V(PRINTER)                                                      
*                                                                               
MAIN60   DS    0H                                                               
         SR    R0,R0                                                            
         ICM   R0,3,DIRENTLEN      R0 = LENGTH OF THIS ENTRY                    
         AR    R5,R0               R5 -> NEXT DIRE IN BUFFER                    
         BCT   R2,MAIN50                                                        
         DROP  R5                                                               
*                                                                               
         B     MAIN35              FILL UP DIRECTORY BUFFER AGAIN               
*                                                                               
MAIN70   DS    0H                                                               
         SAM24                     RETURN TO 24-BIT MODE                        
*                                                                               
         OPEN  (NFSFILES,OUTPUT)                                                
         ICM   R2,15,FILCOUNT      ANY PATHNAMES FOUND?                         
         BZ    MAIN90              NO                                           
*                                                                               
         CLI   DDSTFLAG,C'Y'       GENERATE FILE OF DD STATEMENTS?              
         BNE   MAIN75                                                           
         OPEN  (DDSTMTS,OUTPUT)    YES: OPEN IT                                 
*                                                                               
MAIN75   DS    0H                                                               
         CLI   SORTFLAG,C'Y'       SORT THE PATHNAMES?                          
         BNE   MAIN80              NO                                           
*                                                                               
         GOTO1 =V(QSORT),DMCB,APATHTAB,(R2),L'FILENAME+4,L'FILENAME,4           
*                                                                               
         MVC   LINE,=PL2'99'       FORCE PAGE EJECT                             
         MVC   P(31),=C'*SORTED* PATHNAME LIST FOLLOWS:'                        
         GOTO1 =V(PRINTER)                                                      
         MVI   P,0                 SKIP A LINE                                  
         GOTO1 =V(PRINTER)                                                      
*                                                                               
MAIN80   DS    0H                                                               
         L     R6,APATHTAB         PUT ALL PATHNAMES TO OUTPUT FILE             
MAIN85   DS    0H                                                               
         PUT   NFSFILES,0(R6)                                                   
*                                                                               
         LH    R3,0(R6)            YES: PRINT THE SORTED LIST                   
         SHI   R3,4+1              L'RDW PLUS 1 FOR EXRL INSTRUCTION            
         CLI   SORTFLAG,C'Y'       SORT THE PATHNAMES?                          
         BNE   MAIN88              NO                                           
         EXRL  R3,*+10                                                          
         B     *+10                                                             
         MVC   P(0),4(R6)          PATHNAME                                     
         GOTO1 =V(PRINTER)                                                      
*                                                                               
MAIN88   DS    0H                                                               
         CLI   DDSTFLAG,C'Y'       GENERATE FILE OF DD STATEMENTS?              
         BNE   MAIN89              NO                                           
         MVC   CARD,=CL80'// DD PATH='''                                        
         EXRL  R3,*+10             R3 WAS SET ABOVE                             
         B     *+10                                                             
         MVC   CARD+12(0),4(R6)                                                 
         LA    RF,CARD(R3)                                                      
         MVC   13(2,RF),=C''','                                                 
         PUT   DDSTMTS,CARD                                                     
         MVC   CARD,=CL80'//            FILEDATA=TEXT,PATHOPTS=ORDONLY'         
         PUT   DDSTMTS,CARD                                                     
*                                                                               
MAIN89   DS    0H                                                               
         LA    R6,L'FILENAME+4(R6)                                              
         BCT   R2,MAIN85                                                        
*                                                                               
MAIN90   DS    0H                                                               
         CLOSE NFSFILES                                                         
         CLI   DDSTFLAG,C'Y'       WE GENERATED FILE OF DD STATEMENTS?          
         BNE   MAIN95                                                           
         CLOSE DDSTMTS             YES: CLOSE IT                                
*                                                                               
MAIN95   DS    0H                                                               
         MVI   P,0                 SKIP A LINE                                  
         GOTO1 =V(PRINTER)                                                      
         MVC   P(23),=C'TOTAL NUMBER OF FILES: '                                
         EDIT  FILCOUNT,(8,P+23),ALIGN=LEFT,ZERO=NOBLANK                        
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         XBASE                                                                  
         EJECT                                                                  
         LTORG                                                                  
         SPACE 3                                                                
NFSFILES DCB   DDNAME=NFSFILES,DSORG=PS,MACRF=PM,LRECL=256,RECFM=VB,   +        
               BLKSIZE=0                                                        
*                                                                               
DDSTMTS  DCB   DDNAME=DDSTMTS,DSORG=PS,MACRF=PM,LRECL=80,RECFM=FB,     +        
               BLKSIZE=0                                                        
         SPACE 3                                                                
DUB                DS D                                                         
DMCB               DS 6F                                                        
CARD               DS CL80                                                      
WORK               DS CL17                                                      
*                                                                               
APATHTAB           DS A            A(PATHNAME TABLE)                            
EXTFILT            DC CL3' '       PATHNAME EXTENSION FILTER                    
EXTFILT_TYPE       DC C' '         DEFAULT IS POSITIVE EXTENSION FILTER         
SORTFLAG           DC C'N'         ASSUME WE'RE NOT SORTING PATHNAMES           
DDSTFLAG           DC C'N'         GENERATE FILE OF DD STATEMENTS: Y/N          
FILCOUNT           DC F'0'         PATHNAME COUNTER                             
MAXNAMES           EQU 5000        MAXIMUM NUMBER OF PATHNAMES                  
*                                                                               
PLIST              DS 13A          CALL PARMLIST WORK AREA                      
*                                                                               
FILENAME_RDW       DC F'0'                                                      
FILENAME           DS CL252        SO LRECL DOESN'T EXCEED 256                  
*                                                                               
RETURN_CODE        DS F            RETURN CODE (ERRNO)                          
REASON_CODE        DS F            REASON CODE                                  
RETURN_VALUE       DS F            RETURN VALUE (0, -1 OR OTHER)                
BUFFER_LENGTH      DS F                                                         
PATHNAME_LENGTH    DS F                                                         
DIRECTORY_FILE_DESCRIPTOR DS F                                                  
PATHNAME           DC CL80' '                                                   
BUFFER_ALET        DC F'0'                                                      
BUFFER_ADDRESS     DS F                                                         
*                                                                               
                   DS 0D                                                        
                   DC C'*BUFFER*'                                               
BUFFER             DS (BUFFER_LENGTH_Q)X                                        
BUFFER_LENGTH_Q    EQU 10000                                                    
         EJECT                                                                  
         BPXYDIRE                                                               
         EJECT                                                                  
* CVT (MACRO)                                                                   
* DDDPRINT                                                                      
         PRINT OFF                                                              
         CVT   DSECT=YES,LIST=YES                                               
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011DDNFSNAMES09/03/15'                                      
         END                                                                    
