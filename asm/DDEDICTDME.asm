*          DATA SET DDEDICTDME AT LEVEL 033 AS OF 04/01/03                      
*PHASE EDICTDME EDICTDMP                                                        
*INCLUDE REGSAVE                                                                
*INCLUDE KHDUMMY                                                                
*INCLUDE ADDAY                                                                  
*INCLUDE CARDS                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE DMDMGRL                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE STXITER                                                                
         TITLE 'EDICTDMP - DUMP EDICT FILE RECORDS FOR STATS'                   
EDICTDMP CSECT                                                                  
*                                                                               
         PRINT NOGEN                                                            
         NBASE 0,EDICTDMP,=V(REGSAVE)                                           
*                                                                               
         ENTRY UTL                                                              
         ENTRY SSB                                                              
*                                                                               
         L     R9,=V(CPRINT)       SET FOR REPORT                               
         USING DPRINT,R9                                                        
         MVC   TITLE(28),=C'EDICT TRANSACTION FILE DUMP PROGRAM'                
*                                                                               
         GOTO1 =V(STXITER),DMCB,A(DUMPLIST)                                     
         XC    METHODS,METHODS                                                  
         LA    R2,METHODS                                                       
         LA    R3,METHODSX                                                      
*                                                                               
RDCARD   GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   =C'/*',CARD         END-OF-FILE?                                 
         BE    OPEN                                                             
*                                                                               
         CLC   =C'METHOD=',CARD                                                 
         BNE   *+14                                                             
         MVC   0(1,R2),CARD+7                                                   
         B     RCNEXT                                                           
*                                                                               
         CLC   =C'EASYLINK',CARD                                                
         BNE   *+12                                                             
         MVI   0(R2),C'E'                                                       
         B     RCNEXT                                                           
*                                                                               
         CLC   =C'BDE',CARD                                                     
         BNE   *+12                                                             
         MVI   0(R2),C'T'                                                       
         B     RCNEXT                                                           
*                                                                               
         CLC   =C'BDF',CARD                                                     
         BNE   *+12                                                             
         MVI   0(R2),C'P'                                                       
         B     RCNEXT                                                           
*                                                                               
         CLC   =C'FAXGATE',CARD                                                 
         BNE   *+12                                                             
         MVI   0(R2),C'X'                                                       
         B     RCNEXT                                                           
*                                                                               
         MVC   P(30),=CL30'**** INVALID METHOD ****'                            
         GOTO1 =V(PRINTER)                                                      
         MVC   P(L'CARD),CARD                                                   
         GOTO1 =V(PRINTER)                                                      
         DC    H'0'                                                             
*                                                                               
RCNEXT   AHI   R2,1                                                             
         CR    R2,R3                                                            
         BL    RDCARD                                                           
         MVC   P(30),=CL30'**** TOO MANY METHODS ****'                          
         GOTO1 =V(PRINTER)                                                      
         DC    H'0'                TOO MANY METHODS                             
*                                                                               
OPEN     GOTO1 =V(DADDS),DMCB,DAOPEN,BLOCK,0,EDICTFL,0,0                        
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,=C'DADDS',RDID,BLOCK,0,                +        
               EDICTFL,=X'00010100',0                                           
         OC    12(2,R1),12(R1)                                                  
         BZ    *+6                                                              
         DC    H'0'                DIE ON ANY ERROR                             
*                                                                               
         MVC   BLKSIZE+2(2),DMCB+14  RETURNED PHYSICAL RECORD LENGTH            
         LA    RF,BLOCK            THE FIRST LOGICAL RECORD HAS                 
         USING EDFILD,RF            NUMBER OF TRACKS/DAY                        
         CLI   EDFMON,EDFMONPQ                                                  
         BE    *+6                                                              
         DC    H'0'                WE'RE LOOKING AT THE WRONG RECORD            
         ZIC   R5,EDFBKPTK         PHYSICAL RECORDS PER TRACK                   
         MVC   TKPERDAY,EDFTKPDY   NUMBER OF TRACKS/DAY                         
         MVC   LRECL,EDFLRECL      LOGICAL RECORD LENGTH                        
         MVC   LRECPBK+1(1),EDFRCPBK  LOGICAL RECORDS PER BLOCK                 
         DROP  RF                                                               
*                                                                               
         MVC   P(17),=C'TRACKS PER DAY = '                                      
         EDIT  TKPERDAY,(5,P+17),ALIGN=LEFT                                     
         GOTO1 =V(PRINTER)                                                      
         MVC   P(29),=C'RECORDS (BLOCKS) PER TRACK = '                          
         EDIT  (R5),(3,P+29),ALIGN=LEFT                                         
         GOTO1 =V(PRINTER)                                                      
         MVC   P(24),=C'LOGICAL RECORD LENGTH = '                               
         EDIT  LRECL,(5,P+24),ALIGN=LEFT                                        
         GOTO1 =V(PRINTER)                                                      
         MVC   P(24),=C'LOGICAL RECORDS/BLOCK = '                               
         EDIT  LRECPBK,(5,P+24),ALIGN=LEFT                                      
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         OPEN  (FILEOUT,OUTPUT)                                                 
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         EJECT                                                                  
         GOTO1 =V(DATCON),DMCB,(5,0),(3,YMD)                                    
*                                                                               
         LA    R8,31               DAYS/MONTH                                   
*                                                                               
DATELOOP MVC   P(31),=C'DUMPING RECORDS FOR DAY NUMBER '                        
         EDIT  (B1,YMD+2),(2,P+31),ALIGN=LEFT,ZERO=NOBLANK                      
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         ZIC   R2,YMD+2            DAY NUMBER TO DUMP                           
         BCTR  R2,0                                                             
         MH    R2,TKPERDAY                                                      
         LA    R2,1(R2)            R2 = STARTING TRACK NUMBER. . .              
         LH    RF,TKPERDAY         . . . FOR THE DAY TO DUMP                    
         BCTR  RF,0                                                             
         LA    R1,0(RF,R2)                                                      
         STCM  R1,3,LASTTRK        LAST TRACK NUMBER                            
*                                                                               
TRKLOOP  STCM  R2,3,DSKADR         TRACK NUMBER                                 
*                                                                               
         LA    R3,1                                                             
BLKLOOP  STC   R3,DSKADR+2         BLOCK NUMBER                                 
*                                                                               
         GOTO1 =V(DADDS),DMCB,RDID,BLOCK,0,EDICTFL,DSKADR,0                     
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   9(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R6,1                LOGICAL RECORD NUMBER                        
         LA    R7,BLOCK                                                         
         USING EDFILD,R7                                                        
RECLOOP  CLI   EDFMON,EDFMONPQ     IS THIS A 'PERMANENT' RECORD?                
         BE    SKIPIT                                                           
         CLC   EDFMON,YMD+1        MATCH ON MONTH NUMBER?                       
         BNE   NEXTDAY             NO - NO MORE FOR THIS DAY                    
         CLI   EDFSTAT,EDFNOOP     IS THIS A NO-OP RECORD?                      
         BE    SKIPIT              YES - SKIP IT                                
*                                                                               
*                                  MATCH THE METHOD OF TRANSACTION              
         LA    RE,METHODS                                                       
NXMETH10 CLI   0(RE),0             END OF METHOD LIST                           
         BE    SKIPIT                                                           
         CLC   EDFMETH,0(RE)       CORRECT METHOD OF TRANSMISSION?              
         BE    NXMETHX                                                          
*                                                                               
NXMETH20 AHI   RE,1                                                             
         B     NXMETH10                                                         
NXMETHX  DS    0H                                                               
*                                                                               
         CLI   EDFSYS,X'81'        IS THIS AN EDICT TRANSACTION RECORD?         
         BL    *+12                                                             
         CLI   EDFSYS,X'A9'                                                     
         BNH   SKIPIT              NO - SKIP IT                                 
*                                                                               
         MVI   FREDREC,X'40'                                                    
         MVC   FREDREC+1(L'FREDREC-1),FREDREC                                   
*                                                                               
         MVC   FREDREC,0(R7)                                                    
         PUT   FILEOUT,FREDREC     WRITE THE RECORD TO FILE                     
*                                                                               
         CLI   EDFMETH,C'E'                                                     
         BE    WRT040                                                           
         CLI   EDFMETH,C'X'                                                     
         BNE   PRTLN                                                            
*                                                                               
WRT040   MVI   FREDREC,X'40'                                                    
         MVC   FREDREC+1(L'FREDREC-1),FREDREC                                   
         ZIC   R0,EDFSNTDY                                                      
         SLL   R0,4                                                             
         O     R0,=X'0000000F'                                                  
         XC    DUB,DUB                                                          
         ST    R0,DUB+4                                                         
         UNPK  SENTDAY,DUB                                                      
*                                                                               
         TM    EDFSTAT,EDFSTSNT    WAS REPORT SENT                              
         BNO   NEXT                                                             
         OC    EDFSNTIM,EDFSNTIM                                                
         BNZ   *+14                                                             
         MVC   SENTTIME,=C'2400'   ZERO TIME MEANS MIDNIGHT                     
         B     NEXT                                                             
         GOTO1 =V(HEXOUT),DMCB,EDFSNTIM,SENTTIME,2,=C'TOG'                      
         CLC   =F'4',DMCB+16                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
NEXT     ZIC   R0,EDFRCVDY                                                      
         SLL   R0,4                                                             
         O     R0,=X'0000000F'                                                  
         XC    DUB,DUB                                                          
         ST    R0,DUB+4                                                         
         UNPK  DLVDDAY,DUB                                                      
*                                                                               
         TM    EDFSTAT,EDFSTRCV+EDFSTCAN    WAS REPORT RECEIVED OR CANX         
         BZ    NEXT5                                                            
         OC    EDFRCVTM,EDFRCVTM                                                
         BNZ   *+14                                                             
         MVC   DLVDTIME,=C'2400'   ZERO TIME MEANS MIDNIGHT                     
         B     NEXT5                                                            
         GOTO1 =V(HEXOUT),DMCB,EDFRCVTM,DLVDTIME,2,=C'TOG'                      
         CLC   =F'4',DMCB+16                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
NEXT5    SR    R0,R0                                                            
         ICM   R0,3,EDFPQUID                                                    
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  USERID,DUB                                                       
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,7,EDFEZBOX                                                    
         SLL   R0,4                                                             
         O     R0,=X'0000000F'                                                  
         XC    DUB,DUB                                                          
         ST    R0,DUB+4                                                         
         UNPK  MAILBOX+2(6),DUB                                                 
         MVC   MAILBOX(2),=C'62'                                                
*                                                                               
         GOTO1 =V(HEXOUT),DMCB,EDFSTAT,STATUS,1,=C'TOG'                         
         CLC   =F'2',DMCB+16                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,EDFPAGES                                                    
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PAGES,DUB                                                        
*                                                                               
         MVC   DESTINAT,EDFDEST                                                 
*                                                                               
         MVC   FULL,DSKADR                                                      
         STC   R6,FULL+3                                                        
         GOTO1 =V(HEXOUT),DMCB,FULL,DISKADDR,4,=C'TOG'                          
         CLC   =F'8',DMCB+16                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   SYSTEM,EDFSYS       SYSTEM                                       
         MVC   TYPE,EDFTYPE        REQUEST TYPE                                 
         MVC   DESTTYPE,EDFDSTTY   DESTINATION TYPE                             
         GOTO1 =V(HEXOUT),DMCB,EDFERROR,ERROR,1,=C'TOG'                         
         CLC   =F'2',DMCB+16                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PRTLN    DS    0H                                                               
*                                                                               
         MVC   FULL,DSKADR                                                      
         STC   R6,FULL+3                                                        
         GOTO1 =V(HEXOUT),DMCB,FULL,P,4,=C'TOG'                                 
         CLC   =F'8',DMCB+16                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   P+10(120),FREDREC                                                
         GOTO1 =V(PRINTER)                                                      
*                                                                               
SKIPIT   AH    R7,LRECL            BUMP TO NEXT RECORD                          
         LA    R6,1(R6)                                                         
         CH    R6,LRECPBK          ANY MORE RECORDS IN THIS BLOCK?              
         BNH   RECLOOP                                                          
         DROP  R7                                                               
*                                                                               
         LA    R3,1(R3)                                                         
         CR    R3,R5                                                            
         BNH   BLKLOOP                                                          
*                                                                               
         LA    R2,1(R2)                                                         
         CLM   R2,3,LASTTRK                                                     
         BNH   TRKLOOP                                                          
*                                                                               
NEXTDAY  GOTO1 =V(DATCON),DMCB,(3,YMD),WORK                                     
         GOTO1 =V(ADDAY),DMCB,WORK,WORK+6,F'-1'                                 
         GOTO1 =V(DATCON),DMCB,WORK+6,(3,YMD)                                   
*                                                                               
         BCT   R8,DATELOOP         31 DAYS                                      
*                                                                               
         CLOSE FILEOUT                                                          
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   P(4),=C'DONE'                                                    
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         XBASE                                                                  
         EJECT                                                                  
DUMPLIST DS    0F                                                               
         DC    A(EDICTDMP),V(DUMMY)                                             
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
         SPACE 3                                                                
         LTORG                                                                  
         SPACE 3                                                                
DUB      DS    D                                                                
DMCB     DS    8F                                                               
*                                                                               
UTL      DC    F'0',F'0'                                                        
         SPACE 2                                                                
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
         SPACE 3                                                                
         DS    0D                                                               
WORK     DS    CL17                                                             
CARD     DS    CL80                                                             
FULL     DS    F                                                                
DSKADR   DC    F'0'                TTBR                                         
METHODS  DS    CL20                METHOD OF TRANSMISSION TO REPORT             
METHODSX DC    X'00'                                                            
YMD      DC    XL3'00'             DATE TO DUMP (BINARY YMD)                    
BLKSIZE  DC    A(0)                PHYSICAL BLOCK SIZE                          
LASTTRK  DS    H                   LAST TRACK NUMBER                            
TKPERDAY DS    H                   TRACKS PER DAY                               
LRECL    DS    H                   LOGICAL RECORD LENGTH                        
LRECPBK  DC    H'0'                LOGICAL RECORDS PER BLOCK                    
*                                                                               
FREDREC  DS    CL256                                                            
         ORG   FREDREC                                                          
SENTDAY  DS    CL2                 DD                                           
SENTTIME DS    CL4                 HHMM                                         
DLVDDAY  DS    CL2                 DD                                           
DLVDTIME DS    CL4                 HHMM                                         
STATUS   DS    CL2                 STATUS BYTE                                  
USERID   DS    CL5                 NUMERIC USERID                               
MAILBOX  DS    CL8                 EASYLINK MAILBOX NUMBER                      
DESTINAT DS    CL16                DESTINATION                                  
PAGES    DS    CL5                 NUMBER OF FAXED PAGES                        
DISKADDR DS    CL8                 EDICT FILE DISK ADDRESS                      
         DS    CL1                                                              
SYSTEM   DS    CL1                 SYSTEM                                       
TYPE     DS    CL1                 TYPE                                         
DESTTYPE DS    CL1                 DESTINATION TYPE                             
ERROR    DS    CL1                 ERROR                                        
         ORG                                                                    
*                                                                               
EDICTFL  DMDA                                                                   
*                                                                               
FILEOUT  DCB   DDNAME=FILEOUT,DSORG=PS,MACRF=PM                                 
*                                                                               
         DS    0D                                                               
         DC    C'*BLOCK**'                                                      
BLOCK    DC    18432X'00'                                                       
         EJECT                                                                  
       ++INCLUDE DMGREQUS                                                       
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
       ++INCLUDE DDEDICTFIL                                                     
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'033DDEDICTDME04/01/03'                                      
         END                                                                    
