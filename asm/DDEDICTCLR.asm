*          DATA SET DDEDICTCLR AT LEVEL 007 AS OF 04/02/03                      
*PHASE EDICTCLA EDICTCLR                                                        
*INCLUDE REGSAVE                                                                
*INCLUDE DMDMGRL                                                                
*INCLUDE ADDAY                                                                  
*INCLUDE CARDS                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE NUMVAL                                                                 
*INCLUDE PDUMPER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE STXITER                                                                
*INCLUDE KHDUMMY                                                                
         TITLE 'EDICTCLR - CLEAR RECORDS IN EDICT TRANSACTION FILE'             
EDICTCLR CSECT                                                                  
*                                                                               
         PRINT NOGEN                                                            
         NBASE 0,EDICTCLR,=V(REGSAVE)                                           
*                                                                               
         ENTRY UTL                                                              
         ENTRY SSB                                                              
*                                                                               
         L     R9,=V(CPRINT)       SET FOR REPORT                               
         USING DPRINT,R9                                                        
         MVC   TITLE(28),=C'EDICT TRANSACTION FILE CLEAR PROGRAM'               
*                                                                               
         GOTO1 =V(STXITER),DMCB,A(DUMPLIST)                                     
*                                                                               
READCARD GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   =C'/*',CARD         END-OF-FILE?                                 
         BE    CHKDATE                                                          
*                                                                               
         MVC   P(80),CARD          PRINT EACH PARAMETER CARD                    
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         CLI   CARD,C'*'           COMMENT CARD?                                
         BE    READCARD            YES                                          
*                                                                               
         CLC   =C'ERASEDAY=',CARD                                               
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   =C'ALL',CARD+9                                                   
         BNE   *+16                                                             
         MVI   ERASEALL,C'Y'                                                    
         MVI   YMD+2,1             START CLEARING FROM DAY 1                    
         B     READCARD                                                         
*                                                                               
         GOTO1 =V(NUMVAL),DMCB,CARD+9,(2,0)                                     
         CLI   DMCB,0                                                           
         BNE   INVALID                                                          
         CLC   DMCB+4(4),=F'1'                                                  
         BL    INVALID                                                          
         CLC   DMCB+4(4),=F'31'                                                 
         BH    INVALID                                                          
*                                                                               
         MVC   YMD+2(1),DMCB+7     SAVE DAY NUMBER                              
         B     READCARD                                                         
*                                                                               
INVALID  MVC   P(18),=C'INVALID DATE GIVEN'                                     
         GOTO1 =V(PRINTER)                                                      
         DC    H'0'                INVALID DATE= VALUE                          
*                                                                               
CHKDATE  CLI   YMD+2,0             IF DATE CAME FROM INPUT CARD. . .            
         BNE   NOTTODAY            . . . THEN IT'S THE ACTUAL DATE              
*                                                                               
         MVC   P(39),=C'NO DATE GIVEN: CLEARING TOMORROW''S DATA'               
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         GOTO1 =V(DATCON),DMCB,(5,0),WORK                                       
         GOTO1 =V(ADDAY),DMCB,WORK,WORK+6,F'1'                                  
         GOTO1 =V(DATCON),DMCB,WORK+6,(3,YMD)  TOMORROW'S DATE                  
*                                                                               
NOTTODAY GOTO1 =V(DADDS),DMCB,DAOPEN,BLOCK,0,EDICTFL,0,0                        
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
         GOTO1 =V(PRINTER)                                                      
         EJECT                                                                  
DATELOOP MVC   P(29),=C'CLEARING DATA FOR DAY NUMBER '                          
         EDIT  (B1,YMD+2),(2,P+29),ALIGN=LEFT,ZERO=NOBLANK                      
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         ZIC   R2,YMD+2            DAY NUMBER TO CLEAR                          
         BCTR  R2,0                                                             
         MH    R2,TKPERDAY                                                      
         LA    R2,1(R2)            R2 = STARTING TRACK NUMBER. . .              
         LH    RF,TKPERDAY         . . . FOR THE DAY TO CLEAR                   
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
         LA    RE,BLOCK            WE'LL PROBABLY CLEAR ENTIRE BLOCK            
         L     RF,=F'18432'        LENGTH OF BLOCK                              
         CLC   DSKADR,=X'00010100' BUT FOR VERY FIRST BLOCK, WE MUST...         
         BNE   *+12                ... LEAVE FIRST LOGICAL RECORD ALONE         
         AH    RE,LRECL            RE POINTS TO SECOND LOGICAL RECORD           
         SH    RF,LRECL            RF = L'BLOCK WITHOUT FIRST LOG. REC          
*                                                                               
         XCEFL                                                                  
*                                                                               
         GOTO1 =V(DADDS),DMCB,WTID,BLOCK,BLKSIZE,EDICTFL,DSKADR,0               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   9(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R3,1(R3)                                                         
         CR    R3,R5                                                            
         BNH   BLKLOOP                                                          
*                                                                               
         LA    R2,1(R2)                                                         
         CLM   R2,3,LASTTRK                                                     
         BNH   TRKLOOP                                                          
*                                                                               
         CLI   ERASEALL,C'Y'       ERASE ENTIRE MONTH?                          
         BNE   GOODBYE                                                          
*                                                                               
         ZIC   R1,YMD+2            THE DAY NUMBER WE JUST CLEARED               
         LA    R1,1(R1)            BUMP TO NEXT DAY                             
         STC   R1,YMD+2                                                         
         CLI   YMD+2,31            LAST DAY OF THE MONTH                        
         BNH   DATELOOP                                                         
*                                                                               
GOODBYE  MVC   P(4),=C'DONE'                                                    
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         XBASE                                                                  
         EJECT                                                                  
DUMPLIST DS    0F                                                               
         DC    A(EDICTCLR),V(DUMMY)                                             
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
CARD     DS    CL80                                                             
WORK     DS    CL17                                                             
DSKADR   DC    F'0'                TTBR                                         
YMD      DC    XL3'00'             DATE TO CLEAR (BINARY YMD)                   
BLKSIZE  DC    A(0)                PHYSICAL BLOCK SIZE                          
LASTTRK  DS    H                   LAST TRACK NUMBER                            
TKPERDAY DS    H                   TRACKS PER DAY                               
LRECL    DS    H                   LOGICAL RECORD LENGTH                        
ERASEALL DC    C'N'                'Y' = ERASE ENTIRE MONTH                     
*                                                                               
EDICTFL  DMDA                                                                   
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
**PAN#1  DC    CL21'007DDEDICTCLR04/02/03'                                      
         END                                                                    
