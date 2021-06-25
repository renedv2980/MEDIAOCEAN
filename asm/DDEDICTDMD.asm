*          DATA SET DDEDICTDMD AT LEVEL 019 AS OF 09/28/01                      
*PHASE EDICTDMA EDICTDMD                                                        
*INCLUDE REGSAVE                                                                
*INCLUDE CARDS                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE DATVAL                                                                 
*INCLUDE DMDMGRL                                                                
         TITLE 'EDICTDMP - DUMP EDICT DARE RECORDS TO FLAT FILE'                
EDICTDMP CSECT                                                                  
*                                                                               
         PRINT NOGEN                                                            
         NBASE 0,EDICTDMP,=V(REGSAVE),R9,R8,R7                                  
*                                                                               
         USING EDFILD,R4                                                        
*                                                                               
         ENTRY UTL                                                              
         ENTRY SSB                                                              
*                                                                               
         GOTO1 =V(DADDS),DMCB,DAOPEN,BLOCK,0,EDICTFL,0,0                        
*                                                                               
         GOTO1 =V(DADDS),DMCB,RDID,BLOCK,0,EDICTFL,=X'00010100',0               
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                COULD NOT READ FIRST BLOCK OF FILE           
         LA    R4,BLOCK                                                         
         CLI   EDFMON,EDFMONPQ     IS THIS THE "PERMANENT" RECORD?              
         BE    *+6                                                              
         DC    H'0'                NO -- WHAT RECORD IS THIS?                   
         MVC   TKPERDAY,EDFTKPDY   SAVE NUMBER OF TRACKS/DAY                    
         MVC   BLKPRTRK+1(1),EDFBKPTK PHYSICAL RECORDS PER TRACK                
         MVC   RCPERBLK,EDFRCPBK   SAVE NUMBER OF RECORDS/BLOCK                 
         MVC   EDLRECL,EDFLRECL    SAVE LOGICAL RECORD LENGTH                   
*                                                                               
         OPEN  (FILEOUT,OUTPUT)                                                 
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         EJECT                                                                  
         GOTO1 =V(DATCON),DMCB,(5,0),(10,DATE)  DEFAULT TO TODAY'S DATA         
*                                                                               
READCARD GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   =C'/*',CARD         END-OF-FILE?                                 
         BE    CHKPARMS                                                         
*                                                                               
         CLI   CARD,C'*'           COMMENT CARD?                                
         BE    READCARD            YES                                          
*                                                                               
         CLC   =C'DATE=',CARD                                                   
         BNE   READCARD                                                         
         MVC   DATE(8),CARD+5                                                   
         B     READCARD                                                         
*                                                                               
CHKPARMS GOTO1 =V(DATVAL),DMCB,DATE,YYMMDD                                      
         OC    DMCB(4),DMCB                                                     
         BNZ   *+6                                                              
         DC    H'0'                INVALID DATE= VALUE                          
*                                                                               
         GOTO1 =V(DATCON),DMCB,YYMMDD,(3,YMD)                                   
         ZIC   R2,YMD+2            DAY NUMBER                                   
         BCTR  R2,0                                                             
         MH    R2,TKPERDAY                                                      
         LA    R2,1(R2)            STARTING TRACK NUMBER FOR TODAY              
         LH    R1,TKPERDAY                                                      
         BCTR  R1,0                                                             
         AR    R1,R2                                                            
         STH   R1,LASTTRK          LAST TRACK NUMBER FOR TODAY                  
         EJECT                                                                  
NEXTTRK  STCM  R2,3,DISKADDR       TRACK NUMBER                                 
*                                                                               
         LA    R3,1                                                             
NEXTBLK  STC   R3,DISKADDR+2                                                    
*                                                                               
         GOTO1 =V(DADDS),DMCB,RDID,BLOCK,0,EDICTFL,DISKADDR,0                   
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R5,1                                                             
         LA    R4,BLOCK                                                         
NEXTREC  CLC   EDFMON,YMD+1        IS THIS RECORD FROM THIS MONTH?              
         BNE   CLOSE               NO -- EOF                                    
*                                                                               
         CLI   EDFSTAT,EDFNOOP     IS THIS A NO-OP RECORD?                      
         BE    SKIPIT              YES                                          
         CLI   EDFSYS,EDFDAREQ     IS THIS A DARE RECORD?                       
         BNE   SKIPIT              NO                                           
*                                                                               
         XC    0(14,R4),0(R4)      CLEAR CONTROL DATA                           
         GOTO1 =V(DATCON),DMCB,YYMMDD,(15,4(R4))                                
         PUT   FILEOUT,((4))       WRITE THE DARE RECORD TO FILE                
*                                                                               
SKIPIT   AH    R4,EDLRECL          BUMP TO NEXT RECORD IN BLOCK                 
         LA    R5,1(R5)                                                         
         ZIC   R0,RCPERBLK                                                      
         CR    R5,R0               ANY MORE RECORDS IN THIS BLOCK?              
         BNH   NEXTREC                                                          
*                                                                               
         LA    R3,1(R3)            BUMP BLOCK NUMBER                            
         CH    R3,BLKPRTRK                                                      
         BNH   NEXTBLK                                                          
*                                                                               
         LA    R2,1(R2)            BUMP TRACK NUMBER                            
         CH    R2,LASTTRK                                                       
         BNH   NEXTTRK                                                          
         DC    H'0'                TODAY'S PARTITION IS FULL                    
*                                                                               
CLOSE    CLOSE FILEOUT                                                          
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XBASE                                                                  
         EJECT                                                                  
         LTORG                                                                  
         SPACE 3                                                                
DMCB     DS    8F                                                               
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
UTL      DC    F'0',F'0'                                                        
*                                                                               
         DS    0D                                                               
CARD     DS    CL80                                                             
DATE     DC    CL9' '              TODAY'S DATE (MM/DD/YY)                      
YYMMDD   DS    CL6                 TODAY'S DATE (EBCDIC YYMMDD)                 
YMD      DS    XL3                 TODAY'S DATE (BINARY YMD)                    
JULDATE  DS    XL4                 TODAY'S JULIAN DATE (00YYDDDF)               
BLKPRTRK DC    H'0'                NUMBER OF BLOCKS PER TRACK                   
TKPERDAY DC    H'0'                NUMBER OF TRACKS PER DAY IN FILE             
LASTTRK  DS    H                   LAST TRACK NUM. IN TODAY'S PARTITION         
EDLRECL  DS    H                   LOGICAL RECORD LENGTH                        
RCPERBLK DS    X                   NUMBER OF LOGICAL RECORDS/BLOCK              
DISKADDR DC    XL4'00'             EDICT FILE DISK ADDRESS                      
*                                                                               
EDICTFL  DMDA                                                                   
*                                                                               
FILEOUT  DCB   DDNAME=FILEOUT,DSORG=PS,LRECL=128,RECFM=FB,MACRF=PM              
*                                                                               
         DS    0D                                                               
         DC    C'**I/O***'                                                      
BLOCK    DC    18432X'00'          FIRST FOUR BYTES ARE DISK ADDRESS            
         EJECT                                                                  
       ++INCLUDE DMGREQUS                                                       
         EJECT                                                                  
       ++INCLUDE DDEDICTFIL                                                     
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'019DDEDICTDMD09/28/01'                                      
         END                                                                    
