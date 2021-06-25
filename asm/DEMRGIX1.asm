*          DATA SET DEMRGIX1   AT LEVEL 001 AS OF 12/09/20                      
*PROCESS USING(WARN(15))                                                        
*PHASE DEMRGX1A                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE DATCON                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
         TITLE 'DEMOS POST-CONVERSION TRACE'                                    
***********************************************************************         
*                                                                     *         
* THIS IS A DFSORT USER EXIT PROGRAM.                                 *         
* SEE IBM'S DFSORT: APPLICATION PROGRAMMING GUIDE FOR DETAILS.        *         
*                                                                     *         
* THIS EXIT IS INVOKED BY ICETOOL PAN SOURCE MODULE DEMRGICE          *         
*                                                                     *         
***********************************************************************         
DEMRGX1  CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         ENTRY E35                 MUST BE "E35" (FOR DFSORT)                   
         ENTRY SSB                                                              
*                                                                               
         REQUS                                                                  
*                                                                               
         USING E35,RF              RF = WHERE WE ARE LOADED                     
E35      SAVE  (14,12),,DEMRGX1    SAVE DFSORT'S REGISTERS                      
         STMH  GR0,GRF,DFSORTHH    SAVE DFSORT'S REGS (HIGH HALVES)             
         DROP  RF                                                               
*                                                                               
*                                  INITIALIZE OUR USUAL RD CHAIN                
         LR    RB,RF               USE RB AS PROGRAM BASE                       
         USING E35,RB                                                           
         L     RE,=V(REGSAVE)      GET OUR SAVE AREA CHAIN                      
         ST    RD,4(,RE)           SAVE BACKWARD POINTER IN OUR AREA            
         ST    RE,8(,RD)                                                        
         LR    RD,RE               SET OUR SAVE AREA                            
*                                                                               
         L     RA,=V(CPRINT)       SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
*                                                                               
         ICM   R5,15,0(R1)         A(RECORD PASSED BY DFSORT) OR 0              
         BZ    EOF                                                              
*                                                                               
         CLI   FRSTTIME,C'Y'       FIRST TIME THROUGH?                          
         BNE   CHKQKEY                                                          
*                                                                               
         MVI   FRSTTIME,C'N'       YES                                          
         GOTO1 =V(DATCON),DMCB,(5,0),(X'1E',TODAY14)  TYPE-14 DATE              
         MVC   TITLE,=CL60'NATIONAL DEMOS POST-CONVERSION SORT JOB XXXX+        
               XXXX(JNNNNN)'                                                    
         LA    R2,FULL                                                          
         EXTRACT (2),'S',FIELDS=(ASID)                                          
         LA    R1,FULL                                                          
         L     R2,0(R1)                                                         
         LOCASCB ASID=(R2)         GET ASCB ADDRESS INTO R1                     
         L     R2,ASCBASSB-ASCB(R1) R2 = A(ASSB)                                
         SAM31 ,                   SWITCH TO 31-BIT MODE                        
         L     R1,ASSBJSAB-ASSB(R2) R1 = A(JSAB)                                
         MVC   TITLE+50(5),JSABJBID-JSAB+3(R1)  JOB#####                        
         SAM24 ,                   SWITCH BACK TO 24-BIT MODE                   
         LA    R2,FULL                                                          
         EXTRACT (2),FIELDS=TIOT                                                
         L     R2,FULL                                                          
         MVC   TITLE+40(8),0(R2)   JOBNAME                                      
*                                                                               
CHKQKEY  DS    0H                                                               
         USING PMKEY,R5                                                         
         CLI   PMCODE,PMCODEQU     "Q" RECORD?                                  
         BNE   CHKZKEY                                                          
*                                                                               
         MVC   P(6),=C'QKEY: '                                                  
         MVC   P+6(1),PMCODE       "Q"                                          
         MVC   P+7(1),PMMEDIA      MEDIA                                        
         MVC   P+8(1),PMSRC        SOURCE                                       
         MVC   P+10(5),PMSTAT      STATION                                      
         MVC   P+16(2),=C'20'      *Y2K* ASSUME 21ST CENTURY                    
         EDIT  (B1,PMBOOK),(2,P+18),FILL=0 BOOK YEAR                            
         MVI   P+20,C'/'                                                        
         EDIT  (B1,PMBOOK+1),(2,P+21),FILL=0 BOOK WEEK                          
         MVC   P+24(1),PMSTYP      STATION TYPE                                 
         MVC   P+26(1),PMBTYP      BOOK TYPE                                    
         GOTO1 =V(HEXOUT),DMCB,0(R5),P+63,L'PMKMAJOR,=C'TOG'                    
         OC    DMCB+16(4),DMCB+16                                               
         JZ    *+2                                                              
         GOTO1 =V(PRINTER)                                                      
         B     DELREC                                                           
         DROP  R5                                                               
*                                                                               
CHKZKEY  DS    0H                                                               
         USING PZKEY,R5                                                         
         CLI   PZCODE,PZCODEQU     "Z" PASSIVE?                                 
         BNE   CHKJKEY                                                          
         CLI   PZMEDIA,C'N'        LOOKING FOR ZNN KEYS...                      
         BNE   DELREC                                                           
         CLI   PZSRC,C'N'                                                       
         BNE   DELREC                                                           
*                                                                               
         CLC   PZUPLDAT,TODAY14    ...THAT WERE UPDATED TODAY...                
         BNE   *+14                                                             
         MVC   P+55(7),=C'UPDATED'                                              
         B     *+20                                                             
         CLC   PZUPFDAT,TODAY14    ...OR ADDED TODAY                            
         BNE   DELREC                                                           
         MVC   P+55(5),=C'ADDED'                                                
         MVC   P(6),=C'ZKEY: '                                                  
         B     PRTZKEY                                                          
         DROP  R5                                                               
*                                                                               
CHKJKEY  DS    0H                                                               
         USING PJKEY,R5                                                         
         CLI   PJCODE,PJCODEQU     "J" RECORD?                                  
         BNE   DELREC                                                           
         CLI   PJMEDIA,C'N'        LOOKING FOR JNNPPPP KEYS                     
         BNE   DELREC                                                           
         CLI   PJSRC,C'N'                                                       
         BNE   DELREC                                                           
         CLC   =C'PPPP',PJSTAT                                                  
         BNE   DELREC                                                           
         OC    PJBOOK,PJBOOK       NO BOOK = BITMAP "J" KEYS                    
         BNZ   DELREC                                                           
         MVC   P(6),=C'JKEY: '                                                  
*                                                                               
PRTZKEY  DS    0H                                                               
         USING PZKEY,R5                                                         
*                                                                               
* THE "J" AND "Z" KEYS ARE STRUCTURED ALMOST IDENTICALLY. THAT'S WHY            
* WE CAN USE THE SAME CODE HERE TO TRACE BOTH FLAVORS IN SYSPRINT               
* (I.E., THAT'S WHY WE CAN USE THE PZKEY DSECT FOR PJKEY).                      
*                                                                               
         MVC   P+6(1),PZCODE                                                    
         MVC   P+7(1),PZMEDIA                                                   
         MVC   P+8(1),PZSRC                                                     
         MVC   P+9(5),PZSTAT                                                    
         MVC   P+15(10),=C'NIELSEN#: '                                          
         GOTO1 =V(HEXOUT),DMCB,PZEXTNUM,P+25,L'PZEXTNUM,=C'TOG'                 
         OC    16(4,R1),16(R1)                                                  
         JZ    *+2                                                              
         MVC   P+38(10),=C'DDS NTI#: '                                          
         SR    R1,R1                                                            
         ICM   R1,B'0011',PZINTNUM                                              
         EDIT  (R1),(5,P+48),ZERO=NOBLANK                                       
         GOTO1 =V(HEXOUT),DMCB,0(R5),P+63,L'PZKMAJOR,=C'TOG'                    
         OC    DMCB+16(4),DMCB+16                                               
         JZ    *+2                                                              
         GOTO1 =V(PRINTER)                                                      
         DROP  R5                                                               
         B     DELREC                                                           
*                                                                               
KEEPREC  DS    0H                                                               
         SGR   GRF,GRF             SET RC=0: KEEP RECORD                        
         SGR   GR1,GR1                                                          
         LR    R1,R5               SET RECORD POINTER                           
         B     GOBACK                                                           
*                                                                               
DELREC   DS    0H                                                               
         LGHI  GRF,4               SET RC=4: DELETE RECORD                      
         B     GOBACK                                                           
*                                                                               
ADDREC   DS    0H                                                               
         LGHI  GRF,12              SET RC=12: INSERT RECORD                     
         SGR   GR1,GR1                                                          
*********LA    R1,NEWREC           SET RECORD POINTER                           
         B     GOBACK                                                           
*                                                                               
ERROR    DS    0H                                                               
         LGHI  GRF,16              DFSORT WILL TERMINATE WITH RC=16             
         B     GOBACK                                                           
*                                                                               
EOF      DS    0H                                                               
         GOTO1 =V(PRINT),DMCB,=C'CLOSE' CLOSE SYSPRINT (PREVENT SC03)           
         LGHI  GRF,8               SET RC=8:EOF                                 
*                                                                               
GOBACK   DS    0H                                                               
         LMH   GR0,GR0,DFSORTHH    RESTORE DFSORT'S HIGH HALVES                 
         LMH   GR2,GRE,DFSORTHH+8                                               
         L     RD,4(,RD)                                                        
         L     RE,12(,RD)                                                       
         LM    R2,RC,28(RD)        RESTORE DFSORT'S REGS                        
         BSM   0,RE                RETURN TO DFSORT                             
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
         ORG   DEMRGX1+(((*-DEMRGX1)/256)+1)*256  FOR I-CACHE PIPELINE          
*                                                                               
DUB      DS    D                                                                
FULL     DS    F                                                                
DFSORTHH DS    16F                 HIGH HALVES OF DFSORT'S REGISTERS            
DMCB     DS    6F                                                               
TODAY14  DS    XL2                 TODAY'S DATE (TYPE 14)                       
FRSTTIME DC    C'Y'                                                             
WORK     DS    CL17                                                             
*                                                                               
         DS    0D                                                               
         DC    CL16'******SSB*******'                                           
* ++INCLUDE FASSBOFF                                                            
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
         ORG   SSOOFF                                                           
SSB      DC    XL(SSOOFFX-SSOOFF)'00'                                           
         ORG   SSOXTND                                                          
         DC    X'FF'               SET EXTENDED OFFLINE SSB                     
         ORG   SSOMTIND            SYSTEM DATAMGR FLAGS                         
         DC    AL1(SSOWRTN)        WRITE=NO (DON'T OPEN FOR UPDATE)             
         ORG                                                                    
*                                                                               
         EJECT                                                                  
         PRINT OFF                                                              
*                                                                               
         IHAASCB                                                                
         IHAASSB                                                                
         IAZJSAB                                                                
*                                                                               
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
       ++INCLUDE DEDEMFILE                                                      
         PRINT ON                                                               
         SPACE 3                                                                
* REDEFINE THE LAST 5 BYTES OF THE "Z" EXTENDED PASSIVE KEY. THIS IS            
* HOW THE RECORDS APPEAR IN THE CONVERSION OUTPUT FILE.                         
*                                                                               
PZKEY    DSECT                                                                  
         ORG   PZSTATUS                                                         
PZUPFDAT DS    XP(DT14)L2          FIRST LOAD DATE FOR THIS NIELSEN#            
PZUPLDAT DS    XP(DT14)L2          LATEST LOAD DATE FOR THIS NIELSEN#           
PZUPSTAT DS    X                   STATUS BYTE FOR UPDATE INPUT                 
         ORG                                                                    
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001DEMRGIX1  12/09/20'                                      
         END                                                                    
