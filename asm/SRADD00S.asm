*          DATA SET SRADD00S   AT LEVEL 011 AS OF 05/01/02                      
*PHASE T12900A,*                                                                
*INCLUDE QSORT                                                                  
         TITLE '$ADDRESS - LIST SYSTEM CORE ADDRESSES'                          
         PRINT NOGEN                                                            
ADDRESS  CSECT                                                                  
         NMOD1 ADDWRKX-ADDWRK,**$ADD**,CLEAR=YES                                
         USING ADDWRK,RC           RC=A(W/S)                                    
         MVC   SRPARS,0(R1)                                                     
         L     R3,SRPAR6                                                        
         USING SRADDFFD,R3         R3=A(TWA)                                    
         L     R4,SRPAR4                                                        
         USING COMFACSD,R4         R4=A(COMFACS)                                
         L     RA,SRPAR1                                                        
         USING SYSFACD,RA          RA=A(SYSFACS)                                
*                                                                               
         L     R2,VSSB                                                          
         USING SSBD,R2                                                          
         MVC   LOWADD,SSBLOADR     SET LOW CORE ADDRESS AND                     
         MVC   RELADD,SSBLOADR     DEFAULT RELOCATION FACTOR                    
         DROP  R2                                                               
         LA    R2,SRVP2H                                                        
         CLI   5(R2),0                                                          
         BE    AD0                                                              
         TM    4(R2),X'02'                                                      
         BZ    ERR1                                                             
         CLI   5(R2),6                                                          
         BH    ERR2                                                             
         SR    R8,R8               RIGHT ALIGN INTO DUB                         
         IC    R8,5(R2)                                                         
         LA    R9,8                                                             
         SR    R9,R8                                                            
         LA    R9,DUB(R9)                                                       
         MVC   DUB,=8C'0'                                                       
         BCTR  R8,0                                                             
         EX    R8,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R9),8(R2)                                                    
         LA    R1,RELADD                                                        
         ST    R1,DMCB+4                                                        
         GOTO1 CHEXIN,DMCB,DUB,,8                                               
*                                                                               
AD0      DS    0H                                                               
         LA    R2,SRVP1H                                                        
         MVI   DISPIT,X'FF'        DEFAULT DISPLAY IS ALL                       
         CLI   5(R2),0                                                          
         BE    AD1                                                              
         CLI   5(R2),3                                                          
         BNE   ERR3                                                             
         LA    R5,DISPTAB                                                       
*                                                                               
AD0A     CLI   0(R5),X'FF'         CHECK FOR VALID DISPLAY TYPE                 
         BE    ERR3                                                             
         CLC   0(3,R5),8(R2)                                                    
         BE    *+12                                                             
         LA    R5,L'DISPTAB(R5)                                                 
         B     AD0A                                                             
         MVC   DISPIT,8(R5)                                                     
*                                                                               
AD1      DS    0H                                                               
         LA    R9,ADDTAB           R9=A(NEXT TABLE ENTRY)                       
         SR    R2,R2               R2=# ENTRIES IN TABLE                        
         SR    R8,R8                                                            
         LA    R5,BASTAB           R5=A(BASIC TABLE)                            
         EJECT                                                                  
* ADDRESS FROM SYSFAC/COMFAC/SRPARM                                             
*                                                                               
AD2      CLI   0(R5),X'FF'         END OF TABLE                                 
         BE    AD6                                                              
         TM    DISPIT,X'80'        TEST REQUIRED                                
         BZ    AD6                                                              
         EX    R8,0(R5)            GET A(ENTRY) IN R6                           
         LTR   R6,R6                                                            
         BZ    AD4                                                              
         MVC   0(16,R9),4(R5)      MOVE FIELD NAME                              
         ST    R6,16(R9)                                                        
         LA    R9,L'ADDTAB(R9)                                                  
         LA    R2,1(R2)                                                         
*                                                                               
AD4      LA    R5,L'BASTAB(R5)                                                  
         B     AD2                                                              
* ADDRESSES FROM SELIST                                                         
*                                                                               
AD6      L     R5,VSELIST                                                       
         LH    R6,0(R5)                                                         
         L     R7,2(R5)                                                         
         LA    R5,6(R5)                                                         
         USING SELISTD,R5                                                       
         MVI   LSTOVSYS,X'FF'      NE ON FIRST COMPARE                          
         TM    DISPIT,X'40'        TEST REQUIRED                                
         BZ    AD10                                                             
*                                                                               
AD8      MVC   0(16,R9),=C'        PRGMS   '                                    
         MVC   0(7,R9),SENAME                                                   
         MVC   16(4,R9),SEPGMS                                                  
         MVI   16(R9),0            CLEAR HOB                                    
         CLC   LSTOVSYS,SEOVSYS                                                 
         MVC   LSTOVSYS,SEOVSYS                                                 
         BE    *+12                                                             
         LA    R9,L'ADDTAB(R9)                                                  
         LA    R2,1(R2)                                                         
         MVC   0(16,R9),=C'        FILES   '                                    
         MVC   0(7,R9),SENAME                                                   
         MVC   16(4,R9),SEFILES                                                 
         OC    SEFILES,SEFILES                                                  
         BNZ   *+14                                                             
         XC    0(L'ADDTAB,R9),0(R9)                                             
         B     *+12                                                             
         LA    R9,L'ADDTAB(R9)                                                  
         LA    R2,1(R2)                                                         
         BXLE  R5,R6,AD8                                                        
* ADDRESSES FROM TCB                                                            
*                                                                               
AD10     L     R5,VTCB                                                          
         LH    R6,0(R5)                                                         
         L     R7,2(R5)                                                         
         LA    R5,6(R5)                                                         
         USING TCBD,R5                                                          
         TM    DISPIT,X'20'        TEST REQUIRED                                
         BZ    AD14                                                             
*                                                                               
AD12     MVC   0(16,R9),=C'       WRK AREA '                                    
         MVC   0(6,R9),TCBID+1                                                  
         MVC   16(4,R9),TCBWRKA                                                 
         LA    R9,L'ADDTAB(R9)                                                  
         LA    R2,1(R2)                                                         
         MVC   0(16,R9),=C'       PGM AREA '                                    
         MVC   0(6,R9),TCBID+1                                                  
         MVC   16(4,R9),TCBPGMA                                                 
         LA    R9,L'ADDTAB(R9)                                                  
         LA    R2,1(R2)                                                         
         MVC   0(16,R9),=C'       MAP AREA '                                    
         MVC   0(6,R9),TCBID+1                                                  
         MVC   16(4,R9),TCBMAP                                                  
         LA    R9,L'ADDTAB(R9)                                                  
         LA    R2,1(R2)                                                         
         MVC   0(16,R9),=C'       I/O AREA '                                    
         MVC   0(6,R9),TCBID+1                                                  
         MVC   16(4,R9),TCBFILES                                                
         LA    R9,L'ADDTAB(R9)                                                  
         LA    R2,1(R2)                                                         
         MVC   0(16,R9),=C'       TWA AREA '                                    
         MVC   0(6,R9),TCBID+1                                                  
         MVC   16(4,R9),TCBTWA                                                  
         LA    R9,L'ADDTAB(R9)                                                  
         LA    R2,1(R2)                                                         
         BXLE  R5,R6,AD12                                                       
* ADDRESSES FROM SSB                                                            
*                                                                               
AD14     L     R5,VSSB                                                          
         USING SSBD,R5                                                          
         TM    DISPIT,X'80'        TEST REQUIRED                                
         BZ    AD15                                                             
         MVC   0(16,R9),=CL16'HIGH CORE'                                        
         MVC   16(4,R9),SSBHIADR                                                
         LA    R9,L'ADDTAB(R9)                                                  
         LA    R2,1(R2)                                                         
         MVC   0(16,R9),=CL16'JOBTAB'                                           
         MVC   16(4,R9),SSBAJOB                                                 
         LA    R9,L'ADDTAB(R9)                                                  
         LA    R2,1(R2)                                                         
         MVC   0(16,R9),=CL16'CTRYTAB'                                          
         MVC   16(4,R9),SSBACTRY                                                
         LA    R9,L'ADDTAB(R9)                                                  
         LA    R2,1(R2)                                                         
         MVC   0(16,R9),=CL16'LANGTAB'                                          
         MVC   16(4,R9),SSBALANG                                                
         LA    R9,L'ADDTAB(R9)                                                  
         LA    R2,1(R2)                                                         
         MVC   0(16,R9),=CL16'BCTAB'                                            
         MVC   16(4,R9),SSBABC                                                  
         LA    R9,L'ADDTAB(R9)                                                  
         LA    R2,1(R2)                                                         
         MVC   0(16,R9),=CL16'DICTTAB'  TABLE OF DICTIONARY ADDRESSES           
         MVC   16(4,R9),SSBADICT                                                
         LA    R9,L'ADDTAB(R9)                                                  
         LA    R2,1(R2)                                                         
*                                                                               
* SORT ADDTAB INTO EITHER NAME OR CORE ADDRESS ORDER                            
*                                                                               
AD15     LA    R9,ADDTAB                                                        
         CLI   SRVP3H+5,0          TEST SORT ON NAME REQUESTED                  
         BNE   AD20                YES                                          
         GOTO1 =V(QSORT),DMCB,(R9),(R2),24,6,16,RR=RB                           
         B     AD30                                                             
*                                                                               
AD20     GOTO1 =V(QSORT),DMCB,(R9),(R2),24,16,0,RR=RB                           
         ZIC   RE,SRVP3H+5                                                      
         BCTR  RE,0                                                             
AD25     EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R9),SRVP3       FIND FIRST NAME >= START STRING              
         BNL   AD30                WE'VE GOT IT - START DISPLAYING HERE         
         LA    R9,L'ADDTAB(R9)                                                  
         BCT   R2,AD25                                                          
* FORMAT SCREEN                                                                 
*                                                                               
AD30     LA    R2,SRVL1H                                                        
         BAS   RE,FORMAT                                                        
         LA    R2,SRVL2H                                                        
         BAS   RE,FORMAT                                                        
         LA    R2,SRVL3H                                                        
         BAS   RE,FORMAT                                                        
* EXIT                                                                          
*                                                                               
         MVC   SRVMSG(L'MSG),MSG                                                
         FOUT  SRVMSGH                                                          
         OI    SRVIDH+6,X'40'                                                   
         B     EXIT                                                             
*                                                                               
ERR1     MVC   SRVMSG(L'MSG1),MSG1                                              
         B     ERRX                                                             
*                                                                               
ERR2     MVC   SRVMSG(L'MSG2),MSG2                                              
         B     ERRX                                                             
*                                                                               
ERR3     MVC   SRVMSG(L'MSG3),MSG3                                              
         B     ERRX                                                             
*                                                                               
ERRX     FOUT  SRVMSGH                                                          
         NI    SRVIDH+6,X'BF'                                                   
         OI    SRVP2H+6,X'40'                                                   
*                                                                               
EXIT     XMOD1 1                                                                
         EJECT                                                                  
* FORMAT 4 BYTE ADDRESS INTO 6 BYTE OUTPUT                                      
*                                                                               
CVHEX    NTR1                                                                   
         L     R6,DUB              RELOCATE ADCON                               
         S     R6,LOWADD                                                        
         A     R6,RELADD                                                        
         ST    R6,DUB                                                           
         GOTO1 CHEXOUT,DMCB,DUB,WORK,4,=C'TOG'                                  
         MVC   16(8,R9),WORK                                                    
         CLC   16(2,R9),=C'00'                                                  
         BNE   *+10                                                             
         MVC   16(2,R9),=C'  '                                                  
         XIT1                                                                   
* BUILD SCREEN                                                                  
*                                                                               
FORMAT   NTR1                                                                   
         LA    R3,16                                                            
*                                                                               
FORMAT2  MVC   DUB(4),16(R9)                                                    
         CLI   0(R9),0                                                          
         BE    *+8                                                              
         BAS   RE,CVHEX                                                         
         MVC   8(24,R2),0(R9)                                                   
         FOUT  (R2)                                                             
         LA    R2,96(R2)           BUMP 3 LINE ON SCREEN                        
         LA    R9,L'ADDTAB(R9)                                                  
         BCT   R3,FORMAT2                                                       
         XIT1  REGS=(R9)                                                        
*                                                                               
*                                                                               
MSG      DC    C'CORE ADDRESSES DISPLAYED - ENTER SVC REQUEST'                  
MSG1     DC    C'** ERROR ** INVALID HEXADECIMAL'                               
MSG2     DC    C'** ERROR ** FIELD LENGTH EXCEEDS MAXIMUM'                      
MSG3     DC    C'** ERROR ** INVALID INPUT FIELD'                               
*                                                                               
DISPTAB  DS    0CL9                                                             
         DC    C'ALL     ',X'FF'                                                
         DC    C'FAC     ',X'80'                                                
         DC    C'SYS     ',X'40'                                                
         DC    C'TSK     ',X'20'                                                
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* TABLE OF BASIC ADDRESSES FROM SRPARM/SYSFACS/COMFACS                          
* BYTES 0-03=EXECUTABLE LOAD INSTRUCTION                                        
*       4-21=NAME OF A-TYPE                                                     
*                                                                               
         CNOP  0,4                                                              
BASTAB   DS    0CL20                                                            
         L     R6,SRPAR1                                                        
         DC    CL16'SYSFACS'                                                    
         L     R6,SRPAR4                                                        
         DC    CL16'COMFACS'                                                    
         L     R6,VDATAMGR                                                      
         DC    CL16'DATAMGR'                                                    
         L     R6,VDMOD000                                                      
         DC    CL16'DMFILES'                                                    
         L     R6,VDADDS                                                        
         DC    CL16'DADDS'                                                      
         L     R6,VLCWRITE                                                      
         DC    CL16'LCWRITE'                                                    
         L     R6,VSSB                                                          
         DC    CL16'SSB'                                                        
         L     R6,VUTL                                                          
         DC    CL16'UTL'                                                        
         L     R6,VSELIST                                                       
         DC    CL16'SELIST'                                                     
         L     R6,VUPDTAB                                                       
         DC    CL16'UPDATIVE SOONS'                                             
         L     R6,VTCB                                                          
         DC    CL16'TCB'                                                        
         L     R6,VPHLIST                                                       
         DC    CL16'PHLIST'                                                     
         L     R6,VPRQ                                                          
         DC    CL16'PRINTER QUEUES'                                             
         L     R6,VPRQENTS                                                      
         DC    CL16'PRQ ENTRIES   '                                             
         L     R6,VCHKPT2                                                       
         DC    CL16'REQADDR LISTS'                                              
         L     R6,VADRBUFF                                                      
         DC    CL16'ADRBUFF'                                                    
         L     R6,LOWADD                                                        
         DC    CL16'LOW CORE'                                                   
         L     R6,VTSTTAB                                                       
         DC    CL16'TSTTAB'                                                     
         L     R6,VLOCKTAB                                                      
         DC    CL16'LOCKTAB'                                                    
         L     R6,VTICTOCT                                                      
         DC    CL16'TIMER TABLE'                                                
         DC    X'FFFF'                                                          
         EJECT                                                                  
*              DSECT TO COVER W/S                                               
*                                                                               
ADDWRK   DSECT                                                                  
SRPARS   DS    0CL24                                                            
SRPAR1   DS    F                                                                
SRPAR2   DS    F                                                                
SRPAR3   DS    F                                                                
SRPAR4   DS    F                                                                
SRPAR5   DS    F                                                                
SRPAR6   DS    F                                                                
*                                                                               
DMCB     DS    6F                                                               
*                                                                               
DUB      DS    D                                                                
WORK     DS    CL20                                                             
LOWADD   DS    A                                                                
RELADD   DS    A                                                                
LSTOVSYS DS    C                                                                
DISPIT   DS    C                                                                
*                                                                               
ADDTAB   DS    300CL24                                                          
ADDWRKX  EQU   *                                                                
*                                                                               
* DDFLDIND                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDFLDIND                                                       
         PRINT ON                                                               
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
*                                                                               
* FADSECTS                                                                      
       ++INCLUDE FADSECTS                                                       
         EJECT                                                                  
SRADDFFD DSECT                                                                  
         DS    CL64                                                             
* SRADDFFD                                                                      
       ++INCLUDE SRADDFFD                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011SRADD00S  05/01/02'                                      
         END                                                                    
