*          DATA SET CTCONVPROF AT LEVEL 123 AS OF 02/27/98                      
*PHASE CONVPROF                                                                 
*INCLUDE DMDMGRL                                                                
*INCLUDE DMUTLCT                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTERL                                                               
*INCLUDE HEXOUT                                                                 
*INCLUDE FATABOFF                                                               
*INCLUDE LOADER                                                                 
         TITLE 'CONVPROF - DELETE UNWANTED USER PROFILE RECORDS'                
CONVPROF CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE WORKX-WORKD,**CONV**,RA,WORK=A(WORKC),CLEAR=YES                  
         USING WORKD,RC            RC=A(GLOBAL W/S)                             
*                                                                               
         L     R9,VCPRINT          INITIALISE PRINTER                           
         USING DPRINT,R9                                                        
         MVC   TITLE(23),=C'CONTROL FILE CONVERSION'                            
         GOTO1 VPRINTER                                                         
*                                                                               
         BAS   RE,INIT             OPEN FILES ECT                               
*                                                                               
         LA    RF,LIST             USING LIST RATHER THAN TSAR?                 
         CLI   0(RF),255                                                        
         BNE   *+8                                                              
         BAS   RE,READ             READ ID/AUTH RECORDS                         
*                                                                               
         BAS   RE,MAIN             READ ALL RECORDS - DELETE PROFILES           
*                                                                               
XBASE    XBASE                     PROG EXIT                                    
         EJECT                                                                  
*************************************************************                   
*        INITIALISATION / OPEN FILES                        *                   
*************************************************************                   
         SPACE 1                                                                
INIT     NTR1  ,                                                                
         MVI   WORK+00,C'N'        OPEN CONTROL FILE                            
         MVC   WORK+01(7),CTFILE                                                
         MVI   WORK+08,C'X'                                                     
         GOTO1 ADATAMGR,DMCB,DMOPEN,CONTROL,WORK,IO                             
*                                                                               
         OPEN  (TAPEOUT,OUTPUT)    OPEN TAPEOUT                                 
*                                                                               
         LA    RF,LIST             USING LIST RATHER THAN TSAR?                 
         CLI   0(RF),255                                                        
         BE    INIT02                                                           
         XIT1  ,                                                                
*                                                                               
INIT02   L     R0,LOCLEN           LENGTH OF BUFFER SET HERE                    
         GETMAIN RU,LV=(0)                                                      
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ST    R1,LOCBUFF          R1 HOLDS BUFFER ADDRESS                      
         ST    R1,LOCLEN           R0 HOLDS BUFFER LENGTH                       
*                                                                               
         GOTO1 VLOADER,PLIST,=CL8'T00A7D'                                       
         ICM   RF,15,4(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    RF,ATSAR            LOAD TSAROFF                                 
*                                                                               
         USING TSARD,TSBUFF                                                     
         XC    TSBUFF,TSBUFF                                                    
         MVI   TSKEYL,3            SET LENGTH OF KEY                            
         MVC   TSRECL,=Y(4)        SET DUMMY MAX REC LENGTH                     
         MVC   TSABUF,LOCBUFF      SET A(BUFFER)                                
         MVC   TSAREC,LOCLEN       SET LENGTH OF BUFFER                         
         MVI   TSOFFACT,TSAINI     SET INITIALISE                               
         OI    TSIND2,TSI2MANY     SET FOR MANY RECORDS                         
         GOTO1 ATSAR,TSBUFF                                                     
         XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* BUILD TSAR BUFFER OF ID AND AUTH RECORDS                            *         
***********************************************************************         
         SPACE 1                                                                
READ     NTR1  ,                                                                
         LA    R2,IO                                                            
         USING CTIKEY,R2                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         GOTO1 ADATAMGR,DMCB,DMRDHI,CTFILE,CTIKEY,CTIKEY                        
         B     READ04                                                           
*                                                                               
READ02   GOTO1 ADATAMGR,DMCB,DMRSEQ,CTFILE,CTIKEY,CTIKEY                        
*                                                                               
READ04   CLI   8(R1),0             THESE SHOULD NOT FAIL                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   CTIKTYP,CTIKTYPQ    STILL ID RECORDS?                            
         BNE   READ06              NO                                           
         OC    CTIKID(8),CTIKID    ONLY ADD ID NUMBERS                          
         BNZ   READ02                                                           
*                                                                               
         XC    TSREC,TSREC                                                      
         MVI   TSREC,C'I'                                                       
         MVC   TSREC+1(2),CTIKNUM                                               
         LA    RF,TSREC                                                         
         ST    RF,TSAREC                                                        
         MVI   TSOFFACT,TSAADD                                                  
         GOTO1 ATSAR,TSBUFF                                                     
         CLI   TSERRS,0                                                         
         BE    READ02                                                           
         DC    H'0'                                                             
*                                                                               
READ06   LA    R2,IO               NOW READ ALL ACCESS RECORDS                  
         USING CT5KEY,R2                                                        
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ                                                 
         GOTO1 ADATAMGR,DMCB,DMRDHI,CTFILE,CT5KEY,CT5KEY                        
         B     READ10                                                           
*                                                                               
READ08   GOTO1 ADATAMGR,DMCB,DMRSEQ,CTFILE,CT5KEY,CT5KEY                        
*                                                                               
READ10   CLI   8(R1),0             THESE SHOULD NOT FAIL                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   CT5KTYP,CT5KTYPQ    STILL ACCESS RECORDS?                        
         BNE   READX               NO                                           
*                                                                               
         XC    TSREC,TSREC                                                      
         MVI   TSREC,C'A'                                                       
         MVC   TSREC+1(2),CT5KALPH                                              
         LA    RF,TSREC                                                         
         ST    RF,TSAREC                                                        
         MVI   TSOFFACT,TSAADD                                                  
         GOTO1 ATSAR,TSBUFF                                                     
         CLI   TSERRS,0                                                         
         BE    READ08                                                           
         DC    H'0'                                                             
*                                                                               
READX    XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* MAIN PROGRAM LOOP                                                   *         
***********************************************************************         
         SPACE 1                                                                
MAIN     NTR1  ,                                                                
         LA    R2,IO                                                            
         USING CTUKEY,R2                                                        
         XC    CTUKEY,CTUKEY                                                    
         GOTO1 ADATAMGR,DMCB,DMRDHI,CTFILE,CTUKEY,CTUKEY                        
         B     MAIN04                                                           
*                                                                               
MAIN02   GOTO1 ADATAMGR,DMCB,DMRSEQ,CTFILE,CTUKEY,CTUKEY                        
*                                                                               
MAIN04   CLI   8(R1),0                                                          
         BE    MAIN06                                                           
         TM    8(R1),X'80'         TEST FOR EOF                                 
         BO    MAINX                                                            
         DC    H'0'                OTHER ERRORS ARE DEADLY                      
*                                                                               
MAIN06   XR    RE,RE                                                            
         ICM   RE,3,CTULEN         GET RECORD LEN                               
         LA    RE,4(RE)                                                         
         SLL   RE,16                                                            
         STCM  RE,15,IOL           SET TAPE IO LEN                              
*                                                                               
         CLI   CTUKTYP,CTUKTYPQ    ONLY USER PROFILE RECORDS                    
         BNE   MAIN10                                                           
         OC    CTUKAGY,CTUKAGY     KEEP FIELD RECORDS                           
         BZ    MAIN10                                                           
*                                                                               
         LA    RF,LIST                                                          
         CLI   0(RF),255           USING LIST?                                  
         BE    MAIN07              NO                                           
*                                                                               
MAIN06A  CLI   0(RF),255           IN DROP LIST?                                
         BE    MAIN10              NO - KEEP IT                                 
         CLC   CTUKAGY,0(RF)                                                    
         BE    MAIN12              DROP RECORD                                  
         LA    RF,L'CTUKAGY(RF)                                                 
         B     MAIN06A                                                          
*                                                                               
MAIN07   XC    TSREC,TSREC                                                      
         MVI   TSREC,C'A'                                                       
         MVC   TSREC+1(2),CTUKAGY                                               
         MVC   TSSREC,TSREC        SAVE RECORD                                  
         LA    RF,TSREC                                                         
         ST    RF,TSAREC                                                        
         MVI   TSOFFACT,TSARDH                                                  
         GOTO1 ATSAR,TSBUFF                                                     
         CLI   TSERRS,0            TSAR ERROR - IGNORE IT                       
         BNE   MAIN08                                                           
         CLC   TSSREC(3),TSREC                                                  
         BE    MAIN10              KEEP IT - ACCESS RECORD ON FILE              
*                                                                               
MAIN08   XC    TSREC,TSREC                                                      
         MVI   TSREC,C'I'                                                       
         MVC   TSREC+1(2),CTUKAGY                                               
         MVC   TSSREC,TSREC        SAVE RECORD                                  
         LA    RF,TSREC                                                         
         ST    RF,TSAREC                                                        
         MVI   TSOFFACT,TSARDH                                                  
         GOTO1 ATSAR,TSBUFF                                                     
         CLI   TSERRS,0                                                         
         BNE   MAIN12              READ ERROR - LOSE RECORD                     
         CLC   TSSREC(3),TSREC                                                  
         BNE   MAIN12              KEEP IT - ACCESS RECORD ON FILE              
*                                                                               
MAIN10   PUT   TAPEOUT,IOL         PUT TO TAPE                                  
         B     MAIN02                                                           
*                                                                               
MAIN12   BAS   RE,PLINE            PRINT DETAILS OF DROPPED RECORD              
         B     MAIN02                                                           
*                                                                               
MAINX    CLOSE (TAPEOUT)           CLOSE TAPE & EXIT                            
         XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* PRINT DETAILS OF DROPPED RECORD                                     *         
***********************************************************************         
         SPACE 1                                                                
PLINE    NTR1  ,                                                                
         MVC   P,SPACES                                                         
         USING CTUREC,R2                                                        
*                                                                               
         CLC   CTUKAGY,=XL2'5000'                                               
         BH    PLINE02                                                          
         MVC   P(6),=CL6'Usrid='                                                
         EDIT  (B2,CTUKUID),(6,P+6),ALIGN=LEFT                                  
         B     PLINE04                                                          
*                                                                               
PLINE02  MVC   P(6),=CL6'Alpha='                                                
         MVC   P+6(2),CTUKAGY                                                   
*                                                                               
PLINE04  LA    R6,P+15                                                          
         MVC   0(7,R6),=C'System='                                              
         MVC   7(L'CTUKSYS,R6),CTUKSYS                                          
         CLI   CTUKSYS,C'A'                                                     
         BNE   *+10                                                             
         MVC   7(7,R6),=CL7'Account'                                            
         CLI   CTUKSYS,C'M'                                                     
         BNE   *+10                                                             
         MVC   7(7,R6),=CL7'Media'                                              
         LA    R6,14(R6)                                                        
         CLI   0(R6),C' '                                                       
         BH    *+8                                                              
         BCT   R6,*-8                                                           
         MVI   1(R6),C','                                                       
         LA    R6,3(R6)                                                         
*                                                                               
         MVC   0(8,R6),=C'Program='                                             
         MVC   8(L'CTUKPROG,R6),CTUKPROG                                        
         CLI   CTUKPROG,C' '                                                    
         BH    PLINE06                                                          
         MVC   0(8+L'CTUKPROG,R6),SPACES                                        
         MVC   0(7,R6),=C'Report='                                              
         MVC   7(L'CTUKPROG-1,R6),CTUKPROG+1                                    
*                                                                               
PLINE06  LA    R6,8+L'CTUKPROG(R6)                                              
         CLI   0(R6),C' '                                                       
         BH    *+8                                                              
         BCT   R6,*-8                                                           
         MVI   1(R6),C','                                                       
         LA    R6,3(R6)                                                         
*                                                                               
PLINE10  LA    R1,CTUKMED+3                                                     
         OC    0(3,R1),0(R1)       MED/CLI?                                     
         BZ    PLINE12             YES                                          
         LA    R1,1(R1)                                                         
         OC    0(2,R1),0(R1)       ACC?                                         
         BZ    PLINE14             YES                                          
*                                                                               
PLINE12  MVC   0(6,R6),=C'Media='                                               
         MVC   6(L'CTUKMED,R6),CTUKMED                                          
         OC    CTUKMED,CTUKMED                                                  
         BNZ   *+10                                                             
         MVC   6(7,R6),=C'Default'                                              
         LA    R6,13(R6)                                                        
         CLI   0(R6),C' '                                                       
         BH    *+8                                                              
         BCT   R6,*-8                                                           
         MVI   1(R6),C','                                                       
         LA    R6,3(R6)                                                         
*                                                                               
         MVC   0(7,R6),=C'Client='                                              
         MVC   7(L'CTUKCLT,R6),CTUKCLT                                          
         OC    CTUKCLT,CTUKCLT                                                  
         BNZ   PLINE16                                                          
         MVC   7(7,R6),=C'Default'                                              
         B     PLINE16                                                          
         MVC   0(7,R6),=C'Writer='                                              
         MVC   7(6,R6),CTUKNAM                                                  
         B     PLINE16                                                          
*                                                                               
PLINE14  MVC   0(7,R6),=C'AccPak='                                              
         MVC   7(L'CTUKUNT,R6),CTUKUNT                                          
         LA    R6,7+L'CTUKUNT(R6)                                               
         EDIT  (B3,CTUKACT),(6,(R6)),ALIGN=LEFT,ZERO=NOBLANK                    
*                                                                               
PLINE16  GOTO1 VPRINTER                                                         
         XIT1  ,                                                                
         EJECT                                                                  
         LTORG                                                                  
         SPACE 1                                                                
COMFACS  DS    0A                                                               
ADATAMGR DC    V(DATAMGR)                                                       
VCPRINT  DC    V(CPRINT)                                                        
VPRINT   DC    V(PRINT)                                                         
VPRINTER DC    V(PRINTER)                                                       
VLOADER  DC    V(LOADER)                                                        
         SPACE 1                                                                
LIST     DC    C'AC'                                                            
         DC    C'WE'                                                            
         DC    C'CM'                                                            
         DC    C'DB'                                                            
         DC    C'BE'                                                            
         DC    C'SU'                                                            
         DC    C'DE'                                                            
         DC    C'DP'                                                            
         DC    C'HM'                                                            
         DC    C'HC'                                                            
         DC    C'HH'                                                            
         DC    C'JU'                                                            
         DC    C'GY'                                                            
         DC    C'LH'                                                            
         DC    C'LT'                                                            
         DC    C'ME'                                                            
         DC    C'MT'                                                            
         DC    C'I1'                                                            
         DC    C'MB'                                                            
         DC    C'NT'                                                            
         DC    C'NO'                                                            
         DC    C'NW'                                                            
         DC    C'PJ'                                                            
         DC    C'US'                                                            
         DC    C'RU'                                                            
         DC    C'MH'                                                            
         DC    C'TO'                                                            
         DC    C'WC'                                                            
         DC    AL1(255)                                                         
         SPACE 1                                                                
TAPEOUT  DCB   DDNAME=TAPEOUT,DSORG=PS,MACRF=(PM),RECFM=VB,            *        
               BLKSIZE=8200,LRECL=2048,BUFNO=2                                  
         SPACE 1                                                                
DMOPEN   DC    C'OPEN   '                                                       
DMREAD   DC    C'DMREAD '                                                       
DMRSEQ   DC    C'DMRSEQ '                                                       
DMRDHI   DC    C'DMRDHI '                                                       
         SPACE 1                                                                
CONTROL  DC    C'CONTROL'                                                       
CTFILE   DC    C'CTFILE '                                                       
*                                                                               
ATSAR    DS    A                   A(TSAROFF)                                   
LOCBUFF  DC    F'0'                A(GETMAIN BUFFER)                            
LOCLEN   DC    A(1024*1024)        L'GETMAIN BUFFER                             
*                                                                               
         DS    0D                                                               
         DC    CL8'TSARBUFF'                                                    
TSBUFF   DS    CL64                TSAR BUFFER                                  
*                                                                               
         DS    0D                                                               
         DC    CL8'TSARRECD'                                                    
TSREC    DS    CL8                                                              
         DC    CL8'TSSAVREC'                                                    
TSSREC   DS    CL8                                                              
*                                                                               
         EJECT                                                                  
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* DDDPRINTL                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDDPRINTL                                                      
         PRINT ON                                                               
         SPACE 1                                                                
WORKD    DSECT                     ** GLOBAL WORKING STORAGE **                 
*                                                                               
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
FLAGS    DS    X                                                                
*                                                                               
WORK     DS    CL80                                                             
*                                                                               
DMCB     DS    6F                                                               
PLIST    DS    6F                                                               
PARM     DS    6F                                                               
*                                                                               
IOL      DS    F                                                                
IO       DS    2000X                                                            
*                                                                               
WORKX    EQU   *                                                                
         EJECT                                                                  
WORKC    CSECT                     ** WORKING STORAGE POOL **                   
         DS    (64*1024)X                                                       
         SPACE 1                                                                
*FADSECTS                                                                       
       ++INCLUDE FADSECTS                                                       
*DDTSARD                                                                        
       ++INCLUDE DDTSARD                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'123CTCONVPROF02/27/98'                                      
         END                                                                    
