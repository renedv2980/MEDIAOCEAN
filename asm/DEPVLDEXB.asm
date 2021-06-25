*          DATA SET DEPVLDEXB  AT LEVEL 127 AS OF 08/23/00                      
*PHASE DEPVLEBA PVLDEXTB                                                        
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'PVLDEXT - LOAD/DUMP MODEL EXTERNAL ROUTINE'                     
* -------------------------------------------------------------------           
* PARAMETER LIST                                                                
*                                                                               
* P1=A(RECORD)  PASS FIRST BYTE X'00'= INITIALISE                               
*                               X'01'= RECORD IN CORE                           
*                               X'FF'= END OF FILE                              
*               RETURN VALUE    X'00'= KEEP RECORD                              
*                               X'FF'= PURGE RECORD                             
*                               X'FF'/C'EOJ'=PURGE & CAUSE EOJ                  
* P2=A(TAPEOUT) PASS FIRST BYTE X'80'= TAPE INPUT                               
*                               X'40'= TAPE OUTPUT                              
*                               X'20'= RECORD IS I/S FILE RECORD                
* P3=A(PARAM CARD)                                                              
* P4=A(FILE DEFN)                                                               
* P5=A(PRINTER)                                                                 
* P6=A(CPRINT)                                                                  
* -------------------------------------------------------------------           
*                                                                               
         PRINT NOGEN                                                            
PVLDEXT  CSECT                                                                  
         NMOD1 WORKX-WORKD,*PVLDEXT,RR=R5                                       
         USING WORKD,RC                                                         
         EJECT                                                                  
* CONTROL FLOW LOGIC                                                            
*                                                                               
PVXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         SPACE 1                                                                
         L     RE,=V(PRNTBL)                                                    
         AR    RE,R5                                                            
         ST    RE,VPRNTBL                                                       
         SPACE 2                                                                
         CLI   PLIST,X'00'                                                      
         BE    PVINIT              INITIALISE                                   
         CLI   PLIST,X'01'                                                      
         BE    PVXREC              PROCESS                                      
         CLI   PLIST,X'FF'                                                      
         BE    PVXEOF              END-OF-FILE                                  
         B     PVXIT                                                            
         SPACE 2                                                                
PVXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         B     PVXIT                                                            
         SPACE 2                                                                
PVXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         B     PVXIT                                                            
         SPACE 2                                                                
PVINIT   LA    RE,COUNTS                                                        
         L     RF,=F'15000'                                                     
         XCEF                                                                   
*                                                                               
         OPEN  (PNOUT,(OUTPUT))                                                 
*                                                                               
         B     PVXIT                                                            
         SPACE 2                                                                
PVXIT    XMOD1                                                                  
         EJECT                                                                  
*******************************************************************             
*THIS EXTERN COPIES ALL HUTTN RECDS FROM SEP/90 FOWARD                          
*******************************************************************             
PVXREC   DS    0H                                                               
         L     R3,AREC             POINT TO RECORD                              
         USING PMKEY,R3                                                         
*                                                                               
*                                                                               
         CLC   0(3,R3),=C'QWN'     OUTPUT QNN-HUTTN RECS TO TAPE                
         BNE   PVXPURGE                                                         
         MVC   P(100),0(R3)                                                     
         GOTO1 VPRINTER                                                         
*  COMPARE IF KEY THE SAME EXCEPT FOR BOOKTYPE                                  
         CLC   PMKMAJOR(PMSTYP-PMKMAJOR),SVPMKEY                                
         BE    PVXPURGE                                                         
         MVC   SVPMKEY,PMKMAJOR                                                 
*                                                                               
         GOTO1 CREATENP                                                         
*        LA    R4,MYPNKEY                                                       
*        MVC   P(18),0(R4)                                                      
*        GOTO1 VPRINTER                                                         
*        B     PVXKEEP                                                          
         B     PVXPURGE                                                         
*---------------------------------------------------------------------          
PRGTBL   DS    0CL4                                                             
         DC    AL2(9831),X'05',X'01'        SPRT ALL REG 24:TOT->REG            
         DC    AL2(9832),X'05',X'04'        SPRT ALL SPC 24:TOT->SPC            
         DC    AL2(9841),X'05',X'01'        NEWS TOTAL REG:TOT->REG             
         DC    X'FFFF'                                                          
**********************************************************************          
*  CREATE THE N POINTERS                                                        
**********************************************************************          
CREATENP NTR1                                                                   
         XC    MYPNOUT,MYPNOUT                                                  
         LA    R4,MYPNKEY                                                       
         USING PNKEY,R4                                                         
         L     R3,AREC                                                          
         USING PMKEY,R3                                                         
         MVI   PNCODE,PNCODEQU                                                  
         MVC   PNMEDIA,PMMEDIA                                                  
         MVC   PNSRC,PMSRC                                                      
         MVC   PNBOOK,PMBOOK                                                    
         MVC   PNSTAT,PMSTAT                                                    
*                                                                               
         MVC   MYPNSTAT+2(2),=X'FFFF'                                           
*                                                                               
**************************************************                              
*   GET THE '20' ELEM                                                           
         LR    R2,R3               R2 -> RECD                                   
         LA    R2,PMDATA                                                        
CREAT40  CLI   0(R2),X'20'         LOCATE THE '20' ELEM                         
         BE    CREAT50                                                          
         CLI   0(R2),0                                                          
         BE    CREATX              EOF -- JUST KEEP IT                          
         BAS   RE,NEXTEL                                                        
         B     CREAT40             NEXT ELEM                                    
*                                                                               
         USING PHELEM,R2                                                        
CREAT50  DS    0H                                                               
*                                                                               
         ZIC   R7,1(R2)     LENGTH OF 20 ELEM                                   
         BCTR  R7,0                                                             
         EX    R7,*+8                                                           
         B     *+10                                                             
         MVC   P(0),0(R2)                                                       
         GOTO1 VPRINTER                                                         
*                                                                               
         MVC   PNPNUM,PHPNUM       MOVE IN PROGRAM NUMBER                       
         XC    PNDW,PNDW                                                        
         MVC   PNDW,PHDWKS         MOVE IN WEEK INTO LOWER NIBBLE               
         OC    PHDWKS,PHDWKS                                                    
         BZ    *+6                                                              
         DC    H'0'                                                             
********************************************************                        
*   GET THE '22' ELEM                                                           
         LR    R2,R3               R2 -> RECD                                   
         LA    R2,PMDATA                                                        
CREAT70  CLI   0(R2),X'22'         LOCATE THE '22' ELEM                         
         BE    CREAT80                                                          
         CLI   0(R2),0                                                          
         BE    CREATX              EOF -- JUST KEEP IT                          
         BAS   RE,NEXTEL                                                        
         B     CREAT70             NEXT ELEM                                    
*                                                                               
         USING NTELEM,R2                                                        
CREAT80  DS    0H                                                               
         MVC   PNSTIM,NTSQH        MOVE IN START QTR HOUR                       
         MVC   PNBSIND,NTTYPE      MOVE IN INDICATOR BRKOUT/SPCIAL              
         MVC   PNACTDUR,NTDUR     ACTUAL DURATION ????                          
         MVC   PNBSIND,NTTYPE      MOVE IN INDICATOR BRKOUT/SPCIAL              
*                                                                               
         CLI   PMSTYP,C'B'                                                      
         BE    CREAT82                                                          
         CLI   PMSTYP,C'S'                                                      
         BE    CREAT82                                                          
         B     *+10                                                             
CREAT82  MVC   PNBSIND,PMSTYP                                                   
*                                                                               
*  LOOK UP NTDAY TABLE                                                          
         LA    R6,NTDAYTAB                                                      
CREAT85  CLC   =X'FFFF',0(R6)      CANT FIND DAY MUST BE A VAR DAY              
         BE    CREAT200                                                         
         CLC   NTDAY,0(R6)                                                      
         BE    CREAT150                                                         
         LA    R6,L'NTDAYTAB(R6)                                                
         B     CREAT85                                                          
CREAT150 DS    0H                                                               
         MVC   PNACTDAY,1(R6)                                                   
         TM    NTDAY,X'40'                                                      
         BZ    CREAT155                                                         
         NI    PNDW,X'0F'                                                       
         OI    PNDW,X'10'                                                       
*                                                                               
*        LA    R7,L'MYPNKEY                                                     
         LA    R7,L'MYPNOUT                                                     
*        LA    R7,4(R7)        LENGTH +4 BYTE HEADER                            
         STCM  R7,3,MYPLEN                                                      
         PUT   PNOUT,MYPNOUT                                                    
*                                                                               
*                                                                               
         MVC   P(18),0(R4)                                                      
         GOTO1 VPRINTER                                                         
CREAT155 DS    0H                                                               
         TM    NTDAY,X'20'                                                      
         BZ    CREAT160                                                         
         NI    PNDW,X'0F'                                                       
         OI    PNDW,X'20'                                                       
*                                                                               
*        LA    R7,L'MYPNKEY                                                     
         LA    R7,L'MYPNOUT                                                     
*        LA    R7,4(R7)        LENGTH +4 BYTE HEADER                            
         STCM  R7,3,MYPLEN                                                      
         PUT   PNOUT,MYPNOUT                                                    
*                                                                               
         MVC   P(18),0(R4)                                                      
         GOTO1 VPRINTER                                                         
CREAT160 DS    0H                                                               
         TM    NTDAY,X'10'                                                      
         BZ    CREAT165                                                         
         NI    PNDW,X'0F'                                                       
         OI    PNDW,X'30'                                                       
*                                                                               
*        LA    R7,L'MYPNKEY                                                     
         LA    R7,L'MYPNOUT                                                     
*        LA    R7,4(R7)        LENGTH +4 BYTE HEADER                            
         STCM  R7,3,MYPLEN                                                      
         PUT   PNOUT,MYPNOUT                                                    
*                                                                               
         MVC   P(18),0(R4)                                                      
         GOTO1 VPRINTER                                                         
CREAT165 DS    0H                                                               
         TM    NTDAY,X'08'                                                      
         BZ    CREAT170                                                         
         NI    PNDW,X'0F'                                                       
         OI    PNDW,X'40'                                                       
*                                                                               
*        LA    R7,L'MYPNKEY                                                     
         LA    R7,L'MYPNOUT                                                     
*        LA    R7,4(R7)        LENGTH +4 BYTE HEADER                            
         STCM  R7,3,MYPLEN                                                      
         PUT   PNOUT,MYPNOUT                                                    
*                                                                               
         MVC   P(18),0(R4)                                                      
         GOTO1 VPRINTER                                                         
CREAT170 DS    0H                                                               
         TM    NTDAY,X'04'                                                      
         BZ    CREAT175                                                         
         NI    PNDW,X'0F'                                                       
         OI    PNDW,X'50'                                                       
*                                                                               
*        LA    R7,L'MYPNKEY                                                     
         LA    R7,L'MYPNOUT                                                     
*        LA    R7,4(R7)        LENGTH +4 BYTE HEADER                            
         STCM  R7,3,MYPLEN                                                      
         PUT   PNOUT,MYPNOUT                                                    
*                                                                               
         MVC   P(18),0(R4)                                                      
         GOTO1 VPRINTER                                                         
CREAT175 DS    0H                                                               
         TM    NTDAY,X'02'                                                      
         BZ    CREAT180                                                         
         NI    PNDW,X'0F'                                                       
         OI    PNDW,X'60'                                                       
*                                                                               
*        LA    R7,L'MYPNKEY                                                     
         LA    R7,L'MYPNOUT                                                     
*        LA    R7,4(R7)        LENGTH +4 BYTE HEADER                            
         STCM  R7,3,MYPLEN                                                      
         PUT   PNOUT,MYPNOUT                                                    
*                                                                               
         MVC   P(18),0(R4)                                                      
         GOTO1 VPRINTER                                                         
CREAT180 DS    0H                                                               
         TM    NTDAY,X'01'                                                      
         BZ    CREATX                                                           
         NI    PNDW,X'0F'                                                       
         OI    PNDW,X'70'                                                       
*                                                                               
*        LA    R7,L'MYPNKEY                                                     
         LA    R7,L'MYPNOUT                                                     
*        LA    R7,4(R7)        LENGTH +4 BYTE HEADER                            
         STCM  R7,3,MYPLEN                                                      
         PUT   PNOUT,MYPNOUT                                                    
*                                                                               
         MVC   P(18),0(R4)                                                      
         GOTO1 VPRINTER                                                         
CREAT200 DS    0H                                                               
         MVI   PNACTDAY,X'09'                                                   
         MVC   P(18),0(R4)                                                      
         GOTO1 VPRINTER                                                         
*                                                                               
*        B     PVXKEEP                                                          
*                                                                               
CREATX   XIT1                                                                   
**********************************************************************          
*  THIS ROUTINE JUST ADVANCES THE POINTER TO NEXT ELEMENT                       
*ENTRY:   R2 MUST POINT TO START OF ELEMENT IN REC                              
*EXIT :   R2 WILL POINT TO NEXT ELEMENT                                         
**********************************************************************          
NEXTEL   SR    R1,R1                                                            
NEXTEL25 IC    R1,1(R2)            ELEMENT LENGTH                               
         AR    R2,R1               R2 POINTS TO NEXT ELEMENT                    
NEXTELX  BR    RE                                                               
**********************************************************************          
*DELETE RECORD                                                                  
**********************************************************************          
PVXDELET DS    0H                  DELETE KEY                                   
*                                                                               
PVXREC51 LA    RE,COUNTS                                                        
PVXREC5A OC    0(5,RE),0(RE)       OPEN SLOT                                    
         BZ    PVXREC5B                                                         
         CLC   DUB(5),0(RE)        FIND THE SLOT                                
         BNE   *+14                                                             
         CLC   STATN,9(RE)                                                      
         BE    PVXREC5C                                                         
         LA    RE,14(RE)                                                        
         B     PVXREC5A                                                         
PVXREC5B MVC   0(5,RE),DUB         SET KEY IN TABLE                             
         MVC   9(5,RE),STATN                                                    
PVXREC5C SR    RF,RF               ADD UP RECORDS                               
         ICM   RF,15,5(RE)                                                      
         A     RF,=F'1'                                                         
         STCM  RF,15,5(RE)                                                      
         B     PVXPURGE                                                         
         DROP  R3                                                               
         EJECT                                                                  
* END-OF-FILE PROCESSING                                                        
*                                                                               
PVXEOF   MVC   P(21),=C'***RECORDS DELETED***'                                  
         GOTO1 VPRINTER                                                         
         LA    R2,COUNTS                                                        
         CLOSE (PNOUT,)                                                         
PVXEOF1  OC    0(5,R2),0(R2)                                                    
         BZ    PVXIT                                                            
         SR    R9,R9                                                            
         ICM   R9,15,5(R2)                                                      
         MVC   P(3),0(R2)                                                       
         EDIT  (R9),(8,P+10)                                                    
         ZIC   R9,3(R2)                                                         
         EDIT  (R9),(2,P+4)                                                     
         ZIC   R9,4(R2)                                                         
         EDIT  (R9),(2,P+6)                                                     
         MVC   P+20(5),9(R2)                                                    
         GOTO1 VPRINTER                                                         
         LA    R2,14(R2)                                                        
         B     PVXEOF1                                                          
         EJECT                                                                  
******                                                                          
******   NTDAY, PNACTDAY TABLE                                                  
NTDAYTAB DS    0CL2                                                             
         DC    X'7C',AL1(0)                                                     
         DC    X'40',AL1(1)                                                     
         DC    X'20',AL1(2)                                                     
         DC    X'10',AL1(3)                                                     
         DC    X'08',AL1(4)                                                     
         DC    X'04',AL1(5)                                                     
         DC    X'02',AL1(6)                                                     
         DC    X'01',AL1(7)                                                     
         DC    X'7F',AL1(8)                                                     
         DC    X'FFFF'                                                          
*                                                                               
*NOUT    DCB   DDNAME=TAPEOUT,DSORG=PS,RECFM=FB,MACRF=PM,                       
*              LRECL=18,BLKSIZE=27646                                           
*                                                                               
                                                                                
PNOUT    DCB   DDNAME=OTAPE,DSORG=PS,RECFM=VB,LRECL=27,BLKSIZE=5000,   X        
               MACRF=PM                                                         
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
KEEPS    DC    F'0'                                                             
HEAD     DC    C'**RECORD**'                                                    
COUNTS   DS    15000C                                                           
         EJECT                                                                  
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
STATN    DS    CL5                                                              
APARM    DS    A                                                                
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
VDATAMGR DS    A                                                                
         SPACE 1                                                                
VPRNTBL  DS    A                                                                
HALF     DS    H                                                                
ELCODE   DS    CL1                                                              
WORK     DS    CL64                                                             
*                                                                               
MYPNOUT  DS    0CL(L'PNKMAJOR+4+5)                                              
MYPLEN   DS    CL4                                                              
MYPNKEY  DS    CL(L'PNKMAJOR)                                                   
MYPNSTAT DS    CL1                                                              
MYPNDXDA DS    XL4                                                              
*                                                                               
SVPMKEY  DS    CL(L'PMKMAJOR)                                                   
WORKX    EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
       ++INCLUDE DEDEMFILE                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'127DEPVLDEXB 08/23/00'                                      
         END                                                                    
