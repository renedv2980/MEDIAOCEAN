*          DATA SET SPSNV06    AT LEVEL 044 AS OF 01/04/08                      
*PHASE T21006A                                                                  
***********************************************************************         
*                                                                               
*  TITLE: T21006 - TURNAROUND REQUEST                                           
*                                                                               
*  CALLED FROM: INVOICE CONTROLLER (T21000), WHICH CALLS                        
*               DDGENCON (T00A30) WHICH CALLS THIS.                             
*                                                                               
*  CALLS TO:    DATAMGR                                                         
*                                                                               
*  SCREENS:     SPSNVF9 (T210F9) -- REQUEST SCREEN                              
*                                                                               
*  LOCALS: REGISTER USAGE                                                       
*          R0 - WORK                                                            
*          R1 - WORK                                                            
*          R2 - WORK (SCREEN FIELD HEADER)                                      
*          R3 - WORK                                                            
*          R4 - OVERLAY SAVED STORAGE    (MYAREAD)                              
*          R5 - MINIO CONTROL BLOCK      (MINBLKD)                              
*          R6 - MINELEM                                                         
*          R7 - SECOND BASE                                                     
*          R8 - SPOOLD                                                          
*          R9 - SYSD                                                            
*          RA - TWA                                                             
*          RB - FIRST BASE                                                      
*          RC - GEND                                                            
*          RD - SYSTEM                                                          
*          RE - SYSTEM/WORK                                                     
*          RF - SYSTEM/WORK                                                     
*                                                                               
***********************************************************************         
T21006   TITLE 'SPSNV06 - SPOT INVOICE REQUEST OVERLAY'                         
T21006   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T21006*,R7,RR=R3                                              
         L     RC,0(R1)            STANDARD CODING                              
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN + OUR SCREEN                     
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         LA    R4,SYSSPARE         OVERLAY SAVED STORAGE                        
         USING MYAREAD,R4                                                       
         LA    R5,MINBLOCK         MINIO CONTROL BLOCK                          
         USING MINBLKD,R5                                                       
         ST    R3,RELO                                                          
*                                                                               
         MVI   IOOPT,C'Y'          WE DO OUR OWN I/O'S                          
*                                                                               
         BAS   RE,SETPFTBL                                                      
*                                                                               
         L     R1,ACOMFACS                                                      
         MVC   VGLOBBER,CGLOBBER-COMFACSD(R1)                                   
         GOTO1 VGLOBBER,DMCB,=C'PUTD',=C'INV',3,GLVPGM                          
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY?                                
         BE    VKEY                                                             
         CLI   MODE,VALREC         VALIDATE RECORD?                             
         BE    VREC                                                             
*                                                                               
MAINX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* SETUP THE PFKEYS                                                              
***********************************************************************         
SETPFTBL NTR1                                                                   
         SR    R2,R2                                                            
         CLI   PFKEY,0                                                          
         BE    STPF10                                                           
         CLI   PFKEY,12                                                         
         BL    STPF20                                                           
STPF10   LA    R2,PFTABLE                                                       
         OI    CTLRFLG1,CF1NOCLR   DON'T CLEAR APPLICATION STORAGE              
*                                                                               
STPF20   GOTO1 INITIAL,DMCB,(R2)                                                
*                                                                               
         CLI   PFKEY,12                                                         
         BNE   STPFX                                                            
*                                                                               
         LA    R2,SPFTABLE                                                      
         CLI   CALLSP,1            CAN WE GO BACK FROM WHENCE WE CAME?          
         BNE   *+8                 NO                                           
         MVI   PFKEY,24            YES                                          
*                                                                               
         GOTO1 INITIAL,DMCB,(R2)                                                
*                                                                               
STPFX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE KEY                                                              
***********************************************************************         
VKEY     DS    0H                                                               
         MVI   MISCFLG1,0                                                       
*                                                                               
         CLI   CALLSP,1                                                         
         BNE   *+14                                                             
         MVC   REQPFLN(11),=CL11'PF12=Return'                                   
         B     *+10                                                             
         MVC   REQPFLN(11),=CL11'PF12=List'                                     
         OI    REQPFLNH+6,X'80'                                                 
*****                                                                           
* VALIDATE THE MEDIA                                                            
*****                                                                           
VKMED00  DS    0H                                                               
         LA    R2,REQMEDH                                                       
*                                                                               
         TM    4(R2),X'20'                                                      
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1RPCHG+MF1NOREQ                                       
*                                                                               
VKMED10  CLI   5(R2),0                                                          
         BNE   VKMED20                                                          
         GOTO1 VGLOBBER,DMCB,=C'GETF',(R2),,GLVSPMD                             
         CLI   5(R2),0                                                          
         BE    PLSENTER                                                         
*                                                                               
VKMED20  GOTO1 VALIMED                                                          
         MVC   REQMDNM(L'MEDNM),MEDNM       SHOW MEDIA NAME                     
         OI    REQMDNMH+6,X'80'                                                 
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'PUTF',(R2),,GLVSPMD                             
*                                                                               
VKMEDX   OI    4(R2),X'20'                                                      
*****                                                                           
* VALIDATE THE CLIENT                                                           
*****                                                                           
VKCLT00  DS    0H                                                               
         LA    R2,REQCLTH                                                       
*                                                                               
         TM    4(R2),X'20'                                                      
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1RPCHG                                                
*                                                                               
         CLI   5(R2),0                                                          
         BNE   VKCLT10                                                          
         GOTO1 VGLOBBER,DMCB,=C'GETF',(R2),,GLVSPCLT                            
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
*                                                                               
VKCLT10  GOTO1 VALICLT                                                          
         L     R1,AIO1                                                          
         MVC   SVCLTTYP,CPROF-CLTHDR(R1)                                        
*                                                                               
         MVC   REQCLNM,CLTNM       SHOW CLIENT NAME                             
         OI    REQCLNMH+6,X'80'                                                 
*                                                                               
         GOTO1 GETPRFIL,DMCB,=C'S0I2',PROF0I2                                   
*                                                                               
VKCLTX   GOTO1 VGLOBBER,DMCB,=C'PUTF',(R2),,GLVSPCLT                            
         OI    4(R2),X'20'                                                      
*****                                                                           
* VALIDATE THE STATION                                                          
*****                                                                           
VKSTA00  DS    0H                                                               
         LA    R2,REQSTAH                                                       
*                                                                               
         TM    4(R2),X'20'                                                      
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1RPCHG                                                
*                                                                               
         CLI   5(R2),0                                                          
         BNE   VKSTA10                                                          
         GOTO1 VGLOBBER,DMCB,=C'GETF',(R2),,GLVSPSTA                            
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
*                                                                               
VKSTA10  GOTO1 VALISTA                                                          
*                                                                               
         GOTO1 GETFACT,DMCB,0                                                   
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
*                                                                               
         MVC   SVSYSTEM,FAOVSYS                                                 
*                                                                               
         CLI   FAOVSYS,3           NET SYSTEM?                                  
         BNE   VKSTA20                                                          
         DROP  R1                                                               
         GOTO1 GETPRFIL,DMCB,=C'sI2Z',PROFI2Z    !!! lowercase 's' !!!          
         CLI   PROFI2Z+6,C'Y'     READ PROFILES BY SUBMEDIA?                    
         BNE   VKSTA20                                                          
         L     R1,AIO                                                           
         USING STAREC,R1                                                        
         CLI   STYPE,C' '                                                       
         BNH   VKSTA20                                                          
         CLC   QMED,STYPE                                                       
         BE    VKSTA20                                                          
         MVC   SVMEDIA,QMED                                                     
         MVC   QMED,STYPE          GET SUBMEDIA                                 
         GOTO1 GETPRFIL,DMCB,=C'S0I2',PROF0I2                                   
         MVC   QMED,SVMEDIA                                                     
         DROP  R1                                                               
*                                                                               
VKSTA20  DS    0H                                                               
***************                                                                 
* NETWORK NOT ALLOWED SO MATCHING COULD BE SIMPLIFIED                           
***************                                                                 
         CLC   QNTWK,SPACES                                                     
         BH    INVLFLD                                                          
*                                                                               
         MVC   REQSTNM,MKTNM       SHOW STATION NAME                            
         OI    REQSTNMH+6,X'80'                                                 
*                                                                               
VKSTAX   GOTO1 VGLOBBER,DMCB,=C'PUTF',(R2),,GLVSPSTA                            
         OI    4(R2),X'20'                                                      
*                                                                               
VKX      DS    0H                                                               
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'SI2N'                                                 
         NI    WORK,X'BF'          LOWERCASE "S"                                
         MVC   WORK+4(2),AGENCY                                                 
         GOTO1 GETPROF,DMCB,(X'90',WORK),PROFI2N,DATAMGR                        
*                                                                               
         B     XIT                                                              
*                                                                               
***********************************************************************         
* CREATE THE REQUEST                                                            
***********************************************************************         
VREC     DS    0H                                                               
         XC    RQOPTS,RQOPTS                                                    
         MVI   SEQSW,C'N'                                                       
*****                                                                           
* VALIDATE THE NAME                                                             
*****                                                                           
VRNAM00  LA    R2,REQNAMEH                                                      
*                                                                               
         TM    4(R2),X'20'                                                      
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1RPCHG                                                
*                                                                               
         CLI   5(R2),0                                                          
         BE    VRNAMX                                                           
*                                                                               
         MVC   RQNAME,SPACES                                                    
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   RQNAME(0),8(R2)                                                  
         MVI   RQSW,C'Y'                                                        
*                                                                               
VRNAMX   OI    4(R2),X'20'                                                      
*****                                                                           
* VALIDATE THE BOOK                                                             
*****                                                                           
VRBOK00  LA    R2,REQBOOKH                                                      
*                                                                               
         TM    4(R2),X'20'                                                      
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1RPCHG                                                
*                                                                               
         CLI   5(R2),0                                                          
         BE    VRBOKX                                                           
*                                                                               
         MVI   RQSW,C'Y'                                                        
*                                                                               
         CLC   =C'NO',8(R2)        BOOK=NO?                                     
         BNE   VRBOK10                                                          
         CLI   5(R2),2                                                          
         BH    VRBOK10                                                          
         MVI   RQBOOK,X'FF'                                                     
         B     VRBOKX                                                           
*                                                                               
VRBOK10  CLC   =C'ACT',8(R2)       BOOK=ACT?                                    
         BNE   VRBOK20                                                          
         MVC   RQBOOK,=CL6'ACT'                                                 
         B     VRBOKX                                                           
*                                                                               
VRBOK20  GOTO1 DATVAL,DMCB,(2,8(R2)),RQBOOK                                     
         OC    DMCB,DMCB                                                        
         BZ    BKHUTINV            BOOK-HUT INVALID                             
         MVC   RQBOOK+4(2),=2C' '                                               
         CLC   DMCB+3(1),5(R2)     ONLY DATE GIVEN?                             
         BE    VRBOK30             YES                                          
*                                                                               
         LA    RF,8(R2)            LOOK FOR HUT                                 
         A     RF,DMCB                                                          
         MVC   RQBOOK+4(2),1(RF)                                                
         CLC   RQBOOK+4(2),=C'NO'                                               
         BE    VRBOK30                                                          
         OC    RQBOOK+4(2),=2C'0'                                               
         CLC   RQBOOK+4(2),=2C'0'                                               
         BNH   BKHUTINV                                                         
         CLC   RQBOOK+4(2),=C'12'                                               
         BH    BKHUTINV                                                         
*                                                                               
VRBOK30  MVC   WORK(4),RQBOOK                                                   
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(3,WORK)                                    
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),REQMED                                                  
         MVI   KEY+2,C'N'          NIELSEN                                      
         CLI   SVCPROF+3,C'0'                                                   
         BE    VRBOK40                                                          
         CLC   WORK(2),=X'5E01'    JAN94+ ALWAYS NEILSEN                        
*        BNL   VRBOK40                                                          
         BNL  *+8                                                               
         MVI   KEY+2,C'A'          ELSE ARB                                     
*                                                                               
         CLI   COUNTRY,C'C'        US AGENCY?                                   
         BE    VRBOK40             NO, CANADA - SKIP                            
         CLI   REQMED,C'R'                                                      
         BNE   *+8                                                              
         MVI   KEY+2,C'A'          US RADIO STATIONS USE ARB                    
*                                                                               
VRBOK40  MVC   KEY+3(2),WORK                                                    
         XC    KEY+3(2),=X'FFFF'                                                
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'DEMDIR',KEY,AIO,DMWORK                
         TM    DMCB+8,X'FF'                                                     
         BNZ   NORATBOK                                                         
         L     R6,AIO                                                           
         CLC   KEY(5),0(R6)                                                     
         BNE   NORATBOK                                                         
*                                                                               
VRBOKX   OI    4(R2),X'20'                                                      
*****                                                                           
* VALIDATE THE TURNAROUND PRODUCT                                               
*****                                                                           
VRPRD00  DS    0H                                                               
         LA    R2,REQPRODH                                                      
         XC    SVTPGRP,SVTPGRP                                                  
*                                                                               
         TM    4(R2),X'20'                                                      
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1RPCHG                                                
*                                                                               
         CLI   5(R2),0                                                          
         BNE   VRPRD05                                                          
         GOTO1 VGLOBBER,DMCB,=C'GETF',(R2),,GLVSPPRD                            
         CLI   5(R2),0                                                          
         BE    VRPRD80                                                          
         OI    MISCFLG1,MF1RPCHG                                                
*                                                                               
VRPRD05  MVI   RQSW,C'Y'                                                        
*                                                                               
         CLI   5(R2),2                                                          
         BL    INVLFLD                                                          
*                                                                               
         CLC   =C'PGR=',8(R2)      PRODUCT GROUP ENTERED?                       
         BNE   VRPRD06                                                          
         BAS   RE,VALPGRP          YES, VALIDATE PRODUCT GROUP                  
         BNE   INVLFLD                                                          
         B     VRPRD80                                                          
*                                                                               
VRPRD06  MVC   BLOCK(10),SPACES                                                 
         OC    BLOCK(7),8(R2)                                                   
         CLI   BLOCK+2,C'-'                                                     
         BNE   VRPRD10                                                          
         MVI   BLOCK+2,C' '                                                     
         MVI   BLOCK+3,C'-'                                                     
         MVC   BLOCK+4(3),8+3(R2)                                               
         OC    BLOCK+4(3),SPACES                                                
*                                                                               
****VRPRD10  GOTO1 CKPRDCOD,DMCB,BLOCK,SVTPRD                                   
VRPRD10  CLC   =C'ALL',BLOCK       ALL PRODUCTS                                 
         BNE   VRPRD13                                                          
         MVC   SVTPRDN,BLOCK                                                    
         B     VRPRD16                                                          
*                                                                               
VRPRD13  L     R1,ATIOB            SET UP ERROR MESSAGE CURSOR                  
         USING TIOBD,R1                                                         
         OI    TIOBINDS,TIOBSETC                                                
         LR    R0,R2                                                            
         SR    R0,RA                                                            
         STH   R0,TIOBCURD                                                      
         MVI   TIOBCURI,0                                                       
         DROP  R1                                                               
*                                                                               
         MVC   FAKEFLDH,0(R2)                                                   
         MVI   FAKEFLDH+5,3                                                     
         XC    FAKEFLD,FAKEFLD                                                  
         MVC   FAKEFLD(3),BLOCK                                                 
*                                                                               
         LR    R0,R2                                                            
         LA    R2,FAKEFLDH                                                      
         GOTO1 VALIPRD             EXITS ON ERROR BECAUSE OF GOTO1 READ         
         LR    R2,R0                                                            
*                                                                               
         MVC   SVTPRD,BPRD                                                      
         MVC   SVTPRDN,QPRD                                                     
*                                                                               
VRPRD16  CLI   BLOCK+3,X'40'                                                    
         BH    VRPRD20                                                          
         CLC   BLOCK+4(4),BLOCK+3                                               
         BNE   BADPROD2                                                         
         MVI   SVTPRD2,0                                                        
         XC    SVTPRD2N,SVTPRD2N                                                
         B     VRPRD60                                                          
*                                                                               
VRPRD20  CLI   BLOCK+3,C'-'                                                     
         BNE   BADPROD2                                                         
         CLI   BLOCK+7,X'40'                                                    
         BNE   BADPROD2                                                         
*                                                                               
         CLC   =C'YES',BLOCK+4     CHECK 2ND PRD PIG OPTIONS                    
         BE    VRPRD30                                                          
         CLC   =C'ALL',BLOCK+4                                                  
         BE    VRPRD30                                                          
         CLC   =C'NO ',BLOCK+4                                                  
         BNE   VRPRD40                                                          
VRPRD30  MVC   SVTPRD2N,BLOCK+4                                                 
         B     VRPRD60                                                          
*                                                                               
VRPRD40  DS    0H                                                               
*****    GOTO1 CKPRDCOD,DMCB,BLOCK+4,SVTPRD2                                    
*****    BNE   BADPROD                                                          
         XC    FAKEFLD,FAKEFLD                                                  
         MVC   FAKEFLD(3),BLOCK+4                                               
*                                                                               
         L     R1,ATIOB            SET UP ERROR MESSAGE CURSOR                  
         USING TIOBD,R1                                                         
         OI    TIOBINDS,TIOBSETC                                                
         MVI   TIOBCURI,4                                                       
         DROP  R1                                                               
*                                                                               
         LR    R0,R2                                                            
         LA    R2,FAKEFLDH                                                      
         GOTO1 VALIPRD             EXITS ON ERROR BECAUSE OF GOTO1 READ         
         LR    R2,R0                                                            
*                                                                               
         MVC   SVTPRD2,BPRD                                                     
         MVC   SVTPRD2N,QPRD                                                    
*                                                                               
VRPRD60  DS    0H                                                               
         MVC   RQPRD(L'SVTPRDN),SVTPRDN                                         
         MVC   RQPRD+3(L'SVTPRD2N),SVTPRD2N                                     
*                                                                               
VRPRD80  DS    0H                                                               
         CLI   SVSYSTEM,3          NET?                                         
         BE    VRPRD90             YES - GO TO NET PROFILE                      
*                                                                               
* SPOT PROFILE HERE                                                             
*                                                                               
         CLI   SVCLTTYP,C'0'       TPOL CLIENT?                                 
         BNE   VRPRD100            YES, SKIP                                    
         B     VRPRDX                                                           
*                                                                               
* NET PROFILE                                                                   
*                                                                               
VRPRD90  DS    0H                                                               
         CLI   PI2NPOLN,C'P'                                                    
         BE    VRPRD101                                                         
         CLI   PI2NPOLN,C'A'                                                    
         BNE   VRPRDX                                                           
*                                                                               
* MAKE SURE PRODUCT IS 'ALL' OR A SPECIFIC CODE                                 
VRPRD100 DS    0H                                                               
         CLC   RQPRD(3),=C'   '                                                 
         BNH   I2NALLER                                                         
         CLC   RQPRD(3),=C'POL'                                                 
         BE    I2NALLER                                                         
         B     VRPRDX                                                           
*                                                                               
* MAKE SURE PRODUCT IS 'POL'                                                    
VRPRD101 DS    0H                                                               
         CLC   RQPRD(3),=C'POL'                                                 
         BNE   I2NPOLER                                                         
*                                                                               
VRPRDX   OI    4(R2),X'20'                                                      
         L     R1,ATIOB            DON'T SET UP ERROR MESSAGE CURSOR            
         USING TIOBD,R1                                                         
         NI    TIOBINDS,X'FF'-TIOBSETC                                          
         MVI   TIOBCURI,4                                                       
         DROP  R1                                                               
*****                                                                           
* VALIDATE THE TURNAROUND ESTIMATE                                              
*****                                                                           
VREST00  DS    0H                                                               
         LA    R2,REQESTMH                                                      
*                                                                               
         TM    4(R2),X'20'                                                      
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1RPCHG                                                
*                                                                               
         CLI   5(R2),0                                                          
         BNE   VREST05                                                          
         MVI   SVTEST,0            SET OFF ESTIMATE                             
         XC    SVTESTN,SVTESTN                                                  
         MVI   SVTIDNUM,0                                                       
         GOTO1 VGLOBBER,DMCB,=C'GETF',(R2),,GLVSPEST                            
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
*                                                                               
VREST03  OI    MISCFLG1,MF1RPCHG                                                
*        OI    4(R2),X'08'         VALID NUMERIC                                
*                                                                               
VREST05  MVI   RQSW,C'Y'                                                        
*                                                                               
* ESTIMATE PRESENT HERE                                                         
*                                                                               
         CLI   SVSYSTEM,3          NET?                                         
         BE    *+16                YES - GO DIRECTLY TO NET PROFILE             
*                                                                               
* SPOT PROFILE HERE                                                             
         CLI   PI2NESTS,C'Y'                                                    
         BE    VREST06             FORCE TO 'NO'                                
         B     VREST10             PROCESS NORMALLY                             
*                                                                               
* NET PROFILE HERE                                                              
         CLI   PI2NESTN,C'Y'                                                    
         BNE   VREST10             PROCESS NORMALLY                             
*                                                                               
VREST06  CLI   5(R2),X'02'         ONLY ALLOW 'NO' ESTIMATE                     
         BNE   I2NESTER                                                         
         CLC   =C'NO',8(R2)                                                     
         BNE   I2NESTER                                                         
         B     VREST26                                                          
*                                                                               
* NORMAL PROCESSING                                                             
*                                                                               
VREST10  DS    0H                                                               
         TM    4(R2),X'08'         VALID NUMERIC?                               
         BO    VREST30             YES - PROCESS IT                             
*                                                                               
VREST20  DS    0H                  CHECK IF 'ALL'                               
         CLI   5(R2),X'03'                                                      
         BNE   VREST25                                                          
         CLC   =C'ALL',8(R2)                                                    
         BNE   VREST25                                                          
*                                                                               
VREST21  DS    0H                                                               
         MVI   SVTEST,X'00'        SET ESTIMATE TO 'ALL'                        
         MVC   SVTESTN,=C'ALL'                                                  
         B     VREST100                                                         
*                                                                               
VREST25  DS    0H                  CHECK IF 'NO'                                
         CLI   5(R2),X'02'                                                      
         BNE   BADESTM                                                          
         CLC   =C'NO',8(R2)                                                     
         BNE   BADESTM                                                          
*                                                                               
VREST26  MVI   SVTEST,X'00'        SET ESTIMATE TO 'NO'                         
         MVC   SVTESTN,=C'NO '                                                  
         B     VREST100                                                         
*                                                                               
VREST30  ZIC   RF,5(R2)            NUMERIC HERE                                 
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         UNPK  WORK(3),DUB                                                      
         OI    WORK+2,X'F0'        3 BYTE ESTIMATE CODE                         
*                                                                               
         CVB   R0,DUB                                                           
         LTR   R0,R0                                                            
         BZ    BADESTM                                                          
         STC   R0,SVTEST           TURNAROUND ESTIMATE                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),BCLT                                                    
         MVC   KEY+4(3),SVTPRDN                                                 
*                                                                               
         CLC   SVTPRDN,=C'   '                                                  
         BH    *+10                                                             
         MVC   KEY+4(3),=C'POL'                                                 
         CLC   SVTPRDN,=C'ALL'                                                  
         BNE   *+10                                                             
         MVC   KEY+4(3),=C'POL'                                                 
*                                                                               
         MVC   KEY+7(1),SVTEST                                                  
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'SPTDIR',KEY,AIO                   
         CLI   DMCB+8,0                                                         
         BNE   BADESTM                                                          
*                                                                               
         L     R6,AIO                                                           
         CLC   KEY(13),0(R6)                                                    
         BNE   BADESTM                                                          
         MVC   SVTESTN,WORK                                                     
*                                                                               
         OC    SVTPRD2N,SVTPRD2N                                                
         BZ    VREST60                                                          
*                                                                               
         XC    KEY,KEY             TEST ESTIMATE FOR PIGGYBACK                  
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),BCLT                                                    
         MVC   KEY+4(3),SVTPRD2N                                                
         MVC   KEY+7(1),SVTEST                                                  
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'SPTDIR',KEY,AIO                   
         CLI   DMCB+8,0                                                         
         BNE   BADESTM                                                          
*                                                                               
         L     R6,AIO                                                           
         CLC   KEY(13),0(R6)                                                    
         BNE   BADESTM                                                          
         MVC   SVTESTN,WORK                                                     
*                                                                               
VREST60  CLI   SVTIDNUM,0          FILM TYPE CHECK - NO-OPED                    
         BE    VREST70                                                          
         CLI   SVTPRD2,0                                                        
         BNE   BADIDERR            INPUT NOT COMPATIBLE WITH ID NUMBER          
         CLI   SVTEST,0                                                         
         BNE   BADIDERR                                                         
         MVC   SVTPRD2,SVTIDNUM                                                 
*                                                                               
VREST70  DS    0H                                                               
*                                                                               
VREST100 MVC   RQEST,SVTESTN                                                    
*                                                                               
VRESTX   OI    4(R2),X'20'                                                      
*****                                                                           
* VALIDATE THE TURNAROUND PERIOD                                                
*****                                                                           
VRPER00  DS    0H                                                               
         LA    R2,REQPERDH                                                      
*                                                                               
         TM    4(R2),X'20'                                                      
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1RPCHG                                                
*                                                                               
         CLI   5(R2),0                                                          
         BNE   VRPER05                                                          
         GOTO1 VGLOBBER,DMCB,=C'GETF',(R2),,GLVSPPER                            
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         OI    MISCFLG1,MF1RPCHG                                                
*                                                                               
VRPER05  GOTO1 SCANNER,DMCB,(R2),(1,BLOCK)                                      
         CLI   DMCB+4,0                                                         
         BE    INVLFLD                                                          
*                                                                               
         LA    R3,BLOCK                                                         
         CLI   0(R3),0                                                          
         BE    INVLFLD                                                          
*                                                                               
         MVI   BYTE,C'M'           SET HAVE 'MOS' ENTERED                       
         LA    R6,22(R3)                                                        
         ZIC   RF,0(R3)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),=C'MOS'                                                 
         BE    VRPER10                                                          
*                                  CHECK FOR DATE ENTERED BY ITSELF             
         MVI   BYTE,0              NO 'MOS' ENTERED                             
         CLI   0(R3),8             8 MAX LENGTH                                 
         BH    INVLFLD                                                          
         LA    R6,12(R3)                                                        
*                                                                               
VRPER10  GOTO1 DATVAL,DMCB,0(R6),DUB    FIRST TRY FULL DATE                     
         CLI   DMCB+3,0                                                         
         BE    VRPER20                                                          
*                                                                               
         GOTO1 GETDAY,DMCB,DUB,FULL                                             
         CLI   DMCB,1              MUST BE MONDAY                               
         BE    VRPER50                                                          
         B     INVLFLD                                                          
*                                                                               
VRPER20  CLI   BYTE,C'M'           IF 'MOS' ENTERED, SKIP LENGTH TESTS          
         BE    VRPER30                                                          
         CLI   0(R3),6             IF YM ONLY, 6 MAX LENGTH                     
         BH    INVLFLD                                                          
         CLI   0(R3),4                         5 MINIMUM                        
         BL    INVLFLD                                                          
*                                                                               
VRPER30  GOTO1 DATVAL,DMCB,(2,0(R6)),DUB     YM ONLY                            
         MVC   DUB+4(2),=C'  '                                                  
         CLC   DUB(2),=C'75'       MUST BE 1975 OR HIGHER                       
         BL    INVLFLD                                                          
         CLI   DMCB+3,0                                                         
         BE    INVLFLD                                                          
*                                                                               
VRPER50  MVC   RQMOS,DUB                                                        
         MVI   RQSW,C'Y'                                                        
*                                                                               
VRPERX   OI    4(R2),X'20'                                                      
*****                                                                           
* VALIDATE THE TURNAROUND OPTIONS                                               
*****                                                                           
VROPT00  DS    0H                                                               
         LA    R2,REQOPTNH                                                      
         MVI   PSEUDOPT,C'N'                                                    
         XC    INTOPT,INTOPT                                                    
         XC    RQACNNUM,RQACNNUM                                                
         MVI   MOSOVRDE,0                                                       
*                                                                               
         TM    4(R2),X'20'                                                      
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1RPCHG                                                
*                                                                               
         CLI   5(R2),0                                                          
         BE    VROPTX                                                           
*                                                                               
         LA    RE,BLOCK                                                         
         LA    RF,480                                                           
         XCEFL                                                                  
*                                                                               
         GOTO1 SCANNER,DMCB,(R2),(X'8F',BLOCK)                                  
         CLI   DMCB+4,0                                                         
         BE    INVLFLD                                                          
         CLI   DMCB+4,14           TOO MANY OPTIONS                             
         BH    INVLFLD                                                          
*                                                                               
         L     R1,ATIOB                                                         
         USING TIOBD,R1                                                         
         OI    TIOBINDS,TIOBSETC   SET CURSOR ON ERROR                          
         LR    R0,R2                                                            
         SR    R0,RA                                                            
         STH   R0,TIOBCURD                                                      
         DROP  R1                                                               
*                                                                               
         LA    R3,BLOCK                                                         
VROPTLP  CLI   0(R3),0                                                          
         BE    VROPTX                                                           
*                                                                               
         L     R1,ATIOB                                                         
         USING TIOBD,R1                                                         
         MVC   TIOBCURI,4(R3)      SET CURSOR DISPLACEMENT                      
         DROP  R1                                                               
*                                                                               
         ZIC   RF,0(R3)            BUY OPTION                                   
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),=C'BUY'                                                 
         BNE   VROPT10                                                          
         MVI   RQSW,C'Y'                                                        
         MVC   RQBUYOPT,22(R3)                                                  
         B     VROPTNX                                                          
*                                                                               
VROPT10  CLC   =C'I4',12(R3)                                                    
         BNE   VROPT20                                                          
         MVI   RQSW,C'Y'                                                        
         MVC   RQREP,12(R3)                                                     
         B     VROPTNX                                                          
*                                                                               
VROPT20  CLC   =C'SEQ',12(R3)      OPTION TO SEQUENCE FIELDS                    
         BNE   VROPT30                                                          
         CLI   1(RA),C'*'          DDS ONLY                                     
         BNE   VROPTINV                                                         
         MVI   SEQSW,C'Y'                                                       
         B     VROPTNX                                                          
*                                                                               
VROPT30  CLC   =C'MCT',12(R3)      MCT SWITCH                                   
         BNE   VROPT40                                                          
         MVI   PSEUDOPT,C'M'                                                    
         B     VROPTNX                                                          
*                                                                               
VROPT40  CLC   =C'RES',12(R3)      RESPONSE SWITCH                              
         BNE   VROPT50                                                          
         MVI   PSEUDOPT,C'R'                                                    
         B     VROPTNX                                                          
*                                                                               
VROPT50  CLC   =C'ID',12(R3)       ACN NUMBER?                                  
         BNE   VROPT60                                                          
         CLC   =C'CK',AGENCY       ONLY COKE CAN USE THIS OPTION                
         BNE   VROPTINV                                                         
         TM    3(R3),X'80'         HAS TO BE ALL NUMERIC                        
         BZ    VROPTINV                                                         
         CLI   1(R3),5             5 DIGITS                                     
         BNE   VROPTINV                                                         
         MVC   RQACNNUM,22(R3)                                                  
         B     VROPTNX                                                          
*                                                                               
VROPT60  DS    0H                                                               
         CLC   =C'MOS',12(R3)      MONTH OF SERVICE?                            
         BNE   VROPT70                                                          
         CLI   0(R3),3                                                          
         BNE   VROPT70                                                          
         CLI   1(R3),1                                                          
         BNE   VROPTINV                                                         
         CLI   22(R3),C'C'             CALENDAR                                 
         BE    *+12                                                             
         CLI   22(R3),C'B'             OR BROADCAST                             
         BNE   VROPTINV                                                         
         MVC   MOSOVRDE,22(R3)     SAVE THE MONTH OF SERVICE OVERRIDE           
         B     VROPTNX                                                          
*                                                                               
VROPT70  CLC   =C'INTONLY',12(R3)  CK INTONLY OPTION                            
         BNE   VROPT80                                                          
         CLI   INTOPT,0            ALREADY SET ?                                
         BNE   VROPTINV                                                         
         MVI   INTOPT,C'O'                                                      
         B     VROPTNX                                                          
*                                                                               
VROPT80  CLC   =C'INT',12(R3)      CK INT OPTION                                
         BNE   VROPT90                                                          
         CLI   INTOPT,0            ALREADY SET ?                                
         BNE   VROPTINV                                                         
         MVI   INTOPT,C'I'                                                      
         B     VROPTNX                                                          
*                                                                               
VROPT90  DS    0H                                                               
*                                                                               
VROPTINV B     BADRQST             INVALID REQUEST                              
*                                                                               
VROPTNX  LA    R3,32(R3)                                                        
         B     VROPTLP                                                          
*                                                                               
VROPTX   OI    4(R2),X'20'                                                      
*                                                                               
         L     R1,ATIOB                                                         
         USING TIOBD,R1                                                         
         NI    TIOBINDS,X'FF'-TIOBSETC   DON'T NEED TO SET CURSOR               
         DROP  R1                                                               
         EJECT                                                                  
***********************************                                             
* BUILD THE TURNAROUND REQUEST                                                  
***********************************                                             
BTREQ00  DS    0H                                                               
         LA    R2,REQNAMEH                                                      
         TM    MISCFLG1,MF1NOREQ   FIRST TIME THROUGH?  (CHANGED MEDIA)         
         BNZ   PLSENTER            YES, PLEASE ENTER FIELDS AS REQUIRED         
*                                                                               
         MVC   TEMPDATE(6),RQMOS   SET MOS FOR THIS REQ IN TEMPDATE             
         CLI   RQMOS,C' '          USE GIVEN MOS                                
         BH    *+10                                                             
         MVC   TEMPDATE(4),PERDEYMD   OR PERIOD END DATE(4)                     
         CLI   TEMPDATE,C' '       MUST HAVE SOME MOS                           
         BNH   BTREQX                                                           
*                                                                               
         CLI   RQSW,C'Y'           TEST SPECIFICALLY REQUESTED                  
         BE    BTREQ2                                                           
         CLI   PI2RAUTO,C'Y'       TEST AUTO REQ FOR $INV                       
         BNE   BTREQX              NO REQ                                       
*                                                                               
BTREQ2   MVC   LRQMOS,TEMPDATE     SAVE MOS FOR THIS REQ                        
         L     R6,AIO                                                           
         USING RCRDD,R6                                                         
         XC    RCRDCTL,RCRDCTL                                                  
         MVI   RCRDAREA,C' '                                                    
         MVC   RCRDAREA+1(79),RCRDAREA                                          
*                                                                               
         CLI   SVSYSTEM,3          NET?                                         
         BE    BTREQ2A             YES - GO DIRECTLY TO NET PROFILE             
*                                                                               
*        CLI   PI2NPOLS,C'P'                                                    
*        BE    BTREQ2C                                                          
*        CLI   PI2NPOLS,C'A'                                                    
*        BE    BTREQ2C                                                          
         CLI   PI2NESTS,C'Y'                                                    
         BE    BTREQ2C                                                          
         B     BTREQ2D                                                          
*                                                                               
BTREQ2A  CLI   PI2NPOLN,C'P'                                                    
         BE    BTREQ2C                                                          
         CLI   PI2NPOLN,C'A'                                                    
         BE    BTREQ2C                                                          
         CLI   PI2NESTN,C'Y'                                                    
         BNE   BTREQ2D                                                          
*                                                                               
BTREQ2C  MVI   RCRDOPT1+4,C'Y'                                                  
*                                                                               
BTREQ2D  DS    0H                                                               
         MVC   RCRDCODE,=C'U2'                                                  
         CLI   RQREP,C' '          ANY OVERRIDING REQUEST?                      
         BNH   *+10                                                             
         MVC   RCRDCODE,RQREP                                                   
*                                                                               
         MVC   RCRDAGY,AGENCY                                                   
         MVC   RCRDMED,QMED                                                     
         MVC   RCRDCLT,QCLT                                                     
         OC    RCRDCLT,SPACES                                                   
*                                                                               
         CLI   SVTPGRP,X'00'       ANY PRODUCT GROUP                            
         BE    BTREQ2F                                                          
         MVC   RCRDPGRP(1),SVTPGRP                                              
         MVC   RCRDPRD,SVTPGRP+1                                                
         CLI   SVTPGRP+1,C'0'      NUMERIC?                                     
         BL    BTREQ2Z                                                          
         NI    RCRDPGRP,X'BF'                                                   
         B     BTREQ2Z                                                          
*                                                                               
BTREQ2F  MVC   RCRDPRD,RQPRD                                                    
         CLI   RQPRD,C' '                                                       
         BH    BTREQ3D                                                          
         CLI   PI2RPOL,C'A'        IF NO PRD, TEST PROFILE                      
         BE    BTREQ3B             PRD = ALL                                    
         MVC   RCRDPRD,=C'POL'     ELSE POL IF PRESENT                          
*                                                                               
BTREQ2Z  LA    RF,SVCLIST                                                       
BTREQ3   CLI   3(RF),0                                                          
         BE    BTREQ3B                                                          
         CLI   3(RF),X'FF'                                                      
         BE    BTREQ3D                                                          
         LA    RF,4(RF)                                                         
         B     BTREQ3                                                           
*                                                                               
BTREQ3B  MVC   RCRDPRD,=C'ALL'     ELSE ALL                                     
*                                                                               
BTREQ3D  CLI   RQPRD+3,C' '                                                     
         BNH   *+10                                                             
         MVC   RCRDPRD2,RQPRD+3                                                 
*                                                                               
         CLI   RQEST,C' '                                                       
         BNH   *+10                                                             
         MVC   RCRDEST,RQEST                                                    
*                                                                               
         MVC   HALF,BMKTSTA                                                     
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  RCRDMKT,DUB                                                      
*                                                                               
         MVC   RCRDSTA,QSTA                                                     
         CLI   RCRDSTA+3,C'-'                                                   
         BNE   *+12                                                             
         MVI   RCRDSTA+3,C' '                                                   
         B     BTREQ6                                                           
*                                                                               
         CLI   RCRDSTA+4,C'/'                                                   
         BE    BTREQ6                                                           
         CLI   RCRDSTA+4,C'A'        KEEP X OF WABCX                            
         BNL   BTREQ6                                                           
         MVC   RCRDSTA+4(1),QSTA+5   WABC-X                                     
*                                                                               
BTREQ6   OC    RCRDSTA,=6C' '                                                   
*                                                                               
         CLI   RCRDSTA,C'0'                                                     
         BL    *+8                                                              
         MVI   RCRDSTA+4,C'/'                                                   
*                                                                               
         CLI   RQMOS,C' '          IF MOS SPECIFICALLY REQ'D                    
         BNH   *+14                                                             
         MVC   RCRDSDAT(6),RQMOS   USE IT                                       
         B     *+10                                                             
         MVC   RCRDSDAT(4),PERDEYMD   ELSE USE PERIOD END DATE                  
*                                                                               
         CLI   NETPAKSW,C'Y'                                                    
         BNE   *+14                                                             
         MVC   RCRDAREA+56(1),QSUBMED QCOMPARE                                  
         B     BTREQ8M                                                          
*                                                                               
         CLI   RQBOOK,C' '         TEST BOOK SPECIFIED                          
         BNH   BTREQ7B                                                          
         CLI   RQBOOK,X'FF'        TEST BOOK=NO                                 
         BE    BTREQ8M                                                          
         MVC   RCRDBK1(6),RQBOOK                                                
         B     BTREQ8M                                                          
*                                                                               
BTREQ7B  DS    0H                  NO BOOK GIVEN                                
         CLI   PI2RHUT,C'N'        TEST HUT OPTION                              
         BNE   *+10                                                             
         MVC   RCRDBK1+4(2),=C'NO'                                              
         MVC   TEMPDATE(6),RCRDSDAT  USE MOS                                    
         CLI   TEMPDATE+4,C' '            IF FULL DATE GIVEN                    
         BNH   BTREQ7D                                                          
         GOTO1 ADDAY,DMCB,TEMPDATE,TEMPDATE,6   ADD 6 DAYS TO ENSURE            
*                                  RIGHT CALENDAR MONTH                         
BTREQ7D  DS    0H                                                               
         PACK  DUB,TEMPDATE+2(2)   LOOK UP BOOK BASED ON MOS MONTH              
         CVB   R1,DUB                                                           
         SR    R0,R0                                                            
         IC    R0,PI2RHUT(R1)      BOOKS IN PROFI2R+4 THRU +15                  
         LTR   R0,R0               TEST VALUE PRESENT                           
         BZ    BTREQ8M             NO - NO BOOK                                 
         CH    R0,=H'13'           MONTH 13 = ACT BOOK                          
         BNE   BTREQ8F                                                          
         MVC   RCRDBK1(4),=C'ACT '                                              
         B     BTREQ8M                                                          
*                                                                               
BTREQ8F  DS    0H                                                               
         MVI   BYTE,0                                                           
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  RCRDBK1+2(2),DUB    SET MONTH                                    
         MVC   RCRDBK1(2),TEMPDATE   SET YEAR = MOS YEAR                        
         CR    R0,R1               UNLESS BOOK GT MOS                           
         BNH   BTREQ8J                                                          
*                                                                               
BTREQ8H  DS    0H                                                               
         MVC   WORK(4),RCRDBK1                                                  
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 ADDAY,DMCB,(C'Y',WORK),WORK+10,-1                                
         MVC   RCRDBK1(2),WORK+10                                               
*                                               SEE IF BOOK EXISTS              
BTREQ8J  DS    0H                                                               
         MVC   WORK(4),RCRDBK1                                                  
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(3,WORK)                                    
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),QMED                                                    
         MVI   KEY+2,C'N'          NIELSON                                      
         CLI   SVCPROF+3,C'0'                                                   
         BE    *+8                                                              
         MVI   KEY+2,C'A'          ARB                                          
         MVC   KEY+3(2),WORK                                                    
         XC    KEY+3(2),=X'FFFF'                                                
*                                                                               
         LA    R3,200(R6)                                                       
*                                  NOTE- USE IOAREA+200 BECAUSE                 
*                                       REQUEST BUILT AT IOAREA                 
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'DEMDIR',KEY,(R3),DMWORK               
         TM    DMCB+8,X'FF'                                                     
         BZ    *+12                                                             
BTREQER1 LA    R2,REQBOOKH                                                      
         B     NORATBOK                                                         
*                                                                               
         CLC   KEY(5),0(R3)                                                     
         BE    BTREQ8M             BOOK OK                                      
*                                                                               
         CLI   BYTE,0              NOT ON FILE, TRY TO BACK UP 1 YEAR           
         BNE   BTREQER1            ALREADY HAVE DONE                            
         MVI   BYTE,1                                                           
         B     BTREQ8H                                                          
*                                                                               
BTREQ8M  CLC   =C'U2',RCRDCODE     U2 REQUEST?                                  
         BNE   BTREQ8O                                                          
         CLC   =C'CK',AGENCY       COKE AGENCY?                                 
         BNE   BTREQ8O                                                          
         CLI   RQACNNUM,C' '       ANY ACN NUMBER?                              
         BNH   BTREQ8O                                                          
         MVC   RCRDBK1(L'RQACNNUM),RQACNNUM  YES, PUT IN BOOK PORTION           
         MVI   RCRDHUT1+1,C' '                                                  
*                                                                               
BTREQ8O  MVC   RCRDRQTR(10),RQNAME                                              
         CLI   RQBUYOPT,C' '                                                    
         BNH   *+10                                                             
         MVC   RCRDOPT1,RQBUYOPT                                                
*                                                                               
         CLI   PSEUDOPT,C'R'         RESPONSE                                   
         BNE   *+8                                                              
         MVI   RCRDAREA+32,C'R'                                                 
*                                                                               
         CLI   PSEUDOPT,C'M'         MCT                                        
         BNE   *+8                                                              
         MVI   RCRDAREA+32,C'M'                                                 
*                                                                               
         PACK  DUB,RCRDCODE                                                     
         CVB   R0,DUB                                                           
         STC   R0,RCRDCTL+10                                                    
         MVI   RCRDCTL+14,106                                                   
         IC    R0,0(RA)                                                         
*                                                                               
         MVI   RCRDREC2,C' '       CLEAR 2ND CARD IN CASE WE NEED IT            
         MVC   RCRDREC2+1(L'RCRDREC2-1),RCRDREC2                                
*                                                                               
         CLI   MOSOVRDE,C' '                                                    
         BNH   BTREQ9G                                                          
         MVC   RCRDMOSO,MOSOVRDE                                                
         MVI   RCRDCONT,C'*'       CONTINUED INDICATOR                          
         OI    RCRDCTL+15,X'12'    SET HAVE 2ND REQ                             
         MVI   RCRDCTL+14,186                                                   
*                                                                               
BTREQ9G  CLI   REQSTA,C'0'         FOR LOCAL CABLE - no network!!               
         B     BTREQ9H                                                          
***      BNH   BTREQ9H                                                          
***      MVC   RCRDCNET,QNTWK      IF NO NETWORK                                
***      CLI   RCRDCNET,C' '                                                    
***      BH    *+10                                                             
***      MVC   RCRDCNET,=C'ALL'    GO FOR ALL NETWORKS                          
*                                                                               
***      MVI   RCRDCONT,C'*'       CONTINUED INDICATOR                          
***      OI    RCRDCTL+15,X'10'    SET HAVE 2ND REQ                             
***      MVI   RCRDCTL+14,186                                                   
*                                                                               
BTREQ9H  CLI   INTOPT,0            INT/INTONLY OPTION?                          
         BE    BTREQ9J                                                          
         MVC   RCRDINT,INTOPT                                                   
         MVI   RCRDCONT,C'*'       CONTINUED INDICATOR                          
         OI    RCRDCTL+15,X'12'    SET HAVE 2ND REQ                             
         MVI   RCRDCTL+14,186                                                   
*                                                                               
BTREQ9J  DS    0H                                                               
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'REQUEST',                     X        
               RCRDCTL,RCRDCTL,((R0),0)                                         
         TM    8(R1),X'FF'                                                      
         BZ    BTREQ10                                                          
         B     PRX                                                              
*                                                                               
BTREQ10  DS    0H                                                               
         CLC   =C'U2',RCRDCODE     IF U2 REQUEST FOR COKE THEN                  
         BNE   BTREQ20             ALSO ADD A CD REQUEST                        
         CLC   =C'CK',AGENCY                                                    
         BNE   BTREQ20                                                          
         MVC   TEMPDATE(4),RCRDSDAT                                             
         MVC   TEMPDATE+4(2),=C'15' USE 15TH TO GET RIGHT BRD MONTH             
         GOTO1 GETBROAD,DMCB,(1,TEMPDATE),RCRDSDAT,GETDAY,ADDAY                 
         CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   RCRDCODE(2),=C'CD'  CD REQUEST                                   
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'REQUEST',                     X        
               RCRDCTL,RCRDCTL,((R0),0)                                         
         TM    8(R1),X'FF'                                                      
         BZ    BTREQ20                                                          
         B     PRX                                                              
*                                                                               
BTREQ20  DS    0H                                                               
         NI    MISCFLG1,X'FF'-MF1RPCHG                                          
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(23),=C'** REQUEST GENERATED **'                          
         OI    CONHEADH+6,X'80'                                                 
*                                                                               
BTREQX   DS    0H                                                               
         DROP  R6                                                               
*                                                                               
PRX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* CHECK THE PRODUCT TO SEE IF IN CLIENT PRODUCT LIST                            
*                                                                               
* ON ENTRY:    PARAM 1             A(EBCDIC PRODUCT CODE)                       
*              PARAM 2             A(BINARY PRODUCT CODE)                       
***********************************************************************         
CKPRDCOD NTR1                                                                   
         L     R2,0(R1)                                                         
         L     R3,4(R1)                                                         
*                                                                               
         LA    RE,SVCLIST                                                       
CKPRDLP  CLI   0(RE),0                                                          
         BE    CKPRDNO                                                          
*                                                                               
         CLC   0(L'QPRD,RE),0(R2)                                               
         BE    *+12                                                             
         LA    RE,4(RE)                                                         
         B     CKPRDLP                                                          
*                                                                               
         MVC   0(1,R3),3(RE)                                                    
*                                                                               
CKPRDYES B     YES                                                              
*                                                                               
CKPRDNO  B     NO                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE READS A MINIO ELEMENT.  MINEKEY MUST BE SET BY CALLER            
***********************************************************************         
MINIORD  NTR1                                                                   
         GOTO1 MINIO,DMCB,('MINRD',(R5))                                        
         CLI   MINERR,0                                                         
         BE    XIT                                                              
         DC    H'0'                DIE ON ANY ERROR                             
         SPACE 2                                                                
***********************************************************************         
* THIS ROUTINE READ HIGH A MINIO ELEMENT.  MINEKEY MUST BE SET BY               
* THE CALLER.                                                                   
***********************************************************************         
MINIOHI  NTR1                                                                   
         GOTO1 MINIO,DMCB,('MINHI',(R5))                                        
         CLI   MINERR,0            RETURN 'YES' IF NO ERRORS                    
         BE    YES                                                              
         B     NO                  OTHERWISE RETURN 'NO'                        
         SPACE 2                                                                
***********************************************************************         
* THIS ROUTINE READ SEQUENTIAL FOR A MINIO ELEMENT.                             
***********************************************************************         
MINIOSEQ NTR1                                                                   
         GOTO1 MINIO,DMCB,('MINSEQ',(R5))                                       
         CLI   MINERR,0            RETURN 'YES' IF NO ERRORS                    
         BE    YES                                                              
         CLI   MINERR,MINEEOF      RETURN 'NO' IF END-OF-FILE                   
         BE    NO                                                               
         DC    H'0'                DIE ON ANY OTHER ERROR                       
         SPACE 2                                                                
***********************************************************************         
* THIS ROUTINE WRITES OUT A MINIO ELEMENT.  MINELEM MUST BE SET                 
***********************************************************************         
MINIOWRT NTR1                                                                   
         OI    MNIOFLAG,X'80'      REMEMBER TO CLOSE MINIO FILE                 
         GOTO1 MINIO,DMCB,('MINWRT',(R5))                                       
         CLI   MINERR,0                                                         
         BE    XIT                                                              
         DC    H'0'                DIE ON ANY ERROR                             
         SPACE 2                                                                
***********************************************************************         
* THIS ROUTINE ADDS A MINIO ELEMENT.  MINELEM MUST BE SET BY THE CALLER         
***********************************************************************         
MINIOADD NTR1                                                                   
         OI    MNIOFLAG,X'80'      REMEMBER TO CLOSE MINIO FILE                 
         GOTO1 MINIO,DMCB,('MINADD',(R5))                                       
         CLI   MINERR,0                                                         
         BE    YES                                                              
         CLI   MINERR,MINEDUP      DUPLICATE KEY?                               
         BE    NO                  YES, RETURN A NO                             
         DC    H'0'                DIE ON ANY ERROR                             
         EJECT                                                                  
***********************************************************************         
* VALIDATES PRODUCT GROUP   (COPIED FROM **SPREQ03**)                           
*                                                                               
* ON ENTRY:    (R2)                FIELD HEADER OF THE PRD GROUP                
***********************************************************************         
VALPGRP  NTR1                                                                   
         CLI   5(R2),6             AT LEAST 6 CHARS                             
         BL    VALPGNO                                                          
* GET SCHEME CODE                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D01'                                                  
         MVC   KEY+2(1),BAGYMD     AGY/MED                                      
         MVC   KEY+3(2),BCLT       CLT                                          
         MVC   KEY+5(1),8+4(R2)    SCHEME CODE                                  
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'SPTDIR',KEY,AIO                   
         CLI   DMCB+8,0                                                         
         BNE   VALPGNO                                                          
         MVC   SVTPGRP(4),8+4(R2)  PUT SCHEME IN SAVED FIELD                    
         OC    SVTPGRP,SPACES                                                   
***********************************                                             
****  WE'RE NOT GOING TO FUSS OVER THIS TOO MUCH BECAUSE                        
****   THEY CAN BE A LITTLE MORE SPECIFIC IN THE NINV PROGRAM                   
***********************************                                             
*&&DO                                                                           
         MVC   SVTPGRP(1),8+4(R2)  PUT SCHEME IN REQUEST                        
         CLC   8+5(3,R2),=C'POL' CHK PGR=APOLNNN                                
         BNE   VALPGV2A                                                         
         CLI   8+8(R2),C' '                                                     
         BE    VALPGV2A                                                         
         CLI   8+8(R2),C'Z'        CHK NUMERIC                                  
         BH    *+8                                                              
         B     VALPGVE             ERROR/NOT NUMERIC                            
         NI    SVTPGRP,X'BF'       MAKE A  LOW CHARACTER                        
VALPGV2A CLC   8+5(3,R2),=C'ALL'                                                
         BNE   VALPGV3                                                          
         OI    FIND,X'40'          PGP=XALL                                     
         B     VALPGVZ                                                          
VALPGV3  CLI   IFLD+5,C'Z'                                                      
         BH    VALPGV5             NUMERIC INPUT                                
*                                                                               
*                                                                               
*                      X PRD CODE INPUT SO VALIDATE                             
*                                                                               
         CLC   IFLD+5(3),=C'POL'                                                
         BNE   *+12                                                             
         OI    FIND,X'10'          XPOL  INPUT                                  
         B     VALPGV4                                                          
*                                                                               
         OI    FIND,X'80'          XABC  INPUT                                  
VALPGV4  MVC   KEY(13),KEYS      RESTORE KEY FOR PRD READ                       
         MVC   KEY+4(3),IFLD+5                                                  
         GOTO1 ARSPT                                                            
         CLC   FERN,=AL2(FE)                                                    
         BL    VALPGVO             DISK ERROR                                   
         BH    *+14                                                             
         MVC   FERN,=AL2(PRONOF)                                                
         B     VALPGVO                                                          
         MVC   NAME(20),SPTREC+28                                               
         CLI   IFLD+8,C' '         CHK PGR=APOLNNN                              
         BE    VALPGVO             NO/EXIT                                      
         SPACE                                                                  
*                                  YES                                          
* PGR=APOLNNN, SO FUDGE IT TO PGR=ANNN THEN VALIDATE AS USUAL *                 
         MVC   KEYS(13),KEY        SAVE KEY IN KEYS                             
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D01'                                                  
         MVC   KEY+2(3),KEYS+1     AGY/MED/CLT                                  
         MVC   KEY+5(1),IFLD+4     SCHEME CODE                                  
         GOTO1 ARSPT                                                            
         CLC   FERN,=AL2(FE)                                                    
         BL    VALPGVO                                                          
         BH    *+14                                                             
         MVC   FERN,=AL2(53)             NOT FOUND                              
         B     VALPGVO                                                          
         SPACE                                                                  
         ZIC   R4,IFLDH+5                                                       
         SH    R4,=H'3'            SUBTRACT "POL" LENGTH                        
         STC   R4,IFLDH+5                                                       
         MVC   IFLD+5(3),IFLD+8    MOVE NNN TO POL SO THAT PGR=ANNN             
         SPACE                                                                  
*                                                                               
VALPGV5  LA    R7,SPTREC+24                                                     
         CLI   0(R7),X'01'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    R4,R4                                                            
         IC    R4,14(R7)                                                        
         SR    R5,R5                                                            
         IC    R5,27(R7)                                                        
         AR    R4,R5                                                            
         IC    R5,40(R7)                                                        
         AR    R4,R5                                                            
         ST    R4,FULL             STORE TOTAL LENGTH                           
*                                                                               
VALPGV6  SR    R7,R7                                                            
         IC    R7,IFLDH+5                                                       
         SH    R7,=H'5'                                                         
         C     R7,FULL                                                          
         BNE   VALPGVE             WRONG LENGTH                                 
*                                                                               
VALPGV7  XC    TEMP(4),TEMP                                                     
         MVI   FULL,0                                                           
         LA    R4,TEMP                                                          
         LA    R5,IFLD+5                                                        
         CLI   0(R5),C'*'          CANNOT START WITH A *                        
         BE    VALPGVE                                                          
VALPGV7A CLI   0(R5),C'*'                                                       
         BE    VALPGV7X            NO MORE VALIDATION                           
         CLI   0(R5),C'0'                                                       
         BL    VALPGVE                                                          
         CLI   FULL,1              * WAS INPUT BEFORE A NUMBER                  
         BE    VALPGVE             ERROR                                        
         MVC   0(1,R4),0(R5)                                                    
VALPGV7C LA    R4,1(R4)                                                         
         LA    R5,1(R5)                                                         
         BCT   R7,VALPGV7A         R7 HAS ADJUSTED INPUT LENGTH                 
         B     VALPGV8                                                          
*                                                                               
VALPGV7X OI    FIND,X'20'                                                       
         OI    FULL,1                                                           
         B     VALPGV7C                                                         
*                                                                               
VALPGV8  CLI   FULL,1                                                           
         BE    VALPGVZ             * INPUT NO FUTHER VALIDATION                 
         PACK  FULL+1(3),TEMP(5)                                                
         MVC   KEY+6(2),FULL+1                                                  
         GOTO1 ARSPT                                                            
         CLC   FERN,=AL2(FE)                                                    
         BL    VALPGVO                                                          
         BH    *+14                                                             
         MVC   FERN,=AL2(53)             NOT FOUND                              
         B     VALPGVO                                                          
         OI    FIND,X'20'                                                       
         B     VALPGVZ                                                          
*                                                                               
VALPGVE  MVC   FERN,=AL2(FLDINV)                                                
***      B     VALPGVO                                                          
         B     VALPGXIT     ELSE IN SOON ONLY GET SOON ERROR MESSAGE            
*                                                                               
VALPGVZ  MVC   KEY(13),KEYS        RESTORE KEY                                  
*                                                                               
VALPGVO  LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(3,R7),IFLD+5                                                   
         FOUT  (R6),NAME                                                        
*                                                                               
VALPGVX  MVC   PROSAVE,FIND                                                     
         CLC   =C'N2',RNUM         IF N2                                        
         BNE   VALPGN2X                                                         
         TM    FIND,X'20'          AND XABC INPUT?                              
         BNO   VALPGN2X                                                         
         NI    RDIV,X'BF'          MAKE DIVISION LOWER CASE                     
VALPGN2X EQU   *                                                                
         L     R7,ASAVE                                                         
         USING T208FFD,R7                                                       
         CLC   =C'SOON',BVROUT                                                  
         BNE   VALPGXIT                                                         
         B     VALPGXIT                                                         
*&&                                                                             
VALPGYES B     YES                                                              
VALPGNO  B     NO                                                               
VALPGXIT B     XIT                                                              
         EJECT                                                                  
RELO     DS    A                                                                
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
***********************************************************************         
* GENERAL INFORMATIONAL MESSAGES                                                
***********************************************************************         
PLSENTER MVI   GERROR1,REQFIELD    PLEASE ENTER FIELDS AS REQUIRED              
         B     INFEXIT                                                          
***********************************************************************         
* REGULAR ERROR MESSAGES                                                        
***********************************************************************         
MISSFLD  MVI   GERROR1,MISSING                                                  
         B     ERREXIT                                                          
*                                                                               
INVLFLD  MVI   GERROR1,INVALID                                                  
         B     ERREXIT                                                          
*                                                                               
RECXISTS MVI   GERROR1,RECEXIST    RECORD ALREADY EXISTS                        
         B     ERREXIT                                                          
*                                                                               
RECNTFND MVI   GERROR1,NOTFOUND    RECORD NOT FOUND                             
         B     ERREXIT                                                          
***********************************************************************         
* SYSTEM 23 ERROR MESSAGES                                                      
***********************************************************************         
SYS23ERR MVI   GETMSYS,23                                                       
         B     ERREXIT                                                          
***********************************************************************         
* SYSTEM SPOT ERROR MESSAGES                                                    
***********************************************************************         
BADRQST  MVI   GERROR1,INVREQ      INVALID REQUEST                              
         B     ERREXIT2                                                         
*                                                                               
BADDTFMT MVI   GERROR1,INVDTFMT    INVALID DATE FORMAT                          
         B     ERREXIT2                                                         
*                                                                               
BADPROD  MVI   GERROR1,INVPROD     INVALID PRODUCT                              
         B     ERREXIT2                                                         
*                                                                               
BADESTM  MVI   GERROR1,INVESTMT    INVALID ESTIMATE                             
         B     ERREXIT2                                                         
*                                                                               
BADPROD2 MVI   GERROR1,INVPROD2    INVALID PRODUCT #2                           
         B     ERREXIT2                                                         
*                                                                               
BADDTEST MVI   GERROR1,INVDTEST    DATES NOT WITHIN ESTIMATE PERIOD             
         B     ERREXIT2                                                         
*                                                                               
BADIDERR MVI   GERROR1,INVIDERR    INPUT NOT COMPATIBLE WITH ID NUMBER          
         B     ERREXIT2                                                         
*                                                                               
NORATBOK MVI   GERROR1,INVRATBK    RATING BOOK NOT ON FILE                      
         B     ERREXIT2                                                         
*                                                                               
BKHUTINV MVI   GERROR1,INVBKHUT    BOOK-HUT INVALID                             
         B     ERREXIT2                                                         
*                                                                               
I2NESTER MVC   GERROR,=AL2(1278)   ESTIMATE MUST BE BLANK OR NO                 
         B     ERREXIT                                                          
*                                                                               
I2NPOLER MVC   GERROR,=AL2(0101)   PRODUCT MUST BE POL                          
         B     ERREXIT                                                          
*                                                                               
I2NALLER MVC   GERROR,=AL2(1279)   SPECIFIC PRODUCT OR ALL                      
         B     ERREXIT                                                          
*                                                                               
ERREXIT2 OI    GENSTAT2,USMYERSY                                                
ERREXIT  MVI   GMSGTYPE,C'E'                                                    
         NI    MNIOFLAG,X'FF'-X'80'  DON'T SAVE CHANGES ON ERRORS               
         B     MYERRXIT                                                         
*                                                                               
INFEXIT  MVI   GMSGTYPE,C'I'                                                    
MYERRXIT GOTO1 MYERR                                                            
*                                                                               
         PRINT GEN                                                              
         GETEL R6,DATADISP,ELCODE                                               
         PRINT NOGEN                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PFKEY TABLE DEFINITIONS FOR INVOICE LIST                                      
***********************************************************************         
PFTABLE  DS    0C                                                               
*                                                                               
* RETURN TO CALLER                                                              
         DC    AL1(PF12X-*,12,0,0,PFTRETRN)                                     
         DC    CL3' ',CL8' ',CL8' '                                             
PF12X    EQU   *                                                                
         DC    X'FF'                                                            
***********************************************************************         
* SPECIAL PFKEY TABLE DEFINITIONS                                               
***********************************************************************         
SPFTABLE DS    0C                                                               
*                                                                               
* GOTO TO LIST IF CALLSP=0 FOR PF12                                             
         DC    AL1(SPF12X-*,12,0,(SPF12X-SPF12)/KEYLNQ,0)                       
         DC    CL3' ',CL8'INVOICE',CL8'LIST'                                    
SPF12    DC    AL1(KEYTYTWA,L'REQMED-1),AL2(REQMED-T210FFD)                     
         DC    AL1(KEYTYTWA,L'REQCLT-1),AL2(REQCLT-T210FFD)                     
         DC    AL1(KEYTYTWA,L'REQSTA-1),AL2(REQSTA-T210FFD)                     
SPF12X   EQU   *                                                                
*                                                                               
* RETURN TO CALLER                                                              
         DC    AL1(SPF24X-*,24,PFTRPROG,0,0)                                    
         DC    CL3' ',CL8' ',CL8' '                                             
SPF24X   EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
       ++INCLUDE SPSNVFEQTB        FIELD EQUATE TABLE                           
         EJECT                                                                  
       ++INCLUDE SPSNVWORKD        SYSTEM AREAS                                 
         EJECT                                                                  
       ++INCLUDE SPSNVRCRD         REQUEST CARD DSECT                           
         EJECT                                                                  
       ++INCLUDE SPGENSNV          INVOICE RECORD DSECT                         
         EJECT                                                                  
       ++INCLUDE SPSNVFFD          BASE SCREEN FOR SYSTEM                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPSNVF9D          OUR MAINTENANCE SCREEN                       
         EJECT                                                                  
* DDGENTWA                                                                      
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* FATIOB                                                                        
* DDGLOBEQUS                                                                    
* DDCOMFACS                                                                     
* DDPERVALD                                                                     
* DDMINBLK                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
       ++INCLUDE FATIOB                                                         
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDMINBLK                                                       
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE SPGENCLT                                                       
         PRINT ON                                                               
         EJECT                                                                  
* MY STORAGE AREA                                                               
MYAREAD  DSECT                                                                  
VGLOBBER DS    A                   A(GLOBBER)                                   
*                                                                               
MISCFLG1 DS    X                                                                
MF1RPCHG EQU   X'80'               REPORT CHANGE POSSIBLE                       
MF1NOREQ EQU   X'40'               PUT OUT 'PLEASE ENTER FIELDS AS...'          
*                                                                               
SVAREA   DS    0C                                                               
SVPRDCOD DS    XL1                 SAVED PRODUCT CODE                           
SVQPRDCD DS    CL3                       EBCDIC PRODUCT CODE                    
SVPR2COD DS    XL1                       PIGGY PRODUCT CODE                     
SVQPR2CD DS    CL3                       EBCDIC PIGGYBACK PRODUCT CODE          
SVESTNUM DS    XL1                       ESTIMATE                               
SVCONNUM DS    CL12                      CONTRACT NUMBER                        
SVTOTCOS DS    PL8                       TOTAL COST                             
SVTOTSPT DS    XL2                       TOTAL NUMBER OF SPOTS                  
SVINVDAT DS    XL2                       INVOICE DATE                           
SVDUEDAT DS    XL2                       DUE DATE                               
SVCTLBYT DS    XL1                       CONTROL BYTE                           
SVFLDLST DS    XL(MAXFLDEQ)              FIELD NUMBER EQUATE LIST               
SVFLDCNT DS    XL1                       FIELD COUNT                            
SVAREAX  DS    0C                                                               
*                                                                               
SVTPRD   DS    XL1                 SAVED REQUEST PRODUCT CODE                   
SVTPRDN  DS    CL3                               PRODUCT NAME                   
SVTPRD2  DS    XL1                               PIGGY PRODUCT CODE             
SVTPRD2N DS    CL3                               PB PRODUCT NAME                
SVTEST   DS    XL1                               ESTIMATE                       
SVTESTN  DS    CL3                               ESTIAMTE CODE (EBCDIC)         
SVTIDNUM DS    XL1                               FILM ID NUMBER                 
SVTPGRP  DS    CL4                                                              
*                                                                               
INTESTSD DS    CL6                 INTERSECTED ESTIMATE START DATE              
INTESTED DS    CL6                                      END DATE                
*                                                                               
BRDDATES DS    CL12                BROADCAST DATES                              
BRDCSDAT DS    XL2                 BROADCAST COMPRESSED START DATE              
BRDCEDAT DS    XL2                 BROADCAST COMPRESSED END DATE                
*                                                                               
PERDSYMD DS    CL6                 PERIOD'S START YYMMDD                        
PERDEYMD DS    CL6                          END   YYMMDD                        
*                                                                               
TEMPDATE DS    CL6                 TEMPORARY DATE FIELD                         
*                                                                               
PERVALST DS    XL56                PERVAL STORAGE                               
*                                                                               
PROF0I2  DS    CL16                I2  PROFILE                                  
P0I2BC   EQU   PROF0I2+9           BROADCAST/CALENDAR MONTHS                    
PROFI2Z  DS    CL16                I2Z PROFILE                                  
PROFI2N  DS    CL16                I2N PROFILE                                  
PI2NPOLS EQU   PROFI2N+4           SPOT-PRD=POL UPDATIVE I2S ONLY               
PI2NESTS EQU   PROFI2N+5           SPOT-EST=NO UPDATIVE I2S ONLY                
PI2NPOLN EQU   PROFI2N+6           NET -PRD=POL UPDATIVE I2S ONLY               
PI2NESTN EQU   PROFI2N+7           NET -EST=NO UPDATIVE I2S ONLY                
SVMEDIA  DS    C                                                                
SVSYSTEM DS    C                                                                
SVCLTTYP DS    X                                                                
*                                                                               
LRQMOS   DS    CL6                 SAVED MOS FOR THIS REQUEST                   
SEQSW    DS    C                   SEQUENCE SWITCH                              
PSEUDOPT DS    C                                                                
INTOPT   DS    C                   INT/INTONLY OPTION                           
MOSOVRDE DS    C                   MONTH OF SERVICE OVERRIDE                    
*                                                                               
RQOPTS   DS    0CL35                                                            
RQSW     DS    C                   REQUEST SWITCH                               
RQPRD    DS    CL6                         PRODUCT                              
RQEST    DS    CL3                         ESTIMATE                             
RQBOOK   DS    CL6                         BOOK                                 
RQNAME   DS    CL10                        NAME                                 
RQMOS    DS    CL6                         MONTH OF SERVICE                     
RQREP    DS    CL2                         REP                                  
RQBUYOPT DS    CL1                         BUY OPTION                           
RQACNNUM DS    CL5                 5 DIGIT ACN NUMBER FOR COKE                  
*                                                                               
MELEM2   DS    XL(L'MELEM)         SECONDARY MINIO ELEMENT                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'044SPSNV06   01/04/08'                                      
         END                                                                    
