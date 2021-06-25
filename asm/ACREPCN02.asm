*          DATA SET ACREPCN02  AT LEVEL 041 AS OF 04/09/15                      
*PHASE ACCN02B                                                                  
*INCLUDE SORTER                                                                 
*INCLUDE DATVAL                                                                 
*INCLUDE ACCEDIT                                                                
*INCLUDE CHOPCON                                                                
*INCLUDE SQUASHER                                                               
*INCLUDE UNDERLIN                                                               
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
         TITLE 'PERSON RECORD REPORT'                                           
ACCN02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACCN**,R9,R7    R9=2ND BASE REGISTER, R7=3RD BASE            
         L     RA,0(R1)                                                         
         USING ACWORKD,RA          RA=A(GLOBAL W/S)                             
         LA    RC,SPACEND                                                       
         USING ACCN02D,RC          RC=A(SAVE W/S)                               
         L     R8,VBIGPRNT                                                      
         USING BIGPRNTD,R8         R8=ADDRESSES WIDE PRINT                      
*                                                                               
         CLI   MODE,RUNFRST        RUN FIRST                                    
         BE    RUNF                                                             
         CLI   MODE,REQFRST        REQUEST FIRST                                
         BE    REQF                                                             
         CLI   MODE,LEDGFRST       LEDGER FIRST                                 
         BE    LDGF                                                             
         CLI   MODE,PROCACC        PROCESS ACCOUNT                              
         BE    PACC                                                             
         CLI   MODE,REQLAST        REQUEST LAST - PRINT SUMMARY                 
         BE    REQL                                                             
EXIT     XMOD1 1                                                                
XYES     SR    RC,RC                                                            
XNO      LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*              RUN FIRST                                              *         
***********************************************************************         
*                                                                               
RUNF     DS    0H                                                               
         MVC   VTYPES(VTYPLNQ),ADCONS                                           
*                                                                               
         L     R2,=A(BOXRC)        SET UP BOX ROUTINE                           
         ST    RC,0(R2)                                                         
         L     R2,VEXTRAS                                                       
         USING RUNXTRAD,R2                                                      
         L     R2,ADMASTD                                                       
         USING MASTD,R2                                                         
         L     R2,MCBXAREA                                                      
         ST    R2,ADBOX                                                         
         L     R2,=A(BXHOOK)                                                    
         ST    R2,HEADHOOK                                                      
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              REQUEST FIRST                                          *         
***********************************************************************         
REQF     DS    0H                                                               
         GOTO1 ADSORTER,DMCB,SORTCARD,RECCARD,0                                 
*                                                                               
         USING ACCOMPD,R4                                                       
         MVI   RCSUBPRG,0                                                       
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         L     R4,ADCMPEL                                                       
         MVC   CMPABBR,ACMPABBR    COMPANY ABBREVIATION                         
         MVC   CMPNAME,SPACES                                                   
         L     R4,ADCMPNAM                                                      
         ZIC   R1,1(R4)                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+4                                                           
         MVC   CMPNAME(0),2(R4)    COMPANY NAME                                 
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              LEDGER FIRST                                           *         
***********************************************************************         
LDGF     DS    0H                                                               
         USING ACHEIRD,R4                                                       
         L     R4,ADLDGHIR         HEIRARACHY ELEMENT                           
         MVC   LEVELA,ACHRLEVA     LEVEL LENGTHS                                
         MVC   LEVELB,ACHRLEVB                                                  
         MVC   LEVELC,ACHRLEVC                                                  
         MVC   LEVELD,ACHRLEVD                                                  
*                                                                               
         MVC   LVLALEN,ACHRLEVA                                                 
         ZIC   R0,LEVELA                                                        
         ZIC   RF,LEVELB                                                        
         SR    RF,R0                                                            
         STC   RF,LVLBLEN                                                       
*                                                                               
         IC    R0,LEVELB                                                        
         IC    RF,LEVELC                                                        
         SR    RF,R0                                                            
         STC   RF,LVLCLEN                                                       
*                                                                               
         IC    R0,LEVELC                                                        
         IC    RF,LEVELD                                                        
         SR    RF,R0                                                            
         STC   RF,LVLDLEN                                                       
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              PROCESS AN ACCOUNT                                     *         
***********************************************************************         
*                                                                               
PACC     L     R3,ADACC                                                         
         LA    R2,SREC                                                          
         USING SRECD,R2                                                         
         XC    SRKEY(SRECLNQ),SRKEY     CLEAR EVERYTHING                        
*                                                                               
         CLI   QOPT1,C'P'          SORT BY PERSON ORDER                         
         BE    PACC05                                                           
         CLI   QOPT1,C'O'          SORT BY OFFICE/PERSON ORDER                  
         BE    PACC03                                                           
         CLI   QOPT1,C'N'          SORT BY NAME ORDER                           
         BE    PACC07                                                           
         CLI   QOPT1,C' '          SORT BY NAME ORDER                           
         BE    PACC07                                                           
PACC03   MVC   SRACCNT,3(R3)       ORIGINAL WAY                                 
         B     PACC50                                                           
*                                                                               
PACC05   DS    0H                                                               
         MVC   SRACCNT,SPACES      CLEAR ACCOUNT                                
         ZIC   RF,LEVELC                                                        
         LA    R1,3(R3)                                                         
         AR    R1,RF               FIND POSITION OF PERSON CODE                 
         ZIC   RF,LVLDLEN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SRACCNT(0),0(R1)    MOVE PERSON INTO SREC ACCOUNT                
         LA    RE,SRACCNT                                                       
         AR    RE,RF               FIND OFFICE/DEPT/SUB LOCATION                
         LA    RE,1(RE)                                                         
*                                                                               
         ZIC   RF,LEVELC                                                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),3(R3)       COPY OFF/DPT CODE TO SREC                    
         B     PACC50                                                           
*                                                                               
         USING GPNELD,R4                                                        
PACC07   MVC   SRACCNT3,3(R3)      ACCOUNT                                      
         MVC   SRLASTN3,SPACES     INITIALIZE NAMES                             
         MVC   SRFIRST3,SPACES                                                  
         L     R4,ADACC                                                         
         MVI   ELCODE,GPNELQ                                                    
         BAS   RE,GETEL                                                         
         BNE   PACC50                                                           
         CLI   GPNTYP,1            LAST NAME?                                   
         BNE   PACC15                                                           
         ZIC   RF,GPNLN                                                         
         SH    RF,=H'4'                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SRLASTN3(0),GPNNME                                               
         BAS   RE,NEXTEL           GET FIRST NAME                               
         BNE   PACC50                                                           
*                                                                               
PACC15   ZIC   RF,GPNLN                                                         
         SH    RF,=H'4'                                                         
         EX    RF,*+8              HAS TO BE FIRST NAME                         
         B     *+10                                                             
         MVC   SRFIRST3(0),GPNNME                                               
*                                                                               
         USING NAMELD,R4                                                        
PACC50   L     R4,ADACC            GET PERSON'S NAME                            
         MVI   ELCODE,NAMELQ                                                    
         BAS   RE,GETEL                                                         
         BNE   PACC100                                                          
         ZIC   RF,NAMLN                                                         
         SH    RF,=H'3'                                                         
         MVC   SRPNAME,SPACES                                                   
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SRPNAME(0),NAMEREC                                               
*                                                                               
         CLI   QOPT1,C'N'           ONLY FOR NAME ORDER                         
         BE    PACC75                                                           
         CLI   QOPT1,C' '           ONLY FOR NAME ORDER                         
         BNE   PACC100                                                          
PACC75   CLC   SRLASTN3,SPACES     NEED AT LEAST A LAST NAME                    
         BNE   PACC100                                                          
         BAS   RE,EXTRNAME                                                      
*                                                                               
         USING ADRELD,R4                                                        
PACC100  L     R4,ADACC                                                         
         MVI   ELCODE,ADRELQ                                                    
         BAS   RE,GETEL                                                         
         BNE   PACC200                                                          
         MVC   SRLOCATN,ADRADD1                                                 
*                                                                               
         USING RSTELD,R4                                                        
PACC200  L     R4,ADACC                                                         
         MVI   ELCODE,RSTELQ                                                    
         BAS   RE,GETEL                                                         
         BNE   PACC300                                                          
         MVC   SRFILTER(2),RSTFILT1                                             
         MVC   SRFILTER+2(1),RSTFILT3                                           
         MVC   SRFILTER+3(1),RSTFILT4                                           
         MVC   SRFILTER+4(1),RSTFILT5                                           
*                                                                               
         USING EMPELD,R4                                                        
PACC300  L     R4,ADACC                                                         
         MVI   ELCODE,EMPELQ                                                    
         BAS   RE,GETEL                                                         
         BNE   PACC301                                                          
         MVC   SRTSLOCK,EMPLOCK                                                 
         MVC   SRSTATUS,EMPSTAT                                                 
         MVC   SRCSTAT,EMPCSTAT                                                 
*                                                                               
PACC301  CLI   QOPT2,C' '          CHECK OPTIONS                                
         BE    PACC310                                                          
         CLI   QOPT2,C'A'                                                       
         BNE   PACC305                                                          
         CLI   SRCSTAT,0           0 = ACTIVE                                   
         BNE   XIT                                                              
         B     PACC310                                                          
PACC305  CLI   QOPT2,C'T'                                                       
         BNE   PACC307                                                          
         CLI   SRCSTAT,EMPCTRM     TERMINATED                                   
         BNE   XIT                                                              
         B     PACC310                                                          
PACC307  CLI   QOPT2,C'L'                                                       
         BNE   PACC310                                                          
         CLI   SRCSTAT,EMPCLOA     LEAVE OF ABSENCE                             
         BNE   XIT                                                              
*                                                                               
PACC310  CLI   QOPT3,C'Y'                                                       
         BNE   PACC320                                                          
         TM    SRSTATUS,EMPSEXEC                                                
         BZ    XIT                                                              
*                                                                               
PACC320  CLI   QOPT4,C'Y'                                                       
         BNE   PACC330                                                          
         TM    SRSTATUS,EMPSPROD                                                
         BZ    XIT                                                              
*                                                                               
PACC330  CLI   QOPT5,C'Y'                                                       
         BNE   PACC340                                                          
         TM    SRSTATUS,EMPSJOB                                                 
         BZ    XIT                                                              
*                                                                               
PACC340  CLI   QOPT6,C'Y'                                                       
         BNE   PACC400                                                          
         TM    SRSTATUS,EMPSACT                                                 
         BZ    XIT                                                              
*                                                                               
PACC400  DS    0H                                                               
         GOTO1 ADSORTER,DMCB,=C'PUT',SREC                                       
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              EXTRACT NAME FROM X'20' ELEMENT                        *         
***********************************************************************         
         USING NAMELD,R4                                                        
EXTRNAME NTR1                                                                   
         LR    R0,RF               LENGTH - 1                                   
         LA    RF,1(RF)            GET ORIGINAL LENGTH                          
         LA    R1,SRLASTN3                                                      
         LA    R3,NAMEREC                                                       
*                                                                               
EXTR10   CLI   0(R3),C','                                                       
         BE    EXTRLLL             FINISH LAST NAME                             
         CLI   0(R3),C' '                                                       
         BE    EXTRFFF             IT WAS FIRST NAME                            
         MVC   0(1,R1),0(R3)       WE ASSUME IT IS LAST NAME                    
         LA    R1,1(R1)                                                         
         LA    R3,1(R3)                                                         
         BCTR  RF,0                                                             
         B     EXTR10                                                           
*                                                                               
EXTRFFF  MVC   SRLASTN3,SPACES     CAN'T USE IT                                 
         LA    R3,NAMEREC                                                       
         AR    R3,R0               R3 POINTS TO LAST CHAR                       
         SR    RF,RF                                                            
EXTR15   CLI   0(R3),C' '                                                       
         BE    EXTR20                                                           
         BCTR  R3,0                                                             
         LA    RF,1(RF)                                                         
         B     EXTR15                                                           
*                                                                               
EXTR20   BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SRLASTN3(0),1(R3)                                                
         BCTR  R3,0                                                             
         LA    R1,NAMEREC                                                       
         SR    R3,R1                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   SRFIRST3(0),NAMEREC                                              
         B     XIT                                                              
*                                                                               
EXTRLLL  LA    R3,2(R3)            WILL POINT TO FIRST NAME                     
         SH    RF,=H'3'                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SRFIRST3(0),0(R3)  COPY FIRST NAME                               
         B     XIT                                                              
***********************************************************************         
*              REQUEST LAST                                           *         
***********************************************************************         
*                                                                               
REQL     DS    0H                                                               
         MVC   SVPCODE,SPACES                                                   
         MVC   SVHIRED,SPACES                                                   
         MVC   SVTERMD,SPACES                                                   
         MVI   FIRST,C'Y'                                                       
*                                                                               
REQL10   GOTO1 ADSORTER,DMCB,=C'GET'                                            
         ICM   R2,15,4(R1)                                                      
         BZ    REQL900                                                          
*                                                                               
         USING SRECD,R2                                                         
         USING PERRECD,R3                                                       
         L     R3,AIO1                                                          
         MVC   SREC,0(R2)                                                       
*                                                                               
         MVC   TMPPCODE,SPACES                                                  
         MVC   TMPOFFIC,SPACES                                                  
*                                                                               
         CLI   QOPT1,C'P'                                                       
         BE    REQL11                                                           
         LA    R1,SRACCNT                                                       
         CLI   QOPT1,C'N'                                                       
         BE    *+12                                                             
         CLI   QOPT1,C' '                                                       
         BNE   *+8                                                              
         LA    R1,SRACCNT3                                                      
*                                                                               
         ZIC   RF,LEVELC                                                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   TMPOFFIC(0),0(R1)   OFFICE CODE                                  
         LA    RF,1(RF)                                                         
         AR    R1,RF                                                            
         ZIC   RF,LVLDLEN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   TMPPCODE(0),0(R1)   PERSON CODE                                  
         B     REQL12                                                           
*                                                                               
REQL11   ZIC   RF,LVLDLEN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   TMPPCODE(0),SRACCNT    PERSON CODE ORDER                         
*                                                                               
         LA    RF,1(RF)            RESTORE ORIG VALUE                           
         LA    R1,SRACCNT                                                       
         AR    R1,RF               FIND OFF/DPT IN ACCOUNT                      
         ZIC   RF,LEVELC                                                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   TMPOFFIC(0),0(R1)                                                
*                                                                               
REQL12   CLC   SVPCODE,TMPPCODE                                                 
         BE    REQL20                                                           
         MVC   SVPCODE,TMPPCODE                                                 
         MVC   SVHIRED,SPACES                                                   
         MVC   SVTERMD,SPACES                                                   
*                                                                               
         MVC   PERKEY,SPACES                                                    
         MVI   PERKTYP,PERKTYPQ    X'0F'                                        
         MVC   PERKCPY,RCCOMPFL    COMPANY CODE                                 
         MVC   PERKCODE,TMPPCODE                                                
*                                                                               
         GOTO1 DATAMGR,DMCB,DMREAD,ACCFIL,AIO1,AIO1                             
         TM    DMCB+8,X'10'        TEST RECORD NOT FOUND                        
         BO    REQL10              NEXT ONE                                     
*                                                                               
         USING EMPELD,R4                                                        
         LR    R4,R3                                                            
         MVI   ELCODE,EMPELQ                                                    
         BAS   RE,GETEL                                                         
         BNE   REQL20                                                           
         GOTO1 DATCON,DMCB,(1,EMPHIR),(5,SVHIRED)                               
         GOTO1 DATCON,DMCB,(1,EMPTRM),(5,SVTERMD)                               
*                                                                               
REQL20   LR    R4,R3                                                            
         USING LOCELD,R4                                                        
         MVI   ELCODE,LOCELQ       X'83'                                        
         BAS   RE,GETEL                                                         
         BNE   REQL10              CAN'T FIND IT, NEXT ONE                      
REQL30   ZIC   RF,LVLALEN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   LOCOFF(0),TMPOFFIC                                               
         BNE   REQL40                                                           
         LA    R1,TMPOFFIC                                                      
         AR    R1,RF                                                            
         LA    R1,1(R1)                                                         
*                                                                               
REQL35   ZIC   RF,LVLBLEN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   LOCDEPT(0),0(R1)                                                 
         BNE   REQL40                                                           
         AR    R1,RF                                                            
         LA    R1,1(R1)                                                         
*                                                                               
         ZIC   RF,LVLCLEN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   LOCSUB(0),0(R1)                                                  
         BE    REQL50                                                           
REQL40   BAS   RE,NEXTEL                                                        
         BNE   REQL10                                                           
         B     REQL30                                                           
*                                                                               
         USING PLINED,R6                                                        
REQL50   CLI   QOPT1,C'P'          DON'T SKIP LINE FOR PERSON ORDER             
         BE    REQL60                                                           
         CLI   QOPT1,C'N'                                                       
         BE    REQL60                                                           
         CLI   QOPT1,C' '                                                       
         BE    REQL60                                                           
*                                                                               
         CLI   FIRST,C'Y'                                                       
         BNE   REQL55                                                           
         MVI   FIRST,C'N'                                                       
         MVC   SVOFFICE,TMPOFFIC                                                
         B     REQL60                                                           
*                                                                               
REQL55   ZIC   RF,LEVELA           LENGTH OF OFFICE                             
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   SVOFFICE(0),TMPOFFIC                                             
         BE    REQL57                                                           
         MVI   FORCEHED,C'Y'       SKIP A PAGE IF OFFICE DIFFERENT              
         MVC   SVOFFICE,TMPOFFIC                                                
         B     REQL60                                                           
*                                                                               
REQL57   ZIC   RF,LEVELB           LENGTH OF OFFICE/DEPT                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   SVOFFICE(0),TMPOFFIC                                             
         BE    REQL60                                                           
         BAS   RE,PRNTXP           SKIP A LINE IF DEPT DIFFERENT                
         MVC   SVOFFICE,TMPOFFIC                                                
*                                                                               
REQL60   LA    R6,XP                                                            
         MVC   PLINCODE(2),=C'1R'                                               
         LA    R1,PLINCODE+2                                                    
         ZIC   RF,LEVELC                                                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),TMPOFFIC                                                 
         AR    R1,RF                                                            
         LA    R1,1(R1)                                                         
         ZIC   RF,LVLDLEN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),TMPPCODE                                                 
*                                                                               
         MVC   PLINNAME,SRPNAME                                                 
         MVC   PLINLOCN,SRLOCATN                                                
         MVC   PLINFILT,SRFILTER                                                
         MVC   PLINHDTE,SVHIRED                                                 
         MVC   PLINTDTE,SVTERMD                                                 
         GOTO1 DATCON,DMCB,(1,LOCSTART),(5,PLINSDTE)                            
         GOTO1 DATCON,DMCB,(1,LOCEND),(5,PLINEDTE)                              
         GOTO1 DATCON,DMCB,(1,SRTSLOCK),(5,PLINLDTE)                            
*                                                                               
         TM    SRSTATUS,EMPSEXEC                                                
         BZ    *+8                                                              
         MVI   PLINEXEC,C'Y'                                                    
         TM    SRSTATUS,EMPSPROD                                                
         BZ    *+8                                                              
         MVI   PLINPROD,C'Y'                                                    
         TM    SRSTATUS,EMPSJOB                                                 
         BZ    *+8                                                              
         MVI   PLINJOB,C'Y'                                                     
         TM    SRSTATUS,EMPSACT                                                 
         BZ    *+8                                                              
         MVI   PLINACT,C'Y'                                                     
         CLI   SRCSTAT,EMPCLOA                                                  
         BNE   *+8                                                              
         MVI   PLINSTAT,C'L'                                                    
         CLI   SRCSTAT,EMPCTRM                                                  
         BNE   *+8                                                              
         MVI   PLINSTAT,C'T'                                                    
         CLI   SRCSTAT,0           ACTIVE                                       
         BNE   *+8                                                              
         MVI   PLINSTAT,C'A'                                                    
*                                                                               
         BAS   RE,PRNTXP                                                        
         BAS   RE,NEXTEL                                                        
         BE    REQL30              ANY MORE LOCATION ELEMENTS?                  
         B     REQL10                                                           
*                                                                               
REQL900  GOTO1 ADSORTER,DMCB,=C'END'                                            
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
***********************************************************************         
*              PRINT A LINE                                           *         
***********************************************************************         
*                                                                               
PRNTXP   NTR1                                                                   
         MVC   XHEAD2+12(L'CMPABBR),CMPABBR                                     
         MVC   XHEAD2+21(L'CMPNAME),CMPNAME                                     
*                                                                               
         CLI   QOPT1,C'P'                                                       
         BE    PRNTXP3                                                          
         CLI   QOPT1,C'N'                                                       
         BE    PRNTXP4                                                          
         CLI   QOPT1,C' '                                                       
         BE    PRNTXP4                                                          
         MVC   XHEAD5+135(L'OFFORDR),OFFORDR                                    
         B     PRNTXP7                                                          
PRNTXP3  MVC   XHEAD5+135(L'PERORDR),PERORDR                                    
         B     PRNTXP7                                                          
*                                                                               
PRNTXP4  MVC   XHEAD5+135(L'NAMORDR),NAMORDR                                    
         B     PRNTXP7                                                          
*                                                                               
PRNTXP7  MVC   XHEAD4+12(2),SVOFFICE                                            
*                                                                               
PRNTX    GOTO1 ACREPORT                                                         
         B     XIT                                                              
***********************************************************************         
*              EXTERNAL ADDRESS LIST                                  *         
***********************************************************************         
*                                                                               
ADCONS   DS    0F                                                               
         DC    V(SORTER)                                                        
         DC    V(PRNTBL)                                                        
         DC    V(DATVAL)                                                        
         DC    V(SQUASHER)                                                      
         DC    V(CHOPCON)                                                       
         DC    V(HELLO)                                                         
         DC    A(IO1)                                                           
         EJECT                                                                  
***********************************************************************         
*              ELEMENT TABLES                                         *         
***********************************************************************         
*                                                                               
*        DS    CL1 = RECORD TYPES WHERE THIS ELEM MUST BE BUILT/UPDATED         
*                    X'10' = 1R RECORD     (CPY/U/L/OF/DPT/SDPT/PER)            
*                    X'01' = PERSON RECORD (X'0F'/CPY/PERSON)                   
*        DS    AL4 = ADDRESS OF ROUTINE TO HANDLE ELEMENT                       
*        DS    AL1 = ACTIONS WHEN THIS ROUTINE SHOULD BE EXECUTED               
*                    BUILD  = ONLY EXECUTE IF BUILDING A NEW RECORD             
*                    UPDATE = ONLY EXECUTE IF UPDATING AN OLD RECORD            
*                                                                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
*              LITERALS                                               *         
***********************************************************************         
*                                                                               
         LTORG                                                                  
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
*                                                                               
EFFS     DC    48X'FF'                                                          
ACCFIL   DC    CL8'ACCOUNT '                                                    
ACCBIG   DC    CL8'ACCBIG  '                                                    
*                                                                               
FIRST    DC    CL1' '              FIRST TIME PRINTING                          
SVPCODE  DC    CL12' '                                                          
SVOFFICE DC    CL12' '             SAVE OFFICE/DEPT                             
SVHIRED  DC    CL8' '                                                           
SVTERMD  DC    CL8' '                                                           
TMPOFFIC DC    CL12' '                                                          
TMPPCODE DC    CL12' '                                                          
*                                                                               
OFFORDR  DC    CL20'OFFICE/DEPT ORDER'                                          
PERORDR  DC    CL20'PERSON CODE ORDER'                                          
NAMORDR  DC    CL20'PERSON NAME ORDER'                                          
*                                                                               
SORTCARD DC    C'SORT FIELDS=(1,48,A),FORMAT=BI,WORK=1 '                        
RECCARD  DC    C'RECORD TYPE=F,LENGTH=99 '                                      
*                                                                               
         EJECT                                                                  
***********************************************************************         
*              BOX HOOK                                               *         
***********************************************************************         
*                                                                               
BXHOOK   DS    0D                                                               
         NMOD1 0,*BHOOK                                                         
         L     RC,BOXRC            RESTORE REG C                                
*                                                                               
         USING BOXD,R4                                                          
         L     R4,ADBOX                                                         
         MVC   BOXCOLS(165),XSPACES                                             
         MVC   BOXROWS,XSPACES                                                  
         MVI   BOXROWS+4,C'T'                                                   
         MVI   BOXROWS+9,C'M'                                                   
         MVI   BOXCOLS,C'L'                                                     
*                                                                               
         CLI   RCSUBPRG,0          REGULAR REPORT BOXES                         
         BNE   BX500                                                            
         MVI   BOXCOLS+16,C'C'                                                  
         MVI   BOXCOLS+23,C'C'                                                  
         MVI   BOXCOLS+61,C'C'                                                  
         MVI   BOXCOLS+68,C'C'                                                  
         MVI   BOXCOLS+78,C'C'                                                  
         MVI   BOXCOLS+88,C'C'                                                  
         MVI   BOXCOLS+91,C'C'                                                  
         MVI   BOXCOLS+94,C'C'                                                  
         MVI   BOXCOLS+97,C'C'                                                  
         MVI   BOXCOLS+100,C'C'                                                 
         MVI   BOXCOLS+103,C'C'                                                 
         MVI   BOXCOLS+113,C'C'                                                 
         MVI   BOXCOLS+123,C'C'                                                 
         MVI   BOXCOLS+133,C'R'                                                 
         B     BX500                                                            
*                                                                               
BX500    MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXBLANK,C'N'                                                    
         XMOD1 1                                                                
BOXRC    DC    A(0)                                                             
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*              BUFFERS                                                *         
***********************************************************************         
*                                                                               
IO1      DS    0D                  IOAREA #1                                    
         DS    2000C                                                            
MAXACC   EQU   *-IO1                                                            
*                                                                               
         EJECT                                                                  
***********************************************************************         
*              SORT RECORD DSECT                                      *         
***********************************************************************         
SRECD    DSECT                                                                  
SRKEY    DS    0CL48                                                            
SRACCNT  DS    CL12                ACCOUNT                                      
SRLASTN  DS    CL18                PERSON LAST NAME  / FOR SORTING              
SRFIRSTN DS    CL18                PERSON FIRST NAME / FOR SORTING              
SRPNAME  DS    CL36                PERSON NAME                                  
SRLOCATN DS    CL5                 LOCATION                                     
SRFILTER DS    CL5                 FILTERS                                      
SRTSLOCK DS    CL3                 TIME SHEET LOCK DATE                         
SRSTATUS DS    CL1                 STATUS BYTE (FROM X'56' ELEM)                
SRCSTAT  DS    CL1                 ACTIVE/TERM/ETC (FROM X'56')                 
*                                                                               
SREND    DS    0C                                                               
SRECLNQ  EQU   *-SRECD                                                          
*                                                                               
         ORG   SRKEY               SORT ORDER BY PERSON NAME                    
SRLASTN3 DS    CL18                PERSON LAST NAME                             
SRFIRST3 DS    CL18                PERSON FIRST NAME                            
SRACCNT3 DS    CL12                OFFICE/DEPT/SUB-DEPT                         
***********************************************************************         
*              PRINT LINE DSECT                                       *         
***********************************************************************         
*                                                                               
PLINED   DSECT                                                                  
PLIN     DS    0H                                                               
         DS    CL1                                                              
PLINCODE DS    CL14                STAFF CODE                                   
         DS    CL2                                                              
PLINFILT DS    CL5                 FILTERS                                      
         DS    CL2                                                              
PLINNAME DS    CL36                NAME                                         
         DS    CL2                                                              
PLINLOCN DS    CL5                 LOCATION                                     
         DS    CL2                                                              
PLINHDTE DS    CL8                 HIRE DATE                                    
         DS    CL2                                                              
PLINTDTE DS    CL8                 TERM DATE                                    
         DS    CL2                                                              
PLINSTAT DS    CL1                 STATUS                                       
         DS    CL2                                                              
PLINEXEC DS    CL1                 EXECUTIVE                                    
         DS    CL2                                                              
PLINPROD DS    CL1                 PROD REQUIRED                                
         DS    CL2                                                              
PLINJOB  DS    CL1                 JOB REQUIRED                                 
         DS    CL2                                                              
PLINACT  DS    CL1                 ACTUAL AS STANDARD HOURS                     
         DS    CL2                                                              
PLINLDTE DS    CL8                 LOCK DATE                                    
         DS    CL2                                                              
PLINSDTE DS    CL8                 START DATE                                   
         DS    CL2                                                              
PLINEDTE DS    CL8                 END DATE                                     
PLINLNQ  EQU   *-PLINED                                                         
*                                                                               
         EJECT                                                                  
***********************************************************************         
*              WORKING STORAGE                                        *         
***********************************************************************         
*                                                                               
ACCN02D  DSECT                                                                  
VTYPES   DS    0A                  EXTERNAL ADDRESSES                           
SORTER   DS    A                   SORTER                                       
PRNTBL   DS    A                   PRNTBL                                       
DATVAL   DS    A                   DATE VALIDATION                              
SQUASHER DS    A                   SQUASHER                                     
CHOPCON  DS    A                   CHOPPER                                      
HELLO    DS    A                   HELLO                                        
AIO1     DS    A                   IO AREA #1 (2000 BYTES)                      
VTYPLNQ  EQU   *-VTYPES                                                         
ADBOX    DS    A                   ADDRESS OF BOX ROUTINE                       
CMPABBR  DS    CL7                 COMPANY ABBREVIATION                         
CMPNAME  DS    CL36                COMPANY NAME                                 
ELCODE   DS    XL1                                                              
LAP      DS    XL1                                                              
COMMAND  DS    CL8                 USED IN DATAMGR CALL                         
TODAYP   DS    PL3                 TODAY'S DATE PACKED                          
ELEM     DS    CL255               ELEMENT BUFFER                               
*                                                                               
SVACT    DS    CL12                                                             
SVPERSN  DS    CL12                                                             
SVKEY    DS    CL42                                                             
LASTTOT  DS    PL8                                                              
*                                                                               
LEVELS   DS    0H                                                               
LEVELA   DS    CL1                 LENGTH OF LEVEL A                            
LEVELB   DS    CL1                 LENGTH OF LEVEL B                            
LEVELC   DS    CL1                 LENGTH OF LEVEL C                            
LEVELD   DS    CL1                 LENGTH OF LEVEL D                            
*                                                                               
LEVELNM  DS    0CL15                                                            
LEVELANM DS    CL15                LEDGER LEVEL NAMES (HIERARCHY)               
LEVELBNM DS    CL15                                                             
LEVELCNM DS    CL15                                                             
LEVELDNM DS    CL15                                                             
*                                                                               
RECORD   DS    XL1                 RECORD TYPE                                  
RECSTAFF EQU   X'10'               1R STAFF RECORD                              
RECPERSN EQU   X'01'               X'0F' PERSON RECORD                          
RECHIST  EQU   X'02'               SALARY HISTORY RECORD                        
*                                                                               
ACTION   DS    XL1                 MODE FOR CURRENT INPUT RECORD                
BUILD    EQU   X'80'               BUILD A NEW RECORD                           
UPDATE   EQU   X'40'               UPDATE AN OLD RECORD                         
*                                                                               
LVLALEN  DS    XL1                                                              
LVLBLEN  DS    XL1                                                              
LVLCLEN  DS    XL1                                                              
LVLDLEN  DS    XL1                                                              
*                                                                               
SREC     DS    CL(SRECLNQ)                                                      
WRK2     DS    CL120                                                            
         EJECT                                                                  
***********************************************************************         
*              OTHER INCLUDES                                         *         
***********************************************************************         
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDREPXTRAD                                                     
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE ACBIGPRNTD                                                     
       ++INCLUDE DDBOXEQUS                                                      
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDLOGOD                                                        
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'041ACREPCN02 04/09/15'                                      
         END                                                                    
