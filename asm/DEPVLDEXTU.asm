*          DATA SET DEPVLDEXTU AT LEVEL 055 AS OF 11/20/07                      
*PHASE DEPVLEUA PVLDEXTU                                                        
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE NETWEEK                                                                
*INCLUDE GETDAY                                                                 
*INCLUDE ADDAY                                                                  
         TITLE 'PVLDEXTU-OANA''S DUMP AND LOAD MODULE'                          
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
* P7=A(DATAMGR)                                                                 
*                                                                               
         PRINT NOGEN                                                            
PVLDEXT  CSECT                                                                  
         NMOD1 WORKX-WORKD,*PVLDEXT,RR=R5                                       
*&&DO                                                                           
VERIFY   B     VERIFYX        BRANCH AROUND CONSTANTS                           
         DC    C'*VERIFY*'    IDENTIFIER SHOWING VERIFY DATA START              
         DC    C'070725'      EXTERNAL INVALID BEFORE THIS DATE                 
         DC    C'070727'      EXTERNAL INVALID AFTER THIS DATE                  
VERIFYX  DS    0H                                                               
*&&                                                                             
*                                                                               
         USING WORKD,RC                                                         
                                                                                
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
PVINIT   L     RE,=A(COUNTS)                                                    
         L     RF,=F'15000'                                                     
         XCEF                                                                   
**       OPEN  (FILOUT,(OUTPUT))                                                
                                                                                
         B     PVXIT                                                            
         SPACE 2                                                                
PVXIT    XMOD1 1                                                                
         EJECT                                                                  
**********************************************************************          
* DELETE ALL ACM CABLE DATA, AND NTI CABLE FOR LIVE+1, LIVE+2, LIVE+3           
* DELETE NTI WEEKLY CABLE DATA - LIVE                                           
**********************************************************************          
PVXREC   DS    0H                                                               
         L     R3,AREC             POINT TO RECORD                              
                                                                                
         CLC   0(3,R3),KEYSAVE                                                  
         BE    RECDS                                                            
         MVC   P,SPACES                                                         
         MVC   P(3),0(R3)                                                       
         GOTO1 VPRINTER                                                         
         MVC   KEYSAVE,0(R3)                                                    
                                                                                
RECDS    DS    0H                                                               
*                                                                               
REC10    DS    0X                                                               
*        CLI   0(R3),C'J'                                                       
*        BE    JREC                                                             
**                                                                              
REC15    CLI   1(R3),X'83'         LOWER CASE 'C': CABLE ACM                    
         BE    RECDEL              DELETE ALL RECORDS FOR CABLE ACM             
*                                                                               
REC20    CLI   1(R3),C'C'          DELETE ALL RECORDS FOR CABLE NTI             
         BNE   PVXKEEP             FROM THE ACM TAPES                           
         CLI   2(R3),SRCLIVE1      VIEWING TYPES LIVE+1                         
         BE    RECDEL                                                           
         CLI   2(R3),SRCLIVE2                    LIVE+2                         
         BE    RECDEL                                                           
         CLI   2(R3),SRCLIVE3                    LIVE+3                         
         BE    RECDEL                                                           
         CLI   2(R3),SRCALV                      A/LIVE                         
         BE    RECDEL                                                           
         CLI   2(R3),SRCALS                      A/LIVE+SD                      
         BE    RECDEL                                                           
         CLI   2(R3),SRCAL7                      A/LIVE+7                       
         BE    RECDEL                                                           
         B     PVXKEEP             KEEP EVERYTHING ELSE                         
*                                                                               
RECDEL   CLI   0(R3),C'N'                                                       
         BNE   RECDELJ                                                          
         USING PNKEY,R3                                                         
         MVC   DUB(3),PNKEY                                                     
         MVC   DUB+3(2),PNBOOK                                                  
         B     PVXREC51            DELETE                                       
         DROP  R3                                                               
RECDELJ  CLI   0(R3),C'J'                                                       
         BNE   RECDELP                                                          
         USING PJKEY,R3                                                         
         MVC   DUB(3),PJKEY                                                     
         MVC   DUB+3(2),PJBOOK                                                  
         B     PVXREC51            DELETE                                       
         DROP  R3                                                               
RECDELP  CLI   0(R3),C'P'                                                       
         BNE   RECDELQ                                                          
         USING PRKEY,R3                                                         
         MVC   DUB(3),PRKEY                                                     
         MVC   DUB+3(2),PRBOOK                                                  
         B     PVXREC51            DELETE                                       
         DROP  R3                                                               
RECDELQ  CLI   0(R3),C'Q'                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PMKEY,R3                                                         
         MVC   DUB(3),PMKEY                                                     
         MVC   DUB+3(2),PMBOOK                                                  
         B     PVXREC51            DELETE                                       
         DROP  R3                                                               
**                                                                              
         USING PJKEY,R3                                                         
JREC     MVC   DUB(3),PJKEY                                                     
         MVC   DUB+3(2),PJBOOK                                                  
         CLI   PJMEDIA,C'C'        CABLE                                        
         BNE   PVXKEEP                                                          
         CLI   PJSRC,C'N'                                                       
         BNE   PVXKEEP                                                          
         TM    PJSTAT,X'F0'                                                     
         BNO   PVXKEEP             NUMERIC STATION                              
         OC    PJBOOK,PJBOOK       NO BOOK                                      
         BNZ   PVXKEEP                                                          
         B     PVXREC51            DELETE                                       
         DROP  R3                                                               
*                                                                               
**********************************************************************          
*                                                                               
*&&DO                                                                           
         USING PJKEY,R3                                                         
JREC     CLI   PJMEDIA,C'C'        CABLE                                        
         BNE   PVXKEEP                                                          
         MVC   DUB(3),PJKEY                                                     
         MVC   DUB+3(2),PJBOOK                                                  
         CLI   PJSRC,C'N'                                                       
         BNE   PVXKEEP                                                          
*        CLI   PJSTAT+4,C'C'                                                    
*        BNE   PVXKEEP                                                          
         TM    PJSTAT,X'F0'        NUMERIC STATION                              
         BNO   PVXKEEP                                                          
         CLC   PJBOOK,=X'6901'                                                  
         BNE   PVXKEEP                                                          
         CLI   PJBTYPE,0                                                        
         BNE   PVXKEEP                                                          
         B     PVXREC51            DELETE                                       
                                                                                
         USING PRKEY,R3                                                         
PREC     CLI   PRMEDIA,C'C'        CABLE                                        
         BNE   PVXKEEP                                                          
         MVC   DUB(3),PRKEY                                                     
         MVC   DUB+3(2),PRBOOK                                                  
         CLI   PRSRC,C'N'                                                       
         BNE   PVXKEEP                                                          
         CLC   PRBOOK,=X'6901'                                                  
         BNE   PVXKEEP                                                          
         CLC   =C'8253',PRSTAT                                                  
         BNE   PVXKEEP                                                          
*        CLI   PRSTAT+4,C'C'                                                    
*        BE    *+12                                                             
*        CLI   PRSTAT+4,C'U'                                                    
*        BNE   PVXKEEP                                                          
         CLI   PRBTYP,0                                                         
         BNE   PVXKEEP                                                          
         B     PVXREC51            DELETE                                       
                                                                                
                                                                                
         USING PNKEY,R3                                                         
NREC     CLI   PNMEDIA,C'C'        CABLE                                        
         BNE   PVXKEEP                                                          
         MVC   DUB(3),PNKEY                                                     
         MVC   DUB+3(2),PNBOOK                                                  
         CLI   PNSRC,C'N'                                                       
         BNE   PVXKEEP                                                          
         CLI   PNBTYP,0                                                         
         BNE   PVXKEEP                                                          
         CLC   PNBOOK,=X'6901'                                                  
         BNE   PVXKEEP                                                          
         TM    PNSTAT,X'F0'        NUMERIC STATION                              
         BNO   PVXKEEP                                                          
*        CLI   PNSTAT+4,C'C'                                                    
*        BNE   PVXKEEP                                                          
         B     PVXREC51            DELETE                                       
*&&                                                                             
*&&DO                                                                           
**********************************************************************          
* FIX DUPLICATE NTI NUMBERS ASSIGNED FOR SYNDICATION (12/10/2004)               
**********************************************************************          
PVXREC   DS    0H                                                               
         L     R3,AREC             POINT TO RECORD                              
                                                                                
         CLC   0(3,R3),KEYSAVE                                                  
         BE    RECDS                                                            
         MVC   P,SPACES                                                         
         MVC   P(3),0(R3)                                                       
         GOTO1 VPRINTER                                                         
         MVC   KEYSAVE,0(R3)                                                    
                                                                                
RECDS    DS    0H                                                               
         L     RE,=A(STBOOK)                                                    
         MVC   STBK,0(RE)                                                       
         CLI   0(R3),C'K'                                                       
         BE    KREC                                                             
         CLI   0(R3),C'J'                                                       
         BE    JREC                                                             
         CLI   0(R3),C'N'                                                       
         BE    NREC                                                             
         B     PVXKEEP                                                          
                                                                                
                                                                                
         USING PJKEY,R3                                                         
JREC     CLI   PJMEDIA,C'N'        NET                                          
         BNE   PVXKEEP                                                          
         MVC   DUB(3),PJKEY                                                     
         MVC   DUB+3(2),PJBOOK                                                  
         CLI   PJSRC,C'N'          NIELSEN                                      
         BNE   PVXKEEP                                                          
         CLC   PJSTAT,=C'PPPPS'    SYNDICATION                                  
         BNE   JREC100                                                          
         OC    PJBOOK,PJBOOK                                                    
         BNZ   PVXKEEP                                                          
         CLI   PJBTYPE,0                                                        
         BNE   PVXKEEP                                                          
                                                                                
         L     R4,=A(NUMTABS)                                                   
         USING NUMTABD,R4                                                       
JREC10   CLI   0(R4),X'FF'                                                      
         BE    PVXKEEP             PROGRAM NOT IN TABLE                         
         CLC   OLDNTI,PJINTNUM                                                  
         BNE   JREC14                                                           
         MVC   PACK16(10),NTILONG                                               
         MVI   PACK16+10,C'0'                                                   
         PACK  DUB,PACK16(11)                                                   
         MVC   WORK(5),DUB+2                                                    
         CLC   PJEXTNUM,WORK                                                    
         BE    PVXREC51            DELETE                                       
                                                                                
JREC14   LA    R4,DATELIST-NUMTABD(R4)                                          
JREC15   CLI   0(R4),0                                                          
         BE    JREC20                                                           
         CLI   0(R4),1                                                          
         BNE   *+8                                                              
         LA    R4,1(R4)                                                         
         LA    R4,L'DATELIST(R4)                                                
         B     JREC15                                                           
                                                                                
JREC20   LA    R4,1(R4)                                                         
         B     JREC10                                                           
         DROP  R4                                                               
                                                                                
JREC100  CLC   PJSTAT,=C'PPPPN'    NETWORK                                      
         BNE   PVXKEEP                                                          
         OC    PJBOOK,PJBOOK                                                    
         BNZ   PVXKEEP                                                          
         CLI   PJBTYPE,0                                                        
         BNE   PVXKEEP                                                          
         CLC   PJINTNUM,=AL2(8923)                                              
         BNE   PVXKEEP                                                          
         MVC   PACK16(10),=C'0000120080'                                        
         MVI   PACK16+10,C'0'                                                   
         PACK  DUB,PACK16(11)                                                   
         MVC   WORK(5),DUB+2                                                    
         CLC   PJEXTNUM,WORK                                                    
         BE    PVXREC51            DELETE                                       
         B     PVXKEEP                                                          
                                                                                
                                                                                
                                                                                
                                                                                
         USING PKKEY,R3                                                         
KREC     CLI   PKMEDIA,C'N'        NET                                          
         BNE   PVXKEEP                                                          
         MVC   DUB(3),PKKEY                                                     
         MVC   DUB+3(2),PKBOOK                                                  
         CLI   PKSRC,C'N'          NIELSEN                                      
         BNE   PVXKEEP                                                          
         CLI   PKSTA+4,C'S'        SYNDICATION                                  
         BNE   PVXKEEP                                                          
*        CLI   PKBTYP,C'A'                                                      
*        BNE   PVXKEEP                                                          
         CLI   PKBTYP,C'U'                                                      
         BE    PVXKEEP                                                          
         CLI   PKBTYP,C'T'                                                      
         BE    PVXKEEP                                                          
         CLI   PKBTYP,C'V'                                                      
         BE    PVXKEEP                                                          
         CLI   PKBTYP,C'W'                                                      
         BE    PVXKEEP                                                          
                                                                                
         L     R4,=A(NUMTABS)                                                   
         USING NUMTABD,R4                                                       
KREC10   CLI   0(R4),X'FF'                                                      
         BE    PVXKEEP             PROGRAM NOT IN TABLE                         
         CLC   OLDNTI,PKPNUM+1                                                  
         BNE   KREC14                                                           
         CLI   NETFLAG,DIFFNET                                                  
         BNE   PVXKEEP                                                          
         CLC   NETW,PKSTA                                                       
         BE    PVXREC51            DELETE                                       
                                                                                
KREC14   LA    R4,DATELIST-NUMTABD(R4)                                          
KREC15   CLI   0(R4),0                                                          
         BE    KREC20                                                           
         CLI   0(R4),1                                                          
         BNE   *+8                                                              
         LA    R4,1(R4)                                                         
         LA    R4,L'DATELIST(R4)                                                
         B     KREC15                                                           
                                                                                
KREC20   LA    R4,1(R4)                                                         
         B     KREC10                                                           
                                                                                
         DROP  R4                                                               
                                                                                
                                                                                
         USING PNKEY,R3                                                         
NREC     CLI   PNMEDIA,C'N'        NET                                          
         BNE   PVXKEEP                                                          
         MVC   DUB(3),PNKEY                                                     
         MVC   DUB+3(2),PNBOOK                                                  
         CLI   PNSRC,C'N'          NIELSEN                                      
         BNE   PVXKEEP                                                          
*        CLI   PNBTYP,C'A'                                                      
*        BNE   PVXKEEP                                                          
         CLI   PNBTYP,C'U'                                                      
         BE    PVXKEEP                                                          
         CLI   PNBTYP,C'T'                                                      
         BE    PVXKEEP                                                          
         CLI   PNBTYP,C'V'                                                      
         BE    PVXKEEP                                                          
         CLI   PNBTYP,C'W'                                                      
         BE    PVXKEEP                                                          
         CLC   PNBOOK,STBK                                                      
         BL    PVXKEEP                                                          
         CLI   PNSTAT+4,C'S'                                                    
         BNE   PVXKEEP                                                          
                                                                                
         L     R4,=A(NUMTABS)                                                   
         USING NUMTABD,R4                                                       
NREC10   CLI   0(R4),X'FF'                                                      
         BE    PVXKEEP             PROGRAM NOT IN TABLE                         
         CLC   OLDNTI,PNPNUM                                                    
         BE    *+18                                                             
         CLC   NEWNTI,PNPNUM                                                    
         BNE   NREC14                                                           
         B     NREC50                                                           
         CLI   NETFLAG,DIFFNET                                                  
         BNE   NREC11                                                           
         CLC   PNSTAT,NETW                                                      
         BE    PVXREC51            DELETE SAME NET W/OLD NTI#                   
         B     PVXKEEP             KEEP DIFF NET W/OLD NTI# UNCHANGED           
                                                                                
NREC11   LA    R6,DATELIST         DELETE PTRS FOR BOOKS THAT DON'T             
         CLI   0(R6),0             APPLY TO OLD NTI#                            
         BE    NREC50                                                           
         CLI   0(R6),1                                                          
         BNE   *+8                                                              
         LA    R6,1(R6)                                                         
NREC12   CLI   0(R6),0                                                          
         BE    NREC50                                                           
         GOTO1 VNETWEEK,DMCB,0(R6),VGETDAY,VADDAY                               
         CLC   PNBOOK(1),DMCB+4    YEAR                                         
         BNE   NREC13                                                           
         CLC   PNBOOK+1(1),DMCB+12 WEEK                                         
         BE    PVXREC51            PURGE                                        
                                                                                
NREC13   LA    R6,6(R6)                                                         
         B     NREC12                                                           
                                                                                
NREC14   LA    R4,DATELIST-NUMTABD(R4)                                          
NREC15   CLI   0(R4),0                                                          
         BE    NREC20                                                           
         CLI   0(R4),1                                                          
         BNE   *+8                                                              
         LA    R4,1(R4)                                                         
         LA    R4,L'DATELIST(R4)                                                
         B     NREC15                                                           
                                                                                
NREC20   LA    R4,1(R4)                                                         
         B     NREC10                                                           
         DROP  R4                                                               
                                                                                
NREC50   BAS   RE,CKNDEL           SEE IF WE HAVE PROG THAT MATCHES DAY         
         BNE   PVXREC51                                                         
         B     PVXKEEP                                                          
                                                                                
CKNDEL   NTR1                      R3->PNKEY                                    
                                                                                
         OPEN  (TAPNDEL,(INPUT))                                                
                                                                                
CKNDEL10 GET   TAPNDEL,IO                                                       
         CLC   PNKMAJOR,IO+4                                                    
         BE    CKDAYMN                                                          
         B     CKNDEL10                                                         
                                                                                
CKDAYMY  CLOSE (TAPNDEL)                                                        
         CR    RB,RB                                                            
         B     XIT                                                              
CKDAYMN  CLOSE (TAPNDEL)                                                        
         CHI   RB,0                                                             
         B     XIT                                                              
XIT      XIT1                                                                   
                                                                                
                                                                                
PRINTCM1 NTR1                                                                   
         GOTO1 =V(HEXOUT),DMCB,0(R3),P,25,0                                     
         EDIT  PNPNUM,(5,P),FILL=0,ZERO=NOBLANK                                 
         GOTO1 VPRINTER                                                         
         B     XIT                                                              
PRINTCM2 NTR1                                                                   
         GOTO1 =V(HEXOUT),DMCB,0(R3),P,25,0                                     
         MVC   P+40(7),=C'VARIOUS'                                              
         GOTO1 VPRINTER                                                         
         B     XIT                                                              
*&&                                                                             
         EJECT                                                                  
**********************************************************************          
* REMOVE BAD J-POINTERS FOR CABLE MOVIE GOER (12/03/2004)                       
* BAD POINTERS ARE FOR 10/11/2004 (BET,DHLT)                                    
**********************************************************************          
*&&DO                                                                           
PVXREC   DS    0H                                                               
         L     R3,AREC             POINT TO RECORD                              
         CLI   0(R3),C'P'                                                       
         BE    PREC                                                             
         CLI   0(R3),C'Q'                                                       
         BE    QREC                                                             
         CLI   0(R3),C'N'                                                       
         BE    NREC                                                             
         CLI   0(R3),C'J'                                                       
         BE    JREC                                                             
         CLI   0(R3),C'K'                                                       
         BE    KREC                                                             
         CLI   0(R3),C'C'          CORRECTION RECORDS... JUST IN CASE           
         BE    CREC                                                             
         B     PVXKEEP                                                          
                                                                                
         USING PRKEY,R3                                                         
PREC     B     PVXKEEP                                                          
                                                                                
                                                                                
         USING PMKEY,R3                                                         
QREC     B     PVXKEEP                                                          
                                                                                
                                                                                
         USING PNKEY,R3                                                         
NREC     B     PVXKEEP                                                          
                                                                                
                                                                                
         USING PJKEY,R3                                                         
JREC     CLI   PJMEDIA,C'C'        CABLE                                        
         BNE   PVXKEEP                                                          
         MVC   DUB(3),PJKEY                                                     
         MVC   DUB+3(2),PJBOOK                                                  
         CLI   PJSRC,C'N'          NET                                          
         BNE   PVXKEEP                                                          
         CLI   PJBTYPE,C'M'        MOVIE GOER                                   
         BNE   PVXKEEP                                                          
         OC    PJEXTNUM,PJEXTNUM                                                
         BZ    PVXKEEP                                                          
                                                                                
         OC    PJBOOK,PJBOOK                                                    
         BZ    PVXKEEP                                                          
         CLC   PJBOOK,=X'682A'     10/11/2004                                   
         BNE   JREC30                                                           
                                                                                
         CLC   PJSTAT,=C'6200C'    BET                                          
         BNE   JREC10                                                           
         MVC   HALF,PJINTNUM                                                    
         XC    HALF,=X'FFFF'                                                    
         SR    R0,R0                                                            
         ICM   R0,3,HALF                                                        
         CHI   R0,X'3F'                                                         
         BL    PVXREC51                                                         
         B     PVXKEEP                                                          
                                                                                
JREC10   CLC   PJSTAT,=C'8255C'    DHLT                                         
         BNE   JREC20                                                           
         MVC   HALF,PJINTNUM                                                    
         XC    HALF,=X'FFFF'                                                    
         SR    R0,R0                                                            
         ICM   R0,3,HALF                                                        
         CHI   R0,X'41'                                                         
         BL    PVXREC51                                                         
         B     PVXKEEP                                                          
JREC20   DS    0H                                                               
JREC30   B     PVXKEEP                                                          
                                                                                
                                                                                
         USING PKKEY,R3                                                         
KREC     B     PVXKEEP                                                          
                                                                                
                                                                                
         USING CHKEY,R3                                                         
CREC     B     PVXKEEP                                                          
*&&                                                                             
         EJECT                                                                  
                                                                                
                                                                                
PVXREC2  L     R1,KEEPS                                                         
         LA    R1,1(R1)                                                         
         ST    R1,KEEPS                                                         
         SR    R0,R0                                                            
         D     R0,=F'20'           DUMP 1 OF EVERY 20 RECORDS                   
         LTR   R0,R0               TEST FOR REMAINDER                           
         BNZ   PVXREC4             YES                                          
         MVC   HALF,20(R3)         EXTRACT RECORD LENGTH                        
         LH    R5,HALF                                                          
         CLI   HALF,X'FF'          TEST FOR PASSIVE RECORD                      
         BNE   *+8                                                              
         LA    R5,23               LENGTH OF PASSIVE RECORD                     
         LA    R2,L'HEAD                                                        
         MVC   WORK(L'HEAD),HEAD                                                
         GOTO1 VPRNTBL,DMCB,((R2),WORK),(R3),C'DUMP',(R5),=C'2D'                
         SPACE                                                                  
PVXREC4  B     PVXKEEP                                                          
         EJECT                                                                  
         USING PRKEY,R3                                                         
PVXREC5  MVC   DUB(3),PRKEY        SAVE CODE/MEDIA/SOURCE                       
         MVC   DUB+3(2),PRBOOK     SAVE BOOK                                    
PVXREC51 DS    0H                                                               
         MVC   IO(2),=AL2(27)                                                   
         XC    IO+2(2),IO+2                                                     
         MVC   IO+4(23),0(R3)                                                   
**       PUT   FILOUT,IO                                                        
*                                                                               
*        GOTO1 =V(HEXOUT),DMCB,0(R3),P,25,0                                     
*        MVC   P+60(25),0(R3)                                                   
*        GOTO1 VPRINTER                                                         
*                                                                               
         L     RE,=A(COUNTS)                                                    
PVXREC5A OC    0(5,RE),0(RE)       OPEN SLOT                                    
         BZ    PVXREC5B                                                         
         CLC   DUB(5),0(RE)        FIND THE SLOT                                
         BE    PVXREC5C                                                         
         LA    RE,9(RE)                                                         
         B     PVXREC5A                                                         
PVXREC5B MVC   0(5,RE),DUB         SET KEY IN TABLE                             
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
**       CLOSE FILOUT                                                           
         L     R2,=A(COUNTS)                                                    
PVXEOF1  OC    0(5,R2),0(R2)                                                    
         BZ    PVXIT                                                            
         SR    R9,R9                                                            
         ICM   R9,15,5(R2)                                                      
         MVC   P(3),0(R2)          PRINT CODE/MEDIA/SOURCE                      
         EDIT  (R9),(8,P+10)       PRINT NO OF RECORDS                          
         ZIC   R9,3(R2)                                                         
         EDIT  (R9),(2,P+4)        PRINT YEAR                                   
         ZIC   R9,4(R2)                                                         
         EDIT  (R9),(2,P+6)        PRINT WEEK                                   
         GOTO1 VPRINTER                                                         
         LA    R2,9(R2)                                                         
         B     PVXEOF1                                                          
         EJECT                                                                  
                                                                                
         LTORG                                                                  
         GETEL R6,DATADISP,ELCODE                                               
                                                                                
*&&DO                                                                           
FILOUT   DCB   DDNAME=FILOUT,DSORG=PS,RECFM=VB,LRECL=2000,BLKSIZE=8200,*        
               MACRF=PM                                                         
*&&                                                                             
                                                                                
*&&DO                                                                           
TAPNDEL  DCB   DDNAME=TAPNDEL,DSORG=PS,EODAD=CKDAYMY,RECFM=VB,         *        
               LRECL=02000,BLKSIZE=08200,MACRF=GM                               
*&&                                                                             
DAYTAB   DC    X'10',X'40'                                                      
         DC    X'20',X'20'                                                      
         DC    X'30',X'10'                                                      
         DC    X'40',X'08'                                                      
         DC    X'50',X'04'                                                      
         DC    X'60',X'02'                                                      
         DC    X'70',X'01'                                                      
         DC    X'90',X'90'                                                      
         DC    X'FF'                                                            
*                                                                               
STBOOK   DC    X'680D'        START WITH MAR22/04                               
                                                                                
VNETWEEK DC    V(NETWEEK)                                                       
VGETDAY  DC    V(GETDAY)                                                        
VADDAY   DC    V(ADDAY)                                                         
KEYSAVE  DC    XL23'0'                                                          
KEEPS    DC    F'0'                                                             
HEAD     DC    C'**RECORD**'                                                    
IO       DS    XL2004                                                           
COUNTS   DS    15000C                                                           
                                                                                
* TABLE OF PROGRAMS TO FIX AND DSECT                                            
       ++INCLUDE DEDUPLFIXT                                                     
                                                                                
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
APARM    DS    A                                                                
PLIST    DS    0CL28                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
VDATAMGR DS    A                                                                
DUB2     DS    D                                                                
PACK16   DS    PL16                                                             
DATADISP DS    H                                                                
ELCODE   DS    X                                                                
RECLNTI  DS    CL5                                                              
RECNTI   DS    HL2                                                              
STBK     DS    XL2                                                              
MYKEY    DS    XL18                                                             
MYKEYSV  DS    XL18                                                             
BYTE     DS    X                                                                
COMMAND  DS    CL8                                                              
SVDA     DS    A                                                                
SVSTATUS DS    X                                                                
                                                                                
VPRNTBL  DS    A                                                                
HALF     DS    H                                                                
WORK     DS    CL64                                                             
WORKX    EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
       ++INCLUDE DEDEMFILE                                                      
       ++INCLUDE DEDEMTABD                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'055DEPVLDEXTU11/20/07'                                      
         END                                                                    
