*          DATA SET SRPQU01    AT LEVEL 006 AS OF 08/25/20                      
*PHASE T13101A                                                                  
         TITLE '$PQ - PRINTER CONTROLLER'                                       
         PRINT NOGEN                                                            
PQ01     CSECT                                                                  
SRPQU01  NMOD1 000,**$PQ1**,R8,R9,RR=R4,CLEAR=YES                               
         LR    RC,R1                                                            
         USING PQUWKD,RC                                                        
         ST    R4,RELO                                                          
*                                                                               
         SAM31 ,                   THIS IS NOW IN 31-BIT MODE                   
*                                                                               
         LARL  RA,COMMON                                                        
         USING COMMON,RA                                                        
*                                                                               
         L     R1,APARM                                                         
         L     R2,12(R1)                                                        
         USING COMFACSD,R2         R2=A(COM FAC LIST)                           
         L     R3,20(R1)                                                        
         USING SRPQUFFD,R3         R3=A(TWA)                                    
         L     R2,ASAVESTR                                                      
         USING PQSAVED,R2                                                       
*                                                                               
         TM    INTFLAG,INTRUN      IF INTERNAL IGNORE THIS BIT                  
         BO    P3VAL                                                            
         MVCDD SRVP3A,SR#PRRID                                                  
         MVCDD SRVP4A,SR#PRROP                                                  
         MVCDD SRVPFK,SR#PQU29                                                  
         MVCDD SRVHD1,SR#PQU27                                                  
**NOP    MVC   SRVHD2,DC@HDR2                                                   
*                                                                               
P3VAL    DS    0H                  P3=PRINTER NUM                               
         LA    R4,SRVP3H                                                        
         ST    R4,CURSOR                                                        
         USING FLDHDRD,R4          R4=A(SCR FLD HDR)                            
         XC    APRN,APRN                                                        
         XC    PRNNAME,PRNNAME     CLEAR PRINTER-NAME FIELD                     
         MVI   PNMLEN,0                                                         
         MVI   PNUMFLAG,0                                                       
         MVI   PRNNUM,1            DEFAULT PRINTER NUM TO 1                     
         MVC   PRNID,LOGONID       DEFAULT PRINTER LOCN TO LOGON ID             
         CLI   FLDILEN,0                                                        
         BNE   P3V0                                                             
         L     RF,ATIA             GET DEFAULT FROM SAVE PAGE                   
         USING SRSD,RF                                                          
         OC    SRTRMPID,SRTRMPID                                                
         BZ    P3V1                                                             
         MVC   FLDDATA(12),SRTRMPID                                             
         LA    R1,FLDDATA+12                                                    
         CLI   0(R1),0                                                          
         BNE   *+8                                                              
         BCT   R1,*-8                                                           
         LA    R1,1(R1)                                                         
         LA    RF,FLDDATA                                                       
         SR    R1,RF                                                            
         STC   R1,FLDILEN                                                       
         DROP  RF                                                               
*                                                                               
P3V0     TM    DDS,DDSTRM          A DDS TRM CAN SPECIFY LUID                   
         BE    P3V0A                                                            
         TM    FLDIIND,X'08'       ASSUME LINE/ADDR IF ALPHA AND NO             
         BO    P3V0A                                                            
         SR    R0,R0                                                            
         IC    R0,FLDILEN                                                       
         LA    R1,FLDDATA                                                       
         CLI   0(R1),C','                                                       
         BE    P3V0A                                                            
         LA    R1,1(R1)                                                         
         BCT   R0,*-12                                                          
         LA    R0,X'60'            SET DO NOT SEARCH UTL/READREC                
         XC    DMCB(16),DMCB                                                    
         GOTO1 ATERMVAL,DMCB,((R0),(R4))                                        
         TM    DMCB,X'40'                                                       
         BO    P3V0A               INVALID INPUT FORMAT                         
         CLI   FLDILEN,4           IF 4 CHARACTERS OR LESS                      
         BH    *+12                                                             
         TM    DMCB,X'20'          AND TERMINAL RECORD NOT FOUND                
         BO    P3V0A               ASSUME PRINTER-NAME ENTERED                  
         SR    R5,R5                                                            
         ICM   R5,7,DMCB+1                                                      
         MVC   PRNSYM,0(R5)        SAVE STANDARD FORMAT                         
         B     P3VD                                                             
*                                                                               
P3V0A    L     R6,ACIREC                                                        
         GOTO1 ASCANNER,DMCB,(R4),(2,(R6))                                      
         CLI   4(R1),1                                                          
         BL    ERR6                                                             
         BE    *+8                                                              
         LA    R6,32(R6)                                                        
         CLI   1(R6),0             FORMAT IS (PRINTER-ID,)PRINTER-NUM           
         BNE   ERR6                OR (PRINTER-ID,)PRINTER-NAME                 
         MVC   PRNNAME,12(R6)      SAVE INPUT TEXT FOR PRINTER-NAME             
         MVC   PNMLEN,0(R6)        SAVE INPUT LENGTH                            
         TM    2(R6),X'80'         TEST IF NUMERIC                              
         BZ    P3V0A1                                                           
         CLC   4(4,R6),=F'1'                                                    
         BL    ERR6                                                             
         CLC   4(4,R6),=F'255'                                                  
*NOP     BH    ERR6                                                             
         MVC   PRNNUM,7(R6)        SAVE INPUT # FOR PRINTER-NUM                 
         MVC   PNUMFLAG,2(R6)      SAVE NUMERIC INPUT FLAG                      
*                                                                               
P3V0A1   L     R6,ACIREC                                                        
         CLI   4(R1),1                                                          
         BE    P3V1                                                             
         CLI   1(R6),0                                                          
         BNE   ERR6                                                             
         CLI   0(R6),0                                                          
         BE    ERR6                                                             
         CLI   0(R6),10                                                         
         BH    ERR6                                                             
         MVC   PRNIDA,12(R6)       EXTRACT PRINTER LOCATION NAME                
         XC    PRNID,PRNID                                                      
*                                                                               
P3V1     TM    TRMTYP1,TTYPETWX    A TWX TERM CAN ONLY REF ITSELF               
         BZ    P3V1B                                                            
         CLI   PRNNUM,1                                                         
         BNE   ERR6                                                             
         CLC   PRNID,LOGONID                                                    
         BNE   ERR6                                                             
         L     R5,AUTL                                                          
P3V1A    MVC   PRNSYM,TSYM-UTLD(R5)                                             
         B     P3VG                                                             
*                                                                               
P3V1B    TM    TRMTYP2,TTYPEIAT    AN IAT TERM HAS ONLY ONE PRINTER             
         BZ    P3V2                                                             
         CLI   PRNNUM,1                                                         
         BNE   ERR6                                                             
         CLC   PRNID,LOGONID                                                    
         BNE   ERR6                                                             
         L     R5,AUTL                                                          
P3V1C    MVC   PRNSYM,TSYM-UTLD(R5)                                             
         IC    R1,PRNSYM+6         PRINTER IS LIUD+1 AT POSN LUID+6             
         LA    R1,1(R1)                                                         
         STC   R1,PRNSYM+6                                                      
         B     P3VD                                                             
*                                                                               
P3V2     L     R5,ACIREC           READ USER ID RECORD                          
         USING CTIREC,R5                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID+8(2),LOGONID                                              
         GOTO1 ADATAMGR,DMCB,DMREAD,CTFILE,(R5),(R5)                            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
P3V3     OC    PRNID,PRNID         PRINTER LOCATION MUST BE COMPATIBLE          
         BNZ   P3VA                                                             
         TM    DDS,DDSTRM          ANY LOCATION FOR DDS TERMINALS               
         BNE   P3V5                                                             
         GOTO1 AGETIDS,CIP1,(C'C',(R5)),0,(C'A',ADATAMGR),PRNIDA                
         TM    12(R1),X'01'                                                     
         BZ    ERR6                NO MATCH ON COMPATIBLE ID LIST               
*                                                                               
P3V5     L     R5,ACIREC           READ PRINTER LOCATION ID REC                 
         MVC   CTIKID,PRNIDA                                                    
         GOTO1 ADATAMGR,DMCB                                                    
         CLI   8(R1),0                                                          
         BNE   ERR6                                                             
         LA    R5,CTIDATA                                                       
         SR    R6,R6                                                            
P3V6     CLI   0(R5),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R5),X'02'                                                      
         BE    *+14                                                             
         IC    R6,1(R5)                                                         
         AR    R5,R6                                                            
         B     P3V6                                                             
         MVC   PRNID,2(R5)         SAVE PRINTER LOCATION ID NUM                 
*                                                                               
P3VA     EQU   *                                                                
         L     R5,ACIREC           CHECK ID OPTIONS PRINTER NAME FLAG           
         LA    R5,CTIDATA                                                       
         SR    RF,RF                                                            
P3VA2    CLI   0(R5),0                                                          
         BE    P3VA4                                                            
         CLI   0(R5),CTIDOELQ                                                   
         BE    *+14                                                             
         IC    RF,1(R5)                                                         
         AR    R5,RF                                                            
         B     P3VA2                                                            
         MVC   IDOPTFL2,CTIDOFL2-CTIDOD(R5)                                     
         TM    IDOPTFL2,CTIDOFPN                                                
         BZ    P3VA4                                                            
         OC    PRNNAME,PRNNAME     INVALID IF NO PRINTER-NAME ENTERED           
         BZ    ERR6                                                             
         TM    PNUMFLAG,X'80'      TEST NUMERIC ENTERED                         
         BO    *+12                1 NUMERIC CHARACTER ALLOWED                  
         CLI   PNMLEN,2            PRINTER NAME 2-4 CHARACTERS                  
         BL    ERR6                                                             
         CLI   PNMLEN,L'PRNNAME                                                 
         BH    ERR6                                                             
         LA    RF,PRNNAME+L'PRNNAME-1                                           
         LA    RE,L'PRNNAME                                                     
         CLI   0(RF),0                                                          
         BNE   P3VCN                                                            
         MVI   0(RF),C' '          SPACE FILE PRNNAME                           
         BCTR  RF,0                                                             
         BCT   RE,*-14                                                          
         B     P3VCN               BRANCH TO RPOCESS PRINTER-NAME               
P3VA4    TM    PNUMFLAG,X'80'      INVALID IF NON-NUMERIC ENTERED               
         BZ    ERR6                                                             
         L     R5,ACIREC           SCAN FOR VALID PRINTER # ELEMENTS            
         LA    R5,CTIDATA          FOR PRINTER-NUMBER ENTRY                     
         USING CTPRND,R5                                                        
         SR    R6,R6                                                            
         SR    R7,R7                                                            
         XC    PRNSYM,PRNSYM                                                    
P3VB     CLI   0(R5),0                                                          
         BE    P3VC1                                                            
         CLI   0(R5),X'3A'                                                      
         BNE   P3VC                                                             
         OC    CTPRNFLG,CTPRNFLG   CHECK FOR LIST ENTRY                         
         BZ    P3VB1                                                            
         LTR   R7,R7               SAVE FIRST PRINTER NUM                       
         BNZ   *+8                                                              
         IC    R0,CTPRNNUM                                                      
         LA    R7,1(R7)            BUMP NUM OF PRINTERS                         
         CHI   R7,1                                                             
         BNH   *+12                                                             
         CLI   FLDILEN,0           MUST INPUT NUM IF MORE THAN ONE              
         BE    ERR5                                                             
         CLC   CTPRNNUM,PRNNUM                                                  
         BNE   P3VC                                                             
         MVC   PRNSYM,CTPRNLIN                                                  
         B     P3VC                                                             
P3VB1    STM   R5,R6,DUB1                                                       
         LA    R6,CXREC            READ LIST RECORD                             
         USING CTWREC,R6                                                        
         XC    CTWKEY,CTWKEY                                                    
         MVI   CTWKTYP,C'W'                                                     
         MVI   CTWKREC,C'R'                                                     
         MVC   CTWKID,CTPRNLST                                                  
         GOTO1 ADATAMGR,DMCB,DMREAD,CTFILE,(R6),(R6)                            
         CLI   8(R1),0                                                          
         BNE   P3VB1X                                                           
         LA    R5,CTWDATA                                                       
         DROP  R6                                                               
         SR    R6,R6                                                            
P3VB1B   CLI   0(R5),0                                                          
         BE    P3VB1X                                                           
         CLI   0(R5),X'A4'                                                      
         BNE   P3VB1C                                                           
         LTR   R7,R7               SAVE FIRST PRINTER NUM                       
         BNZ   *+8                                                              
         IC    R0,11(R5)                                                        
         LA    R7,1(R7)            BUMP NUM OF PRINTERS                         
         CHI   R7,1                                                             
         BNH   *+12                                                             
         CLI   FLDILEN,0           MUST INPUT NUM IF MORE THAN ONE              
         BE    ERR5                                                             
         CLC   11(1,R5),PRNNUM                                                  
         BNE   P3VB1C                                                           
         MVC   PRNSYM,3(R5)                                                     
P3VB1C   IC    R6,1(R5)                                                         
         AR    R5,R6                                                            
         B     P3VB1B                                                           
P3VB1X   LM    R5,R6,DUB1                                                       
*                                                                               
P3VC     IC    R6,1(R5)                                                         
         AR    R5,R6                                                            
         B     P3VB                                                             
P3VC1    OC    PRNSYM,PRNSYM       WAS PRINTER FOUND                            
         BNZ   P3VD                YES                                          
         CLI   FLDILEN,0                                                        
         BNE   ERR6                INV PRINTER NUM INPUT                        
         CHI   R7,1                                                             
         BNE   ERR6                                                             
         STC   R0,PRNNUM           RESET DEFAULT IF ONLY ONE PRINTER            
         MVI   FLDILEN,1                                                        
         B     P3VA                                                             
*                                                                               
         USING CTIREC,R5                                                        
P3VCN    EQU   *                   (PRINTER ID,)PRINTER-NAME ENTERED            
         XC    PNMDEFID,PNMDEFID                                                
         TM    IDOPTFL2,CTIDOFPI                                                
         BO    P3VCN10                                                          
         TM    IDOPTFL2,CTIDOFPP                                                
         BO    P3VCN20                                                          
         B     P3VCN30                                                          
*                                                                               
P3VCN10  L     R5,ACIREC           FIND PRINTER NAME ID ELEMENT                 
         LA    R5,CTIDATA                                                       
         SR    RF,RF                                                            
P3VCN12  CLI   0(R5),0                                                          
         BE    P3VCN30                                                          
         CLI   0(R5),CTPNIELQ                                                   
         BE    *+14                                                             
         IC    RF,1(R5)                                                         
         AR    R5,RF                                                            
         B     P3VCN12                                                          
         MVC   PNMDEFID,CTPNIUID-CTPNID(R5)                                     
         B     P3VCN30                                                          
*                                                                               
P3VCN20  L     R5,ACIREC           FIND AGENCY ELEMENT                          
         LA    R5,CTIDATA                                                       
         SR    RF,RF                                                            
P3VCN22  CLI   0(R5),0                                                          
         BE    P3VCN30                                                          
         CLI   0(R5),X'06'                                                      
         BE    *+14                                                             
         IC    RF,1(R5)                                                         
         AR    R5,RF                                                            
         B     P3VCN22                                                          
         MVC   HALF,2(R5)                                                       
*                                                                               
         L     R5,ACIREC           READ AGENCY ALPHA RECORD                     
         DROP  R5                                                               
         USING CT5REC,R5                                                        
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,HALF                                                    
         GOTO1 ADATAMGR,DMCB,DMREAD,CTFILE,(R5),(R5)                            
         CLI   8(R1),0                                                          
         BNE   P3VCN30                                                          
         LA    R5,CT5DATA          FIND PRICIPLE ID ELEMENT                     
         SR    RF,RF                                                            
P3VCN24  CLI   0(R5),0                                                          
         BE    P3VCN30                                                          
         CLI   0(R5),X'02'                                                      
         BE    *+14                                                             
         IC    RF,1(R5)                                                         
         AR    R5,RF                                                            
         B     P3VCN24                                                          
         MVC   PNMDEFID,2(R5)                                                   
         B     P3VCN30                                                          
*                                                                               
P3VCN30  L     R5,ACIREC           READ PRINTER-NAME RECORD                     
         DROP  R5                                                               
         USING CTPNREC,R5                                                       
         XC    CTPNKEY,CTPNKEY                                                  
         MVI   CTPNKTYP,CTPNKTYQ                                                
         MVC   CTPNKUIN,PRNID                                                   
         MVC   CTPNKNAM,PRNNAME                                                 
         GOTO1 ADATAMGR,DMCB,DMREAD,CTFILE,(R5),(R5)                            
         CLI   8(R1),0                                                          
         BNE   P3VCN40                                                          
         LA    R5,CTPNDATA                                                      
         SR    RF,RF                                                            
P3VCN32  CLI   0(R5),0                                                          
         BE    ERR6                                                             
         CLI   0(R5),CTPNLELQ                                                   
         BE    *+14                                                             
         IC    RF,1(R5)                                                         
         AR    R5,RF                                                            
         B     P3VCN32                                                          
         MVC   PRNSYM,CTPNLID-CTPNLD(R5)                                        
         B     P3VD                                                             
*                                                                               
P3VCN40  EQU   *                                                                
         OC    PNMDEFID,PNMDEFID   CHECK FOR DEFAULT PNAME USERID               
         BZ    ERR6                                                             
         L     R5,ACIREC           READ DEFUALT ID PRINTER-NAME RECORD          
         DROP  R5                                                               
         USING CTPNREC,R5                                                       
         XC    CTPNKEY,CTPNKEY                                                  
         MVI   CTPNKTYP,CTPNKTYQ                                                
         MVC   CTPNKUIN,PNMDEFID                                                
         MVC   CTPNKNAM,PRNNAME                                                 
         GOTO1 ADATAMGR,DMCB,DMREAD,CTFILE,(R5),(R5)                            
         CLI   8(R1),0                                                          
         BNE   ERR6                                                             
         LA    R5,CTPNDATA                                                      
         SR    RF,RF                                                            
P3VCN42  CLI   0(R5),0                                                          
         BE    ERR6                                                             
         CLI   0(R5),CTPNLELQ                                                   
         BE    *+14                                                             
         IC    RF,1(R5)                                                         
         AR    R5,RF                                                            
         B     P3VCN42                                                          
         MVC   PRNSYM,CTPNLID-CTPNLD(R5)                                        
         B     P3VD                                                             
         DROP  R5                                                               
*                                                                               
P3VD     L     R5,AVUTL            SEARCH UTL FOR PRINTER                       
         USING UTLD,R5                                                          
         LH    R6,0(R5)                                                         
         L     R7,2(R5)                                                         
         LA    R5,6(R5)                                                         
P3VD1    TM    TTYPE,TTYPETWX      SPECIAL CODE FOR TWX TERMINALS               
         BO    *+14                                                             
         OC    TPRNT,TPRNT         IGNORE NON PRINTER TERMINALS                 
         BZ    P3VD2                                                            
         CLC   PRNSYM,TSYM-UTLD(R5)                                             
         BE    P3VG                                                             
P3VD2    BXLE  R5,R6,P3VD1                                                      
*                                                                               
P3VE     CLC   APRN,=F'2'                                                       
         BE    P3VF                TERMID AND VTAMLUID NOT IN UTL               
         BL    *+6                                                              
         DC    H'0'                FUCKING HELL                                 
         LA    R0,X'F0'            SET PRNSYM/NOUTL/READREC/SRCHREC             
         XC    DMCB(16),DMCB                                                    
         GOTO1 ATERMVAL,DMCB,((R0),PRNSYM)                                      
         TM    DMCB,X'20'                                                       
         BO    ERR6                TERMINAL RECORD NOT FOUND                    
         TM    DMCB,X'10'                                                       
         BO    ERR6                TERMINAL NOT AUTH FOR THIS FACPAK            
         SR    R5,R5                                                            
         ICM   R5,7,DMCB+13                                                     
         BZ    ERR6                NO TERMINAL DEFN ELEMENT                     
         USING CTTRMD,R5                                                        
         TM    CTTRMTYP,X'02'      TEST IF TWX TERMINAL                         
         BO    *+4                                                              
         TM    CTTRMDEV,X'80'      TEST IF PRINTER DEVICE                       
         BZ    ERR6                                                             
         SR    RE,RE               GET RETURNED TERMID/VTAMLUID                 
         ICM   RE,7,DMCB+1                                                      
         CLI   8(RE),C' '          TEST IF VTAM LUID SPECIFIED                  
         BE    P3VF                NO                                           
         MVC   PRNSYM,8(RE)        YES MUST SWITCH TO VTAM LUID                 
         MVC   APRN,=F'2'                                                       
         B     P3VD                2ND PASS TO SEE IF VTAM ID IN UTL            
*                                                                               
P3VF     XC    DMCB(20),DMCB       CREATE UTL/PRQ ENTRIES FOR PRINTER           
         LA    RF,PRNSYM                                                        
         ST    RF,DMCB                                                          
         GOTO1 ATERMBLD,DMCB                                                    
         CLI   DMCB,0              TEST FOR ERRORS                              
         BE    P3VF1                                                            
         TM    DMCB,X'C0'          TEST FOR UTL OR PRQ FULL                     
         BZ    ERR6                                                             
         B     ERR7                                                             
P3VF1    MVI   NEWTRM,1            SET NEW TERMINAL ADDED                       
         MVC   APRN,=F'3'                                                       
         B     P3VD                3ND PASS TO VALIDATE NEW ENTRY               
*                                                                               
P3VG     ST    R5,APRN             SAVE A(PRINTER UTL ENTRY)                    
         USING UTLD,R5                                                          
         MVC   PRN,TNUM            SAVE PRINTER TRM NUM                         
         MVC   PRNTYP,TTYPE        SAVE PRINTER TYPE                            
         MVI   PRNTYP1,0           SAVE PRINTER SUB TYPE                        
         MVI   APRNQ,0             SAVE A(PRINTER QUEUE)                        
         MVC   APRNQ+1(3),TPRNT                                                 
*                                                                               
P3VH     TM    PRNTYP,TTYPETWX     A TWX PRINTER CAN ONLY REF ITSELF            
         BZ    P3VX                                                             
         TM    DDS,DDSTRM          UNLESS REFERENCED BY A DDS TRM               
         BNE   *+14                                                             
         CLC   PRN,TRM                                                          
         BNE   ERR6                                                             
         L     R5,APRQ             SEARCH PRINTER QUEUES FOR TWX PRTR           
         LH    R6,0(R5)                                                         
         L     R7,2(R5)                                                         
         LA    R5,6(R5)                                                         
P3VH1    CLC   APRN+1(3),PRQUTLA-PRQD(R5)                                       
         BE    P3VH2                                                            
         ZIC   R6,PRQNEMAX-PRQD(R5)                                             
         LTR   R6,R6                                                            
         BNZ   *+8                                                              
         LA    R6,8                                                             
         SLL   R6,4                                                             
         LA    R6,PRENTRY-PRQD(R6) SET LENGTH OF ENTRY                          
         BXLE  R5,R6,P3VH1                                                      
         B     ERR6                                                             
P3VH2    ST    R5,APRNQ            SAVE A(TWX PRINTER QUEUE)                    
*                                                                               
P3VX     EQU   *                                                                
*                                                                               
P4VAL    LA    R4,SRVP4H           P4=OPTIONS FOR PRINTER ACTIONS               
         ST    R4,CURSOR                                                        
         XC    FULL,FULL                                                        
         XC    HALF,HALF                                                        
         XC    SPAGE,SPAGE                                                      
         XC    EPAGE,EPAGE                                                      
         XC    IFFILTS(IFFILTL),IFFILTS                                         
         CLI   FLDILEN,0                                                        
         BE    P4VXX                                                            
         LA    RE,ACTNOPTN         POINT TO TABLE OF VALID OPTIONS              
P4VAL1   CLI   0(RE),0                                                          
         BE    P4VXX               IGNORE IF NO OPTIONS FOR ACTION              
         CLC   ACTN,0(RE)                                                       
         BE    P4VAL2                                                           
         LA    RE,8(RE)                                                         
         B     P4VAL1                                                           
P4VAL2   MVC   DUB1,0(RE)          SAVE ACTN TABLE ENTRY                        
         IC    R0,DUB1+1           GET MAX NUM OF OPTNS FOR ACTN                
         L     R6,ACIREC                                                        
         GOTO1 ASCANNER,DMCB,(R4),((R0),(R6))                                   
         MVC   HALF+1(1),4(R1)                                                  
         CLI   HALF+1,0            HALF+1=NUMBER OF FIELDS                      
         BE    ERR14               NUMBER/SYNTAX OF INPUT FIELDS                
         MVI   HALF,1              HALF+0=FIELD NUMBER                          
*                                                                               
P4V0     ZIC   R1,0(R6)            SET R1 TO LENGTH OF OPTN                     
         LTR   R1,R1                                                            
         BZ    ERR14                                                            
         CLI   1(R6),0             CHECK IF KEYWORD=... FORMAT                  
         BE    P4V0A                                                            
         MVI   FLAG1,X'80'         SET DEFAULT EQ SIGN                          
         LA    RE,11(R6,R1)                                                     
         CLI   0(RE),X'4C'         CHECK AND SET LT SIGN                        
         BNE   *+8                                                              
         MVI   FLAG1,X'D0'                                                      
         CLI   0(RE),X'6E'         CHECK AND SET GT SIGN                        
         BNE   *+8                                                              
         MVI   FLAG1,X'B0'                                                      
         CLI   0(RE),X'61'         CHECK AND SET NE SIGN                        
         BNE   *+8                                                              
         MVI   FLAG1,X'70'                                                      
         CLI   FLAG1,X'80'         ADJUST LEN IF SIGN VALUE LAST CHR            
         BE    *+14                                                             
         MVI   0(RE),C' '                                                       
         BCTR  R1,0                                                             
         STC   R1,0(R6)                                                         
         CLI   0(R6),3             KEYWORD MUST BE 3 THRU 8 CHRS LONG           
         BL    ERRF                                                             
P4V0A    CLI   0(R6),8                                                          
         BH    ERRF                                                             
         BCTR  R1,0                                                             
         L     R7,DUB1+4           GET ADDR OF TABLE FOR ACTION                 
         A     R7,RELO                                                          
*                                                                               
P4V1     CLI   0(R7),0             SEARCH OPTION NAME TABLE                     
         BNE   P4V1A                                                            
         TM    6(R7),X'20'         TEST IF SPECIAL IN LAST ENTRY                
         BO    P4V2                                                             
         B     ERRF                                                             
P4V1A    TM    2(R6),X'80'         TEST IF OPTION IS AN INTEGER                 
         BZ    P4V1B                                                            
         TM    6(R7),X'10'                                                      
         BO    P4V2                                                             
         B     P4V1D                                                            
P4V1B    EX    0,0(R7)             SET RF TO A(KEYWORD)                         
         EX    R1,*+8              TEST IF MATCHES ALPHA NAME                   
         B     *+10                                                             
         CLC   12(0,R6),0(RF)                                                   
         BE    P4V2                                                             
P4V1D    LA    R7,L'OPTOPTN(R7)                                                 
         B     P4V1                                                             
*                                                                               
P4V2     TM    6(R7),X'01'         TEST IF DDS ONLY OPTION                      
         BZ    P4V2A                                                            
         TM    DDS,DDSTRM                                                       
         BZ    ERRF                                                             
P4V2A    TM    6(R7),X'02'         TEST IF SINGLE VALUE ONLY                    
         BZ    P4V2B                                                            
         CLI   1(R6),0                                                          
         BNE   ERRF                                                             
         B     P4V3                                                             
P4V2B    TM    6(R7),X'04'         TEST KEYWORD=... FORMAT                      
         BZ    P4V2C                                                            
         CLI   1(R6),0                                                          
         BE    ERRF                                                             
         CLI   FLAG1,X'80'         TEST IF SIGN VALUE ALLOWED                   
         BE    P4V3                                                             
         CLI   FLAG1,X'70'         TEST NE ALLOWED                              
         BNE   *+12                                                             
         TM    5(R7),X'80'                                                      
         BZ    ERRFA                                                            
         CLI   FLAG1,X'D0'         TEST LT ALLOWED                              
         BNE   *+12                                                             
         TM    5(R7),X'40'                                                      
         BZ    ERRFA                                                            
         CLI   FLAG1,X'B0'         TEST GT ALLOWED                              
         BNE   *+12                                                             
         TM    5(R7),X'20'                                                      
         BZ    ERRFA                                                            
         B     P4V3                                                             
P4V2C    EQU   *                                                                
*                                                                               
P4V3     L     RF,8(R7)            GOTO ROUTINE FOR VALUE                       
         LTR   RF,RF                                                            
         BZ    P4VX                                                             
         A     RF,RELO                                                          
         BR    RF                  R6=A(SCANNER TABLE ENTRY)                    
*                                                                               
P4VX     LA    R6,32(R6)           BACK FOR NEXT KEYWORD                        
         IC    RF,HALF                                                          
         LA    RF,1(RF)            BUMP FIELD NUMBER                            
         STC   RF,HALF                                                          
         CLC   HALF(1),HALF+1      COMPARE WITH MAXIMUM                         
         BNH   P4V0                                                             
P4VXX    B     FUNCT               NO MORE FIELDS                               
*                                                                               
STRV00   XC    FULL(2),FULL        START SPECIAL (PAGE NUMBER)                  
         LA    RF,12(R6)                                                        
         ZIC   RE,0(R6)                                                         
         SH    RE,=H'1'                                                         
         BM    STRV00A                                                          
         CLI   12(R6),C'-'         TEST FOR NEGATIVE INTEGER                    
         BNE   *+16                                                             
         LA    RF,1(RF)                                                         
         SH    RE,=H'1'                                                         
         BM    STRV00A                                                          
         CHI   RE,3                                                             
         BH    STRV00A                                                          
         MVC   DUB(4),=4C'0'                                                    
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVZ   DUB(0),0(RF)                                                     
         CLC   DUB(4),=4C'0'                                                    
         BNE   STRV00A                                                          
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,RF)                                                      
         CVB   R0,DUB                                                           
         STH   R0,FULL             SET VALUE IN FULL                            
         CLI   12(R6),C'-'         SET NEGATIVE PAGE NUMBER                     
         BNE   *+8                                                              
         OI    FULL,X'80'                                                       
STRV00A  OC    FULL(2),FULL        TEST VALID FORMAT                            
         BZ    ERR14                                                            
*                                                                               
STRV00B  IC    RF,FULL+3           BUMP PAGE NUM FIELDS INPUT                   
         LA    RF,1(RF)                                                         
         STC   RF,FULL+3                                                        
         CLI   FULL+3,2            TEST FOR PAGE1,PAGE2                         
         BL    STRV00C                                                          
         BH    ERR14                                                            
         OC    PQSSUBID,PQSSUBID   BUT REPORT MUST BE INPUT                     
         BZ    ERR14                                                            
         OC    PQSSEQ,PQSSEQ       AND REPORT MUST BE SPECIFIC                  
         BZ    ERR14                                                            
*                                                                               
STRV00C  CLI   FULL+3,1            CHECK FIRST FIELD                            
         BNE   STRV00D                                                          
         MVC   SPAGE(2),FULL       SAVE START PAGE                              
         B     P4VX                                                             
*                                                                               
STRV00D  MVC   EPAGE(2),FULL       SECOND FIELD IS START SECOND PAGE            
         CLC   EPAGE(2),PAGEEROR                                                
         BE    ERR14                                                            
         TM    EPAGE,X'80'         CANT BE NEGATIVE                             
         BO    ERR14                                                            
         CLC   SPAGE(2),EPAGE      CHECK PAGE1 LE PAGE2                         
         BH    ERR14                                                            
         B     P4VX                                                             
*                                                                               
STRV01   MVC   EPAGE(2),PAGELAST   START LAST                                   
         B     P4VX                                                             
STRV02   MVC   FULL(2),PAGEEROR    START ERROR                                  
         B     STRV00B                                                          
*                                                                               
FLSV01   MVC   SPAGE(2),=X'0001'   FLUSH PRINTER                                
         B     P4VX                                                             
FLSV02   MVC   SPAGE(2),=X'0002'   FLUSH REPORT                                 
         B     P4VX                                                             
FLSV03   MVC   SPAGE(2),=X'0004'   FLUSH QUEUE                                  
         B     P4VX                                                             
*                                                                               
STPV01   MVC   SPAGE(2),=X'0001'   STOP PRINTER                                 
         B     P4VX                                                             
STPV02   MVC   SPAGE(2),=X'0002'   STOP REPORT                                  
         B     P4VX                                                             
STPV03   MVC   SPAGE(2),=X'0004'   STOP QUEUE                                   
         B     P4VX                                                             
*                                                                               
SCHV01   MVC   IFCOPF,FLAG1        SCHED COPYS=                                 
         LA    R7,IFCOPV                                                        
         TM    3(R6),X'80'         MUST BE INTEGER BETWEEN 1 AND 15             
         BZ    SCHV01W                                                          
         L     RE,8(R6)                                                         
         LTR   RE,RE                                                            
         BNP   SCHV01W                                                          
         CHI   RE,15                                                            
         BH    SCHV01W                                                          
         STC   RE,0(R7)                                                         
         B     SCHV01X                                                          
SCHV01W  B     ERRFB                                                            
SCHV01X  B     P4VX                                                             
*                                                                               
SCHV02   LA    R4,SRVP2H           SCHED PAGES=                                 
         OC    PQSSEQ,PQSSEQ                                                    
         BZ    ERR1                                                             
         LA    R4,SRVP4H                                                        
         TM    3(R6),X'80'         MUST BE INTEGER BETWEEN 1 AND 9999           
         BZ    SCHV02W                                                          
         L     RE,8(R6)                                                         
         LTR   RE,RE                                                            
         BNP   SCHV02W                                                          
         CHI   RE,9999                                                          
         BH    SCHV02W                                                          
         STCM  RE,3,SPAGE          SET START PAGE                               
         LA    R6,32(R6)           NEXT SCANBLK                                 
         IC    RF,HALF                                                          
         LA    RF,1(RF)            BUMP FIELD NUMBER                            
         STC   RF,HALF                                                          
         TM    2(R6),X'80'                                                      
         BNZ   SCHV02A                                                          
         CLC   12(1,R6),SR@LAST    IF NOT NUMERIC IT MUST BE LAST               
         BNE   SCHV02W                                                          
         MVC   EPAGE,PAGELAST                                                   
         B     SCHV02X                                                          
SCHV02A  L     RE,4(R6)                                                         
         LTR   RE,RE                                                            
         BNP   SCHV02W                                                          
         CHI   RE,9999                                                          
         BH    SCHV02W                                                          
         STCM  RE,3,EPAGE                                                       
         B     SCHV02X                                                          
SCHV02W  B     ERR14                                                            
SCHV02X  B     P4VX                                                             
         EJECT                                                                  
                                                                                
***********************************************************************         
* FUNCT                                                                         
***********************************************************************         
FUNCT    L     R6,APRNQ                                                         
         USING PRQD,R6                                                          
         TM    INTFLAG,INTRUN      IF INTERNAL IGNORE THIS BIT                  
         BO    FUNCT1                                                           
         CLI   PFKEY,0             IF PFKEY USED                                
         BNE   STA                 DEFAULT TO STATUS                            
         LA    R4,SRVSA1H                                                       
FUNCT0   CLI   8(R4),C'*'          TEST FOR OLD ACTIONS                         
         BNE   *+14                                                             
         XC    8(4,R4),8(R4)       AND DELETE THEM                              
         MVI   5(R4),0                                                          
         CLI   5(R4),0             TEST ANY INPUT                               
         BNE   STA                 DEFAULT TO STATUS                            
         LA    R4,93(R4)           NEXT SELECT LINE                             
         LA    R1,SRVPFKH                                                       
         CR    R4,R1               TEST FOR END OF SCREEN                       
         BL    FUNCT0                                                           
*                                                                               
FUNCT1   CLI   ACTN,X'11'                                                       
         BE    STR                                                              
         CLI   ACTN,X'12'                                                       
         BE    STA                                                              
         CLI   ACTN,X'13'                                                       
         BE    STP                                                              
         CLI   ACTN,X'14'                                                       
         BE    SCH                                                              
         CLI   ACTN,X'15'                                                       
         BE    FLS                                                              
         CLI   ACTN,X'16'          DEAD                                         
         BE    ALT                                                              
         CLI   ACTN,X'17'                                                       
         BE    KIL                                                              
         CLI   ACTN,X'18'                                                       
         BE    MOD                                                              
         CLI   ACTN,X'19'                                                       
         BNE   *+16                                                             
         MVI   ACTN,X'17'                                                       
         MVI   ACTN1,2                                                          
         B     KIL                                                              
         CLI   ACTN,X'1A'                                                       
         BE    PID                                                              
         CLI   ACTN,X'1B'                                                       
         BE    STA                                                              
         DC    H'0'                                                             
*                                                                               
ALT      EQU   *                                                                
*                                                                               
PID      LA    R4,SRVP3            SET DEFAULT PRINTER                          
         L     RF,ATIA                                                          
         USING SRSD,RF                                                          
         MVC   SRTRMPID(12),SRVP3                                               
         MVI   RESULT,12                                                        
         LA    R4,SRVP1H           CURSOR POS FOR MSGOUT                        
         B     MSGOUT                                                           
         DROP  RF                                                               
*                                                                               
MOD      TM    PRQMODE,X'80'       MODE SET PRINTER MANUAL/AUTO                 
         BO    MOD1                                                             
         CLI   ACTN1,MODEMAN                                                    
         BNH   ERR19               PRINTER IS ALREADY MANUAL                    
*NOP     CLI   PRQNE,0                                                          
*NOP     BE    ERR21               CANT SET TO AUTO WITH EMPTY QUEUE            
         OI    PRQMODE,X'80'                                                    
         B     MOD2                                                             
MOD1     CLI   ACTN1,MODEAUTO                                                   
         BE    ERR20               PRINTER IS ALREADY AUTO                      
         NI    PRQMODE,X'7F'                                                    
MOD2     MVI   RESULT,11           SET MODE CHANGED RESULT                      
         MVI   NEWTRM,2            FLAG Q CHANGED                               
         MVI   DSPFLAG,0                                                        
         MVI   ACTN2,1                                                          
         BRAS  RE,DSP              DISPLAY PRINTER STATUS/QUEUE                 
         LA    R4,SRVP1H                                                        
         B     MSGOUT                                                           
         EJECT                                                                  
                                                                                
*----------------------------------------------------------------------         
* STR                                                                           
*----------------------------------------------------------------------         
STR      CLI   PQSSUBID,0          WAS REPORT ID SPECIFIED                      
         BE    STR1                NO                                           
         CLI   PQSDDSFN,C'U'       ARE WE USING U=                              
         BNE   *+14                                                             
         CLC   PQSSUBID,=C'ALL'    IS IT U=USER,ALL                             
         BE    STR1                IF SO DON'T SCHEDULE WHOLE QUEUE             
         CLI   ACTN1,4                                                          
         BE    STR0A                                                            
         CLI   ACTN1,0             YES CANT INPUT START,XXXX                    
         BE    STR0A                                                            
         LA    R4,SRVP1H                                                        
         ST    R4,CURSOR                                                        
         B     ERR22                                                            
*                                                                               
STR0A    OC    SPAGE(2),SPAGE      WAS START PAGE SPECIFIED                     
         BZ    SCH                 NO SCHEDULE REPORT                           
         OC    EPAGE(2),EPAGE      WAS START,END PAGE SPECIFIED                 
         BNZ   SCH                 YES SCHEDULE REPORT                          
*                                                                               
STR1     L     R6,APRNQ            CHECK PRINTER STATUS FOR STARTING            
         USING PRQD,R6                                                          
         MVC   MSHIFTS,PRQNE       SET MAXIMUM NUM OF QUEUE SHIFTS              
         MVI   NSHIFTS,0                                                        
         OC    PRCIADDR,PRCIADDR   IF PRINTER ACTIVE TEST FOR RESTART           
         BNZ   STR20                                                            
         OC    SPAGE(2),SPAGE                                                   
         BZ    STR2                                                             
         LA    R4,SRVP4H                                                        
         ST    R4,CURSOR                                                        
         B     ERR9                CANT BE RESTART IF INACTIVE                  
*                                                                               
STR1A    LA    R4,SRVP3H           RETURN FROM SCHED ROUTINE                    
         CLI   PRQNE,1                                                          
         BE    STR10               SCHED HAS SEARCHED FIRST ENTRY               
*                                                                               
STR2     LA    R4,SRVP3H           POINT TO FIRST QUEUE ENTRY                   
         CLI   PRQNE,0             TEST EMPTY QUEUE                             
         BE    STR11                                                            
         CLC   NSHIFTS,MSHIFTS     TEST CYCLED WHOLE QUEUE                      
         BNL   STR11                                                            
*                                                                               
*        MAKE SURE PRINTER IS STILL INACTIVE                                    
*                                                                               
         TM    PRSTAT1,PRS1ARS     IGNORE RESTART PENDING PRINTERS              
         BO    STR11B                                                           
         CLI   PRSTAT,0            IGNORE IF PRINTER ALREADY ACTIVE             
         BNE   STR11B                                                           
         OC    PRCIADDR,PRCIADDR   IGNORE IF ADYTHING IN PRCIADDR               
         BNZ   STR11B                                                           
*                                                                               
         OI    PNEX,X'80'          SET ACTIVE ENTRY FLAG                        
*                                                                               
         TM    PNCOPYS,PNCTIME     TEST FOR SPECIFIC REPORT                     
         BO    STR2A                                                            
         OC    PNSEQN,PNSEQN                                                    
         BZ    STR2A                                                            
         L     RE,ASSB                                                          
         XC    WORK,WORK           GET SPECIFIC REPORTS THE FAST WAY            
         XC    DUB,DUB                                                          
         MVC   WORK(2),PNSRCID                                                  
         MVC   WORK+2(2),PNSEQN                                                 
         MVI   LOCKFLAG,C'Y'                                                    
         LA    R1,WORK                                                          
         BRAS  RE,GETREPT                                                       
         BNE   STR11               NO MATCH                                     
         LR    R5,R1                                                            
         USING PQRECD,R5                                                        
         CLI   PQAGERT,X'FE'       A PRINTING REPORT CAN BE CONSIDERED          
         BNE   *+14                IF IT HAS 2ND PAGE                           
         OC    EPAGE(2),EPAGE                                                   
         BZ    STR10                                                            
*                                                                               
         TM    PRQATT2,PRQAASS     TEST ALLOW SPECIAL STATUS                    
         BZ    *+12                                                             
         TM    PQSTAT,PQSTHO       TEST FOR SPECIAL STATUS                      
         BO    *+12                                                             
         TM    PQSTAT,PQSTAC       ELSE ACTIVES ONLY                            
         BZ    STR10                                                            
         TM    PQSTAT,PQSTIN       IGNORE INVISIBLE REPORTS                     
         BO    STR10                                                            
         TM    PQATTB,PQATJOBI     IGNORE REPORTS THAT CONTAIN JCL              
         BO    STR10                                                            
         TM    PQATTB,PQATNP       TEST NON PRINTABLE REPORT                    
         BO    STR10                                                            
         TM    PQATTB,PQATPW       TEST PASSWORD PROTECTED REPORT               
         BNO   STR2A1                                                           
         TM    PRNTYP,RMC          DO NOT SEND TO SHUTTLE                       
         BO    STR10                                                            
*                                                                               
STR2A1   LA    RF,DATEC            SET RF TO NEW/OLD CMPRSD DATE                
         TM    PQTYP1,PQTYNCD                                                   
         BO    *+8                                                              
         LA    RF,DATECO                                                        
         CLC   PQAGELD,0(RF)       BYPASS FUTURE DATED REPORTS                  
         BNH   *+12                                                             
         TM    DDS,DDSTRM                                                       
         BZ    STR10                                                            
         MVC   DUB+4(4),CIADDR     SAVE CIADDR OF LOWEST CTIME                  
         MVC   WORK(8),PQKEY       SAVE KEY OF LOWEST SEQ NUM                   
         MVC   DUB1+4(1),PRTQID+4  SAVE PRTQ FILE ID                            
         B     STR10               *NOP* CODE STR2A THRU STR9                   
*                                                                               
STR2A    XC    NDX,NDX             FIND WHICH PRTQ FILE TO SEARCH               
         XC    APRTQLST,APRTQLST                                                
         TM    PNSRCID,X'80'       TEST IF GENERIC ID                           
         BO    STR2B               YES                                          
         MVC   NXSRCID,PNSRCID     NO PRTQ ID FROM SPECIFIC USER ID             
         GOTO1 ADATAMGR,DMCB,(0,GFILE),PRTQUE,NDX,,CXREC                        
         MVC   PRTQID,NXUSRINF                                                  
         B     STR2C                                                            
STR2B    LA    RE,PRTQLST+8        POINT TO FIRST PRTQ FILE                     
         ST    RE,APRTQLST                                                      
         MVC   PRTQID+4(1),1(RE)   SET PRTQ ID FROM LIST                        
         MVI   PRTQID+5,C' '                                                    
STR2C    XC    DUB,DUB             INITIALISE REPORT FOUND DATA                 
         MVC   DUB(2),=4X'FF'                                                   
         XC    DUB1,DUB1                                                        
         XC    WORK(8),WORK                                                     
*                                                                               
STR3     GOTO1 ADATAMGR,DMCB,(0,BUFFER),PRTQID,,,CXREC                          
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
         MVC   BUFFDATA,CXREC      SAVE V(PQTAB)/V(PQFILE)                      
         MVC   CIDATA,CXREC+12     SAVE FILE DATA FOR THIS PRTQ FILE            
         MVC   FIWRES,PRTQID       SET RESOURCE                                 
         BRAS  RE,FIRSET           SET INDEX ADDRESSES FOR RESOURCE             
         JNE   *+2                 INVALID PRTQ                                 
         MVC   FIWNDA,FIWP1A       A(START OF PART1 INDEXES)                    
         MVI   RDIND,0             RESET READ FOR UPDATE                        
         LA    R5,FIWNDX                                                        
         USING PQRECD,R5           R5=A(PRTQUE INDEX ENTRY)                     
         B     STR4A                                                            
*                                                                               
STR4     BRAS  RE,FIRNSN           NEXT PRINT QUEUE REPORT                      
         BNE   STR9                END OF PART1S                                
STR4A    L     R1,FIWNDA                                                        
         MVC   FIWNDX(L'PQINDEX),SI1NDX-SI1PARD(R1)                             
         BRAS  RE,FIRNC            A(INDEX NODE) TO A(CI)                       
         MVC   CIADDR,FIWCIA                                                    
*&&US                                                                           
         CLC   PNSRCID,=X'0406'    IF GRAFNET MUST BE CLASS G                   
         BNE   STR61                                                            
         CLI   PNCLASS,C'G'                                                     
         BNE   ERRG2                                                            
         B     STR6C                                                            
STR61    CLI   PNCLASS,C'G'                                                     
         BE    ERRG4                                                            
         CLI   PNCLASS,C'N'        CLASS N (DARE) NOTHING TO PRINT              
         BE    ERR24                                                            
*&&                                                                             
         TM    PNSRCID,X'80'       TEST GENERIC ID                              
         BO    STR6A                                                            
         CLC   PQSRCID,PNSRCID     TEST USER ID MATCH IN QUEUE                  
         BE    STR6C                                                            
         B     STR8                                                             
*                                                                               
STR6A    CLI   AGENIDS,X'FF'       TEST VGENIDS FOUND                           
         BE    STR8                NO                                           
         GOTO1 AGENIDS,DMCB,PNSRCID,ADATAMGR                                    
         BNE   STR8                                                             
         LM    RE,RF,0(R1)         RE=N'ENTRIES, RF=A(ENTRIES)                  
         CLC   PQSRCID,0(RF)       MATCH SOURCE ID                              
         BE    STR6C                                                            
         LA    RF,2(RF)            BUMP TO NEXT                                 
         BCT   RE,*-14                                                          
         B     STR8                                                             
*                                                                               
STR6C    CLI   PNCLASS,0           SAME CLASS (POSITIVE OR NEGATIVE)            
         BE    STR6C1                                                           
         TM    PNCLASS,X'40'                                                    
         BZ    *+18                                                             
         CLC   PQCLASS,PNCLASS                                                  
         BNE   STR8                                                             
         B     STR6C1                                                           
         MVC   FLAG,PNCLASS                                                     
         OI    FLAG,X'40'                                                       
         CLC   PQCLASS,FLAG                                                     
         BE    STR8                                                             
STR6C1   EQU   *                                                                
*&&US                                                                           
         CLI   PNCLASS,C'G'        IF NOT GRAFNET DON'T PROCESS G               
         BE    STR6D                                                            
         CLI   PQCLASS,C'G'                                                     
         BE    STR8                                                             
         CLI   PQCLASS,C'N'        IGNORE CLASS N (DARE)                        
         BE    STR8                                                             
*&&                                                                             
STR6D    CLC   PQSUBID,=C'LU1'     IF THIS IS AN LU1 REPORT                     
         BNE   *+12                                                             
         TM    PQSTAT,PQSTKE       AND ITS KEEP                                 
         BO    STR6DT              IGNORE ALL GENERIC TESTS                     
*                                                                               
         CLC   PNSUBID,SR@ALL      SAME SUB-USER ID                             
         BE    STR6E                                                            
*                                                                               
         CLI   PNSUBID+1,C'*'      ALLOW X* GENERIC                             
         BNE   *+18                                                             
         CLC   PNSUBID(1),PQSUBID                                               
         BE    STR6E                                                            
         B     STR8                                                             
         CLI   PNSUBID+2,C'*'      ALLOW XX* GENERIC                            
         BNE   *+18                                                             
         CLC   PNSUBID(2),PQSUBID                                               
         BE    STR6E                                                            
         B     STR8                                                             
STR6DT   CLC   PQSUBID,PNSUBID                                                  
         BNE   STR8                                                             
*                                                                               
STR6E    CLI   PQSEQ,1             BYPASS HIGH ORDER CI'S                       
         BH    STR8                                                             
         TM    PRQATT2,PRQAASS     TEST ALLOW SPECIAL STATUS                    
         BZ    *+12                                                             
         TM    PQSTAT,PQSTHO       TEST FOR SPECIAL STATUS                      
         BO    *+12                                                             
         TM    PQSTAT,PQSTAC       ELSE ACTIVES ONLY                            
         BZ    STR8                                                             
         TM    PQSTAT,PQSTIN       IGNORE INVISIBLE REPORTS                     
         BO    STR8                                                             
*                                                                               
         TM    PNCOPYS,PNCTIME     TEST SEQ NUM CONTAINS TIME                   
         BZ    STR6E01                                                          
         LA    RF,DATEC            SET RF TO NEW/OLD CMPRSD DATE                
         TM    PQTYP1,PQTYNCD                                                   
         BO    *+8                                                              
         LA    RF,DATECO                                                        
         CLC   PQAGELD,0(RF)       IF DATE < TODAY THIS IS A HIT                
         BL    STR6F                                                            
         BH    STR8                                                             
         CLC   PQAGELT,PNSEQN      IF CTIME < PN TIME ITS A HIT                 
         BNH   STR6F                                                            
         B     STR8                                                             
*                                                                               
STR6E01  OC    PNSEQN,PNSEQN       ZERO SEQN MEANS ALL                          
         BZ    STR6F                                                            
         CLC   PQREPNO,PNSEQN      ELSE MUST BE EXACT MATCH                     
         BNE   STR8                                                             
*                                                                               
         CLI   PQAGERT,X'FE'       A PRINTING REPORT CAN BE CONSIDERED          
         BNE   STR6F               IF IT IS SPECIFIC AND HAS 2ND PAGE           
         CLC   PQREPNO,PNSEQN                                                   
         BNE   STR8                                                             
         OC    EPAGE(2),EPAGE                                                   
         BZ    STR8                                                             
*                                                                               
STR6F    TM    PQATTB,PQATJOBI     IGNORE REPORTS THAT CONTAIN JCL              
         BO    STR8                                                             
         TM    PQATTB,PQATNP       TEST NON PRINTABLE REPORT                    
         BZ    STR6G                                                            
         B     STR8                                                             
*                                                                               
STR6G    TM    PQATTB,PQATPW       TEST PASSWORD PROTECTED REPORT               
         BNO   STR6H                                                            
         TM    PRNTYP,RMC          DO NOT SEND TO SHUTTLE                       
         BO    STR8                                                             
         CLC   PQREPNO,PQSSEQ      MUST BE SPECIFIC                             
         BNE   STR8                                                             
*                                                                               
STR6H    LA    RF,DATEC            SET RF TO NEW/OLD CMPRSD DATE                
         TM    PQTYP1,PQTYNCD                                                   
         BO    *+8                                                              
         LA    RF,DATECO                                                        
         CLC   PQAGELD,0(RF)       BYPASS FUTURE DATED REPORTS                  
         BNH   STR6J                                                            
         TM    DDS,DDSTRM                                                       
         BZ    STR8                                                             
*                                                                               
STR6J    EQU   *                   REPORT IS A CANDIDATE                        
*                                                                               
STR6S    TM    PNSRCID,X'80'       TEST IF MATCHED ON GENERIC                   
         BZ    STR6T               NO                                           
         OC    DUB1(2),DUB1        TEST IF FIRST MATCH                          
         BNZ   *+14                NO                                           
         MVC   DUB1(2),PQSRCID     YES SAVE SPECIFIC USER ID                    
         B     STR6T                                                            
         CLC   DUB1(2),PQSRCID     TEST IF SAME USER ID AS PREV                 
         BNE   STR8                                                             
*                                                                               
STR6     MVC   HALF3,PQAGELD                                                    
         TM    PQTYP1,PQTYNCD      TEST NEW/OLD CMPRSD DATE                     
         BO    STR6T                                                            
         GOTO1 ADATCON,DMCB,(2,HALF3),(30,HALF3)                                
STR6T    CLC   HALF3,DUB           TEST WITH LOWEST CTIME SO FAR                
         BL    STR6U                                                            
         BH    STR8                                                             
         CLC   PQAGELT,DUB+2                                                    
         BH    STR8                                                             
STR6U    MVC   DUB(2),HALF3        SAVE LOWEST CDATE                            
         MVC   DUB+2(2),PQAGELT    SAVE LOWEST CTIME                            
         MVC   DUB+4(4),CIADDR     SAVE A(CI) OF LOWEST CTIME                   
         MVC   WORK(8),PQKEY       SAVE KEY OF LOWEST SEQ NUM                   
         MVC   DUB1+4(1),PRTQID+4  SAVE PRTQ FILE ID                            
*                                                                               
STR8     B     STR4                BUMP TO NEXT INDEX ENTRY                     
*                                                                               
STR9     ICM   RE,15,APRTQLST      WERE WE LOOKING FOR GENERIC ID               
         BZ    STR10               NO                                           
         OC    DUB+4(4),DUB+4      YES EXIT IF FOUND A REPORT                   
         BNZ   STR10                                                            
         LA    RE,8(RE)            BUMP TO NEXT PRTQ FILE IN LIST               
         ST    RE,APRTQLST                                                      
         CLI   0(RE),0             TEST END OF PRTQ FILE LIST                   
         BE    STR10                                                            
         MVC   PRTQID+4(1),1(RE)   SET PRTQ FILE ID FROM LIST                   
         MVI   PRTQID+5,C' '                                                    
         B     STR3                BACK TO SEARCH THIS PRTQ FILE                
*                                                                               
STR10    OC    DUB+4(4),DUB+4      SHIFT QUEUE LEFT IF NO ACTVS                 
         BNZ   STR12                                                            
         BAS   RE,SSET             SUSPEND TIMER                                
         ZIC   R7,NSHIFTS          BUMP SHIFT COUNTER                           
         LA    R7,1(R7)                                                         
         STC   R7,NSHIFTS                                                       
         NI    PNEX,X'7F'          SET INACTIVE ENTRY FLAG                      
         SR    R1,R1                                                            
         ICM   R1,3,PNNEXT                                                      
         BZ    STR11               END OF Q                                     
         STH   R1,HALF                                                          
         BCTR  R1,0                LOCATE QUEUE ENTRY                           
         MH    R1,=Y(L'PNTRY)                                                   
         LA    R1,6(R1)                                                         
         A     R1,APRQENTS                                                      
         TM    PNEX,X'40'          TEST FOR PERMANENT                           
         BZ    STR10A                                                           
*                                                                               
         XC    PNTRY,0(R1)         SWAP ENTRYS                                  
         XC    0(L'PNTRY,R1),PNTRY                                              
         XC    PNTRY,0(R1)                                                      
         XC    10(2,R1),10(R1)     XC PNNEXT ON NEW LAST                        
         SR    R1,R1                                                            
         ICM   R1,3,PNLAST                                                      
         BNZ   *+6                                                              
         DC    H'0'                THERE MUST BE A LAST ENTRY                   
         BCTR  R1,0                                                             
         MH    R1,=Y(L'PNTRY)                                                   
         LA    R1,6(R1)                                                         
         A     R1,APRQENTS                                                      
         MVC   PNLAST,HALF                                                      
         OC    PNNEXT,PNNEXT       TEST FOR PNNEXT = ZERO                       
         BNZ   *+14                                                             
         MVC   PNNEXT,HALF         SET NEW NEXT ENTRY                           
         B     *+10                                                             
         MVC   10(2,R1),HALF       SET NEW LAST ENTRY                           
         B     STR2                                                             
*                                                                               
STR10A   MVC   PNTRY,0(R1)                                                      
         LH    RF,HALF             DELETE TEMP ENTRY                            
         GOTO1 ALCM,DMCB,VTDELPRQ,(RF),0                                        
         OC    PNNEXT,PNNEXT                                                    
         BNZ   *+10                                                             
         XC    PNLAST,PNLAST                                                    
         SR    R1,R1                                                            
         IC    R1,PRQNE                                                         
         BCTR  R1,0                                                             
         STC   R1,PRQNE                                                         
         B     STR2                                                             
*                                                                               
STR11    CLI   PRQNE,0             NO REPORT FOUND AVAIL FOR PRINTING           
         BE    ERR11                                                            
         CLI   ACTN1,MODEAUTO      START,AUTO WAS INPUT                         
         BE    STR11A                                                           
         TM    PRQMODE,X'80'       TEST IF ALREADY IN AUTO MODE                 
         BO    STR11A                                                           
         B     ERR11                                                            
STR11A   OI    PRQMODE,X'80'       NO ENTRYS ACTIVE FOR AUTO MODE               
         XC    PRCIADDR,PRCIADDR                                                
         MVC   PRID,PRNID                                                       
         MVC   PRNUM,PRNNUM                                                     
         MVI   PRSTAT,0                                                         
         XC    PRHDR1,PRHDR1                                                    
*                                                                               
STR11B   MVI   RESULT,1                                                         
         MVI   DSPFLAG,0           SET PART DISPLAY                             
         MVI   ACTN2,1             SET FIRST ENTRY                              
         BRAS  RE,DSP                                                           
         B     MSGOUT                                                           
*                                                                               
STR12    TM    PNCOPYS,PNCENDP     TEST FOR START & END                         
         BZ    STR12AA                                                          
         MVC   SPAGE,PNSUBID                                                    
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RE,3,SPAGE                                                       
         ICM   RF,1,PNSUBID+2                                                   
         AR    RF,RE                                                            
         STCM  RF,3,EPAGE                                                       
*                                                                               
STR12AA  L     R5,ACIREC           REPORT FOUND FOR PRINTING                    
         MVC   PRTQID+4(1),DUB1+4  SET FILE ID OF PRTQ FILE                     
         MVI   PRTQID+5,C' '                                                    
         GOTO1 ADATAMGR,DMCB,(0,BUFFER),PRTQID,,,(R5)                           
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
         MVC   BUFFDATA,0(R5)      SAVE V(PQTAB)/V(PQFILE)                      
         MVC   CIDATA,12(R5)       SAVE FILE DATA FOR THIS PRTQ FILE            
         MVC   FIWRES,PRTQID                                                    
         BRAS  RE,FIRSET           SET INDEX ADDRESSES FOR RESOURCE             
         JNE   *+2                                                              
*                                                                               
         MVC   CIADDR,DUB+4                                                     
         MVC   FIWCIA,CIADDR                                                    
         BRAS  RE,FIRCN                                                         
*                                                                               
         L     R5,ACIREC           READ FIRST CI REC OF REPORT                  
         USING PQRECD,R5                                                        
         GOTO1 ADATAMGR,DMCB,(X'00',DMREAD),PRTQID,CIADDR,(R5)                  
         CLI   8(R1),0                                                          
         BNE   STR12E                                                           
         CLC   WORK(8),0(R5)       COMPARE KEYS IN REC AND INDEX                
         BNE   STR12E                                                           
*                                                                               
STR12A   TM    PQATTB,PQATPW       TEST IF REPORT SECURITY PROTECTED            
         BNO   STR12B                                                           
         TM    PRNTYP,RMC          DO NOT SEND TO SHUTTLE                       
         BO    STR12F                                                           
         MVC   GSECVAL,PQSECINF    VALIDATE SECURITY                            
         L     RF,ASECVAL                                                       
         BASR  RE,RF                                                            
         BE    STR12B                                                           
         TM    PNCOPYS,PNCTIME     TEST IF QUEUE ENTRY HAS END TIME             
         BO    STR12F                                                           
         OC    PNSEQN,PNSEQN       TEST IF SINGLE REPORT ENTRY                  
         BZ    STR12F                                                           
         CLC   PQREPNO,PNSEQN      MUST BE EXACT MATCH FOR SECURITY             
         BNE   STR12F                                                           
*                                                                               
STR12B   EQU   *                                                                
*&&UK                                                                           
         CLI   PQCLASS,C'0'        TEST IF APS REPORT                           
         BNE   *+12                                                             
         OI    PNEX,PREXRLU        SET REPORT IS LINE UP                        
         B     STR13                                                            
*&&                                                                             
         TM    PQSTAT,PQSTKE       TEST IF LINEUP REPORT                        
         BZ    STR13                                                            
         CLC   PQDESC(7),=C'*LINEUP='                                           
         BNE   STR13                                                            
         OI    PNEX,PREXRLU        SET REPORT IS LINE UP                        
         B     STR13                                                            
*                                                                               
STR12E   DC    H'0'                DIE IF ERROR IN FIRST CI OF REP              
*                                                                               
STR12F   B     STR11               TREAT AS NO REPORT FOUND                     
*                                                                               
STR13    OC    EPAGE(2),EPAGE      UPDATE INDEX ENTRY                           
         BNZ   STR14                                                            
         CLC   PQSRCID,PUBMIN      EXCEPT IF PRINTING PUBLIC REPORT             
         BL    *+14                                                             
         CLC   PQSRCID,PUBMAX                                                   
         BNH   STR14                                                            
         TM    PNEX,PREXGRP        OR IF PRINTING A GROUP ID REPORT             
         BO    STR14                                                            
*                                                                               
         BRAS  RE,FIRRLOCK                                                      
         L     R5,FIWNDA                                                        
         LA    R5,SI1NDX-SI1PAR(R5)                                             
         OI    PQSTAT,PQSTPG                                                    
         MVI   PQAGERT,X'FE'       SET VALUE TO SHOW INDEX HAS SYSID            
         MVC   PQAGEDD(2),PRN                                                   
         OC    PQAGEDD(1),SYSID    SET FACPAK ID AND TERMINAL NUMBER            
         BRAS  RE,FIRRUNLK                                                      
*                                                                               
STR14    MVC   PRCIADDR,CIADDR     BUILD PRINTER QUEUE HEADER                   
         MVC   PRID,PRNID                                                       
         MVC   PRNUM,PRNNUM                                                     
         MVI   PRSTAT,0                                                         
         CLI   ACTN1,MODEAUTO      START,AUTO WAS INPUT                         
         BNE   *+8                                                              
         OI    PRQMODE,X'80'                                                    
*                                                                               
         XC    PRHDR1,PRHDR1       CLEAR COUNTERS ETC                           
         MVC   PRPRTQA,PRTQID+4    SET PRTQ FILE ID                             
         MVC   PR1CIFST,CIADDR     SET FIRST CI DISK ADDR                       
         MVC   PR1KEY,PQKEY        SET REPORT KEY                               
         MVC   PR1COPYS,PNCOPYS    SET NUMBER OF COPYS REQUESTED                
         NI    PR1COPYS,X'0F'                                                   
         CLI   PR1COPYS,0          TEST IF COPYS REQUESTED                      
         BNE   SETQ1               YES                                          
*                                                                               
         LR    RF,R5               SAVE R5                                      
         L     R5,ACIREC                                                        
         MVC   PR1COPYS,PQCOPIES   NO SET COPYS DEFINED FOR REPORT              
         LR    R5,RF               RESTORE R5                                   
*                                                                               
         NI    PR1COPYS,X'0F'                                                   
         TM    PNCOPYS,PNCTIME     TEST IF SINGLE REPORT ENTRY                  
         BO    SETQ1               NO                                           
         OC    PNSEQN,PNSEQN                                                    
         BZ    SETQ1                                                            
         OC    PNCOPYS,PR1COPYS    SET COPIES SO IT WILL SHOW IN $PQ            
SETQ1    CLI   PR1COPYS,0                                                       
         BNE   *+8                                                              
         MVI   PR1COPYS,1                                                       
         MVC   PRADDR,CIADDR       SET ADDR/DISP OF FIRST PRINT LINE            
         LA    RE,PQDATA-PQINDEX                                                
         STH   RE,PRDISP                                                        
         L     RE,ACIREC                                                        
         MVC   PNFAFP,PQMAKER-PQKEY(RE)  FORMS CODE FOR AFP                     
         OC    PNFAFP,SPACES                                                    
         MVC   PNCAFP,PQMAXCPL-PQKEY(RE) MAX CHRS PER LINE FOR AFP              
         MVC   PRHDR1F,PRHDR1      INITIALISE CHECKPOINT VERSIONS               
         MVC   PRHDR1P,PRHDR1                                                   
         MVC   PRHDR1S,PRHDR1                                                   
*                                                                               
         MVI   RESULT,2                                                         
         MVI   NEWTRM,2            FLAG Q CHANGED                               
         OC    EPAGE(2),EPAGE                                                   
         BNZ   STR21               GO AND FIND PAGE1,PAGE2                      
         B     PSS                 GO AND ACTIVATE PRINTER                      
*                                                                               
STRERR   EQU   *                                                                
         DC    H'0'                DIE IF CANT READ CX REC                      
*                                                                               
STR20    MVI   RESULT,3            SET RESULT TO STARTED                        
         LA    R4,SRVP4H           RESTART PRINTER                              
         OC    SPAGE(2),SPAGE                                                   
         BZ    ERR8                MUST HAVE RESTART PAGE                       
         TM    PRSTAT,PRSACTV                                                   
         BO    ERR15                                                            
         CLC   SPAGE(2),PAGEEROR   CHECK FOR RESTART ON ERROR                   
         BNE   *+12                                                             
         TM    PRSTAT,PRSERR                                                    
         BZ    ERR15                                                            
*                                                                               
STR21    TM    SPAGE,X'80'         NEGATIVE START PAGE                          
         BZ    STR22                                                            
         NI    SPAGE,X'7F'                                                      
         SR    R0,R0                                                            
         ICM   R0,3,PRPAGES                                                     
         SH    R0,SPAGE            R0=CURRENT-INPUT                             
         BP    *+8                                                              
         LA    R0,1                                                             
         STH   R0,SPAGE            SET START PAGE NUMBER                        
*                                                                               
STR22    L     R5,ACIREC           VALIDATE START,END PAGES                     
         USING PQRECD,R5                                                        
         L     RE,BUSAVE           SET DISK ADDR IN BUFF SAVE STORAGE           
         AR    RE,R5                                                            
         MVC   SKFSTCI-SKBUFFD(4,RE),PRCIADDR                                   
         MVC   SKINTNO-SKBUFFD(1,RE),CFPQINUM                                   
         MVC   SKEXTNO-SKBUFFD(1,RE),BUPQFILE                                   
         XC    SAVE(12),SAVE       RECORD ZERO GIVES REPORT HEADER              
         MVC   SAVE+4(4),=C'PAGE'                                               
         GOTO1 ADATAMGR,DMCB,(X'00',RANDOM),PRTQID,NDX,SAVE,(R5)                
         CLI   8(R1),0                                                          
         BNE   STRERR                                                           
         CLC   PQSRCID,PR1KEY      CHECK REPORT MATCHES                         
         BNE   ERR2                                                             
STR25    CLC   SPAGE(2),PAGELAST   SET/CHECK PAGE NUMBERS                       
         BNE   *+10                                                             
         MVC   SPAGE(2),PQPAGES                                                 
         CLC   SPAGE(2),PAGEEROR   CHECK/SET ERROR RESTART                      
         BNE   *+10                                                             
         MVC   SPAGE(2),PRPAGES                                                 
         CLC   EPAGE(2),PAGELAST                                                
         BNE   *+10                                                             
         MVC   EPAGE(2),PQPAGES                                                 
         OC    EPAGE(2),EPAGE      TEST PAGE1,PAGE2 FORMAT                      
         BZ    STR25B                                                           
         CLC   SPAGE(2),PQPAGES                                                 
         BH    STR25A                                                           
         CLC   EPAGE(2),PQPAGES                                                 
         BH    STR25A                                                           
         CLC   SPAGE(2),EPAGE                                                   
         BNH   STR26                                                            
STR25A   MVC   BYTE,PRQMODE        SAVE MODE                                    
         MVC   BYTE1,PRQESCN       SAVE ESCAPE SEQUENCE NUMBER                  
         XC    PRHDR0,PRHDR0       RESET PRINTER QUEUE HDR & ENTRY              
         MVC   PRQESCN,BYTE1                                                    
         XC    PRHDR1,PRHDR1                                                    
         XC    PNTRY,PNTRY                                                      
         LA    R4,SRVP4H                                                        
         B     ERR9                                                             
STR25B   CLC   SPAGE(2),PAGEEROR                                                
         BE    STR26                                                            
         CLC   SPAGE(2),PQPAGES                                                 
         BH    ERR9                                                             
*                                                                               
STR26    XC    SAVE(12),SAVE       FIND START PAGE GIVEN BY SPAGE               
         MVC   SAVE+2(2),SPAGE                                                  
         MVC   SAVE+4(4),=C'PAGE'                                               
         GOTO1 ADATAMGR,DMCB,(X'00',RANDOM),PRTQID,NDX,SAVE,(R5)                
         CLI   8(R1),0                                                          
         BNE   STRERR                                                           
         L     RE,BUSAVE           POINT TO BUFFER SAVE STORAGE                 
         AR    RE,R5                                                            
         USING SKBUFFD,RE                                                       
*                                                                               
STR40    MVC   PRLPP,PQLPP         SET PRINTER QUEUE HEADER                     
         MVC   PRSEQ,PQSEQ                                                      
         MVC   PRCIHIGH,SKENDCI                                                 
         MVC   PRCINEXT,SKNXTCI                                                 
         MVC   PRDISP,SKDISP                                                    
         MVC   PRADDR,SKADDR                                                    
         XC    PRLNCTR,PRLNCTR                                                  
         MVC   PRLINES,SKLINES                                                  
         MVC   PRPAGES,SKPAGES+2                                                
         CLI   PRSEQ,0                                                          
         BNE   STR42                                                            
         OC    PRCINEXT,PRCINEXT                                                
         BZ    STR42                                                            
         MVI   PRSEQ,1                                                          
         DROP  RE                                                               
*                                                                               
STR42    EQU   *                                                                
         OC    EPAGE(2),EPAGE                                                   
         BZ    *+8                                                              
         OI    PR1FLAG,PR1FPP      SET PARTIAL PRINT FLAG                       
*                                                                               
         MVC   PRHDR1F,PRHDR1      INITIALISE CHECKPOINT VERSIONS               
         MVC   PRHDR1P,PRHDR1                                                   
         MVC   PRHDR1S,PRHDR1                                                   
*                                                                               
         L     R5,APRN                                                          
         OI    TSTAT-UTLD+1(R5),X'02' SET RESTART FLAG                          
         MVI   PRSTAT1,PRS1MRS                                                  
         B     PSS                 GO START PRINTER                             
                                                                                
*----------------------------------------------------------------------         
* STOP PRINTER - STP                                                            
*----------------------------------------------------------------------         
STP      L     R6,APRNQ                                                         
         USING PRQD,R6                                                          
         TM    PRSTAT,PRSACTV      MUST BE ACTIVE                               
         BZ    ERR13                                                            
         NI    PRSTAT,X'F8'        CLEAR STOP FLAGS                             
         IC    RF,SPAGE+1                                                       
         STC   RF,DUB                                                           
         CLI   DUB,0               DEFAULT IS STOP PRINTER NOW                  
         BNE   *+8                                                              
         MVI   DUB,X'01'                                                        
         OC    PRSTAT,DUB          SET STOP FLAGS                               
*                                                                               
         CLI   DUB,1               STOP NOW                                     
         BNE   *+12                                                             
         MVI   RESULT,5                                                         
         B     PSS                                                              
         MVI   RESULT,6            STOP SCHEDULED                               
         MVI   NEWTRM,2            FLAG Q CHANGED                               
         MVI   DSPFLAG,0           SET PART DISPLAY                             
         MVI   ACTN2,1             SET FIRST ENTRY                              
         BRAS  RE,DSP                                                           
         LA    R4,SRVP3H                                                        
         B     MSGOUT                                                           
                                                                                
*----------------------------------------------------------------------         
* FLUSH PRINTER - FLS                                                           
*----------------------------------------------------------------------         
FLS      L     R6,APRNQ            FLUSH PRINTER                                
         USING PRQD,R6                                                          
         TM    PRSTAT,PRSACTV      MUST BE ACTIVE                               
         BZ    ERR13                                                            
         NI    PRSTAT,X'8F'        CLEAR FLUSH FLAGS                            
         IC    RF,SPAGE+1                                                       
         SLL   RF,4                                                             
         STC   RF,DUB                                                           
         CLI   DUB,0               DEFAULT IS FLUSH REPORT                      
         BNE   *+8                                                              
         MVI   DUB,X'20'                                                        
         OC    PRSTAT,DUB          SET FLUSH FLAGS                              
*                                                                               
         MVI   RESULT,7                                                         
         MVI   DSPFLAG,0           SET PART DISPLAY                             
         MVI   ACTN2,1             SET FIRST ENTRY                              
         BRAS  RE,DSP                                                           
         LA    R4,SRVP3H                                                        
         B     MSGOUT                                                           
                                                                                
*----------------------------------------------------------------------         
* STA                                                                           
*----------------------------------------------------------------------         
STA      XC    PQSCIADS(255),PQSCIADS                                           
         MVI   SELFLAG,C'N'        CLR INPUT FLAG                               
         OC    PQSCHANG,PQSCHANG                                                
         BZ    *+12                                                             
         MVI   PQSQPOS,0           SET TO 0 IF ANY CHANGES                      
         B     STATX                                                            
         LA    R4,SRVSA1H                                                       
         SR    RE,RE                                                            
         IC    RE,PQSQPOS                                                       
STA01    CLI   5(R4),0             TEST ANY INPUT                               
         BNE   STAT02                                                           
STATNXT  LA    RE,1(RE)            BUMP Q POSN                                  
         LA    R4,93(R4)           NEXT SELECT LINE                             
         LA    R1,SRVPFKH                                                       
         CR    R4,R1               TEST FOR END OF SCREEN                       
         BL    STA01                                                            
         B     STA05               GO PROCESS ACTIONS IF ANY                    
*                                                                               
STAT02   MVI   SELFLAG,C'Y'        FLAG INPUT FOUND                             
         LA    R1,PQSCIADS(RE)                                                  
         CLI   5(R4),1                                                          
         BNE   STAT03                                                           
         CLC   8(1,R4),SR@TOP      TEST FOR TOP INPUT                           
         BNE   *+8                                                              
         MVI   0(R1),C'T'                                                       
         OC    PRCIADDR,PRCIADDR   IF PRINTER IS ACTIVE                         
         BZ    *+10                                                             
         LTR   RE,RE               DELETE IS INVALID FOR 1ST ENTRY              
         BZ    STATERR                                                          
         CLC   8(1,R4),SR@DEL      TEST FOR DEL INPUT                           
         BNE   *+8                                                              
         MVI   0(R1),C'D'                                                       
         CLI   0(R1),0             TEST SOMETHING VALID WAS INPUT               
         BE    STATERR                                                          
         B     STATOK                                                           
STAT03   CLI   5(R4),2                                                          
         BNE   STATERR                                                          
         CLC   8(1,R4),SR@DEL      TEST FOR DD                                  
         BNE   STATERR                                                          
         CLC   9(1,R4),SR@DEL                                                   
         BNE   STATERR                                                          
         B     STATOK                                                           
*                                                                               
STATERR  MVC   8(3,R4),SR@ERROR                                                 
         B     STATNXT                                                          
STATOK   MVC   8(3,R4),=C'   '                                                  
         B     STATNXT                                                          
*                                                                               
STA05    LA    R7,PQSCIADS         R7=SAVED ACTIONS                             
         CLI   0(R7),C'T'                                                       
         BNE   *+8                                                              
         MVI   0(R7),0                                                          
         LA    R5,PQSCIADS+255                                                  
STA05A   MVC   FLAG,0(R7)          SAVE COMMAND IN FLAG                         
         CLI   0(R7),C'D'                                                       
         BE    STA06                                                            
         CLI   0(R7),C'T'                                                       
         BE    STA07                                                            
STA05B   MVI   0(R7),0                                                          
         LA    R7,1(R7)            NEXT                                         
         CR    R7,R5                                                            
         BNE   STA05A                                                           
         B     STATX                                                            
*                                                                               
STA06    LR    R1,R5               SQUASH IT OUT                                
         SR    R1,R7                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R7),1(R7)                                                    
         CLI   PQSCIADS,C'T'       DO NOT ALLOW T ON FIRST ENTRY                
         BNE   *+8                                                              
         MVI   PQSCIADS,0                                                       
*                                                                               
STA07    MVI   NEWTRM,2            FLAG Q CHANGED                               
         LR    R1,R7               CALCULATE ENTRY NUMBER                       
         LA    R0,PQSCIADS                                                      
         SR    R1,R0                                                            
         LA    R1,1(R1)                                                         
         STC   R1,BYTE                                                          
         BAS   RE,DELNTRY                                                       
         CLI   FLAG,C'D'                                                        
         BE    STA05A                                                           
*                                                                               
STA08    SR    R1,R1               HALF CONTAINS DELETED ENTRY                  
         ICM   R1,3,HALF                                                        
         BCTR  R1,0                                                             
         MH    R1,=Y(L'PNTRY)                                                   
         LA    R1,6(R1)                                                         
         A     R1,APRQENTS                                                      
*                                                                               
         OC    PRCIADDR,PRCIADDR          TEST FOR ACTIVE TOP ENTRY             
         BNZ   STA09                                                            
         MVC   PNNEXT-PNTRY(2,R1),HALF    SET TO NEW TOP OF QUEUE               
         XC    0(L'PNTRY,R1),PNTRY                                              
         XC    PNTRY,0(R1)                                                      
         XC    0(L'PNTRY,R1),PNTRY                                              
         B     STA05B                                                           
*                                                                               
STA09    MVC   PNNEXT-PNTRY(2,R1),PNNEXT  SET TO NEW 2ND ENTRY                  
         MVC   PNNEXT,HALF                                                      
         B     STA05B                                                           
*                                                                               
STATX    OC    PQSCHANG,PQSCHANG   TEST FOR CHANGE OF INPUT                     
         BNZ   STATX0                                                           
         IC    R1,PQSQPOS                                                       
         CLI   SELFLAG,C'N'        IF NO SELECT INPUT                           
         BNE   *+12                                                             
         CLI   PFKEY,0             ENTER COUNTS AS PFDOWN                       
         BE    *+12                                                             
         CLI   PFKEY,PFDOWN        IF DOWN KEY                                  
         BNE   *+16                                                             
         LA    R1,16(R1)           BUMP TO NEXT SCREEN                          
         CLM   R1,1,PRQNE          TEST IF WE HAVE GONE OVER                    
         BNL   *+12                                                             
         CLI   PFKEY,PFUP7         IF UP KEY                                    
         BNE   *+8                                                              
         SH    R1,=H'16'           BACK TO PREV SCREEN                          
         LTR   R1,R1                                                            
         BNM   *+6                 TEST WE HAVE NOT GONE NEGATIVE               
         SR    R1,R1                                                            
         STC   R1,PQSQPOS                                                       
STATX0   CLI   ACTN2,0             TEST FOR STAT,N                              
         BE    STATX1                                                           
         IC    R1,ACTN2                                                         
         BCTR  R1,0                                                             
         STC   R1,PQSQPOS                                                       
STATX1   BRAS  RE,DSP                                                           
         MVI   RESULT,4            STATUS AND QUEUE DISPLAYED                   
         CLI   SELFLAG,C'N'                                                     
         BE    *+8                                                              
         MVI   RESULT,8            NEW QUEUE DISPLAYED                          
         LA    R4,SRVSA1H                                                       
         B     MSGOUT                                                           
                                                                                
*----------------------------------------------------------------------         
* SCH                                                                           
*----------------------------------------------------------------------         
SCH      LA    R4,SRVP2H           SCHEDULE REPORTS FOR PRINTING                
         ST    R4,CURSOR                                                        
         CLI   PQSSUBID,0                                                       
         BE    ERR4                REPORT ID REQUIRED                           
*                                                                               
SCH2     XC    NDX,NDX             FIND WHICH PRTQ FILE TO SEARCH               
         XC    APRTQLST,APRTQLST                                                
         TM    PQSUSER,X'80'       TEST IF GENERIC ID INPUT                     
         BO    SCH2A               YES                                          
         MVC   NXSRCID,USERID      NO PRTQ ID FROM SPECIFIC USER ID             
         GOTO1 ADATAMGR,PQDMCB,(0,GFILE),PRTQUE,NDX,,CXREC                      
         MVC   PRTQID,NXUSRINF                                                  
         B     SCH3                                                             
*                                                                               
SCH2A    LA    RE,PRTQLST+8        POINT TO FIRST PRTQ FILE                     
         ST    RE,APRTQLST                                                      
         MVC   PRTQID+4(1),1(RE)   SET PRTQ ID FROM LIST                        
         MVI   PRTQID+5,C' '                                                    
*                                                                               
SCH3     XC    DUB,DUB             INITIALISE REPORT FOUND DATA                 
         MVC   DUB(2),=4X'FF'                                                   
         XC    DUB1,DUB1                                                        
         XC    WORK(8),WORK                                                     
         XC    SCANHI,SCANHI                                                    
         XC    SCANLO,SCANLO                                                    
         XC    SCANCNT,SCANCNT                                                  
*                                                                               
SCH4     GOTO1 ADATAMGR,PQDMCB,(0,BUFFER),PRTQID,,,CXREC                        
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
         MVC   BUFFDATA,CXREC      SAVE V(PQTAB)/V(PQFILE)                      
         MVC   CIDATA,CXREC+12     SAVE FILE DATA FOR THIS PRTQ FILE            
         MVC   FIWRES,PRTQID       SET RESOURCE                                 
         BRAS  RE,FIRSET           SET INDEX ADDRESSES FOR RESOURCE             
         JNE   *+2                 INVALID PRTQ                                 
         MVC   FIWNDA,FIWP1A       A(START OF PART1 INDEXES)                    
         MVI   RDIND,X'00'         SET READ FOR UPDATE                          
         LA    R5,FIWNDX                                                        
         USING PQRECD,R5           R5=A(PRTQUE INDEX ENTRY)                     
         B     SCH5A                                                            
*                                                                               
SCH5     BRAS  RE,FIRNSN           NEXT PRINT QUEUE REPORT                      
         BNE   SCH13               END OF PART1S                                
SCH5A    L     R1,FIWNDA                                                        
         MVC   FIWNDX(L'PQINDEX),SI1NDX-SI1PARD(R1)                             
         BRAS  RE,FIRNC            A(INDEX NODE) TO A(CI)                       
         MVC   CIADDR,FIWCIA                                                    
*                                                                               
SCH10    DS    0H                                                               
*&&US                                                                           
         CLC   USERID,=X'0406'     IF GRAFNET MUST BE CLASS G                   
         BNE   SCH101                                                           
         CLI   PQSCLASS,C'G'                                                    
         BNE   ERRG1                                                            
         B     SCH10C                                                           
SCH101   CLI   PQSCLASS,C'G'                                                    
         BE    ERRG3                                                            
         CLI   PQSCLASS,C'N'       CLASS N (DARE) NOTHING TO PRINT              
         BE    ERR24                                                            
*&&                                                                             
         TM    PQSUSER,X'80'       TEST GENERIC ID                              
         BO    SCH10A                                                           
         CLC   PQSRCID,USERID      TEST USER ID MATCH IN QUEUE                  
         BE    SCH10C                                                           
         B     SCH12                                                            
*                                                                               
SCH10A   CLI   AGENIDS,X'FF'       TEST VGENIDS FOUND                           
         BE    SCH12               NO                                           
         GOTO1 AGENIDS,DMCB,PQSUSER,ADATAMGR                                    
         BNE   SCH12                                                            
         LM    RE,RF,0(R1)         RE=N'ENTRIES, RF=A(ENTRIES)                  
         CLC   PQSRCID,0(RF)       MATCH SOURCE ID                              
         BE    SCH10C                                                           
         LA    RF,2(RF)            BUMP TO NEXT                                 
         BCT   RE,*-14                                                          
         B     SCH12                                                            
*                                                                               
SCH10C   CLI   PQSCLASS,0          SAME CLASS (POSITIVE OR NEGATIVE)            
         BE    SCH10C1                                                          
         TM    PQSCLASS,X'40'                                                   
         BZ    *+18                                                             
         CLC   PQCLASS,PQSCLASS                                                 
         BNE   SCH12                                                            
         B     SCH10C1                                                          
         MVC   FLAG,PQSCLASS                                                    
         OI    FLAG,X'40'                                                       
         CLC   PQCLASS,FLAG                                                     
         BE    SCH12                                                            
SCH10C1  EQU   *                                                                
*&&US                                                                           
         CLI   PQSCLASS,C'G'       IF NOT GRAFNET DON'T PROCESS G               
         BE    SCH10D                                                           
         CLI   PQCLASS,C'G'                                                     
         BE    SCH12                                                            
         CLI   PQCLASS,C'N'        DON'T PROCESS N (DARE)                       
         BE    SCH12                                                            
*&&                                                                             
SCH10D   CLC   PQSUBID,=C'LU1'     IF THIS IS AN LU1 REPORT                     
         BNE   *+12                                                             
         TM    PQSTAT,PQSTKE       AND ITS KEEP                                 
         BO    SCH10DT             IGNORE ALL GENERIC TESTS                     
*                                                                               
         CLC   PQSSUBID,SR@ALL     SAME SUB-USER ID                             
         BE    SCH10E                                                           
*                                                                               
         CLI   PQSSUBID+1,C'*'     ALLOW X* GENERIC                             
         BNE   *+18                                                             
         CLC   PQSSUBID(1),PQSUBID                                              
         BE    SCH10E                                                           
         B     SCH12                                                            
         CLI   PQSSUBID+2,C'*'     ALLOW XX* GENERIC                            
         BNE   *+18                                                             
         CLC   PQSSUBID(2),PQSUBID                                              
         BE    SCH10E                                                           
         B     SCH12                                                            
SCH10DT  CLC   PQSUBID,PQSSUBID                                                 
         BNE   SCH12                                                            
*                                                                               
SCH10E   CLI   PQSEQ,1             BYPASS HIGH ORDER CI'S                       
         BH    SCH12                                                            
         TM    PRQATT2,PRQAASS     TEST ALLOW SPECIAL STATUS                    
         BZ    *+12                                                             
         TM    PQSTAT,PQSTHO       TEST FOR SPECIAL STATUS                      
         BO    *+12                                                             
         TM    PQSTAT,PQSTAC       ELSE ACTIVE REPORTS ONLY                     
         BZ    SCH12                                                            
         TM    PQSTAT,PQSTIN       IGNORE INVISIBLE REPORTS                     
         BO    SCH12                                                            
         OC    PQSSEQ,PQSSEQ       TEST FOR ALL SEQ NO                          
         BZ    *+14                                                             
         CLC   PQREPNO,PQSSEQ      SEQ NUMBER IN RANGE                          
         BNE   SCH12                                                            
*                                                                               
         CLI   PQAGERT,X'FE'       A PRINTING REPORT CAN BE SCHEDULED           
         BNE   SCH10F              IF IT IS SPECIFIC AND HAS 2ND PAGE           
         CLC   PQREPNO,PQSSEQ                                                   
         BNE   SCH12                                                            
         OC    EPAGE(2),EPAGE                                                   
         BZ    SCH12                                                            
*                                                                               
SCH10F   TM    PQATTB,PQATJOBI     IGNORE REPORTS THAT CONTAIN JCL              
         BO    SCH12                                                            
         TM    PQATTB,PQATNP       TEST NON PRINTABLE REPORT                    
         BZ    SCH10G                                                           
         B     SCH12                                                            
*                                                                               
SCH10G   TM    PQATTB,PQATPW       TEST PASSWORD PROTECTED REPORT               
         BNO   SCH10H                                                           
         TM    PRNTYP,RMC          DO NOT SEND TO SHUTTLE                       
         BO    SCH12                                                            
         CLC   PQREPNO,PQSSEQ      MUST BE SPECIFIC                             
         BNE   SCH12                                                            
*                                                                               
         L     RF,ACIREC                                                        
         GOTO1 ADATAMGR,DMCB,(X'00',DMREAD),PRTQID,CIADDR,(RF)                  
         CLI   8(R1),0                                                          
         BNE   SCH12                                                            
         L     RF,ACIREC                                                        
         CLC   PQKEY,0(RF)         VALIDATE KEY                                 
         BNE   SCH12                                                            
         MVC   GSECVAL,PQSECINF-PQRECD(RF)                                      
         L     RF,ASECVAL                                                       
         BASR  RE,RF               VALIDATE SECURITY                            
         BNE   ERRSEC                                                           
*                                                                               
SCH10H   LA    RF,DATEC            SET RF TO NEW/OLD CMPRSD DATE                
         TM    PQTYP1,PQTYNCD                                                   
         BO    *+8                                                              
         LA    RF,DATECO                                                        
         CLC   PQAGELD,0(RF)       BYPASS FUTURE DATED REPORTS                  
         BNH   SCH10J                                                           
         TM    DDS,DDSTRM                                                       
         BZ    SCH12                                                            
*                                                                               
SCH10J   EQU   *                   REPORT IS A CANDIDATE                        
*                                                                               
SCH10S   TM    PQSUSER,X'80'       TEST IF MATCHED ON GENERIC                   
         BZ    SCH10T              NO                                           
         OC    DUB1(2),DUB1        TEST IF FIRST MATCH                          
         BNZ   *+14                NO                                           
         MVC   DUB1(2),PQSRCID     YES SAVE SPECIFIC USER ID                    
         B     SCH10T                                                           
         CLC   DUB1(2),PQSRCID     TEST IF SAME USER ID AS PREV                 
         BNE   SCH12                                                            
*                                                                               
SCH10T   SR    R1,R1               BUMP NUM OF HITS                             
         ICM   R1,3,SCANCNT                                                     
         LA    R1,1(R1)                                                         
         STCM  R1,3,SCANCNT                                                     
*                                                                               
         MVC   HALF3,PQAGELD                                                    
         TM    PQTYP1,PQTYNCD      TEST NEW/OLD CMPRSD DATE                     
         BO    SCH10U                                                           
         GOTO1 ADATCON,DMCB,(2,HALF3),(30,HALF3)                                
SCH10U   CLC   HALF3,SCANLO                                                     
         BL    *+14                                                             
         CLC   PQAGELT,SCANLO+2    SAVE HI & LO DATE/TIMES                      
         BH    *+16                                                             
         MVC   SCANLO+0(2),HALF3                                                
         MVC   SCANLO+2(2),PQAGELT                                              
*                                                                               
         CLC   HALF3,SCANHI                                                     
         BH    *+14                                                             
         CLC   PQAGELT,SCANHI+2                                                 
         BL    *+16                                                             
         MVC   SCANHI+0(2),HALF3                                                
         MVC   SCANHI+2(2),PQAGELT                                              
*                                                                               
         CLC   HALF3,DUB           TEST WITH LOWEST SEQ NUM SO FAR              
         BL    SCH11                                                            
         BH    SCH12                                                            
         CLC   PQAGELT,DUB+2                                                    
         BH    SCH12                                                            
SCH11    MVC   DUB(2),HALF3        SAVE LOWEST CDATE                            
         MVC   DUB+2(2),PQAGELT    SAVE LOWEST CTIME                            
         MVC   DUB+4(4),CIADDR     SAVE PAGE/ENTRY OF LOWEST SEQ NUM            
         MVC   DUB1+4(1),PRTQID+4  SAVE PRTQ FILE ID                            
         MVC   WORK(8),PQKEY       SAVE KEY OF LOWEST SEQ NUM                   
*                                                                               
SCH12    B     SCH5                BUMP TO NEXT INDEX ENTRY                     
*                                                                               
SCH13    ICM   RE,15,APRTQLST      WERE WE LOOKING FOR GENERIC ID               
         BZ    SCH14               NO                                           
         OC    DUB+4(4),DUB+4      YES EXIT IF FOUND A MATCHING REPORT          
         BNZ   SCH14                                                            
         LA    RE,8(RE)            BUMP TO NEXT PRTQ FILE IN LIST               
         ST    RE,APRTQLST                                                      
         CLI   0(RE),0             TEST END OF PRTQ FILE LIST                   
         BE    SCH14                                                            
         MVC   PRTQID+4(1),1(RE)   SET PRTQ FILE ID FROM LIST                   
         MVI   PRTQID+5,C' '                                                    
         B     SCH4                BACK TO SEARCH THIS PRTQ FILE                
*                                                                               
SCH14    CLI   ACTN,X'14'          TEST EXPLICIT SCHED ACTION                   
         BNE   SCH14A                                                           
         CLI   ACTN1,TYPEPERM      TEMP SCHEDULE IS DEFAULT                     
         BE    SCH14B                                                           
SCH14A   OC    DUB+4(4),DUB+4      SCHED,TEMP (DEFAULT FOR NO INPUT)            
         BZ    ERR11               ERROR IF NO REPORTS FOUND                    
*                                                                               
SCH14B   XC    WORK1,WORK1         CREATE DUMMY ENTRY IN WORK1                  
         MVC   WORK1(2),USERID                                                  
         OC    PQSUSER,PQSUSER     TEST FOR GENERIC USERID INPUT                
         BZ    *+10                                                             
         MVC   WORK1(2),PQSUSER                                                 
*                                                                               
         MVC   WORK1+2(3),PQSSUBID COPY SUBID                                   
*                                                                               
         CLI   IFCOPF,0            TEST IF COPYS=                               
         BE    *+10                                                             
         MVC   WORK1+9(1),IFCOPV   SET COPIES                                   
*                                                                               
         CLI   ACTN1,TYPEPERM                                                   
         BE    SCH14CA                                                          
         CLC   SCANCNT,=X'0001'    TEST SINGLE REPORT MATCH                     
         BNE   SCH14CA                                                          
         MVC   WORK1+5(2),WORK+5                                                
         B     SCH14D                                                           
*                                                                               
SCH14CA  OC    PQSSEQ,PQSSEQ       IF NO SEQ NO SET TIME NOW                    
         BZ    SCH14C                                                           
         MVC   WORK1+5(2),PQSSEQ   ELSE USE SPECIFIC SEQ NO                     
         B     SCH14D                                                           
*                                                                               
SCH14C   CLI   ACTN1,TYPEPERM      ALL SEQ IS OK FOR PERM                       
         BE    SCH14D                                                           
         OI    WORK1+9,PNCTIME     ELSE SET TIME FLAG                           
         GOTO1 ATICTOC,PARMS,C'BGET'                                            
         L     RF,0(R1)                                                         
         MH    RF,=H'3'                                                         
         SRL   RF,2                                                             
         STCM  RF,3,WORK1+5        AND SET TIME NOW                             
         OC    PQSTIMES,PQSTIMES                                                
         BZ    SCH14D                                                           
         MVC   WORK1+5(2),PQSTIMES+2  USE TIME FROM REPID                       
*                                                                               
SCH14D   MVC   WORK1+7(1),PQSCLASS SET CLASS                                    
         TM    DDS,DDSGRP                                                       
         BZ    SCH14E                                                           
         CLC   WORK1(2),GRPUSER                                                 
         BNE   SCH14E                                                           
         OI    WORK1+8,PREXGRP     SET ENTRY IS FOR A GROUP ID                  
*                                                                               
SCH14E   OC    SPAGE,SPAGE         TEST IF PAGES=                               
         BZ    SCH14F                                                           
         LA    R4,SRVP4H                                                        
         L     R5,ACIREC                                                        
         MVC   PRTQID+4(1),DUB1+4  SET FILE ID OF PRTQ FILE                     
         MVI   PRTQID+5,C' '                                                    
         GOTO1 ADATAMGR,DMCB,(0,BUFFER),PRTQID,,,(R5)                           
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   BUFFDATA,0(R5)      SAVE V(PQTAB)/V(PQFILE)                      
         MVC   CIDATA,12(R5)       SAVE FILE DATA FOR THIS PRTQ FILE            
         MVC   FIWRES,PRTQID       PRINT QUEUE ID                               
         BRAS  RE,FIRSET           SET INDEX ADDRESSES FOR RESOURCE             
         JNE   *+2                                                              
         MVC   CIADDR,DUB+4                                                     
         MVC   FIWCIA,CIADDR       A(CI) NEEDED                                 
         BRAS  RE,FIRCN            CONVERT A(CI) TO A(INDEX NODE)               
*                                                                               
         L     R5,ACIREC           READ FIRST CI REC OF REPORT                  
         USING PQRECD,R5                                                        
         GOTO1 ADATAMGR,DMCB,(X'00',DMREAD),PRTQID,CIADDR,(R5)                  
         CLI   8(R1),0                                                          
         BNE   SCHERR                                                           
         CLC   WORK(8),0(R5)       COMPARE KEYS IN REC AND INDEX                
         BNE   SCHERR                                                           
         CLC   EPAGE,PAGELAST                                                   
         BNE   *+10                                                             
         MVC   EPAGE,PQPAGES                                                    
         XC    WORK1+2(3),WORK1+2                                               
         MVC   WORK1+2(2),SPAGE                                                 
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RE,3,EPAGE                                                       
         ICM   RF,3,SPAGE                                                       
         SR    RE,RF                                                            
         BM    ERR14                                                            
         CHI   RE,255                                                           
         BH    ERR23                                                            
         STC   RE,WORK1+4                                                       
         OI    WORK1+9,PNCENDP                                                  
SCH14F   EQU   *                                                                
*                                                                               
SCH15    LA    R7,PNTRY                                                         
SCH15A   CLC   0(8,R7),WORK1       SEARCH CURRENT PRINTER QUEUE                 
         BE    SCH15B                                                           
         SR    RF,RF                                                            
         ICM   RF,3,10(R7)                                                      
         BZ    SCH16               LAST ENTRY                                   
         BCTR  RF,0                                                             
         MH    RF,=Y(L'PNTRY)                                                   
         LA    R7,6(RF)                                                         
         A     R7,APRQENTS                                                      
         B     SCH15A                                                           
SCH15B   CLI   ACTN,X'14'          EXPLICIT SCHEDULE                            
         BNE   SCH17               NO IGNORE DUPLICATE                          
*                                                                               
SCH16    BAS   RE,SSET             SUSPEND TIMER                                
         SR    R1,R1                                                            
         ICM   R1,1,PRQNE          TEST NUMBER OF QUEUE ENTRYS                  
         BNZ   SCH16A                                                           
         MVC   PNTRY,WORK1         IF 1ST ENTRY JUST MOVE IT IN                 
         XC    HALF,HALF           AND MAKE SURE HALF IS ZERO                   
         B     SCH16B                                                           
*                                                                               
SCH16A   CLM   R1,1,PRQNEMAX                                                    
         BNL   ERR3                QUEUE NOW TOO LONG                           
         LA    RF,WORK1                                                         
         GOTO1 ALCM,DMCB,VTADDPRQ,(RF),0                                        
         ICM   R1,15,8(R1)                                                      
         BZ    ERR7                ERROR QUEUE FULL                             
         STH   R1,HALF             SAVE INDEX ENTRY IN HALF                     
SCH16B   MVI   NEWTRM,2            FLAG Q ENTRY ADDED                           
         SR    R1,R1                                                            
         IC    R1,PRQNE            BUMP NUMBER OF QUEUE ENTRYS                  
         LA    R1,1(R1)                                                         
         STC   R1,PRQNE                                                         
         MVC   MSHIFTS,PRQNE       SET MAXIMUM NUM OF QUEUE SHIFTS              
         MVI   NSHIFTS,0                                                        
         SR    RF,RF                                                            
         ICM   RF,3,PNLAST                                                      
         BNZ   SCH16C                                                           
         LA    RF,PNTRY            IF PNLAST IS ZERO USE 1ST                    
         B     SCH16D                                                           
*                                                                               
SCH16C   BCTR  RF,0                ELSE LOCATE LAST ENTRY                       
         MH    RF,=Y(L'PNTRY)                                                   
         LA    RF,6(RF)                                                         
         A     RF,APRQENTS                                                      
SCH16D   MVC   10(2,RF),HALF       SET NEXT POINTER                             
         MVC   PNLAST,HALF         SAVED INDEX BECOMES NEW LAST                 
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,HALF                                                        
         BNZ   *+12                                                             
         LA    RF,PNTRY            USE 1ST ENTRY IF ZERO                        
         B     SCH16E                                                           
*                                                                               
         BCTR  RF,0                LOCATE NEW ENTRY                             
         MH    RF,=Y(L'PNTRY)                                                   
         LA    RF,6(RF)                                                         
         A     RF,APRQENTS                                                      
*                                                                               
SCH16E   CLI   ACTN1,TYPEPERM      SCHED,PERM WAS INPUT                         
         BNE   *+8                                                              
         OI    8(RF),X'40'         SET PERMANENT ENTRY FLAG                     
         OC    DUB+4(4),DUB+4                                                   
         BZ    *+8                                                              
         OI    8(RF),X'80'         SET ACTIVE ENTRY FLAG                        
*                                                                               
SCH17    CLI   ACTN,X'14'          EXPLICIT SCHEDULE                            
         BE    SCH18               YES EXIT WITH QUEUED RESULT                  
         OC    PRCIADDR,PRCIADDR   NO GO START INACTIVE PRINTER                 
         BZ    STR1A                                                            
*                                                                               
SCH18    BRAS  RE,DSP                                                           
         MVI   RESULT,1                                                         
         B     MSGOUT                                                           
*                                                                               
SCHERR   DC    H'0'                DIE IF CANT READ CX REC                      
                                                                                
*----------------------------------------------------------------------         
* KILL PRINTER - KIL                                                            
*----------------------------------------------------------------------         
KIL      L     R6,APRNQ            KILL PRINTER                                 
         USING PRQD,R6                                                          
         MVI   NEWTRM,2            FLAG Q ENTRY ADDED                           
         OC    PRCIADDR,PRCIADDR   IS PRINTER ACTIVE                            
         BZ    KIL6                NO                                           
         MVC   PRTQID+4(1),PRPRTQA YES GET PRTQ FILE ID                         
         MVI   PRTQID+5,C' '                                                    
         L     R4,ACIREC                                                        
         GOTO1 ADATAMGR,DMCB,(0,BUFFER),PRTQID,,,(R4)                           
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   BUFFDATA,0(R4)      SAVE V(PQTAB)/V(PQFILE)                      
         MVC   CIDATA,12(R4)       SAVE FILE DATA FOR THIS PRTQ FILE            
         MVC   FIWRES,PRTQID       SET RESOURCE                                 
         BRAS  RE,FIRSET           SET INDEX ADDRESSES FOR RESOURCE             
         JNE   *+2                 INVALID PRTQ                                 
*                                                                               
         CLC   PR1SRCID,PUBMIN                                                  
         BL    *+14                                                             
         CLC   PR1SRCID,PUBMAX                                                  
         BNH   KIL6                PUBLIC REPORTS HAVE ACTIVE STATUS            
         TM    PNEX,PREXGRP                                                     
         BO    KIL6                GROUP REPORTS HAVE ACTIVE STATUS             
*                                                                               
KIL1     MVC   CIADDR,PRCIADDR     YES FIND REPORT ENTRY IN INDEX               
         L     R4,ACIREC                                                        
         GOTO1 ADATAMGR,DMCB,(X'00',DMREAD),PRTQID,CIADDR,(R4)                  
         CLI   8(R1),0                                                          
         BNE   KIL6                                                             
*                                                                               
         MVC   FIWCIA,CIADDR       SET A(CI)                                    
         BRAS  RE,FIRCN            CONVERT TO A(INDEX) AND REPORT #             
         BRAS  RE,FIRRLOCK         LOCK THE REPORT                              
         L     R5,FIWNDA                                                        
         LA    R5,SI1NDX-SI1PAR(R5)                                             
         NI    PQSTAT-PQINDEX(R5),255-PQSTPG                                    
         MVC   PQAGEDD-PQINDEX(5,R5),PQAGEDD-PQINDEX(R4)                        
         BRAS  RE,FIRRUNLK         UNLOCK THE REPORT                            
*                                                                               
KIL6     BAS   RE,SSET             SUSPEND TIMER                                
         MVI   RESULT,5            STOP PRINTER                                 
         B     PSS                                                              
*                                                                               
KIL8     EQU   *                   RETURN FROM PSS STOP                         
*                                                                               
KIL9     L     R6,APRNQ                                                         
         MVI   PRSTAT,0            SET PRINTER INACTIVE                         
         XC    PRCIADDR,PRCIADDR                                                
         XC    PRHDR1,PRHDR1                                                    
         XC    PRHDR1F,PRHDR1F                                                  
         XC    PRHDR1P,PRHDR1P                                                  
         XC    PRHDR1S,PRHDR1S                                                  
KIL10    CLI   ACTN1,1             ERASE QUEUE IF KILL,CLEAR                    
         BNE   KILXX                                                            
         MVC   BYTE,PRQMODE        SAVE MODE                                    
         MVC   BYTE1,PRQESCN       SAVE ESCAPE SEQUENCE NUMBER                  
         XC    PRHDR0,PRHDR0                                                    
         MVC   PRQMODE,BYTE                                                     
         MVC   PRQESCN,BYTE1                                                    
         XC    PRHDR1,PRHDR1                                                    
         XC    PRHDR1F,PRHDR1F                                                  
         XC    PRHDR1P,PRHDR1P                                                  
         XC    PRHDR1S,PRHDR1S                                                  
         XC    PNTRY,PNTRY                                                      
         LA    R7,PNTRY                                                         
         SR    RF,RF                                                            
         ICM   RF,3,10(R7)         GET NEXT ENTRY                               
         BZ    KILX                                                             
         XC    0(L'PNTRY,R7),0(R7) CLEAR 1ST ENTRY                              
         STCM  RF,3,HALF                                                        
KIL11    SR    RF,RF                                                            
         ICM   RF,3,HALF           GET INDEX TO NEXT ENTRY                      
         BZ    KILX                                                             
         BCTR  RF,0                                                             
         MH    RF,=Y(L'PNTRY)                                                   
         LA    RF,6(RF)                                                         
         A     RF,APRQENTS         GET A(ENTRY) IN RF                           
         SR    RE,RE                                                            
         ICM   RE,3,10(RF)         NEXT INDEX IS AT 10(RF)                      
         LH    RF,HALF                                                          
         STH   RE,HALF                                                          
         GOTO1 ALCM,DMCB,VTDELPRQ,(RF),0                                        
         BE    KIL11                                                            
         DC    H'0'                                                             
*                                                                               
KILX     XC    PNLAST,PNLAST       CLEAR FUCKING PNLAST                         
*                                                                               
KILXX    MVI   RESULT,9            SET PRINTER KILLED MESSAGE                   
         MVI   DSPFLAG,0                                                        
         BRAS  RE,DSP              DISPLAY QUEUE                                
         LA    R4,SRVP3H                                                        
         B     MSGOUT                                                           
         EJECT                                                                  
                                                                                
***********************************************************************         
* SKIP/ACTIVATE PRINTER                                                         
***********************************************************************         
PSS      L     R6,APRNQ            SKIP/ACTIVATE PRINTER                        
         USING PRQD,R6                                                          
         NI    PRSTAT,X'70'        TURN OFF ALL EXCEPT FLUSH FLAGS              
         CLI   RESULT,5                                                         
         BNE   *+12                                                             
         OI    PRSTAT,PRSSP        SET STOPPED FLAG                             
         B     PSSA                                                             
         OI    PRSTAT,PRSACTV      SET STARTED FLAG                             
         MVI   PRSTAT1,PRS1SOS     SET START OF SESSION                         
         CLI   RESULT,2                                                         
         BE    PSSA                START                                        
         CLI   RESULT,3                                                         
         BNE   PSSA                                                             
         OC    EPAGE(2),EPAGE      RESTART                                      
         BNZ   PSSA                                                             
         MVI   PRSTAT1,PRS1MRS     SET START OF MANUAL RESTART                  
         B     PSSA                                                             
*                                                                               
PSSA     L     R5,APRN             POINT TO TERMINAL                            
         USING UTLD,R5                                                          
*                                                                               
PSSVT    LA    R6,VTPRSTRT         VTAM - LET LCM DO THE TALKING                
         CLI   RESULT,2                                                         
         BE    PSSVT1              DO OPNDST IF STARTING                        
         CLI   RESULT,3                                                         
         BE    PSSVT1              DO PRSTRT IF RESTARTING                      
         CLI   RESULT,5                                                         
         BNE   PSSX                                                             
         LA    R6,VTPRSTOP                                                      
         CLI   ACTN,X'17'          DO CLSDST IF KILL,CLEAR OR RELEASE           
         BNE   PSSX                                                             
         CLI   ACTN1,0                                                          
         BE    PSSX                                                             
*                                                                               
         OI    TSTATU,TSTATDNE     MONITOR DOES CLSDST NOW                      
         B     PSSX                                                             
*                                                                               
PSSVT1   GOTO1 ALCM,DMCB,(R6),(R5)                                              
         B     PSSX                                                             
*                                                                               
PSSX     CLI   ACTN,X'17'          TEST STOP FOR KILL                           
         BE    KIL8                                                             
         MVI   DSPFLAG,0                                                        
         BRAS  RE,DSP                                                           
         LA    R4,SRVSA1H                                                       
         B     MSGOUT                                                           
         DROP  R5                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* DELETE A PRINTER QUEUE ENTRY                                                  
***********************************************************************         
DELNTRY  NTR1                                                                   
*                                                                               
         L     R6,APRNQ                                                         
         USING PRQD,R6                                                          
         CLI   BYTE,0                                                           
         BE    DELNTX                                                           
         LA    RE,PNTRY            RE=FIRST ENTRY                               
         SR    RF,RF                                                            
         IC    RF,BYTE             ENTRY NUMBER IN RF                           
         XC    HALF,HALF                                                        
DELN01   SR    R1,R1                                                            
         ICM   R1,3,PNNEXT-PNTRY(RE)                                            
         BZ    DELN04                                                           
         MVC   HALF1,HALF                                                       
         STH   R1,HALF             SAVE INDEX NUMBER OF NEXT ENTRY              
         ST    RE,FULL             SAVE A(ENTRY)                                
         BCTR  R1,0                LOCATE NEXT ENTRY                            
         MH    R1,=Y(L'PNTRY)                                                   
         LA    RE,6(R1)                                                         
         A     RE,APRQENTS                                                      
         BCT   RF,DELN01                                                        
         B     DELN05                                                           
*                                                                               
DELN04   OC    HALF,HALF           THIS IS THE LAST ENTRY                       
         BNZ   DELN06                                                           
         XC    PNTRY,PNTRY         ONLY ONE ENTRY                               
         MVI   PRQNE,0                                                          
         B     DELNTX                                                           
*                                                                               
DELN05   L     RF,FULL                                                          
         XC    0(L'PNTRY,RF),0(RE) SWAP ENTRY DATA IN CORE                      
         XC    0(L'PNTRY,RE),0(RF)                                              
         XC    0(L'PNTRY,RF),0(RE)                                              
         OC    PNNEXT-PNTRY(2,RF),PNNEXT-PNTRY(RF)                              
         BNZ   DELN07                                                           
DELN06   MVC   PNLAST,HALF1        SET NEW LAST                                 
         L     RF,FULL                                                          
         XC    PNNEXT-PNTRY(2,RF),PNNEXT-PNTRY(RF)                              
DELN07   CLI   FLAG,C'D'           TEST TRUE DELETE                             
         BNE   DELNTX                                                           
         LH    RF,HALF             DELETE NEXT ENTRY                            
         GOTO1 ALCM,DMCB,VTDELPRQ,(RF),0                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         IC    R1,PRQNE                                                         
         BCTR  R1,0                                                             
         STC   R1,PRQNE                                                         
*                                                                               
DELNTX   J     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* CVD, SSET, MSGOUT                                                             
***********************************************************************         
CVD      EDIT  (R0),(8,DUB),ZERO=NOBLANK                                        
         BR    RE                                                               
*                                                                               
SSET     ST    RE,SAVERE                                                        
         GOTO1 ATICTOC,PARMS,C'SSET'                                            
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
         USING PRQD,R6                                                          
MSGOUT   TM    INTFLAG,X'90'                                                    
         BO    MSGOUTX                                                          
         CLI   ACTN,X'1B'          WAS THIS SCAN                                
         BE    SCANOUT                                                          
         ST    R4,CURSOR                                                        
         XC    MSG,MSG             DISPLAY RESULT MESSAGE                       
         LA    R4,MSG                                                           
         MVCDD 0(7,R4),SR#PRR      PRINTER                                      
         TM    PRNTYP,RMC                                                       
         BZ    *+10                                                             
         MVCDD 0(7,R4),SR#SHTL     SHUTTLE                                      
         MVC   8(8,R4),PRNSYM                                                   
         LA    R4,15(R4)                                                        
         CLI   0(R4),C' '                                                       
         BNE   *+10                                                             
         BCTR  R4,0                                                             
         B     *-10                                                             
         MVI   2(R4),C'-'                                                       
         LA    R4,4(R4)                                                         
*                                                                               
         MVI   SWORK,0                                                          
         CLI   RESULT,4                                                         
         BNE   MSGOUT1                                                          
*                                                                               
         EDIT  (B1,PRQNE),(3,SWORK+1),ALIGN=LEFT,ZERO=NOBLANK                   
         LR    R1,R0                                                            
         LA    R1,1(R1)                                                         
         STC   R1,SWORK                                                         
         LA    RF,SWORK(R1)                                                     
*                                                                               
         SR    R0,R0               PAGE N OF N DISPLAYED                        
         SR    R1,R1                                                            
         ICM   R1,1,PQSQPOS                                                     
         BZ    *+8                                                              
         D     R0,=F'16'                                                        
         LA    R1,1(R1)                                                         
         EDIT  (R1),(2,1(RF)),ALIGN=LEFT                                        
         LR    R1,R0                                                            
         LA    R1,1(R1)                                                         
         STC   R1,0(RF)                                                         
         LA    RF,0(R1,RF)                                                      
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         ICM   R1,1,PRQNE                                                       
         BZ    *+8                                                              
         D     R0,=F'16'                                                        
         LA    R1,1(R1)                                                         
         EDIT  (R1),(2,1(RF)),ALIGN=LEFT                                        
         LR    R1,R0                                                            
         LA    R1,1(R1)                                                         
         STC   R1,0(RF)                                                         
         LA    RF,0(R1,RF)                                                      
         MVI   0(RF),0                                                          
*                                                                               
MSGOUT1  CLI   RESULT,3            TEST RESTART                                 
         BNE   MSGOUT2                                                          
         OC    EPAGE(2),EPAGE      TEST PAGE1,PAGE2                             
         BZ    *+8                                                              
         MVI   RESULT,2                                                         
MSGOUT2  ZIC   RE,RESULT                                                        
         BCTR  RE,0                                                             
         L     RF,=A(RSLTMSG)                                                   
         A     RF,RELO                                                          
         AR    RF,RE                                                            
         SR    RE,RE                                                            
         IC    RE,0(RF)                                                         
         GOTO1 AGETTXT,DMCB,(RE),0,(C'I',0),(20,MSG),SWORK,X'00010000'          
         L     R1,CURSOR                                                        
         OI    6(R1),X'40'                                                      
*                                                                               
MSGOUTX  B     EXIT                                                             
                                                                                
***********************************************************************         
* SCANOUT                                                                       
***********************************************************************         
SCANOUT  EQU   *                                                                
         MVC   SCANSYM,PRNSYM                                                   
         EDIT  (B1,PRQNE),(3,SCANQUE),FILL=0                                    
         EDIT  (B1,PR1PEND),(3,SCANPND),FILL=0                                  
         MVC   WORK(10),=X'402120207A20207A2020'                                
         ED    WORK(10),TIMENOW                                                 
         MVC   SCANNOW,WORK+2                                                   
         MVC   WORK(10),=X'402120207A20207A2020'                                
         ED    WORK(10),PRSVHMS                                                 
         MVC   SCANLST,WORK+2                                                   
         L     R1,ASSB                                                          
         MVC   WORK(10),=X'402120207A20207A2020'                                
         ED    WORK(10),SSBTPOPT-SSBD(R1)                                       
         MVC   SCANPOP,WORK+2                                                   
         MVC   SCANTIM,=C'  '                                                   
         TM    SSBSTAT3-SSBD(R1),SSBSRTIM                                       
         BZ    *+10                                                             
         MVC   SCANTIM,=C',T'                                                   
         MVC   SRVMSG(60),SCANMSG                                               
         B     MSGOUTX                                                          
         EJECT                                                                  
                                                                                
***********************************************************************         
* DISPLAY PRINTER QUEUE/STATUS                                                  
***********************************************************************         
DSP      NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R6,APRNQ                                                         
         USING PRQD,R6                                                          
*                                                                               
         THMS                      GET TIMENOW PL4                              
         STCM  R1,15,TIMENOW                                                    
         L     R5,ACIREC                                                        
         USING PQRECD,R5                                                        
         TM    INTFLAG,X'90'                                                    
         BO    DSPQX                                                            
         SR    RF,RF                                                            
         ICM   RF,1,PRQNE-PRQD(R6) RF=NUM OF ENTS                               
         BZ    DSPS                                                             
         CLI   ACTN,X'1B'                                                       
         BE    DSPS                                                             
         CLI   PRQNE-PRQD(R6),255  SET MAX I CAN COPE WITH (WAS 150)            
         BL    *+8                                                              
         LA    RF,255                                                           
*                                                                               
         LA    R6,PNTRY-PRQD(R6)   R6=FIRST ENTRY                               
         USING PNTRY,R6                                                         
*                                                                               
         LA    R7,PQSCIADS+256                                                  
         USING QED,R7                                                           
*                                                                               
DSPQ2    XC    QEFIELD,QEFIELD     COPY QUEUE                                   
         MVC   QEFIELD(L'PNTRY-2),PNTRY                                         
         SR    RE,RE                                                            
         ICM   RE,3,PNNEXT                                                      
         BZ    DSPQ2A              END OF QUEUE WHEN PNNEXT EQU Z               
         BCTR  RE,0                                                             
         MH    RE,=Y(L'PNTRY)                                                   
         LA    R6,6(RE)                                                         
         A     R6,APRQENTS                                                      
         LA    R7,L'QEFIELD(R7)                                                 
         BCT   RF,DSPQ2                                                         
*NOP     DC    H'0'                QUEUE AND PRQNE DO NOT AGREE                 
*                                                                               
DSPQ2A   LA    R7,L'QEFIELD(R7)    SET NEXT QUEUE IN SAVE TO ZEROS              
         XC    QEFIELD,QEFIELD     TO MARK END OF QUEUE                         
         L     R6,APRNQ                                                         
         USING PRQD,R6                                                          
         LA    R7,PQSCIADS+256     POINT TO START OF SAVE AREA                  
*                                                                               
         LA    RE,PRTQLST+8        POINT TO FIRST PRTQ FILE                     
         ST    RE,APRTQLST                                                      
         MVC   PRTQID+4(1),1(RE)   SET PRTQ FILE ID FROM LIST                   
         MVI   PRTQID+5,C' '                                                    
*                                                                               
DSPQ3    GOTO1 ADATAMGR,PQDMCB,(X'00',BUFFER),PRTQID,,,CXREC                    
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   BUFFDATA,CXREC      SAVE V(PQTAB)/V(PQFILE)                      
         MVC   CIDATA,CXREC+12     SAVE FILE DATA FOR THIS PRTQ FILE            
         MVC   FIWRES,PRTQID       SET RESOURCE                                 
         BRAS  RE,FIRSET           SET INDEX ADDRESSES FOR RESOURCE             
         JNE   *+2                 INVALID PRTQ                                 
         MVC   FIWNDA,FIWP1A       A(START OF PART1 INDEXES)                    
         MVI   RDIND,0             RESET READ FOR UPDATE                        
         LA    R5,FIWNDX                                                        
         USING PQRECD,R5           R5=A(PRTQUE INDEX ENTRY)                     
         B     DSPQ4A                                                           
*                                                                               
DSPQ4    BRAS  RE,FIRNSN                                                        
         BNE   DSPQ9                                                            
DSPQ4A   L     R1,FIWNDA                                                        
         MVC   FIWNDX(L'PQINDEX),SI1NDX-SI1PARD(R1)                             
DSPQ4B   BRAS  RE,FIRNC            A(NODE) TO A(CI)                             
         MVC   CIADDR,FIWCIA                                                    
*                                                                               
DSPQ6    CLI   PQSEQ,1             TEST PART 2 CI                               
         BH    DSPQ8                                                            
         TM    PQATTB,PQATNP+PQATJOBI                                           
         BNZ   DSPQ8                                                            
         TM    QESRCID,X'80'       TEST GENERIC ID                              
         BO    DSPQ6B                                                           
         CLC   PQSRCID,QESRCID     TEST USER ID MATCH IN QUEUE                  
         BE    DSPQ6C                                                           
         B     DSPQ7                                                            
*                                                                               
DSPQ6B   CLI   AGENIDS,X'FF'       TEST VGENIDS FOUND                           
         BE    DSPQ7               NO                                           
         GOTO1 AGENIDS,DMCB,QESRCID,ADATAMGR                                    
         BNE   DSPQ7                                                            
         LM    RE,RF,0(R1)         RE=N'ENTRIES, RF=A(ENTRIES)                  
         CLC   PQSRCID,0(RF)       MATCH SOURCE ID                              
         BE    DSPQ6C                                                           
         LA    RF,2(RF)            BUMP TO NEXT                                 
         BCT   RE,*-14                                                          
         B     DSPQ7                                                            
*                                                                               
DSPQ6C   CLI   QECLASS,0           TEST CLASS MATCH IN QUEUE                    
         BE    DSPQ6D                                                           
         TM    QECLASS,X'40'                                                    
         BZ    *+18                                                             
         CLC   PQCLASS,QECLASS                                                  
         BNE   DSPQ7                                                            
         B     DSPQ6D                                                           
         MVC   FLAG,QECLASS                                                     
         OI    FLAG,X'40'                                                       
         CLC   PQCLASS,FLAG                                                     
         BE    DSPQ7                                                            
*                                                                               
DSPQ6D   TM    QECOPIES,PNCENDP    TEST END PAGE IN QESUBID                     
         BO    DSPQ6E              IF SO IGNORE ALL THIS CRAP                   
*                                                                               
         CLC   PQSUBID,=C'LU1'     IF THIS IS AN LU1 REPORT                     
         BNE   *+12                                                             
         TM    PQSTAT,PQSTKE       AND ITS KEEP                                 
         BO    DSPQ6DT             IGNORE ALL GENERIC TESTS                     
*                                                                               
         CLC   QESUBID,SR@ALL      TEST SUBID MATCH IN QUEUE                    
         BE    DSPQ6E                                                           
         CLI   QESUBID+1,C'*'      ALLOW X* GENERIC                             
         BNE   *+18                                                             
         CLC   QESUBID(1),PQSUBID                                               
         BE    DSPQ6E                                                           
         B     DSPQ7                                                            
         CLI   PQSSUBID+2,C'*'     ALLOW XX* GENERIC                            
         BNE   *+18                                                             
         CLC   QESUBID(2),PQSUBID                                               
         BE    DSPQ6E                                                           
         B     DSPQ7                                                            
*                                                                               
DSPQ6DT  CLC   PQSUBID,QESUBID     ELSE MUST BE EXACT MATCH                     
         BNE   DSPQ7                                                            
*                                                                               
DSPQ6E   TM    QECOPIES,PNCTIME    TEST SEQ NUM CONTAINS TIME                   
         BZ    DSPQ6E1                                                          
         LA    RF,DATEC            SET RF TO NEW/OLD CMPRSD DATE                
         TM    PQTYP1,PQTYNCD                                                   
         BO    *+8                                                              
         LA    RF,DATECO                                                        
         CLC   PQAGELD,0(RF)       IF DATE < TODAY THIS IS A HIT                
         BL    DSPQ6E2                                                          
         BH    DSPQ7                                                            
         CLC   PQAGELT,QESEQN      IF CTIME < PN TIME ITS A HIT                 
         BNH   DSPQ6E2                                                          
         B     DSPQ7                                                            
*                                                                               
DSPQ6E1  OC    QESEQN,QESEQN       ZERO SEQN MEANS ALL                          
         BZ    DSPQ6E2                                                          
         CLC   PQREPNO,QESEQN      ELSE MUST BE EXACT MATCH                     
         BNE   DSPQ7                                                            
*                                                                               
DSPQ6E2  TM    PQSTAT,PQSTPG                                                    
         BZ    *+14                                                             
         CLC   CIADDR,PRCIADDR                                                  
         BNE   DSPQ7                                                            
         TM    PRQATT2,PRQAASS     TEST ALLOW SPECIAL STATUS                    
         BZ    *+12                                                             
         TM    PQSTAT,PQSTHO       TEST FOR SPECIAL STATUS                      
         BO    *+12                                                             
         TM    PQSTAT,PQSTAC       ELSE ACTIVE ONLY                             
         BZ    DSPQ7                                                            
         TM    PQSTAT,PQSTIN       IGNORE INVISIBLE REPORTS                     
         BO    DSPQ7                                                            
*                                                                               
DSPQ6E3  TM    PQATTB,PQATPW       TEST IF SECURE REPORT                        
         BZ    DSPQ6E5                                                          
         TM    QECOPIES,PNCTIME    TEST SEQ NUM CONTAINS TIME                   
         BO    DSPQ6E4                                                          
         OC    QESEQN,QESEQN       ZERO SEQN MEANS ALL                          
         BZ    DSPQ6E4                                                          
         CLC   PQREPNO,QESEQN      MUST BE EXACT MATCH FOR SECURITY             
         BE    DSPQ6E5                                                          
DSPQ6E4  LH    RE,QERSEC           BUMP NUMBER OF SECURE REPORTS                
         LA    RE,1(RE)                                                         
         STH   RE,QERSEC                                                        
         B     DSPQ7                                                            
*                                                                               
DSPQ6E5  LH    RE,QERPTS           BUMP NUMBER OF ACTIVE REPORTS                
         LA    RE,1(RE)                                                         
         STH   RE,QERPTS                                                        
*                                                                               
         L     R0,ACIREC                                                        
         GOTO1 ADATAMGR,PQDMCB,(X'00',DMREAD),PRTQID,CIADDR,(R0)                
         CLI   8(R1),0                                                          
         BNE   DSPERR                                                           
         LR    R1,R0                                                            
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,PQPAGES-PQINDEX(R1)                                         
         CLC   CIADDR,PRCIADDR     IS REPORT PRINTING                           
         BNE   DSPQ6F              NO                                           
         SR    RF,RF                                                            
         ICM   RF,3,PRPAGES                                                     
         SR    RE,RF               ADJUST BY PAGES PRINTED SO FAR               
DSPQ6F   LTR   RE,RE                                                            
         BP    *+6                                                              
         SR    RE,RE                                                            
         A     RE,QEPAGES                                                       
         ST    RE,QEPAGES          BUMP NUMBER OF ACTIVE PAGES                  
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,7,PQLINES-PQINDEX(R1)                                         
         CLC   CIADDR,PRCIADDR     IS REPORT PRINTING                           
         BNE   DSPQ6H              NO                                           
*                                                                               
DSPQ6G   S     RF,PRLINES          ADJUST BY LINES PRINTED SO FAR               
DSPQ6H   LTR   RF,RF                                                            
         BP    *+6                                                              
         SR    RF,RF                                                            
         LR    RE,RF                                                            
         A     RE,QELINES                                                       
         ST    RE,QELINES          BUMP NUMBER OF ACTIVE LINES                  
*                                                                               
         LH    RE,PQAVCPL-PQINDEX(R1)                                           
         MR    RE,RE                                                            
         A     RF,QECHRS                                                        
         ST    RF,QECHRS           BUMP NUMBER OF ACTIVE CHRS                   
         B     DSPQ8                                                            
*                                                                               
DSPQ7    LA    R7,L'QEFIELD(R7)    BUMP TO NEXT QUEUE ENTRY                     
         OC    QESRCID,QESRCID                                                  
         BNZ   DSPQ6               TEST LAST ENTRY                              
*                                                                               
DSPQ8    LA    R7,PQSCIADS+256     GO TO START OF QUEUE                         
         B     DSPQ4               BUMP TO NEXT INDEX ENTRY                     
*                                                                               
DSPQ9    ICM   RE,15,APRTQLST      BUMP TO NEXT PRTQ FILE IN LIST               
         BZ    DSPQA                                                            
         LA    RE,8(RE)                                                         
         ST    RE,APRTQLST                                                      
         CLI   0(RE),0             TEST END OF PRTQ FILE LIST                   
         BE    DSPQA                                                            
         MVC   PRTQID+4(1),1(RE)   SET PRTQ FILE ID FROM LIST                   
         MVI   PRTQID+5,C' '                                                    
         B     DSPQ3               BACK TO SEARCH NEXT PRTQ FILE                
*                                                                               
DSPQA    LA    R4,SRVSA1H          DISPLAY SAVE QUEUE                           
         USING NLINED,R4                                                        
*                                                                               
         L     R6,APRNQ                                                         
         CLC   PQSQPOS,PRQNE-PRQD(R6)                                           
         BL    *+8                                                              
         MVI   PQSQPOS,0                                                        
*                                                                               
         SR    R1,R1               BYTE = ENTRY NUMBER                          
         IC    R1,PQSQPOS                                                       
         STH   R1,HALF                                                          
         LA    R1,1(R1)                                                         
         STC   R1,BYTE                                                          
*                                                                               
         LA    R7,L'QEFIELD        R7 = POS IN QUEUE                            
         MH    R7,HALF                                                          
         LA    R7,PQSCIADS+256(R7)                                              
*                                                                               
DSPQB    EDIT  (B1,BYTE),(2,NLNUM),FILL=0                                       
         CLI   BYTE,99                                                          
         BNH   DSPQC                                                            
         EDIT  (B1,BYTE),(3,NLNUM),FILL=0                                       
*                                                                               
DSPQC    CLI   QECLASS,0           QUEUE CLASS (POSITIVE OR NEGATIVE)           
         BE    DSPQD1                                                           
         MVC   NLCLASS,QECLASS                                                  
         TM    NLCLASS,X'40'                                                    
         BO    DSPQD1                                                           
         OI    NLCLASS,X'40'                                                    
         MVI   NLCLASS-1,C'-'                                                   
*                                                                               
DSPQD1   MVI   NLTYPE,C'.'         QUEUE ENTRY TYPE                             
         MVI   NLINEH+5,6          SET TO YELLOW STANDARD                       
         TM    QEPREX,X'40'                                                     
         BZ    *+12                                                             
         MVI   NLTYPE,C'P'         SET PERM                                     
         MVI   NLINEH+5,4          SET TO GREEN PERM                            
*                                                                               
         MVC   GIUSER,QESRCID      GET USERID                                   
         GOTO1 AGETUSER                                                         
         MVC   NLREPORT(8),GIUSERID                                             
         SR    RF,RF                                                            
         IC    RF,GIULEN                                                        
         LA    RF,NLREPORT(RF)                                                  
*                                                                               
         MVI   0(RF),C','                                                       
         ST    RF,FULL                                                          
         TM    QECOPIES,X'40'      TEST FOR PAGES=                              
         BNO   DSPQD2                                                           
         MVC   WORK(2),QESRCID                                                  
         MVC   WORK+2(2),QESEQN                                                 
         MVI   LOCKFLAG,C'N'                                                    
         LA    R1,WORK                                                          
         BRAS  RE,GETREPT          GET REPORT FROM SEQN                         
         L     RF,FULL                                                          
         LA    RF,1(RF)                                                         
         MVC   0(3,RF),2(R1)                                                    
         LA    RF,3(RF)                                                         
         B     DSPQD3                                                           
DSPQD2   L     RF,FULL                                                          
         LA    RF,1(RF)                                                         
         MVC   0(3,RF),QESUBID                                                  
         LA    RF,3(RF)                                                         
*                                                                               
DSPQD3   OC    QESEQN,QESEQN     TEST ANY SEQ AT ALL                            
         BZ    DSPQDX                                                           
         MVI   0(RF),C','                                                       
         LA    RF,1(RF)                                                         
*                                                                               
         TM    QECOPIES,X'80'    TST SEQN EQU TIME VALUE                        
         BO    DSPQD5                                                           
         EDIT  (B2,QESEQN),(5,0(RF)),ALIGN=LEFT                                 
         B     DSPQDX                                                           
*                                                                               
DSPQD5   MVC   WORK,SPACES       OUTPUT TIME VALUE                              
         ST    RF,FULL                                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,3,QESEQN                                                      
         SLL   RF,2                                                             
         D     RE,=F'180'                                                       
         STCM  RF,3,DUB                                                         
         GOTO1 ATIMBER,PARMS,(X'40',10),(X'02',DUB),(4,WORK)                    
         L     RF,FULL                                                          
         MVI   0(RF),C'<'                                                       
         MVC   1(10,RF),WORK                                                    
DSPQDX   EQU   *                                                                
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,QERPTS         NUMBER OF REPORTS                            
         BNZ   *+8                                                              
         MVI   NLINEH+5,2          SET TO RED IF EMPTY                          
         BAS   RE,CVD                                                           
         MVC   NLRPTS,DUB+4                                                     
*                                                                               
         L     R0,QEPAGES          NUMBER OF PAGES                              
         BAS   RE,CVD                                                           
         MVC   NLPAGES,DUB+3                                                    
*                                                                               
         L     R0,QELINES          NUMBER OF LINES                              
         BAS   RE,CVD                                                           
         MVC   NLLINES,DUB+2                                                    
*                                                                               
DSPQE    L     R6,APRNQ                                                         
         USING PRQD,R6                                                          
         SR    RF,RF                                                            
         OC    PRQBUFFL,PRQBUFFL   BYPASS IF NO PRINTER DATA                    
         BZ    DSPQE7                                                           
         OC    QELINES,QELINES     OR IF LINES EQU ZERO                         
         BZ    DSPQE7                                                           
         L     R5,APRN                                                          
         USING UTLD,R5                                                          
         LH    R0,TPRLSP           VTAM GETS SPEED FROM TERMINAL                
         LTR   R0,R0               DEFAULT LINE SPEED = 4800 BAUD               
         BNZ   DSPQE0                                                           
         LH    R0,=H'600'                                                       
*                                                                               
DSPQE0   SR    RE,RE               TIME TO TRANSMIT TOTAL NUM OF CHRS           
         L     RF,QECHRS           =(QECHRS+QELINES)/LINECPS                    
         A     RF,QELINES                                                       
         DR    RE,R0                                                            
         ST    RF,FULL                                                          
         SR    RE,RE               TIME TO POLL TOTAL NUM OF BUFFERS            
         L     RF,QECHRS           =(QECHRS+QELINES)/(TPRBUFFL-40)              
         A     RF,QELINES          *(POLLTIME/10)                               
         LH    R0,PRQBUFFL                                                      
         SH    R0,=H'40'           ADJUST FOR BUFFER SLACK                      
         DR    RE,R0                                                            
         LA    RF,1(RF)            RF=NUM OF BUFFS TO TRANSMIT                  
         LH    RE,QERPTS                                                        
         SRA   RE,1                                                             
         AR    RF,RE               ADJUST FOR MULTIPLE REPORTS                  
         MH    RF,POLLTIME                                                      
         SR    RE,RE                                                            
         D     RE,=F'10'                                                        
         A     RF,FULL                                                          
         ST    RF,FULL                                                          
*                                                                               
DSPQE1   TM    PRQSPDTY,X'20'      TEST IF NEW FORMAT DATA                      
         BO    DSPQE1A                                                          
         MVC   HALF1(2),PRQSPD-1   HALF1=PRINTER SPEED (OLD=2 BYTES)            
         MVI   BYTE1,0             BYTE1=PRINTER SPEED TYPE                     
         CLI   PRQSPDTY,C'L'                                                    
         BNE   *+8                                                              
         MVI   BYTE1,1                                                          
         CLI   PRQSPDTY,C'B'                                                    
         BNE   *+8                                                              
         MVI   BYTE1,2                                                          
         CLI   PRQSPDTY,C'C'                                                    
         BNE   *+8                                                              
         MVI   BYTE1,3                                                          
         B     DSPQE2                                                           
DSPQE1A  SR    RE,RE               NEW STYLE - TYPE IN TOP TWO BITS             
         IC    RE,PRQSPDTY                                                      
         SRL   RE,6                                                             
         STC   RE,BYTE1            0=P,1=L,2=B,3=C                              
         IC    RE,PRQSPD                                                        
DSPQE1A0 CLI   BYTE1,0             PAGES/MIN                                    
         BNE   DSPQE1A1                                                         
         STH   RE,HALF1            REMAINS AS PAGES/MIN                         
         B     DSPQE2                                                           
DSPQE1A1 CLI   BYTE1,1             LINES/6SECS                                  
         BNE   DSPQE1A2                                                         
         MHI   RE,10               CONVERT TO LINES/MIN                         
         STH   RE,HALF1                                                         
         B     DSPQE2                                                           
DSPQE1A2 CLI   BYTE1,3             CHARS/HALFSEC                                
         BH    DSPQEX                                                           
         SLL   RE,1                CONVERT TO CHARS/SEC                         
         STH   RE,HALF1                                                         
*                                                                               
DSPQE2   CLI   BYTE1,3             TEST VALID SPEED TYPE                        
         BH    DSPQEX                                                           
         OC    HALF1,HALF1         TEST VALID SPEED                             
         BZ    DSPQEX                                                           
         CLI   BYTE1,1                                                          
         BE    DSPQE4                                                           
         BH    DSPQE5                                                           
*                                                                               
DSPQE3   SR    RE,RE               PAGE PRINTER PRINTING TIME                   
         L     RF,QEPAGES          =(QEPAGES*60)/TPRSPD                         
         MH    RF,=H'60'                                                        
         LH    R0,HALF1                                                         
         DR    RE,R0                                                            
         A     RF,FULL                                                          
         ST    RF,FULL                                                          
         B     DSPQE7                                                           
*                                                                               
DSPQE4   SR    RE,RE               LINE PRINTER PRINTING TIME                   
         L     RF,QELINES          =(QELINES*60)/TPRSPD                         
         MH    RF,=H'60'                                                        
         LH    R0,HALF1                                                         
         DR    RE,R0                                                            
         A     RF,FULL                                                          
         ST    RF,FULL                                                          
         SR    RE,RE               SLEWRATE=(TPRSPD/LPSFACT) LINES/SEC          
         LH    RF,HALF1                                                         
         LH    R0,LPSFACT                                                       
         DR    RE,R0                                                            
         ST    RF,DUB                                                           
         B     DSPQE6                                                           
*                                                                               
DSPQE5   LH    RF,BFACT            CHR PRINTER PRINTING TIME                    
         CLI   BYTE1,3             =(QECHRS/TPRSPD)*(XFACT/10)                  
         BNE   *+8                                                              
         LH    RF,CFACT                                                         
         M     RE,QECHRS                                                        
         LH    R0,HALF1                                                         
         MH    R0,=H'10'                                                        
         DR    RE,R0                                                            
         A     RF,FULL                                                          
         ST    RF,FULL                                                          
         SR    RE,RE               SLEWRATE=(TPRSPD/CBSFACT) LINES/SEC          
         LH    RF,HALF1                                                         
         LH    R0,CBSFACT                                                       
         DR    RE,R0                                                            
         ST    RF,DUB                                                           
*                                                                               
DSPQE6   L     R0,DUB              TIME TO SPACE PRINTER                        
         LTR   R0,R0               =(QEPAGES*66-QELINES)/SLEWRATE               
         BZ    DSPQE7                                                           
         SR    RE,RE                                                            
         L     RF,QEPAGES                                                       
         MH    RF,=H'66'                                                        
         S     RF,QELINES                                                       
         BNP   DSPQE7                                                           
         DR    RE,R0                                                            
         A     RF,FULL                                                          
         ST    RF,FULL                                                          
*                                                                               
DSPQE7   XC    HALF,HALF           MULTIPLY BY COPYS IF NON ZERO                
         MVC   HALF+1(1),QECOPIES                                               
         NI    HALF+1,X'0F'                                                     
         BZ    DSPQE8                                                           
         MH    RF,HALF                                                          
*                                                                               
DSPQE8   LR    RE,RF               CONVERT TOTAL TIME                           
         SRDL  RE,32                                                            
         D     RE,=F'60'           RE=SECS,RF=MINS                              
         EDIT  (RF),(3,NLTIME),ZERO=NOBLANK                                     
         EDIT  (RE),(2,NLTIME+4),FILL=0                                         
         MVI   NLTIME+3,C'.'                                                    
DSPQEX   DS    0H                                                               
*                                                                               
DSPQG    MVI   SBYTE,0             MISCELLANEOUS OPTIONS                        
         MVI   WORK,C' '                                                        
         MVC   WORK+1(59),WORK                                                  
         LA    RF,WORK                                                          
*                                                                               
DSPQG1   OC    QERSEC,QERSEC       TEST IF SECURE REPORTS FOUND                 
         BZ    DSPQG2                                                           
         MVI   SBYTE,1                                                          
         MVC   0(3,RF),SR@SEC                                                   
         EDIT  (B2,QERSEC),(3,10(RF)),ALIGN=LEFT,WRK=SWORK                      
         LA    RF,20(RF)                                                        
*                                                                               
DSPQG2   OC    HALF,HALF           HALF=COPIES                                  
         BZ    DSPQG3                                                           
         IC    R1,SBYTE                                                         
         LA    R1,1(R1)                                                         
         STC   R1,SBYTE                                                         
         MVC   0(8,RF),SR@COPYS                                                 
         EDIT  (B2,HALF),(3,10(RF)),ALIGN=LEFT,WRK=SWORK                        
         LA    RF,20(RF)                                                        
*                                                                               
DSPQG3   TM    QECOPIES,PNCENDP                                                 
         BNO   DSPQG4                                                           
         IC    R1,SBYTE                                                         
         LA    R1,1(R1)                                                         
         STC   R1,SBYTE                                                         
         MVC   0(8,RF),SR@PAGES                                                 
         EDIT  (B2,QESUBID),(4,10(RF)),ALIGN=LEFT,WRK=SWORK                     
         AR    RF,R0                                                            
         MVI   10(RF),C','                                                      
         SR    RE,RE                                                            
         SR    R1,R1                                                            
         ICM   RE,1,QESUBID+2                                                   
         ICM   R1,3,QESUBID                                                     
         AR    RE,R1                                                            
         EDIT  (RE),(4,11(RF)),ALIGN=LEFT,WRK=SWORK                             
*                                                                               
DSPQG4   GOTO1 AUNSCAN,DMCB,(SBYTE,WORK),(C'C',MSG),C',=  '                     
         MVC   NLOPTS,MSG                                                       
*                                                                               
DSPQN    LA    R4,93(R4)           BUMP TO NEXT DISPLAY LINE                    
         LA    R1,SRVPFKH                                                       
         CR    R4,R1               TEST FOR END OF SCREEN                       
         BNL   DSPS                                                             
         LA    R7,L'QEFIELD(R7)                                                 
         OC    0(2,R7),0(R7)       TEST FOR END OF QUEUE                        
         BZ    DSPS                                                             
         IC    R1,BYTE             BUMP TO NEXT QUEUE ENTRY                     
         LA    R1,1(R1)                                                         
         STC   R1,BYTE                                                          
         B     DSPQB                                                            
*                                                                               
DSPS     LA    R7,PQSCIADS+256                                                  
         L     R5,ACIREC                                                        
         USING PQRECD,R5                                                        
         LA    R4,MSG              STATUS LINE 1 - STATUS & REPORT-ID           
         XC    MSG,MSG                                                          
         MVI   FLAG1,1             PRINTER MESSAGES START AT 1                  
         TM    PRNTYP,RMC                                                       
         BNO   *+8                                                              
         MVI   FLAG1,4             SHUTTLE MESSAGES START AT 4                  
         TM    PRQMODE,X'80'                                                    
         BZ    DSPS1                                                            
         MVI   1(R4),C' '                                                       
         MVC   2(4,R4),SR@AUTO                                                  
         NC    2(4,R4),LOWER                                                    
         MVI   0(R4),6                                                          
         LA    R4,6(R4)                                                         
         B     DSPS2                                                            
DSPS1    MVI   0(R4),1                                                          
         LA    R4,1(R4)                                                         
DSPS2    MVC   CIADDR,PRCIADDR     SET DISK ADDR OF CURRENT REPORT              
         OC    CIADDR,CIADDR                                                    
         BNZ   DSPS2A                                                           
         MVC   1(10,R4),SR@INACT                                                
         NC    1(10,R4),LOWER                                                   
         MVI   0(R4),11                                                         
         B     DSPS2B              IF NO CIADDR MESAGE IS INACTIVE              
DSPS2A   L     R1,APRN                                                          
         OC    TCID-UTLD(4,R1),TCID-UTLD(R1)                                    
         BNZ   DSPS3                                                            
         MVC   1(10,R4),SR@STRTG                                                
         NC    1(10,R4),LOWER                                                   
         MVI   0(R4),11            IF CIADDR BUT NO CID MSG IS STARTING         
DSPS2B   EQU   *                                                                
         MVC   WORK(10),=X'402120207A20207A2020'                                
         ED    WORK(10),PRSVHMS                                                 
         MVC   12(8,R4),WORK+2                                                  
         MVI   11(R4),9                                                         
         B     DSPSA                                                            
*                                                                               
DSPS3    MVC   PRTQID+4(1),PRPRTQA GET PRTQ FILE ID                             
         MVI   PRTQID+5,C' '                                                    
         GOTO1 ADATAMGR,PQDMCB,(X'00',BUFFER),PRTQID,,,(R5)                     
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
         MVC   BUFFDATA,0(R5)      SAVE V(PQTAB)/V(PQFILE)                      
         MVC   CIDATA,12(R5)       SAVE FILE DATA FOR THIS PRTQ FILE            
         MVC   FIWRES,PRTQID       SET RESOURCE                                 
         BRAS  RE,FIRSET           SET INDEX ADDRESSES FOR RESOURCE             
         JNE   *+2                 INVALID PRTQ                                 
         GOTO1 ADATAMGR,PQDMCB,(X'00',DMREAD),,CIADDR,(R5)                      
         CLI   8(R1),0                                                          
         BNE   DSPERR                                                           
         SR    R1,R1                                                            
         IC    R1,FLAG1                                                         
         TM    PRSTAT,PRSACTV                                                   
         BNO   DSPS4                                                            
         LA    R1,1(R1)            SET ACTIVE MESSAGE                           
         B     DSPS5                                                            
DSPS4    LA    R1,2(R1)            SET STOPPED MESSAGE                          
DSPS5    STC   R1,FLAG1                                                         
         TM    PRSTAT,PRSERR                                                    
         BNO   DSPS6                                                            
         MVI   1(R4),C' '                                                       
         MVC   2(12,R4),SR@DTERR                                                
         NC    2(12,R4),LOWER                                                   
         MVI   0(R4),14                                                         
         LA    R4,14(R4)                                                        
         B     DSPS7                                                            
DSPS6    MVI   0(R4),1                                                          
         LA    R4,1(R4)                                                         
DSPS7    MVC   GIUSER,PQSRCID                                                   
         GOTO1 AGETUSER                                                         
         MVC   1(8,R4),GIUSERID                                                 
         SR    RF,RF                                                            
         IC    RF,GIULEN                                                        
         LA    RF,1(RF,R4)                                                      
         MVI   0(RF),C','                                                       
         MVC   1(3,RF),PQSUBID                                                  
         MVI   4(RF),C','                                                       
         EDIT  (B2,PQREPNO),(5,5(RF)),ALIGN=LEFT                                
         LR    R1,R0                                                            
         LA    RF,6(R1,RF)                                                      
         SR    RF,R4                                                            
         STC   RF,0(R4)                                                         
         LA    R4,0(RF,R4)                                                      
         SR    R0,R0                                                            
         ICM   R0,3,PRPAGES        GET CURRENT PAGE                             
         LTR   R0,R0                                                            
         BNZ   *+8                                                              
         LA    R0,1                SET TO ONE IF ZERO                           
         TM    PNCOPYS,PNCENDP                                                  
         BNO   DSPS7A                                                           
         SR    R1,R1                                                            
         SR    RE,RE                                                            
         ICM   RE,1,PNSUBID+2                                                   
         ICM   R1,3,PNSUBID                                                     
         AR    RE,R1                                                            
         CR    R0,RE                                                            
         BNH   DSPS7A                                                           
         LR    R0,RE                                                            
DSPS7A   EDIT  (R0),(4,1(R4)),ALIGN=LEFT                                        
         MVI   0(R4),5                                                          
         LA    R4,5(R4)                                                         
         SR    R0,R0                                                            
         ICM   R0,3,PQPAGES        GET HIGHEST PAGE                             
         TM    PNCOPYS,PNCENDP                                                  
         BNO   DSPS8                                                            
         SR    R1,R1                                                            
         ICM   R0,1,PNSUBID+2                                                   
         ICM   R1,3,PNSUBID                                                     
         AR    R0,R1                                                            
DSPS8    EDIT  (R0),(4,1(R4)),ALIGN=LEFT                                        
         MVI   0(R4),5                                                          
         LA    R4,5(R4)                                                         
*                                                                               
DSPSA    SR    RE,RE                                                            
         IC    RE,FLAG1                                                         
         GOTO1 AGETTXT,DMCB,(RE),SRVINFH,(C'T',0),0,MSG,X'00010000'             
*                                                                               
DSPQX    J     XIT                                                              
*                                                                               
DSPERR   DC    H'0'                                                             
                                                                                
***********************************************************************         
* GET PRINT QUEUE ID FOR SINGLE REPORT                                          
*        R1=A(UUUUNNNN)                                                         
***********************************************************************         
GETREPT  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   SFULL,0(R1)                                                      
         MVC   NDX(2),SFULL        GET PRTQ ID FOR SINGLE REPORT                
         GOTO1 ADATAMGR,DMCB,(0,GFILE),PRTQUE,NDX,,CXREC                        
         MVC   PRTQID,NDX+32                                                    
         MVI   PRTQID+5,C' '                                                    
         GOTO1 ADATAMGR,DMCB,(0,BUFFER),PRTQID,,,CXREC                          
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   CIDATA,CXREC+12     SET CI DATA FOR THIS PRTQ FILE               
         MVC   CIRSN,SFULL+2       SET SEQ NO                                   
*                                                                               
         MVC   FIWRES,PRTQID       SET RESOURCE                                 
         BRAS  RE,FIRSET           SET ADDRESSES FOR THIS RESOURCE              
         JNE   *+2                 INVALID PRTQ                                 
*                                                                               
         XC    FIWREF,FIWREF                                                    
         MVC   FIWREF+2(2),CIRSN                                                
         BRAS  RE,FIRRN                                                         
         BRAS  RE,FIRRC                                                         
         L     R1,FIWNDA                                                        
         MVC   FIWNDX,SI1NDX-SI1PAR(R1)                                         
         MVC   CIADDR,FIWCIA                                                    
         LA    R1,FIWNDX                                                        
*                                                                               
         CLC   0(2,R1),SFULL       TEST IF STILL SAME USERID                    
         XIT1  REGS=(R1)                                                        
         EJECT                                                                  
                                                                                
***********************************************************************         
* EXITS, SHARED ROUTINES, CONSTANTS AND OTHER COMMONLY SHARED VALUES            
***********************************************************************         
         DROP  RB                                                               
COMMON   DS    0D                                                               
                                                                                
***********************************************************************         
* ERRORS                                                                        
***********************************************************************         
XIT      XIT1                                                                   
*                                                                               
ERR1     LA    RE,SREIRI                                                        
         B     ERRV                                                             
ERR2     LA    RE,150              RESTART FALIURE                              
         B     ERRW                                                             
ERR3     LA    RE,151              QUEUE TOO LONG                               
         B     ERRW                                                             
ERR4     LA    RE,SREMRI           MISSING REPT ID                              
         B     ERRV                                                             
ERR5     LA    RE,152              MISSING PRINTER ID                           
         B     ERRV                                                             
ERR6     LA    RE,153              INVALID PRINTER ID                           
         B     ERRV                                                             
ERR7     LA    RE,154              PRINTER QUEUE FULL ** CONTACT DDS **         
         B     ERRV                                                             
ERR8     LA    RE,155              RESTART PAGE NO REQUIRED                     
         B     ERRW                                                             
ERR9     LA    RE,156              INVALID START PAGE                           
         B     ERRW                                                             
ERR10    LA    RE,157              INVALID QUEUE START NUM                      
         B     ERRW                                                             
ERR11    LA    RE,158              NOTHING TO PRINT                             
         B     ERRW                                                             
ERR12    LA    RE,159              QUEUE IS EMPTY                               
         B     ERRW                                                             
ERR13    LA    RE,160              PRINTER NOT ACTIVE                           
         B     ERRW                                                             
ERR14    LA    RE,SREIIFF          INVALID INPUT FIELD FORMT                    
         B     ERRW                                                             
ERR15    LA    RE,161              PRINTER HAS NOT BEEN STOPPED                 
         B     ERRW                                                             
ERR16    LA    RE,162              QUEUE HAS CHANGED                            
         B     ERRX                                                             
ERR17    LA    RE,163              INVALID QUEUE ACTION                         
         B     ERRX                                                             
ERR18    LA    RE,164              PRINTER LINE NOT UP                          
         B     ERRV                                                             
ERR19    LA    RE,165              PRINTER MODE ALREADY MANUAL                  
         B     ERRW                                                             
ERR20    LA    RE,166              PRINTER MODE ALREADY AUTO                    
         B     ERRW                                                             
ERR21    LA    RE,167              QUEUE CANT BE EMPTY FOR AUTO                 
         B     ERRW                                                             
ERR22    LA    RE,SRESACT          INVALID SUB ACTION                           
         B     ERRW                                                             
ERR23    LA    RE,272              PAGES= RANGE MUST BE < 255                   
         B     ERRW                                                             
ERR24    LA    RE,193              NOT AVAILABLE FOR CLASS N                    
         B     ERRW                                                             
ERRG1    LA    RE,188              GRAFNET MUST BE CLASS G (1)                  
         B     ERRW                                                             
ERRG2    LA    RE,189              GRAFNET MUST BE CLASS G (2)                  
         B     ERRW                                                             
ERRG3    LA    RE,190              MAY NOT BE CLASS G (1)                       
         B     ERRW                                                             
ERRG4    LA    RE,191              MAY NOT BE CLASS G (2)                       
         B     ERRW                                                             
ERRSEC   LLC   RE,GSECRES          SECURITY ERROR                               
         B     ERRW                                                             
ERRF     LA    RE,SREIOK1          INVALID OPTION KEYWORD=                      
         LA    RF,12(R6)                                                        
         ST    RF,TXTADR                                                        
         MVI   TXTLEN,X'0A'                                                     
         B     ERRW                                                             
ERRFA    LA    RE,SREIOSK1         INVALID OPTION SIGN KEYWORD=                 
         LA    RF,12(R6)                                                        
         ST    RF,TXTADR                                                        
         MVI   TXTLEN,X'0A'                                                     
         B     ERRW                                                             
ERRFB    LA    RE,SREIVOK1         INVALID VALUE FOR OPTION KEYWORD=            
         LA    RF,12(R6)                                                        
         ST    RF,TXTADR                                                        
         MVI   TXTLEN,X'0A'                                                     
         B     ERRW                                                             
*                                                                               
ERRV     B     ERRX                                                             
*                                                                               
ERRW     ST    RE,FULL1                                                         
         MVI   DSPFLAG,4            DISPLAY BASIC PRINTER STATUS                
         BAS   RE,DSP                                                           
         L     RE,FULL1                                                         
         B     ERRX                                                             
                                                                                
*----------------------------------------------------------------------         
* OUTPUT ERROR MESSAGES   RE=MSG NUMBER                                         
*----------------------------------------------------------------------         
ERRX     ST    R4,CURSOR           CURSOR POS                                   
         TM    INTFLAG,X'80'                                                    
         BNO   *+8                                                              
         OI    INTFLAG,X'40'       FLAG ERROR FOR ROOT                          
         GOTO1 AGETTXT,DMCB,(RE),0,(C'E',0),(TXTLEN,TXTADR),0,X'000100'         
         L     R1,CURSOR                                                        
         OI    6(R1),X'40'                                                      
*                                                                               
EXIT     CLI   NEWTRM,0            TEST IF ADDED NEW PRINTER TERMINAL           
         BE    EXITX                                                            
         L     R5,APRN                                                          
         USING UTLD,R5                                                          
         OI    TSTAT5,TST5TCP      SET TERMINAL CHECKPOINT PENDING              
         L     RE,ASSB                                                          
         OI    SSBVTFL1-SSBD(RE),SSBVTTCP                                       
         MVI   NEWTRM,0            MAKE SURE WE DON'T LEAVE THIS ON             
*                                                                               
EXITX    L     R1,APRQENTS         AND LOOK FOR A HORRIBLE BUG                  
         SH    R1,=H'10'                                                        
         L     R1,0(R1)                                                         
         BCTR  R1,0                                                             
         ST    R1,FULL             GET A(TABLE END)                             
*                                                                               
         L     R1,APRQENTS                                                      
         SH    R1,=H'6'                                                         
         CLI   0(R1),X'FF'         TEST FOR MINUS VALUES                        
         BE    *+14                                                             
         OC    0(12,R1),0(R1)                                                   
         BNZ   *+20                                                             
*                                                                               
         MVC   0(8,R1),=X'000000F02260000C'                                     
         MVC   20(4,R1),FULL                                                    
         B     EXITERR             REPAIR THE TABLE AND EXIT                    
*                                                                               
         OC    10(2,R1),10(R1)                                                  
         BNZ   EXITXX                                                           
         MVC   8(4,R1),FULL        REPAIR THE TABLE AND EXIT                    
*                                                                               
EXITERR  WTO   '**WARNING** PRQ TABLE WAS DAMAGED '                             
*                                                                               
EXITXX   XMOD1                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* SHARED MEMORY ROUTINES                                                        
***********************************************************************         
       ++INCLUDE DDSHFIR                                                        
         EJECT                                                                  
                                                                                
***********************************************************************         
* EQUATES AND CONSTANTS                                                         
***********************************************************************         
BSC      EQU   X'40'               IBM 3270 BSC PRINTER                         
TWX      EQU   X'20'               ADDS TWX TERMINAL PRINTER                    
RMC      EQU   X'10'               SHUTTLE RMC TERMINAL PRINTER                 
RJE      EQU   X'08'               IBM 3780 RJE TERMINAL PRINTER                
*                                                                               
MODEMAN  EQU   X'01'                                                            
MODEAUTO EQU   X'02'                                                            
TYPETEMP EQU   X'01'                                                            
TYPEPERM EQU   X'02'                                                            
*                                                                               
MAXSEQ   DC    F'65000'                                                         
DMREAD   DC    CL8'DMREAD'                                                      
RANDOM   DC    CL8'RANDOM'                                                      
DMWRT    DC    CL8'DMWRT'                                                       
GFILE    DC    CL8'GFILE'                                                       
BUFFER   DC    CL8'BUFFER'                                                      
PRTQUE   DC    CL8'PRTQUE'                                                      
CTFILE   DC    CL8'CTFILE'                                                      
SPACES   DC    CL80' '                                                          
LOWER    DC    20X'BF'                                                          
PAGELAST DC    X'7FFE'                                                          
PAGEEROR DC    X'7FFF'                                                          
FFS      DC    X'FFFFFFFFFFFFFFFF'                                              
DISPMAX  DC    H'12'                                                            
PUBMIN   DC    AL2(32000)          PUBLIC USER IDS IN THIS RANGE                
PUBMAX   DC    AL2(32100)                                                       
*                                                                               
*&&UK                                                                           
POLLTIME DC    H'10'               TIME IN 1/10 SECS TO POLL PRINTER            
BFACT    DC    H'13'               CONVERTS BYDI CPS PRINTING SPEED             
CFACT    DC    H'21'               CONVERTS UNID CPS PRINTING SPEED             
CBSFACT  DC    H'02'               CONVERTS CPS TO LPS SLEW SPEED               
LPSFACT  DC    H'02'               CONVERTS LPM TO LPS SLEW SPEED               
*&&                                                                             
*&&US                                                                           
POLLTIME DC    H'10'                                                            
BFACT    DC    H'13'                                                            
CFACT    DC    H'21'                                                            
CBSFACT  DC    H'02'                                                            
LPSFACT  DC    H'02'                                                            
*&&                                                                             
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* TABLE OF ACTION VALUES THAT CAN HAVE OPTIONS IN P4                            
* XL1    ACTION NUMBER                                                          
* XL1    MAXIMUM NUMBER OF OPTIONS                                              
* XL1    OPTION FLAGS                                                           
* XL1    OPTION FLAGS                                                           
* AL4    ADDR OF TABLE                                                          
***********************************************************************         
         DS    0F                                                               
ACTNOPTN DS    0CL8                                                             
         DC    X'11',X'02',X'00',X'00',A(SCHOPTN)                               
         DC    X'13',X'01',X'00',X'00',A(STPOPTN)                               
         DC    X'14',X'03',X'00',X'00',A(SCHOPTN)                               
         DC    X'15',X'01',X'00',X'00',A(FLSOPTN)                               
         DC    X'00',X'00',X'00',X'00',A(0)                                     
         EJECT                                                                  
*                                                                               
RSLTMSG  DS    0CL1                                                             
         DC    AL1(157)  REORTS SCHEDULED FOR PRINTING  01                      
         DC    AL1(158)  STARTED                        02                      
         DC    AL1(159)  RESTARTED                      03                      
         DC    AL1(184)  STATUS AND QUEUE DISPLAYED     04                      
         DC    AL1(161)  STOPPED                        05                      
         DC    AL1(162)  SCHEDULED FOR STOP             06                      
         DC    AL1(163)  IS FLUSHING                    07                      
         DC    AL1(164)  NEW QUEUE DISPLAYED            08                      
         DC    AL1(165)  IS INACTIVE                    09                      
         DC    AL1(166)  STATUS DISPLAYED               10                      
         DC    AL1(167)  MODE CHANGED                   11                      
         DC    AL1(183)  DEFAULT PRINTER SET            12                      
         DC    AL1(186)  QUEUE HAS N ENTS. NN REPS PNDG 13                      
         EJECT                                                                  
*                                                                               
SCANMSG  DC    C'XXXXXXXX #Q=000,#A=000,TP=00:00:00,TN=00:00:00'                
         DC    C',TL=00:00:00   '                                               
SCANSYM  EQU   SCANMSG+00,8,C'C'                                                
SCANQUE  EQU   SCANMSG+12,3,C'C'                                                
SCANPND  EQU   SCANMSG+19,3,C'C'                                                
SCANPOP  EQU   SCANMSG+26,8,C'C'                                                
SCANNOW  EQU   SCANMSG+38,8,C'C'                                                
SCANLST  EQU   SCANMSG+50,8,C'C'                                                
SCANTIM  EQU   SCANMSG+58,2,C'C'                                                
DC@HDR2  DC    C'---- -- - - ------------------ '                               
         DC    C'---- -----  -----  ----- -----------------------'              
                                                                                
***********************************************************************         
* TABLE OF VALID KEYWORDS FOR FILTERS                                           
* CL8    KEYWORD NAME                                                           
* XL1    KEYWORD NUMBER                                                         
* XL1    KEYWORD SIGN BITS X'80'=NE,X'40'=LT,X'20'=GT                           
* XL1    KEYWORD VALD BITS X'01'=DDSONLY,X'02'=SINGLE,X'04'=KEYWORD             
*                          X'10'=INTEGER,X'20'=SPECIAL                          
* XL1    N/D                                                                    
* XL4    A(PROCESSING ROUTINE)                                                  
***********************************************************************         
         DS    0F                                                               
OPTOPTN  DS    0CL12                                                            
*                                                                               
STROPTN  DC    X'41F0',S(SR@LAST),X'01',X'00',X'02',X'00',A(STRV01)             
         DC    X'41F0',S(SR@ERROR),X'02',X'00',X'02',X'00',A(STRV02)            
         DC    XL4'00000000',X'00',X'00',X'22',X'00',A(STRV00)                  
*                                                                               
STPOPTN  DC    X'41F0',S(SR@PRR),X'01',X'00',X'02',X'00',A(STPV01)              
         DC    X'41F0',S(SR@RPT),X'02',X'00',X'02',X'00',A(STPV02)              
         DC    X'41F0',S(SR@QUEUE),X'03',X'00',X'02',X'00',A(STPV03)            
         DC    XL4'00000000',X'00',X'00',X'00',X'00',A(0)                       
*                                                                               
FLSOPTN  DC    X'41F0',S(SR@PRR),X'01',X'00',X'02',X'00',A(FLSV01)              
         DC    X'41F0',S(SR@RPT),X'02',X'00',X'02',X'00',A(FLSV02)              
         DC    X'41F0',S(SR@QUEUE),X'03',X'00',X'02',X'00',A(FLSV03)            
         DC    XL4'00000000',X'00',X'00',X'00',X'00',A(0)                       
*                                                                               
SCHOPTN  DC    X'41F0',S(SR@COPYS),X'01',X'00',X'04',X'00',A(SCHV01)            
         DC    X'41F0',S(SR@ERROR),X'02',X'00',X'02',X'00',A(STRV02)            
         DC    X'41F0',S(SR@PAGES),X'03',X'00',X'04',X'00',A(SCHV02)            
         DC    X'41F0',S(SR@LAST),X'04',X'00',X'02',X'00',A(STRV01)             
         DC    XL4'00000000',X'00',X'00',X'22',X'00',A(STRV00)                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* NLINED                                                                        
***********************************************************************         
NLINED   DSECT                                                                  
NSELH    DS    CL8                                                              
NSEL     DS    CL4                 ACT FIELD                                    
NLINEH   DS    CL8                                                              
NLINE    DS    0CL73                                                            
NLNUM    DS    CL2                 ENTRY NUMBER                                 
         DS    CL1                                                              
NLTYPE   DS    CL1                 P OR T                                       
         DS    CL1                                                              
NLCLASS  DS    CL1                 CLASS                                        
         DS    CL1                                                              
NLREPORT DS    CL18                REPORTS IDENTIFICATION                       
         DS    CL1                                                              
NLRPTS   DS    CL4                 NUM OF ACTIVE REPORTS                        
         DS    CL1                                                              
NLPAGES  DS    CL5                 NUMBER OF ACTIVE PAGES                       
         DS    CL1                                                              
NLLINES  DS    CL6                 NUMBER OF ACTIVE LINES                       
         DS    CL1                                                              
NLTIME   DS    CL6                 PRINT TIME                                   
         DS    CL1                                                              
NLOPTS   DS    CL22                OPTIONS                                      
         EJECT                                                                  
                                                                                
***********************************************************************         
* QED                                                                           
***********************************************************************         
QED      DSECT                                                                  
QEFIELD  DS    0CL26                                                            
QESRCID  DS    CL2                                                              
QESUBID  DS    CL3                                                              
QESEQN   DS    XL2                                                              
QECLASS  DS    X                                                                
QEPREX   DS    X                                                                
QECOPIES DS    X                                                                
QERPTS   DS    H                                                                
QEPAGES  DS    F                                                                
QELINES  DS    F                                                                
QECHRS   DS    F                                                                
QERSEC   DS    H                                                                
         EJECT                                                                  
       ++INCLUDE SRPQUDD           DICTIONARY REFS                              
         EJECT                                                                  
       ++INCLUDE SRPQUWK                                                        
         EJECT                                                                  
SRPQUFFD DS    CL64                                                             
       ++INCLUDE SRPQUFFD                                                       
         EJECT                                                                  
*FADSECTS                                                                       
*DDCOMFACS                                                                      
*SRERREQUS                                                                      
*DDFLDHDR                                                                       
*CTGENFILE                                                                      
*FAPRQ                                                                          
*FAFACTS                                                                        
*IHAASCB                                                                        
         PRINT OFF                                                              
       ++INCLUDE FADSECTS                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE SRERREQUS                                                      
       ++INCLUDE DDFLDHDR                                                       
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE FAPRQ                                                          
       ++INCLUDE FAFACTS                                                        
         IHAASCB LIST=YES                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006SRPQU01   08/25/20'                                      
         END                                                                    
