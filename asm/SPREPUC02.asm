*          DATA SET SPREPUC02  AT LEVEL 025 AS OF 02/02/21                      
*PHASE SPUC02A                                                                  
*INCLUDE XSORT                                                                  
*INCLUDE PRNTBL                                                                 
***********************************************************************         
* ORIGINALLY SPREPUP02U, CONVERTED TO SPREPUC02 TO BE A REAL REPORT             
* AKAT   LEVEL 17       3/01/10  FIX REPURPOSING BUG                            
* BPOO   LEVEL 27       5/21/98  SORT THE NETWORK LIST                          
***********************************************************************         
SPUC02   TITLE 'SPREPUC02 - MULTI-AGENCY CABLE UPDATE'                          
SPUC02   CSECT                                                                  
         DS    8192C                                                            
         ORG   *-8192                                                           
         PRINT NOGEN                                                            
*                                  GOT 3 BASE REGISTERS                         
         NMOD1 0,SPUC02,R8,R7,RR=R3                                             
*                                                                               
         LR    RC,RB               SET UP MY WORK AREA                          
         AH    RC,=Y(SPUCWRKD-SPUC02)                                           
         USING SPUCWRKD,RC                                                      
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
         ST    R3,RELO                                                          
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    MAIN                                                             
         B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* MAIN                                                                          
***************                                                                 
*     MAIN SECTION OF THE PROGRAM                                               
***********************************************************************         
*                                                                               
MAIN     DS    0H                                                               
         L     RF,=V(XSORT)                                                     
         ST    RF,VXSORT                                                        
         LR    R6,RB               SET ADDRESS OF RECORD FOR SPONSOR            
         A     R6,=A(SPOTREC-SPUC02)                                            
         ST    R6,AREC1                                                         
         LR    R6,RB               SET ADDRESS OF RECORD #2 FOR SPONSOR         
         A     R6,=A(SPOTREC2-SPUC02)                                           
         ST    R6,AREC2                                                         
         ST    R6,ADSTAT                                                        
         ST    R6,ADSTATAD                                                      
*                                                                               
         MVC   AREC,AREC1          DEFAULT RECORD AREA                          
*                                                                               
         OPEN  (FILEIN,INPUT)      OPEN UP INPUT FILE FOR USE                   
*                                                                               
         ZAP   COUNTER1,=P'0'                                                   
         ZAP   COUNTER3,=P'0'                                                   
         ZAP   COUNTER4,=P'0'                                                   
         ZAP   COUNTER5,=P'0'                                                   
         ZAP   COUNTER6,=P'0'                                                   
         XC    ACTHDEND,ACTHDEND                                                
         XC    SYSHDEND,SYSHDEND                                                
         XC    SVDHDEND,SVDHDEND                                                
*                                                                               
         XC    DMCB(4),DMCB                                                     
         MVC   DMCB+4(3),=X'D9000A'                                             
         MVI   DMCB+7,QCABLETB                                                  
         L     RF,ACOMFACS                                                      
         L     RF,(CCALLOV-COMFACSD)(RF)                                        
         GOTO1 (RF),DMCB                                                        
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         ST    RF,ACABLTAB                                                      
         SR    R0,R0                                                            
         IC    R0,5(RF)            GET ENTRY LENGTH                             
         STH   R0,CBLTABLN                                                      
*                                                                               
         GOTO1 =A(GETAGCYS),RR=RELO  FIND ALL AGY'S ON SAME SPOT FILE           
*                                                                               
         CLI   AGYTABLE,X'FF'      ANY AGENCIES FOR THIS SPOT SYSTEM?           
         BNE   MAIN10                                                           
         MVC   P(35),=CL35'NOTE: NO AGENCIES FOR THIS UPDATE'                   
         GOTO1 REPORT                                                           
         B     NOMOREX             NONE                                         
*                                                                               
MAIN10   BAS   RE,ADNCAREP         ADD THE 'NCA' REP                            
         GOTO1 =A(ADCNIREP),RR=RELO                                             
*                                                                               
READALIN XCEFL INPTLINE,4004       CLEAR THE INPUT LINE                         
         MVI   BITFLAG1,0          CLEAR OUR BIT FLAGS                          
*                                                                               
         GET   FILEIN,INPTLINE     READ A LINE FROM THE FILE                    
*                                                                               
         BAS   RE,FETCHINF         FETCH IMPORTANT INFO FROM LINE               
         BNE   READALIN                                                         
*                                                                               
         BAS   RE,PRCSSINF         PROCESS THE INFORMATION                      
         B     READALIN            READ THE NEXT LINE                           
*                                                                               
***********************************************************************         
* NOMORE                                                                        
*                                                                               
* WHAT HAPPENS WHEN THERE IS NO MORE LINES LEFT IN THE FILE                     
***********************************************************************         
*                                                                               
NOMORE   DS    0H                                                               
         OC    ACTHDEND,ACTHDEND   ANYTHING TO DO BEFORE WE LEAVE?              
         BZ    NOMORE10            NO, PRINT NUMBERS                            
*                                                                               
         MVC   SVDHDEND,ACTHDEND   YES, PROCESS LAST HEADEND                    
         MVC   ACTNFLG1,CODEFLG1                                                
         MVC   ACTNFLG2,CODEFLG2                                                
         XC    CBLHDEND,CBLHDEND   SIGNIFY LAST RECORD                          
         OI    BITFLAG1,B1ACTION                                                
         BAS   RE,PRCSSINF         PROCESS THE LAST RECORD                      
*                                                                               
NOMORE10 MVC   P(30),=CL30'NUMBER OF STATIONS ADDED: '                          
         EDIT  (P8,COUNTER1),(17,P+30),ALIGN=LEFT,ZERO=NOBLANK                  
         MVC   P3(32),=CL30'NUMBER OF K PASSIVES ADDED: '                       
         EDIT  (P8,COUNTER6),(17,P3+30),ALIGN=LEFT,ZERO=NOBLANK                 
         MVC   P4(30),=CL30'NUMBER OF ADDRESSES ADDED: '                        
         EDIT  (P8,COUNTER3),(17,P4+30),ALIGN=LEFT,ZERO=NOBLANK                 
         MVC   P5(30),=CL30'NUMBER OF CABLE RECS ADDED: '                       
         EDIT  (P8,COUNTER4),(17,P5+30),ALIGN=LEFT,ZERO=NOBLANK                 
         MVC   P6(30),=CL30'NUMBER OF TRAFFICS ADDED: '                         
         EDIT  (P8,COUNTER5),(17,P6+30),ALIGN=LEFT,ZERO=NOBLANK                 
         GOTO1 REPORT                                                           
*                                                                               
NOMOREX  CLOSE FILEIN                                                           
*                                                                               
         MVI   MODE,REQLAST                                                     
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ADNCAREP                                                                      
***************                                                                 
*     THIS ROUTINE ADD OR CHANGES THE 'NCA' STATION REP RECORD                  
***********************************************************************         
*                                                                               
ADNCAREP NTR1                                                                   
         LA    R2,AGYTABLE                                                      
         USING AGYENTRY,R2                                                      
*                                                                               
NCAREP05 CLI   AGYENTRY,X'FF'                                                   
         BE    NCAREPX                                                          
*                                                                               
         L     R6,AREC                                                          
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING REPKEY,R4                                                        
         MVI   REPKTYPE,C'R'                                                    
         MVI   REPKMED,C'T'                                                     
         MVC   REPKREP,=C'NCA'                                                  
         MVC   REPKAGY,AGYPOWCD                                                 
         MVI   REPKFILL,C'0'                                                    
         MVC   REPKFILL+1(L'REPKFILL-1),REPKFILL                                
         DROP  R4                                                               
*                                                                               
         BAS   RE,STAHIGHD                                                      
         CLI   DMCB+8,X'02'                                                     
         BE    *+14                                                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         NI    BITFLAG2,X'FF'-B2ADDING   SIGNIFY CHANGING FIRST                 
         CLC   KEY(REPKEYLQ),0(R6)                                              
         BE    NCAREP10                                                         
         OI    BITFLAG2,B2ADDING         WE'RE GOING TO ADD                     
         XC    0(L'REPREC,R6),0(R6)                                             
         MVC   0(REPKEYLQ,R6),KEY                                               
*                                                                               
         USING REPREC,R6                                                        
NCAREP10 MVC   REPRLEN,=Y(L'REPREC)                                             
         NI    RCNTL,X'FF'-X'80'                                                
         MVC   RNAME(19),=CL19'AMPERSAND'                                       
         MVC   R1LINE(11),=CL11'PO BOX 3350'                                    
         MVC   R2LINE(6),=CL6'BOSTON'                                           
         MVC   R3LINE,=C'MA '                                                   
         MVI   RUNWNET,C'N'                                                     
         MVC   RBIGZIP(5),=CL5'02241'                                           
         DROP  R6                                                               
*                                                                               
         TM    BITFLAG2,B2ADDING   ADD RECORD?                                  
         BZ    NCAREP20                                                         
         GOTO1 MYTRACE,DMCB,AREC,L'REPREC,                             X        
               =CL21'ADDING REP RECORD NCA',21                                  
*                                                                               
         BAS   RE,STAADD                                                        
         CLI   DMCB+8,0                                                         
         BE    NCAREP30                                                         
         DC    H'0'                                                             
*                                                                               
NCAREP20 GOTO1 MYTRACE,DMCB,AREC,L'REPREC,                             X        
               =CL23'CHANGING REP RECORD NCA',23                                
*                                                                               
         BAS   RE,STAWRT                                                        
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
NCAREP30 LA    R2,L'AGYENTRY(R2)                                                
         B     NCAREP05                                                         
*                                                                               
NCAREPX  B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* FETCHINF                                                                      
***************                                                                 
*     THIS ROUTINE FETCHES ALL THE NECESSARY INFOMATION FROM THE LINE           
* WE GOT FROM THE FILE AND PUTS IT INTO OUR STORAGE AREA                        
***********************************************************************         
*                                                                               
FETCHINF NTR1                                                                   
         LA    R2,INPTDATA         R2=A(DATA OR 1ST FIELD)                      
*                                                                               
         CLI   5(R2),C'0'          SEE IF WE HAVE ACTION CODE DATA              
         BE    FINF00                                                           
         CLI   5(R2),C'1'          SEE IF WE HAVE SYSTEM CODE DATA              
         BE    FINF10                                                           
         CLI   5(R2),C'2'          SEE IF WE HAVE NETWORK CODE DATA             
         BE    FINF20                                                           
*                                                                               
         MVC   P(23),=C'LINE THAT BEGINS WITH: '     INVALID CODE               
         MVC   P+25(10),INPTDATA                                                
         MVC   P+40(12),=C'HAS AN ERROR'                                        
         GOTO1 REPORT                                                           
         B     FINFNO                                                           
*****                                                                           
* ACTION DATA                                                                   
*****                                                                           
*                                                                               
FINF00   OI    BITFLAG1,B1ACTION   GOT AN ACTION                                
*                                                                               
         GOTO1 NEXTFLD,DMCB,(0,(R2)),(L'IN_SYSCD,IN_SYSCD)                      
         BNE   FINFER00                                                         
         L     R2,DMCB                                                          
         EDIT  (B2,IN_SYSCD),(4,CBLHDEND),FILL=0                                
         MVI   CBLHDEND+4,C'T'     CABLE IS A SUBSET OF SPOT TV                 
*                                                                               
         OC    ACTHDEND,ACTHDEND   BEGINNING OF FILE?                           
         BZ    FINF02              YES                                          
*                                                                               
         CLC   ACTHDEND,CBLHDEND   SAME CABLE HEADEND AS BEFORE?                
         BE    FINF02              YES                                          
         MVC   SVDHDEND,ACTHDEND   NO, SAVE THE PREVIOUS HEADEND USED           
         MVC   ACTNFLG1,CODEFLG1       SAVE PREVIOUS ACTION CODE FLAG           
         MVC   ACTNFLG2,CODEFLG2       SAVE PREVIOUS ACTION CODE FLAG           
         MVI   CODEFLG1,0              CLEAR CURRENT ACTION CODE FLAG           
         MVI   CODEFLG2,0              CLEAR CURRENT ACTION CODE FLAG           
*                                                                               
FINF02   GOTO1 NEXTFLD,DMCB,(2,(R2)),(L'IN_ACTCD,IN_ACTCD)                      
         BNE   FINFER00                                                         
*                                                                               
         LA    R1,CODTABLE         VALIDATE THE ACTION CODE                     
         LA    RF,CODEFLG1         FIRST CODE FLAG                              
         LA    R0,8                8 BITS PER ACTION FLAG                       
*                                                                               
FINF04   CLI   0(R1),0             ERROR IF END OF TABLE                        
         BE    FINFER00                                                         
*                                                                               
         CLC   IN_ACTCD,0(R1)      CODE IN THIS ENTRY?                          
         BE    FINF05              YES                                          
         LA    R1,L'CODTABLE(R1)   NO, CHECK THE NEXT ENTRY                     
         BCT   R0,FINF04                                                        
*                                                                               
         LA    RF,CODEFLG2         2ND CODE FLAG                                
         LA    R0,8                8 MORE BITS                                  
         B     FINF04              NEXT BUNCH                                   
*                                                                               
FINF05   MVC   BYTE,0(RF)          SET THE APPROPRIATE BIT ON FOR THIS          
         OC    BYTE,1(R1)              ACTION CODE                              
         MVC   0(1,RF),BYTE                                                     
*                                                                               
FINF00X  MVC   ACTHDEND,CBLHDEND   SAVE CABLE HEADEND FOR THIS ACTION           
         B     FINFYES                                                          
*                                                                               
FINFER00 MVC   P(13),=C'ACTION LINE: '                                          
         MVC   P+15(10),INPTDATA                                                
         MVC   P+30(12),=C'HAS AN ERROR'                                        
         GOTO1 REPORT                                                           
         B     FINFNO                                                           
         EJECT                                                                  
*****                                                                           
* STATION DATA                                                                  
*****                                                                           
*                                                                               
FINF10   DS    0H                                                               
         LA    RE,IN_SYSCD         CLEAR DATA SAVED FROM PREVIOUS LINE          
         LA    RF,IN_DATAX-IN_SYSCD                                             
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         GOTO1 NEXTFLD,DMCB,(0,(R2)),(L'IN_SYSCD,IN_SYSCD)                      
         BNE   FINFER10                                                         
         L     R2,DMCB                                                          
         EDIT  (B2,IN_SYSCD),(4,CBLHDEND),FILL=0                                
         MVI   CBLHDEND+4,C'T'     CABLE IS A SUBSET OF SPOT TV                 
         MVC   SYSHDEND,CBLHDEND                                                
*                                                                               
         CLI   QOPT2,C'Y'          BIG FIX DISK?                                
         BE    *+14                                                             
         CLC   ACTHDEND,SYSHDEND   SAME HEADEND AS THE ACTION CODE?             
         BNE   FINFNO              NO, SKIP THIS SYSTEM                         
*                                                                               
         GOTO1 NEXTFLD,DMCB,(3,(R2)),(L'IN_SYSNM,IN_SYSNM)                      
         BNE   FINFER10                                                         
         L     R2,DMCB                                                          
         OC    IN_SYSNM,SPACES                                                  
*                                                                               
         GOTO1 NEXTFLD,DMCB,(1,(R2)),(L'IN_MSONM,IN_MSONM)                      
         BNE   FINFER10                                                         
         L     R2,DMCB                                                          
         OC    IN_MSONM,SPACES                                                  
*                                                                               
         GOTO1 NEXTFLD,DMCB,(5,(R2)),(L'IN_TPEDL,IN_TPEDL)                      
         BNE   FINFER10                                                         
         L     R2,DMCB                                                          
         OC    IN_TPEDL,SPACES                                                  
*                                                                               
         GOTO1 NEXTFLD,DMCB,(1,(R2)),(L'IN_ORDDL,IN_ORDDL)                      
         BNE   FINFER10                                                         
         L     R2,DMCB                                                          
         OC    IN_ORDDL,SPACES                                                  
*                                                                               
         GOTO1 NEXTFLD,DMCB,(1,(R2)),(L'IN_INCCD,IN_INCCD)                      
         BNE   FINFER10                                                         
         L     R2,DMCB                                                          
         OC    IN_INCCD,SPACES                                                  
*                                                                               
         GOTO1 NEXTFLD,DMCB,(1,(R2)),(L'IN_AMRKT,IN_AMRKT)                      
         BNE   FINFER10                                                         
         L     R2,DMCB                                                          
         OC    IN_AMRKT,SPACES                                                  
*                                                                               
         GOTO1 NEXTFLD,DMCB,(1,(R2)),(L'IN_INCNM,IN_INCNM)                      
         BNE   FINFER10                                                         
         L     R2,DMCB                                                          
         OC    IN_INCNM,SPACES                                                  
*                                                                               
         GOTO1 NEXTFLD,DMCB,(3,(R2)),(L'IN_TRANM,IN_TRANM)                      
         BNE   FINFER10                                                         
         L     R2,DMCB                                                          
         OC    IN_TRANM,SPACES                                                  
*                                                                               
         GOTO1 NEXTFLD,DMCB,(1,(R2)),(L'IN_TRAAD,IN_TRAAD)                      
         BNE   FINFER10                                                         
         L     R2,DMCB                                                          
         OC    IN_TRAAD,SPACES                                                  
*                                                                               
         GOTO1 NEXTFLD,DMCB,(2,(R2)),(L'IN_TRACT,IN_TRACT)                      
         BNE   FINFER10                                                         
         L     R2,DMCB                                                          
         OC    IN_TRACT,SPACES                                                  
*                                                                               
         GOTO1 NEXTFLD,DMCB,(1,(R2)),(L'IN_TRAST,IN_TRAST)                      
         BNE   FINFER10                                                         
         L     R2,DMCB                                                          
         OC    IN_TRAST,SPACES                                                  
*                                                                               
         GOTO1 NEXTFLD,DMCB,(1,(R2)),(L'IN_TRAZP,IN_TRAZP)                      
         BNE   FINFER10                                                         
         L     R2,DMCB                                                          
*                                                                               
         GOTO1 NEXTFLD,DMCB,(1,(R2)),(L'IN_PYREP,IN_PYREP)                      
         BNE   FINFER10                                                         
         L     R2,DMCB                                                          
         OC    IN_PYREP,SPACES                                                  
*                                                                               
         GOTO1 NEXTFLD,DMCB,(1,(R2)),(L'IN_COMNM,IN_COMNM)                      
         BNE   FINFER10                                                         
         L     R2,DMCB                                                          
         OC    IN_COMNM,SPACES                                                  
*                                                                               
         GOTO1 NEXTFLD,DMCB,(1,(R2)),(L'IN_COMAD,IN_COMAD)                      
         BNE   FINFER10                                                         
         L     R2,DMCB                                                          
         OC    IN_COMAD,SPACES                                                  
*                                                                               
         GOTO1 NEXTFLD,DMCB,(2,(R2)),(L'IN_COMCT,IN_COMCT)                      
         BNE   FINFER10                                                         
         L     R2,DMCB                                                          
         OC    IN_COMCT,SPACES                                                  
*                                                                               
         GOTO1 NEXTFLD,DMCB,(1,(R2)),(L'IN_COMST,IN_COMST)                      
         BNE   FINFER10                                                         
         L     R2,DMCB                                                          
         OC    IN_COMST,SPACES                                                  
*                                                                               
         GOTO1 NEXTFLD,DMCB,(1,(R2)),(L'IN_COMZP,IN_COMZP)                      
         BNE   FINFER10                                                         
         L     R2,DMCB                                                          
*                                                                               
         GOTO1 NEXTFLD,DMCB,(1,(R2)),(L'IN_GRPCD,IN_GRPCD)                      
         BNE   FINFER10                                                         
         L     R2,DMCB                                                          
         OC    IN_GRPCD,SPACES                                                  
*                                                                               
         GOTO1 NEXTFLD,DMCB,(1,(R2)),(L'IN_TPESZ,IN_TPESZ)                      
         BNE   FINFER10                                                         
         L     R2,DMCB                                                          
         OC    IN_TPESZ,SPACES                                                  
*                                                                               
         GOTO1 NEXTFLD,DMCB,(1,(R2)),(L'IN_EIXST,IN_EIXST)                      
         BNE   FINFER10                                                         
         L     R2,DMCB                                                          
         OC    IN_EIXST,SPACES                                                  
*                                                                               
FINF10X  B     FINFYES                                                          
*                                                                               
FINFER10 MVC   P(14),=C'STATION LINE: '                                         
         MVC   P+15(4),INPTDATA                                                 
         MVC   P+20(12),=C'HAS AN ERROR'                                        
         GOTO1 REPORT                                                           
         B     FINFNO                                                           
         EJECT                                                                  
*****                                                                           
* NETWORK DATA                                                                  
*****                                                                           
*                                                                               
FINF20   OI    BITFLAG1,B1NETWRK   YES                                          
*                                                                               
         GOTO1 NEXTFLD,DMCB,(0,(R2)),(L'IN_SYSCD,IN_SYSCD)                      
         BNE   FINFER20                                                         
         L     R2,DMCB                                                          
         EDIT  (B2,IN_SYSCD),(4,CBLHDEND),FILL=0                                
         MVI   CBLHDEND+4,C'T'     CABLE IS A SUBSET OF SPOT TV                 
         MVC   NETHDEND,CBLHDEND                                                
*                                                                               
         CLC   ACTHDEND,NETHDEND   SAME CABLE HEADEND AS ACTION'S?              
         BNE   FINFNO                                                           
         TM    CODEFLG1,CDNEWSYS   ADDING NEW SYSTEM?                           
         BZ    FINF25              NO                                           
         CLC   SYSHDEND,NETHDEND   SAME CABLE HEADEND AS SYSTEM'S?              
         BNE   FINFNO              YES, IT HAS TO BE                            
*                                                                               
FINF25   GOTO1 NEXTFLD,DMCB,(2,(R2)),(L'IN_NWKCD,IN_NWKCD)                      
         BNE   FINFER20                                                         
         OC    IN_NWKCD,SPACES                                                  
         B     FINFYES                                                          
*                                                                               
FINFER20 MVC   P(14),=C'NETWORK LINE: '                                         
         MVC   P+15(13),INPTDATA                                                
         MVC   P+30(12),=C'HAS AN ERROR'                                        
         GOTO1 REPORT                                                           
         B     FINFNO                                                           
*                                                                               
FINFYES  B     YES                                                              
*                                                                               
FINFNO   B     NO                                                               
         EJECT                                                                  
***********************************************************************         
* NEXTFLD                                                                       
***************                                                                 
*     THIS ROUTINE POINTS TO THE NEXT FIELD OF THE INPUT LINE.                  
*                                                                               
* ON ENTRY:    DMCB    BYTE  0     NUMBER OF FIELDS TO SKIP                     
*                      BYTES 1-3   ADDRESS OF CURRENT FIELD                     
*                                                                               
*              DMCB+4  BYTE  0     MAX LENGTH OF STORAGE AREA                   
*                      BYTES 1-3   ADDRESS OF WHERE TO STORE FIELD              
*                                                                               
* ON EXIT:     DMCB    BYTE  0     FIRST BYTE OF FIELD WE FOUND                 
*                      BYTES 1-3   ADDRESS OF FIELD WE FOUND                    
***********************************************************************         
*                                                                               
NEXTFLD  NTR1                                                                   
         L     R2,DMCB             R2=A(CURRENT FIELD IN INPTLINE)              
         L     R4,DMCB+4           R4=A(STORAGE AREA)                           
*                                                                               
         ZIC   R1,DMCB+4           CLEAR THE STORAGE AREA                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    0(0,R4),0(R4)                                                    
*                                                                               
         CLI   DMCB,0              SKIP ANY FIELDS?                             
         BNE   *+12                                                             
         LA    R3,1                NO                                           
         B     NFLD25                                                           
*                                                                               
         ZIC   R3,DMCB             R3=NUMBER OF FIELDS TO SKIP                  
NFLDRNTR NI    BITFLAG1,X'FF'-B1DBLQTE-B1COMMA                                  
*                                                                               
NFLDLOOP CLI   0(R2),C'"'          DO WE HAVE A DOUBLE QUOTE?                   
         BNE   NFLD10                                                           
*                                                                               
         TM    BITFLAG1,B1COMMA    GOT A COMMA ALREADY?                         
         BNZ   NFLD25              YES, NEXT FIELD                              
*                                                                               
         TM    BITFLAG1,B1DBLQTE   USING A DOUBLE QUOTE ALREADY?                
         BZ    *+12                                                             
         NI    BITFLAG1,X'FF'-B1DBLQTE                                          
         B     NFLDNXTC                                                         
         OI    BITFLAG1,B1DBLQTE                                                
         B     NFLDNXTC                                                         
*                                                                               
NFLD10   CLI   0(R2),C','          DO WE HAVE A COMMA?                          
         BNE   NFLD20                                                           
*                                                                               
         TM    BITFLAG1,B1COMMA    GOT A COMMA ALREADY?                         
         BNZ   NFLD15                                                           
         TM    BITFLAG1,B1DBLQTE   NO, COMMA IN BETWEEN QUOTES?                 
         BNZ   NFLDNXTC                YES                                      
         OI    BITFLAG1,B1COMMA        NO, WE HAVE A COMMA NOW                  
         B     NFLDNXTC            NEXT CHAR SHOULD BE START OF NXT FLD         
*                                                                               
NFLD15   ST    R2,DMCB             RETURN WITH A COMMA IN DMCB SO WE            
         MVI   DMCB,C','               KNOW WE HAVE 2 COMMAS IN A ROW           
         BCT   R3,NFLDRNTR         RE-ENTER IF WE NEED TO SKIP MORE             
         B     NFLDYES             NO DATA TO STORE                             
*                                                                               
NFLD20   TM    BITFLAG1,B1COMMA                                                 
         BZ    NFLDNXTC                                                         
*                                                                               
NFLD25   ST    R2,DMCB                                                          
         MVC   DMCB(1),0(R2)                                                    
         BCT   R3,NFLDRNTR         RE-ENTER IF WE NEED TO SKIP MORE             
         B     NFLDSTOR                                                         
*                                                                               
NFLDNXTC LA    R2,1(R2)            R2 = A(NEXT CHARACTER)                       
         LA    R0,INPTLINE                                                      
         LR    R1,R2                                                            
         SR    R1,R0                                                            
         CLM   R1,3,INPTLGTH       DID WE GO BEYOND THE DATA?                   
         BL    NFLDLOOP            NO                                           
         B     NFLDNO                                                           
*                                                                               
NFLDSTOR CLI   0(R2),C'"'          DEALING WITH A STRING?                       
         BNE   NFLD40                                                           
*                                                                               
         LA    R2,1(R2)                                                         
         ZIC   R1,DMCB+4           COPY STRING WITHOUT FIRST QUOTE              
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R2)                                                    
*                                                                               
         L     R1,DMCB+4                                                        
         LA    R1,0(R1)            CLEAR HOB                                    
         LR    R0,R1                                                            
NFLD30   CLI   0(R4),C'"'          FOUND SECOND QUOTE?                          
         BE    NFLD35                                                           
         LA    R4,1(R4)            NO, CHECK NEXT CHARACTER                     
         LR    R1,R4                                                            
         SR    R1,R0                                                            
         CLM   R1,1,DMCB+4                                                      
         BNL   NFLDYES                                                          
         B     NFLD30                                                           
*                                                                               
NFLD35   L     R0,DMCB+4           CLEAR FROM SECOND QUOTE ON                   
         LR    R1,R4                                                            
         SR    R1,R0                                                            
         ZIC   R0,DMCB+4                                                        
         SR    R0,R1                                                            
         BCTR  R0,0                                                             
         LR    R1,R0                                                            
         EX    R1,*+8                                                           
         B     NFLDYES                                                          
         XC    0(0,R4),0(R4)                                                    
*                                                                               
NFLD40   LA    R1,1                AT LEAST ONE DIGIT IN NUMBER                 
         LR    R3,R2                                                            
*                                                                               
         LA    R3,1(R3)                                                         
         CLI   0(R3),C','                                                       
         BE    *+12                                                             
         LA    R1,1(R1)            INCREMENT DIGIT COUNTER                      
         B     *-16                                                             
*                                                                               
         BCTR  R1,0                CONVERT NUMBER TO BINARY                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R2)                                                      
         CVB   R1,DUB                                                           
*                                                                               
         CLI   DMCB+4,1            ONE BYTE                                     
         BNE   *+12                                                             
         STC   R1,0(R4)                                                         
         B     NFLDYES                                                          
*                                                                               
         CLI   DMCB+4,2            HALF WORD                                    
         BNE   *+12                                                             
         STH   R1,0(R4)                                                         
         B     NFLDYES                                                          
*                                                                               
         CLI   DMCB+4,3            3 BYTES                                      
         BNE   *+12                                                             
         STCM  R1,7,0(R4)                                                       
         B     NFLDYES                                                          
*                                                                               
         CLI   DMCB+4,4            FULL WORD                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         ST    R1,0(R4)                                                         
         B     NFLDYES                                                          
*                                                                               
NFLDNO   B     NO                  NO MORE DATA                                 
*                                                                               
NFLDYES  B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE PROCESSES THE INFORMATION FROM THE LINE WE GOT FROM THE          
* FILE.                                                                         
***********************************************************************         
*                                                                               
PRCSSINF NTR1                                                                   
         TM    BITFLAG1,B1ACTION   ACTION DATA?                                 
         BNZ   PINFA00             YES                                          
         TM    BITFLAG1,B1NETWRK   NO, NETWORK DATA?                            
         BZ    PINFS00                 NO, SYSTEMS DATA                         
         B     PINFN00                 YES                                      
*****                                                                           
* ACTION DATA                                                                   
*****                                                                           
PINFA00  OC    SVDHDEND,SVDHDEND   LOOKED AT A CABLE BEFORE?                    
         BZ    PINFA500                                                         
*                                                                               
         LA    R2,AGYTABLE         PROCESS FOR ALL AGENCIES IN TABLE            
         USING AGYENTRY,R2                                                      
*                                                                               
PINFA10  CLI   AGYENTRY,X'FF'      END OF AGENCY TABLE?                         
         BE    PINFA500            YES, NO MORE AGENCIES TO PROCESS             
*                                                                               
         ST    R2,ADCURAGY         NO, SAVE A(CURRENT AGENCY ENTRY)             
         MVC   XCPTCLT,=CL3'000'   NOT EXCEPTION RECORD                         
*                                                                               
         TM    ACTNFLG1,CDNTACTV   IF JUST DEACTIVATE THEN                      
         BZ    *+12                                                             
         TM    ACTNFLG1,CDNEWSYS                                                
         BZ    PINFA15             DON'T WORRY ABOUT ALPHA MARKET               
*                                                                               
         OC    AGYMKTNO,AGYMKTNO   ALPHA MARKET VALID FOR AGENCY?               
         BNZ   PINFA15                                                          
         BAS   RE,BADMKTCD         NO                                           
         B     PINFA400                                                         
*                                                                               
PINFA15  DS    0H                                                               
*                                                                               
         CLI   QOPT2,C'Y'          BIG FIX UPDATE?                              
         BNE   PINFA100            NO                                           
*****                                                                           
* BIG FIX UPDATING CODE                                                         
*****                                                                           
         BAS   RE,CKSTATN                                                       
         BNE   PINFA20                                                          
*                                                                               
         L     R6,AREC                                                          
         USING GENSTAD,R6                                                       
         CLC   SMKT,AGYMKTNO                                                    
         BE    *+12                                                             
         BAS   RE,BADMKTCH                                                      
         B     PINFA400                                                         
*                                                                               
         MVC   PRVMRKT,SMKT                                                     
         CLI   SSYSDACT,X'FF'      STATION DEACTIVATED?                         
         BNE   *+12                                                             
         BAS   RE,DACTIVTD         YES, NO ACTIONS ALLOWED                      
         B     PINFA400                                                         
         DROP  R6                                                               
*                                                                               
PINFA20  TM    ACTNFLG1,CDNEWSYS+CDNTACTV    UPDATE OR ADD THE DATA?            
         BO    PINFA30                                                          
*                                                                               
         TM    ACTNFLG1,CDNTACTV   DEACTIVATE THE SYSTEM?                       
         BZ    PINFA30                                                          
*                                                                               
         BAS   RE,DACTVATE         YES                                          
         BNE   PINFA400            COULDN'T DO IT FOR SOME REASON               
*                                                                               
         MVC   P+10(19),=CL19'DEACTIVATING SYSTEM'                              
         GOTO1 REPORT                                                           
         B     PINFA400                                                         
*                                                                               
PINFA30  MVC   NUMMRKT,AGYMKTNO                                                 
         BAS   RE,UPDSTATN         ADD OR UPDATE STATION RECORD                 
*        BNE   PINFA400            HAD AN ERROR                                 
         BAS   RE,UPDADDRS         ADD OR UPDATE ADDRESS RECORD                 
         BAS   RE,UPDCABLE         ADD OR UPDATE CABLE DATA RECORD              
         BAS   RE,UPDTRAFF         ADD OR UPDATE TRAFFIC ADDRESS                
         MVC   P+10(24),=CL24'BIG UPDATE ADD/CHANGE'                            
         GOTO1 REPORT                                                           
         B     PINFA400                                                         
*****                                                                           
* REGULAR UPDATING CODE                                                         
*****                                                                           
PINFA100 DS    0H                                                               
         BAS   RE,CKSTATN          STATION EXISTED BEFORE?                      
         BNE   PINFA200            NO                                           
*****                                                                           
* SYSTEM EXISTED BEFORE                                                         
*****                                                                           
         TM    ACTNFLG1,CDNEWSYS   YES, NEW STATION?                            
         BZ    PINFA110                                                         
         TM    ACTNFLG1,CDNTACTV                                                
         BNZ   PINFA110                                                         
         BAS   RE,SYSNTNEW         YES, CAN'T, SYSTEM IS NOT NEW                
         B     PINFA400                                                         
*                                                                               
PINFA110 L     R6,AREC                                                          
         USING GENSTAD,R6                                                       
         TM    ACTNFLG1,CDNTACTV   IGNORE MARKET CHANGE ERROR IF                
         BNZ   PINFA112              NCC IS DEACTIVATING OR REPURPOSING         
         TM    ACTNFLG2,CDREPURP                                                
         BNZ   PINFA112                                                         
         CLC   SMKT,AGYMKTNO                                                    
         BE    *+12                                                             
         BAS   RE,BADMKTCH         TRYING TO CHANGE MARKETS                     
         B     PINFA400                                                         
*                                                                               
PINFA112 MVC   PRVMRKT,SMKT                                                     
         TM    ACTNFLG2,CDREPURP   REPURPOSING SYSCODE??                        
         BZ    PINFA116            NO                                           
*                                                                               
         CLI   SSYSDACT,X'FF'      THEN THE SYSCODE HAS TO BE DEACTIV           
         BE    *+12                                                             
         BAS   RE,NOREPURP         CAN'T REPURPOSE AS SYSTEM IS ACTIVE          
         B     PINFA400                                                         
*                                                                               
         MVI   SSYSDACT,0          NO LONGER DEACTIVATED                        
*                                  REPURPOSE DOES THE WHOLE THING               
         OI    ACTNFLG1,CDPAYADR+CDNETCHG+CDEXCLUS                              
         OI    ACTNFLG1,CDSYSNAM+CDMSOINT+CDTPEADR                              
         OI    ACTNFLG2,CDEIXSTA+CDGRPCOD                                       
         B     PINFA118                                                         
*                                                                               
PINFA116 CLI   SSYSDACT,X'FF'      DEACTIVATED ALREADY?                         
         BNE   PINFA118                                                         
         BAS   RE,DACTIVTD         YES, NO ACTIONS ALLOWED                      
         DROP  R6                                                               
*                                                                               
         USING GENCBLD,R6          CABLE RECORD MIGHT NOT HAVE BEEN             
         BAS   RE,CKCBLREC             DEACTIVATED                              
         BNE   PINFA180                                                         
*                                                                               
         CLI   CSYSDACT,X'FF'      CABLE REC DEACTIVATED ALREADY?               
         BE    PINFA180                                                         
         MVI   CSYSDACT,X'FF'      NO, DEACTIVATE IT THEN                       
         XC    KEY,KEY                                                          
         MVC   KEY(L'CBLKEY),CBLKEY                                             
*                                                                               
         GOTO1 MYTRACE,DMCB,AREC,CBLLNQ,                               X        
               =CL22'DEACTIVATING CABLE REC',22                                 
*                                                                               
         BAS   RE,STAWRT                                                        
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         B     PINFA180            CHECK EXCEPTIONS RECORDS ALSO                
         DROP  R6                                                               
*                                                                               
PINFA118 TM    ACTNFLG1,CDNTACTV   DEACTIVATE CABLE HEADEND?                    
         BZ    PINFA120                                                         
         TM    ACTNFLG1,CDNEWSYS                                                
         BNZ   PINFA120                                                         
         BAS   RE,DACTVATE         YES                                          
         BNE   PINFA180                                                         
         MVC   P+10(19),=CL19'DEACTIVATING SYSTEM'                              
         GOTO1 REPORT                                                           
         B     PINFA180                                                         
*                                                                               
PINFA120 TM    ACTNFLG1,CDPAYADR   PAYABLE ADDRESS UPDATE?                      
         BZ    PINFA130                                                         
         BAS   RE,UPDADDRS         YES, UPDATE ADDRESS RECORD                   
         MVC   P+10(24),=CL24'CHANGING PAYABLE ADDRESS'                         
         GOTO1 REPORT                                                           
*                                  CHANGE TO STATION RECORD?                    
PINFA130 TM    ACTNFLG1,CDNETCHG+CDEXCLUS+CDSYSNAM+CDMSOINT+CDTPEADR            
         BNZ   PINFA133                                                         
         TM    ACTNFLG2,CDEIXSTA   ELECTRONIC INVOICING?                        
         BZ    PINFA136                                                         
PINFA133 BAS   RE,UPDSTATN         YES, UPDATE STATION RECORD                   
*                                                                               
         TM    ACTNFLG1,CDNETCHG   NETWORK CHANGE                               
         BZ    PINFA136                                                         
         MVC   P+10(15),=CL15'ADDING NETWORKS'                                  
         GOTO1 REPORT                                                           
*                                                                               
PINFA136 TM    ACTNFLG1,CDEXCLUS   EXCLUSITIVITY CHANGE                         
         BZ    PINFA140                                                         
         MVC   P+10(22),=CL22'CHANGING EXCLUSIVITY'                             
         GOTO1 REPORT                                                           
*                                                                               
PINFA140 TM    ACTNFLG1,CDSYSNAM   SYSTEM NAME CHANGE                           
         BZ    PINFA145                                                         
         MVC   P+10(20),=CL20'CHANGING SYSTEM NAME'                             
         GOTO1 REPORT                                                           
*                                                                               
PINFA145 TM    ACTNFLG2,CDEIXSTA   ELECTRONIC INVOICING CHANGE                  
         BZ    PINFA150                                                         
         MVC   P+10(29),=CL29'CHANGING ELECTRONIC INVOICING'                    
         GOTO1 REPORT                                                           
*                                                                               
PINFA150 TM    ACTNFLG1,CDTPEADR   ORDER DEADLINE CHANGE                        
         BZ    PINFA160                                                         
         MVC   P+10(24),=CL24'CHANGING ORDER DEADLINE'                          
         GOTO1 REPORT                                                           
*                                                                               
PINFA160 TM    ACTNFLG1,CDMSOINT   MSO AND INTERCONNECT NAME CHANGE?            
         BZ    PINFA170                                                         
         BAS   RE,UPDCABLE         YES, UPDATE CABLE DATA RECORD                
         MVC   P+10(35),=CL35'CHANGING MSO AND INTERCONNECT NAMES'              
         GOTO1 REPORT                                                           
*                                                                               
PINFA170 TM    ACTNFLG1,CDTPEADR   TAPE ADDRESS UPDATE?                         
         BNZ   *+12                                                             
         TM    ACTNFLG2,CDGRPCOD   GROUP CODE UPDATE?                           
         BZ    PINFA180                                                         
         BAS   RE,UPDTRAFF         YES, UPDATE TRAFFIC ADDRESS                  
*                                                                               
PINFA172 TM    ACTNFLG1,CDTPEADR   TAPE ADDRESS CHANGE                          
         BZ    PINFA173                                                         
         MVC   P+10(21),=CL21'CHANGING TAPE ADDRESS'                            
         MVC   P2+10(23),=CL23'CHANGING TAPE DEADLINE'                          
         GOTO1 REPORT                                                           
*                                                                               
PINFA173 TM    ACTNFLG2,CDGRPCOD   GROUP CODE CHANGE                            
         BZ    PINFA180                                                         
         MVC   P+10(19),=CL19'CHANGING GROUP CODE'                              
         GOTO1 REPORT                                                           
*                                                                               
PINFA180 BAS   RE,XCEPTION         CHECK THE EXCEPTION RECORDS                  
         B     PINFA400                                                         
*****                                                                           
* SYSTEM DID NOT EXIST BEFORE                                                   
*****                                                                           
PINFA200 TM    ACTNFLG1,CDNEWSYS   ADDING THE SYSTEM?                           
         BNZ   *+12                                                             
         BAS   RE,NOSYSTEM         NO, NO SUCH SYSTEM                           
         B     PINFA400                                                         
*                                                                               
         BAS   RE,NEWSYSTM         YES                                          
         MVC   P+10(17),=CL17'ADDING NEW SYSTEM'                                
         GOTO1 REPORT                                                           
*****                                                                           
* PROCESS THE NEXT AGENCY IN TABLE                                              
*****                                                                           
PINFA400 LA    R2,L'AGYENTRY(R2)   PROCESS NEXT AGENCY IN TABLE                 
         B     PINFA10                                                          
*****                                                                           
* DONE WITH THE SAVED CABLE HEADEND                                             
*****                                                                           
PINFA500 MVI   BITFLAG2,0                                                       
         XC    WKTOP24,WKTOP24                                                  
         XC    WKCBLSEQ,WKCBLSEQ                                                
         XC    SVDHDEND,SVDHDEND   SO WE WON'T WRITE OUT RECORD AGAIN           
         MVC   ACTHDEND,CBLHDEND                                                
         B     PINFXIT                                                          
         DROP  R2                                                               
*****                                                                           
* SYSTEM DATA                                                                   
*****                                                                           
PINFS00  BAS   RE,GETMKTNO         GET MKT NUM BASED ON ALPHA MKT               
         MVC   SYSHDEND,CBLHDEND                                                
         CLI   QOPT2,C'Y'                                                       
         BNE   PINFXIT                                                          
         MVC   SVDHDEND,CBLHDEND                                                
         LA    R2,AGYTABLE                                                      
         USING AGYENTRY,R2                                                      
*                                                                               
PINFS10  CLI   AGYENTRY,X'FF'      END OF AGENCY TABLE?                         
         BE    PINFSX              YES, NO MORE AGENCIES TO PROCESS             
         ST    R2,ADCURAGY         NO, SAVE A(CURRENT AGENCY ENTRY)             
         MVC   XCPTCLT,=CL3'000'                                                
*                                                                               
         OC    AGYMKTNO,AGYMKTNO   IF NO ALPHA MARKET CODE                      
         BNZ   *+12                                                             
         BAS   RE,BADMKTCD         THERE TELL USER                              
         B     PINFS30                                                          
         MVC   NUMMRKT,AGYMKTNO                                                 
*                                                                               
         XC    TOP24,TOP24          ***                                         
         XC    CBLSEQ,CBLSEQ        ***                                         
*                                                                               
         BAS   RE,CKSTATN                                                       
         BNE   PINFS20                                                          
         L     R6,AREC                                                          
         USING GENSTAD,R6                                                       
         MVC   PRVMRKT,SMKT                                                     
         MVC   TOP24,SCBL24        SAVE OFF WHAT WE HAVE IN THE RECORD          
         MVC   CBLSEQ,SCBLSEQ      ***                                          
         DROP  R6                                                               
*                                                                               
PINFS20  BAS   RE,UPDSTATN                                                      
*                                                                               
         MVC   P+10(18),=CL18'CHANGED GROUP CODE'                               
         MVC   P2+10(22),=CL22'CHANGED ORDER DEADLINE'                          
         GOTO1 REPORT                                                           
         BAS   RE,UPDTRAFF                                                      
         MVC   P2+10(21),=CL21'CHANGED TAPE DEADLINE'                           
         GOTO1 REPORT                                                           
         BAS   RE,XCEPTION                                                      
*                                                                               
PINFS30  LA    R2,L'AGYENTRY(R2)   PROCESS NEXT AGENCY IN TABLE                 
         B     PINFS10                                                          
*                                                                               
PINFSX   B     PINFXIT                                                          
         DROP  R2                                                               
*****                                                                           
* NETWORK DATA                                                                  
*****                                                                           
PINFN00  BAS   RE,SETNTWRK         SET APPROPRIATE NETWORK BIT OR ERROR         
*                                                                               
PINFXIT  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE HANDLES THE EXCEPTION RECORDS                                    
*                                                                               
* ON ENTRY:    SVDHDEND            CABLE HEADEND                                
***********************************************************************         
*                                                                               
XCEPTION NTR1                                                                   
         L     R2,ADCURAGY                                                      
         USING AGYENTRY,R2                                                      
*                                                                               
         L     R6,AREC                                                          
         XC    KEY,KEY             SET THE STATION RECORD KEY                   
         LA    R4,KEY                                                           
         USING STAKEY,R4                                                        
         MVI   STAKTYPE,C'S'                                                    
         MVI   STAKMED,C'T'                                                     
         MVC   STAKCALL,SVDHDEND                                                
         MVC   STAKAGY,AGYPOWCD                                                 
         DROP  R4                                                               
*                                                                               
         BAS   RE,STAHIGH                                                       
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING STAREC,R6                                                        
XCPT10   CLC   =CL3'000',STAKCLT   ANY EXCEPTION RECORDS?                       
         BE    XCPTX               NO, COULDN'T FIND ANY                        
         MVC   XCPTCLT,STAKCLT                                                  
*                                                                               
         TM    ACTNFLG1,CDNTACTV   DEACTIVATE THIS ALSO?                        
         BZ    XCPT15                                                           
         TM    ACTNFLG1,CDNEWSYS                                                
         BZ    XCPT100             YES                                          
*                                                                               
XCPT15   DS    0H                                                               
*                                                                               
         NI    BITFLAG2,X'FF'-B2ADDING   CHANGE CLIENT SPECIFIC RECORD          
         BRAS  RE,MKSTATN                                                       
*                                                                               
         TM    ACTNFLG1,CDNETCHG   NETWORK CHANGE                               
         BZ    XCPT20                                                           
         MVC   P+10(15),=CL15'ADDING NETWORKS'                                  
         GOTO1 REPORT                                                           
*                                                                               
XCPT20   TM    ACTNFLG1,CDEXCLUS   EXCLUSITIVITY CHANGE                         
         BZ    XCPT25                                                           
         MVC   P+10(20),=CL20'CHANGING EXCLUSIVITY'                             
         GOTO1 REPORT                                                           
*                                                                               
XCPT25   TM    ACTNFLG1,CDTPEADR   ORDER DEADLINE CHANGE                        
         BZ    XCPT30                                                           
         MVC   P+10(22),=CL23'CHANGING ORDER DEADLINE'                          
         GOTO1 REPORT                                                           
*                                                                               
XCPT30   TM    ACTNFLG1,CDSYSNAM   SYSTEM NAME CHANGE                           
         BZ    XCPT32                                                           
         MVC   P+10(20),=CL20'CHANGING SYSTEM NAME'                             
         GOTO1 REPORT                                                           
*                                                                               
XCPT32   TM    ACTNFLG2,CDEIXSTA   ELECTRONIC INVOICING CHANGE                  
         BZ    XCPT35                                                           
         MVC   P+10(29),=CL29'CHANGING ELECTRONIC INVOICING'                    
         GOTO1 REPORT                                                           
*                                                                               
XCPT35   TM    ACTNFLG2,CDGRPCOD   GROUP CODE CHANGE                            
         BZ    XCPT40                                                           
         MVC   P+10(19),=CL19'CHANGING GROUP CODE'                              
         GOTO1 REPORT                                                           
         DROP  R6                                                               
*                                                                               
XCPT40   TM    ACTNFLG1,CDMSOINT                                                
         BZ    XCPT50                                                           
*                                                                               
         BAS   RE,CKCBLREC                                                      
         BNE   XCPT50              CAN'T FIND CABLE RECORD? SKIP IT             
         NI    BITFLAG2,X'FF'-B2ADDING                                          
         GOTO1 =A(MKCBLREC),RR=RELO                                             
*                                                                               
         MVC   P+10(35),=CL35'CHANGING MSO AND INTERCONNECT NAMES'              
         GOTO1 REPORT                                                           
*                                                                               
XCPT50   B     XCPT200             CHECK IF ANY MORE EXCEPTION RECS             
*                                                                               
         USING STAREC,R6                                                        
XCPT100  MVI   SSYSDACT,X'FF'      DEACTIVATE THIS EXCEPTION'S                  
         XC    KEY,KEY                 STATION RECORD                           
         MVC   KEY(L'STAKEY),STAKEY                                             
*                                                                               
         GOTO1 MYTRACE,DMCB,AREC,SCBLSQNQ,                             X        
               =CL25'DEACTIVATING STATION REC',25                               
***      GOTO1 MYTRACE,DMCB,AREC,STANCLNQ,                                      
***            =CL25'DEACTIVATING STATION REC',25                               
*                                                                               
         BAS   RE,STAWRT                                                        
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R6                                                               
*                                                                               
         USING CBLREC,R6                                                        
         BAS   RE,CKCBLREC                                                      
         BNE   XCPT150                                                          
*                                                                               
         MVI   CSYSDACT,X'FF'      DEACTIVATE THIS EXCEPTION'S                  
         XC    KEY,KEY                 CABLE RECORD                             
         MVC   KEY(L'CBLKEY),CBLKEY                                             
*                                                                               
         GOTO1 MYTRACE,DMCB,AREC,CBLLNQ,                               X        
               =CL22'DEACTIVATING CABLE REC',22                                 
*                                                                               
         BAS   RE,STAWRT                                                        
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R6                                                               
*                                                                               
XCPT150  MVC   P+10(19),=CL19'DEACTIVATING SYSTEM'                              
         GOTO1 REPORT                                                           
*                                                                               
XCPT200  XC    KEY,KEY             SET THE STATION RECORD KEY                   
         LA    R4,KEY                                                           
         USING STAKEY,R4                                                        
         MVI   STAKTYPE,C'S'                                                    
         MVI   STAKMED,C'T'                                                     
         MVC   STAKCALL,SVDHDEND                                                
         MVC   STAKAGY,AGYPOWCD                                                 
         MVC   STAKCLT,XCPTCLT                                                  
         DROP  R4                                                               
*                                                                               
         BAS   RE,STAHIGH                                                       
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BAS   RE,STASEQ                                                        
         CLI   DMCB+8,0                                                         
         BE    XCPT10                                                           
         DC    H'0'                                                             
*                                                                               
XCPTX    B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE DEACTIVATES THE SYSTEM                                           
*                                                                               
* ON ENTRY:    SVDHDEND            CABLE HEADEND                                
***********************************************************************         
*                                                                               
DACTVATE NTR1                                                                   
*****                                                                           
* STATION MASTER RECORD                                                         
*****                                                                           
         BAS   RE,CKSTATN          SEE IF THIS STATION EXISTS                   
         BE    *+12                                                             
         BAS   RE,CANTDACT         CAN'T DEACTIVATE A SYSTEM THAT               
         B     DACTNO                  DOESN'T EXIST                            
*                                                                               
         L     R6,AREC                                                          
         USING GENSTAD,R6                                                       
         MVI   SSYSDACT,X'FF'      THIS IS THE DEACTIVATED INDICATOR            
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'STAKEY),STAKEY                                             
*                                                                               
         GOTO1 MYTRACE,DMCB,AREC,SCBLSQNQ,                             X        
               =CL25'DEACTIVATING STATION REC',25                               
***      GOTO1 MYTRACE,DMCB,AREC,STANCLNQ,                                      
***            =CL25'DEACTIVATING STATION REC',25                               
*                                                                               
         BAS   RE,STAWRT                                                        
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R6                                                               
*****                                                                           
* STATION CABLE RECORD                                                          
*****                                                                           
         BAS   RE,CKCBLREC         SEE IF CABLE RECORD EXISTS                   
         BE    *+12                                                             
         BAS   RE,CANTDACT         CAN'T DEACTIVATE A SYSTEM THAT               
         B     DACTNO                  DOESN'T EXIST                            
*                                                                               
         L     R6,AREC                                                          
         USING CBLREC,R6                                                        
         MVI   CSYSDACT,X'FF'      THIS IS THE DEACTIVATED INDICATOR            
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'CBLKEY),CBLKEY                                             
*                                                                               
         GOTO1 MYTRACE,DMCB,AREC,CBLLNQ,                               X        
               =CL22'DEACTIVATING CABLE REC',22                                 
*                                                                               
         BAS   RE,STAWRT                                                        
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R6                                                               
*                                                                               
DACTYES  B     YES                                                              
*                                                                               
DACTNO   B     NO                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE ADDS/CHANGES THE SYSCODE                                         
*                                                                               
* ON ENTRY:    SVDHDEND            CABLE HEADEND                                
***********************************************************************         
*                                                                               
NEWSYSTM NTR1                                                                   
         L     R2,ADCURAGY                                                      
         USING AGYENTRY,R2                                                      
*****                                                                           
* STATION MASTER RECORD                                                         
*****                                                                           
         CLC   SYSHDEND,SVDHDEND   ANY SYSTEM DATA?                             
         BE    *+12                                                             
         BAS   RE,SYSNTADD         NO, SYSTEM NOT ADDED, NO SYSTEM DATA         
         B     NSYSX                                                            
*                                                                               
         BAS   RE,CKSTATN          STATION EXISTS?                              
         BNE   NSYS10                                                           
         CLI   DMCB+8,X'02'        YES, RECORD IS DELETED?                      
         BE    NSYS15              WRITE IT OUT WITH NEW INFO                   
         BAS   RE,SYSNTNEW         SYSTEM IS NOT NEW                            
         B     NSYSX                                                            
*                                                                               
NSYS10   OI    BITFLAG2,B2ADDING   ADD RECORD                                   
         AP    COUNTER1,=P'1'                                                   
         B     *+8                                                              
NSYS15   NI    BITFLAG2,X'FF'-B2ADDING                                          
         XC    TOP24,TOP24                                                      
         XC    CBLSEQ,CBLSEQ                                                    
         BRAS  RE,MKSTATN                                                       
*                                                                               
         MVC   NUMMRKT,AGYMKTNO    FOR THIS MARKET NUMBER                       
*****                                                                           
* "K" PASSIVE RECORD                                                            
*****                                                                           
         BAS   RE,CKSTAKPS         K PASSIVE STATION EXISTS?                    
         BE    NSYS35              YES                                          
         OI    BITFLAG2,B2ADDK     NO, ADD RECORD                               
         AP    COUNTER6,=P'1'                                                   
         B     *+8                                                              
NSYS35   NI    BITFLAG2,X'FF'-B2ADDK                                            
         BAS   RE,MKSTAKPS                                                      
*                                                                               
         BAS   RE,UPDADDRS         ADD STATION ADDRESS RECORD                   
         BAS   RE,UPDCABLE         ADD CABLE DATA RECORD                        
         BAS   RE,UPDTRAFF         ADD TRAFFIC ADDRESS RECORD                   
*                                                                               
NSYSX    B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE UPDATES THE ADDRESS RECORD                                       
*                                                                               
* ON ENTRY:    SVDHDEND            CABLE HEADEND                                
***********************************************************************         
*                                                                               
UPDADDRS NTR1                                                                   
         BAS   RE,CKSTAADD         STATION ADDRESS EXISTS?                      
         BE    UADR10              YES                                          
         OI    BITFLAG2,B2ADDING   NO, ADD IT                                   
         AP    COUNTER3,=P'1'                                                   
         B     UADR30                                                           
*                                                                               
UADR10   TM    ACTNFLG2,CDREPURP   ARE WE REPURPOSING?                          
         BNZ   UADR20              YES, ALWAYS PROCESS CHANGE                   
*                                                                               
         L     R2,ADCURAGY                                                      
         USING AGYENTRY,R2                                                      
         LA    R1,IGNADAGY                                                      
*                                                                               
UADR15   CLI   0(R1),0             END OF TABLE?                                
         BE    UADR20              YES, PROCESS CHANGE                          
*                                                                               
         CLC   AGYPOWCD,0(R1)      IS IT ONE OF THE AGENCIES THAT WANT          
         BE    UADRX                 TO IGNORE ADDRESS CHANGES?                 
         LA    R1,L'IGNADAGY(R1)                                                
         B     UADR15                                                           
*                                                                               
UADR20   NI    BITFLAG2,X'FF'-B2ADDING                                          
*                                                                               
UADR30   BAS   RE,MKSTAADD                                                      
*                                                                               
UADRX    B     XIT                                                              
*                                                                               
IGNADAGY DS    0CL2                                                             
         DC    C'WI'               INITIATIVE                                   
         DC    C'G7'               GSD&M                                        
         DC    C'WD'               WIEDEN & KENNEDY                             
         DC    C'RP'               RUBIN POSTAER                                
         DC    C'OO'               OMNICOM                                      
         DC    X'00'                ***EOT***                                   
*                                                                               
***********************************************************************         
* THIS ROUTINE UPDATES THE STATION RECORD                                       
*                                                                               
* ON ENTRY:    SVDHDEND            CABLE HEADEND                                
***********************************************************************         
*                                                                               
UPDSTATN NTR1                                                                   
         L     R2,ADCURAGY                                                      
         USING AGYENTRY,R2                                                      
         MVC   NUMMRKT,AGYMKTNO                                                 
*                                                                               
         BAS   RE,CKSTATN          STATION EXISTS?                              
         BNE   USTA40              NO                                           
         NI    BITFLAG2,X'FF'-B2ADDING                                          
         L     R6,AREC                                                          
         USING GENSTAD,R6                                                       
*                                                                               
         XC    TOP24,TOP24                                                      
         XC    CBLSEQ,CBLSEQ                                                    
*                                                                               
         TM    ACTNFLG2,CDREPURP   ARE WE REPURPOSING THIS SYSCODE?             
         BZ    USTA10                                                           
         XC    REPRPMKT,REPRPMKT   OLD MARKET FOR DELETING K PASSIVE            
         CLC   SMKT,NUMMRKT                                                     
         BE    USTA50              IF SAME, NOTHING TO DO TO K PASSIVE          
         PACK  DUB,SMKT                                                         
         CVB   R1,DUB                                                           
         STCM  R1,3,REPRPMKT       OLD MARKET FOR DELETING K PASSIVE            
         B     USTA50              YES, NETWORKS ARE STARTING ANEW              
*                                                                               
USTA10   ZICM  R3,STAKLEN,2        MASTER REC HAS LATEST LENGTH?                
         LA    RE,SCBLSQNQ                                                      
         CR    R3,RE                                                            
         BNE   USTA20                                                           
         MVC   TOP24,SCBL24        YES, MAKE A COPY OF NETWORKS IN              
         MVC   CBLSEQ,SCBLSEQ        LOCAL WORKING STORAGE                      
*                                                                               
USTA20   CLC   SMKT,NUMMRKT                                                     
         BE    USTA50                                                           
         BAS   RE,BADMKTCH                                                      
         B     USTANO                                                           
         DROP  R6                                                               
*                                                                               
USTA40   OI    BITFLAG2,B2ADDING   ADD RECORD                                   
         AP    COUNTER1,=P'1'                                                   
USTA50   BRAS  RE,MKSTATN                                                       
*                                                                               
         TM    ACTNFLG2,CDREPURP   ARE WE REPURPOSING THIS SYSCODE?             
         BZ    USTAYES                                                          
         OC    REPRPMKT,REPRPMKT   OLD MARKET DIFF THAN NEW MARKET?             
         BZ    USTAYES             NO DIFFERENCE, K PASSIVE STAYS SAME          
*                                                                               
         BAS   RE,DLSTAKPS         DIFFERENT, DELETE OLD K PASSIVE AND          
*                                                                               
         PACK  DUB,NUMMRKT                                                      
         CVB   R1,DUB                                                           
         STCM  R1,3,BMKT                                                        
         BAS   RE,MKSTAKPS           ADD THE NEW K PASSIVE                      
*                                                                               
USTAYES  B     YES                                                              
*                                                                               
USTANO   B     NO                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE UPDATES THE CABLE DATA RECORD                                    
*                                                                               
* ON ENTRY:    SVDHDEND            CABLE HEADEND                                
***********************************************************************         
*                                                                               
UPDCABLE NTR1                                                                   
         BAS   RE,CKCBLREC         CABLE DATA RECORD EXISTS?                    
         BE    UCBL10              YES                                          
         OI    BITFLAG2,B2ADDING   NO, ADD RECORD                               
         AP    COUNTER4,=P'1'                                                   
         B     *+8                                                              
UCBL10   NI    BITFLAG2,X'FF'-B2ADDING                                          
******** BAS   RE,MKCBLREC                                                      
         GOTO1 =A(MKCBLREC),RR=RELO                                             
*                                                                               
UCBLX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE UPDATES THE TRAFFIC ADDRESS RECORD                               
*                                                                               
* ON ENTRY:    SVDHDEND            CABLE HEADEND                                
***********************************************************************         
*                                                                               
UPDTRAFF NTR1                                                                   
         L     R2,ADCURAGY                                                      
         USING AGYENTRY,R2                                                      
*                                                                               
         BAS   RE,CKTRAADD         TRAFFIC ADDRESS RECORD EXISTS?               
         BE    UTRA10                                                           
         AP    COUNTER5,=P'1'                                                   
******** BAS   RE,MKTRAADD         NO, ADD IT                                   
         GOTO1 =A(MKTRAADD),RR=RELO                                             
         B     UTRAX                                                            
*                                                                               
UTRA10   CLI   DMCB+8,X'02'        YES, RECORD IS DELETED?                      
         BNE   UTRA20              NO, JUST CHANGE THE RECORD                   
*                                                                               
         LA    R4,KEY              CHANGE THE TRAFFIC ADDRESS RECORD            
         USING STADDKEY,R4                                                      
         NI    STADDKEY+13,X'FF'-X'80'   YES, TAKE OFF DELETED BIT              
*                                                                               
         GOTO1 MYTRACE,DMCB,KEY,STAAGYA-STARECD,                       X        
               =CL30'UNDELETING TRAFFIC ADDRESS KEY',30                         
*                                                                               
         BAS   RE,TRWRT                                                         
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R4                                                               
*                                                                               
UTRA20   MVC   KEY(4),KEY+14                                                    
         BAS   RE,TRFGETR                                                       
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AREC                                                          
         USING STARECD,R6                                                       
         NI    STADDKEY+15,X'FF'-X'80'   TAKE OFF DELETED BIT IF ON             
         MVC   STAAGYA,AGYPOWCD                                                 
*                                                                               
         TM    ACTNFLG1,CDTPEADR    TAPE ADDRESS CHANGE?                        
         BNZ   UTRA30               YES                                         
         CLI   QOPT2,C'Y'           NO, BIG FIX UPDATE?                         
         BE    UTRA33                   YES                                     
         B     UTRA40                   NO, MUST BE FOR GROUP CODE              
*                                                                               
UTRA30   LA    R6,STADTAEL                                                      
UTRA31   CLI   0(R6),X'10'         ADDRESS DATA ELEMENT?                        
         BE    UTRA32              YES                                          
         ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         CLI   0(R6),0             END OF RECORD?                               
         BNE   UTRA31                                                           
         DC    H'0'                YES                                          
*                                                                               
         USING STADTAEL,R6                                                      
UTRA32   MVI   STADTAEL,X'10'      ADDRESS DATA ELEMENT                         
         MVI   STADTALN,STADTAX-STADTAEL                                        
         MVC   STALINE1,IN_TRANM                                                
         MVC   STALINE2,IN_TRAAD                                                
         MVC   STALINE3(L'IN_TRACT),IN_TRACT                                    
         OC    STALINE3,SPACES                                                  
         MVC   STALINE4(L'IN_TRAST),IN_TRAST                                    
         MVI   STALINE4+2,C' '                                                  
         MVC   STALINE4+3(5),IN_TRAZP                                           
         OC    STALINE4,SPACES                                                  
         MVC   STACMLT,IN_TPESZ                                                 
         DROP  R6                                                               
*                                                                               
UTRA33   L     R6,AREC                                                          
         USING STARECD,R6                                                       
         LA    R6,STADTAEL                                                      
UTRA34   CLI   0(R6),0             END OF RECORD?                               
         BE    UTRA38                                                           
         CLI   0(R6),X'20'         AMS DATA ELEMENT?                            
         BE    UTRA36              YES, JUST CHANGE THE DEADLINE INFO           
         BH    UTRA38                                                           
         ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     UTRA34                                                           
*                                                                               
         USING STAAMSEL,R6                                                      
UTRA36   MVC   STAAMSTD,IN_TPEDL                                                
         B     UTRA40                                                           
         DROP  R6                                                               
*                                                                               
UTRA38   LA    RE,ELEM             NEW AMS DATA ELEMENT                         
         XC    ELEM,ELEM                                                        
         USING STAAMSEL,RE                                                      
         MVI   STAAMSEL,X'20'                                                   
         MVI   STAMSALN,STAAMSX-STAAMSEL                                        
         MVC   STAAMSTD,IN_TPEDL                                                
         DROP  RE                                                               
         GOTO1 RECUP,DMCB,(C'S',AREC),ELEM,(R6)                                 
*                                                                               
UTRA40   TM    ACTNFLG2,CDGRPCOD   GROUP CODE CHANGE?                           
         BNZ   UTRA41                                                           
         CLI   QOPT2,C'Y'          BIG FIX UPDATE?                              
         BNE   UTRA60                                                           
*                                                                               
UTRA41   L     R6,AREC                                                          
         USING STARECD,R6                                                       
         LA    R6,STADTAEL                                                      
UTRA42   CLI   0(R6),0             END OF RECORD?                               
         BE    UTRA46                                                           
         CLI   0(R6),X'20'         AMS DATA ELEMENT?                            
         BE    UTRA44              YES, JUST CHANGE THE DEADLINE INFO           
         BH    UTRA46                                                           
         ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     UTRA42                                                           
*                                                                               
         USING STAAMSEL,R6                                                      
UTRA44   MVC   STAAMSGC,=CL15'AMS'                                              
         B     UTRA60                                                           
         DROP  R6                                                               
*                                                                               
UTRA46   LA    RE,ELEM             NEW AMS DATA ELEMENT                         
         XC    ELEM,ELEM                                                        
         USING STAAMSEL,RE                                                      
         MVI   STAAMSEL,X'20'                                                   
         MVI   STAMSALN,STAAMSX-STAAMSEL                                        
         MVC   STAAMSGC,=CL15'AMS'                                              
         DROP  RE                                                               
         GOTO1 RECUP,DMCB,(C'S',AREC),ELEM,(R6)                                 
*                                                                               
UTRA60   L     R6,AREC                                                          
         USING STARECD,R6                                                       
         LA    R6,STADTAEL                                                      
UTRA62   CLI   0(R6),0             END OF RECORD?                               
         BE    UTRA66                                                           
         CLI   0(R6),X'F1'         ACTIVITY ELEMENT?                            
         BE    UTRA64              YES, JUST CHANGE THE DATE                    
         BH    UTRA66                                                           
         ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     UTRA62                                                           
*                                                                               
         USING STAACTEL,R6                                                      
UTRA64   GOTO1 DATCON,DMCB,(5,0),(3,STACHGDT)                                   
         B     UTRA68                                                           
         DROP  R6                                                               
*                                                                               
UTRA66   LA    R3,ELEM             NEW AMS DATA ELEMENT                         
         XC    ELEM,ELEM                                                        
         USING STAACTEL,R3                                                      
         MVI   STAACTEL,X'F1'      NO, ADD IT                                   
         MVI   STAACTLN,20                                                      
         GOTO1 DATCON,DMCB,(5,0),(3,STAADDT)                                    
         MVC   STAADDID,AGYPOWCD                                                
         GOTO1 DATCON,DMCB,(5,0),(3,STACHGDT)                                   
         DROP  R3                                                               
         GOTO1 RECUP,DMCB,(C'S',AREC),ELEM,(R6)                                 
*                                                                               
UTRA68   L     R6,AREC                                                          
         USING STARECD,R6                                                       
*****    MVC   STARECD+13(2),=Y(STARECX-STARECD)  <=== THIS IS NOT GOOD         
         DROP  R6                                                               
*                                                                               
         MVC   DATADISP,=Y(STADTAEL-STADDKEY)                                   
         GOTO1 MYTRACE,DMCB,AREC,0,                                    X        
               =CL32'CHANGING TRAFFIC ADDRESS REC',32                           
*                                                                               
         BAS   RE,TRFPUTR                                                       
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
UTRAX    B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE GETS THE MARKET NUMBER BASED ON THE ALPHA MARKET FOUND           
* IN THE LINE FROM THE FILE.                                                    
*                                                                               
* ON ENTRY:    IN_AMRKT            ALPHA MARKET CODE                            
*                                                                               
* ON EXIT:     AGYMKTNO            FOR EACH AGENCY IS FILLED WITH ITS           
*                                    NUMERIC EQUIVALENT                         
***********************************************************************         
*                                                                               
GETMKTNO NTR1                                                                   
         LA    R2,AGYTABLE                                                      
         USING AGYENTRY,R2                                                      
*                                                                               
GMKT10   CLI   AGYENTRY,X'FF'      END OF AGENCY TABLE?                         
         BE    GMKTX               YES, DONE HERE                               
*                                                                               
         XC    AGYMKTNO,AGYMKTNO   CLEAR ANY PREVIOUS MARKET NUMBER             
*                                                                               
         L     R6,AREC                                                          
         XC    KEY,KEY             FIND PASSIVE KEY FOR ALPHA MARKET            
         LA    R4,KEY                                                           
         USING ANMKEYD,R4                                                       
         MVI   ANMKTYPE,ANMKTYPQ                                                
         MVC   ANMKAGCY,AGYPOWCD                                                
         MVI   ANMKMED,C'T'                                                     
         MVC   ANMKAMRK,IN_AMRKT                                                
         DROP  R4                                                               
*                                                                               
         BAS   RE,STAHIGH                                                       
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   KEY(ANMKNMRK-ANMKEYD),0(R6)                                      
         BNE   GMKT20              COULDN'T FIND IT                             
*                                                                               
         USING ANMKEYD,R6                                                       
         MVC   AGYMKTNO,ANMKNMRK   GOT IT                                       
         DROP  R6                                                               
*                                                                               
GMKT20   LA    R2,L'AGYENTRY(R2)                                                
         B     GMKT10                                                           
*                                                                               
GMKTX    B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE SETS THE NETWORK IN OUR WORKING COPY                             
* WKTOP24 AND WKCBLSEQ WILL BE USED FOR ALL AGENCIES ON THE SPOT FILE           
*    THEY ARE TO BE INCORPORATED TO EACH AGENCY'S SYSCODE                       
*                                                                               
* ON ENTRY:    IN_NWKCD            3 BYTE NETWORK CODE                          
*                                                                               
* ON EXIT:     WKTOP24             EITHER WILL BE MODIFIED FOR NETWORK          
*              WKCBLSEQ                                                         
***********************************************************************         
*                                                                               
SETNTWRK NTR1                                                                   
         L     RF,ACABLTAB         RF = A(SPCBLLIST)                            
         L     R4,=X'01000000'     SET INITIAL VALUE FOR TOP24 BINARY           
*                                                                               
         DO WHILE=(CLI,0(RF),NE,X'FF')                                          
           IF (TM,6(RF),X'40',NZ)  DID WE COME UPON A TOP 24 NETWORK?           
             SRL  R4,1             YES, ADJUST TOP24 BINARY                     
           ENDIF ,                                                              
**                                                                              
           IF  (CLC,IN_NWKCD,NE,7(RF)) MATCH ON THE 4 CHAR NCC NETWORK?         
             AH  RF,CBLTABLN           NO, NEXT ENTRY IN SPCBLLIST              
           ELSE ,                                                               
             MVC  HALF,3(RF)           YES, SAVE OFF ITS BINARY CODE            
             IF  (TM,6(RF),X'40',NZ)   MATCHED NETWORK IS A TOP-24?             
               ST    R4,FULL           YES, SAVE CURRENT BIT                    
               OC    WKTOP24,FULL+1      'OR' INTO PREVIOUS                     
               J     STNETYES                                                   
             ELSE ,                    NO, NETWORK IS NOT A TOP-24              
               LA  RE,WKCBLSEQ         POINT TO CBLSEQ WE WANT TO SAVE          
               LA  R1,WKCBLSEQ+L'WKCBLSEQ  BOUNDARY CHECK                       
               DO WHILE=(CR,RE,LT,R1)                                           
                 IF (CLC,0(2,RE),EQ,=2X'00')  IF EMPTY SLOT                     
                   MVC 0(2,RE),HALF           STORE BINARY NETWORK              
                   J   STNETYES               AND WE'RE DONE                    
                 ENDIF ,                                                        
                 LA  RE,2(RE)                                                   
               ENDDO ,                                                          
*                                                                               
               BRAS  RE,NMRNTWKS       SHOW ERROR FOR TOO MANY NETWORKS         
               J     STNETNO                                                    
             ENDIF ,  (TM,6(RF),X'40',NZ)                                       
           ENDIF ,  (CLC,IN_NWKCD,NE,7(RF))                                     
         ENDDO ,  DO WHILE=(CLI,0(RF),NE,X'FF')                                 
         BRAS  RE,BADNTWRK          YES, SEND ERROR INVALID NETWORK             
*                                                                               
STNETNO  B     NO                                                               
*                                                                               
STNETYES B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE SHOW THE CABLE STATION AND NETWORK                               
***********************************************************************         
*                                                                               
SHWCBLST NTR1                                                                   
         XC    P,P                                                              
         GOTO1 REPORT                                                           
*                                                                               
         L     R2,ADCURAGY                                                      
         USING AGYENTRY,R2                                                      
         MVC   P+15(15),=CL15'CABLE STATION: '                                  
         MVC   P+30(5),SVDHDEND                                                 
         MVC   P+40(35),IN_SYSNM                                                
*                                                                               
         CLC   =CL3'000',XCPTCLT                                                
         BE    SHWC03                                                           
         MVC   P(10),=CL10'EXCEPTION:'                                          
         MVC   P+80(8),=CL8'CLIENT: '                                           
         MVC   P+88(L'XCPTCLT),XCPTCLT                                          
         B     SHWC06                                                           
*                                                                               
SHWC03   MVC   P(8),=CL8'AGENCY: '                                              
         MVC   P+8(L'AGYPOWCD),AGYPOWCD                                         
         MVC   P+80(8),=CL8'MARKET: '                                           
         MVC   P+88(4),AGYMKTNO                                                 
*                                                                               
SHWC06   GOTO1 REPORT                                                           
         DROP  R2                                                               
*                                                                               
         LA    R5,MYCABTAB                                                      
         XCEF  (R5),384                                                         
         L     R6,AREC                                                          
         USING STAREC,R6                                                        
         OC    SCBL24,SCBL24       ANY TOP 24 TO PRINT?                         
         BNZ   SHWC20              YES                                          
         OC    SCBLSEQ,SCBLSEQ     ANY CABLE SEQ TO PRINT?                      
         BNZ   SHWC60              YES                                          
         B     SHWCX               NO CABLE NETWORKS TO PRINT AT ALL            
*                                                                               
SHWC20   MVC   P(10),=CL10'NETWORKS: '                                          
         MVC   SVTOP24,SCBL24                                                   
         LA    R1,P+10                                                          
         SR    R2,R2               NUMBER OF CABLE NETWORK PER LINE             
         LA    R3,SVTOP24          USING SAVED VERSION OF TOP 24                
         L     R4,ACABLTAB                                                      
*                                                                               
SHWC30   TM    6(R4),X'40'         TOP 24 CABLE NETWORK?                        
         BZ    SHWC50              NO, GET NEXT ITEM IN TABLE                   
*                                                                               
         TM    0(R3),X'80'         CURRENT TOP 24 BIT IS ON?                    
         BZ    SHWC40              NO                                           
*                                                                               
         MVC   0(3,R5),0(R4)                                                    
         LA    R5,3(R5)                                                         
*                                                                               
SHWC40   ZICM  RE,SVTOP24,3                                                     
         SLL   RE,1                1 BIT LESS TO TEST                           
         STCM  RE,7,SVTOP24                                                     
         LA    R3,SVTOP24                                                       
*                                                                               
SHWC50   AH    R4,CBLTABLN                                                      
         OC    SVTOP24,SVTOP24     STILL MORE TOP 24 BITS ARE ON?               
         BNZ   SHWC30              YES, TEST SOME MORE                          
* NON TOP 24                                                                    
         OC    SCBLSEQ,SCBLSEQ     ANY CABLE SEQ TO PRINT?                      
         BZ    SHWC150                                                          
*                                                                               
SHWC60   LA    R3,SCBLSEQ                                                       
         LA    RE,SCBLSEQ+L'SCBLSEQ                                             
         L     R4,ACABLTAB                                                      
*                                                                               
         OC    SCBL24,SCBL24       IF WE ALREADY DID TOP24                      
         BNZ   SHWC70              THEN LABEL WAS ALREADY DONE                  
         MVC   P(10),=CL10'NETWORKS: '                                          
         LA    R1,P+10                                                          
         SR    R2,R2               NUMBER OF CABLE NETWORK PER LINE             
*                                                                               
SHWC70   TM    6(R4),X'40'         TOP 24 CABLE NETWORK?                        
         BNZ   SHWC100             YES, GET NEXT ITEM IN CABLE TABLE            
*                                                                               
SHWC80   DS    0H                                                               
         CR    R3,RE               NO MORE ROOM IN MYCABTAB                     
         BNL   SHWC100             THEN DON'T SHOW ANYMORE                      
         CLC   0(2,R3),=XL2'0'                                                  
         BE    SHWC100                                                          
         CLC   0(2,R3),3(R4)                                                    
         BNE   SHWC90                                                           
*                                                                               
SHWC85   MVC   0(3,R5),0(R4)       PUT NETWORK CODE INTO MYCABTAB               
         LA    R5,3(R5)            SETUP FOR NEXT ENTRY                         
         B     SHWC100             GOT THIS NETWORK                             
*                                                                               
SHWC90   LA    R3,2(R3)            NEXT ITEM IN CABLE SEQ                       
         B     SHWC80                                                           
*                                                                               
SHWC100  AH    R4,CBLTABLN                                                      
         CLI   0(R4),X'FF'                                                      
         BE    SHWC150                                                          
         LA    R3,SCBLSEQ          REPOINT TO CABLE SEQ                         
         LA    RE,SCBLSEQ+L'SCBLSEQ                                             
         B     SHWC70                                                           
         DROP  R6                                                               
*  COUNT NUMBER OF NETWORKS AND SORT THEM                                       
SHWC150  DS    0H                                                               
         SR    R4,R4                                                            
         LA    R5,MYCABTAB                                                      
SHWC155  CLC   0(3,R5),=X'000000'                                               
         BE    SHWC160                                                          
         LA    R4,1(R4)                                                         
         LA    R5,3(R5)                                                         
         B     SHWC155                                                          
*==============  SORT THE NETWORKS ===========================*                 
SHWC160  DS    0H                                                               
         GOTO1 VXSORT,DMCB,(0,MYCABTAB),(R4),3,3,0                              
         LA    R5,MYCABTAB                                                      
         LA    R1,P+10                                                          
         SR    R2,R2                                                            
*                                                                               
*==============  NOW PRINT THE NETWORKS AFTER SORT ============*                
SHWC170  CLC   0(3,R5),=X'000000'                                               
         BE    SHWC200                                                          
         LA    RE,20                   COMPARE IF PRINT LINE HAS 20             
*                                      NETWORKS, IF SO PRINT AND GOTO           
         CR    R2,RE                   NEXT PRINT LINE                          
         BNE   SHWC180                                                          
         GOTO1 REPORT                                                           
         LA    R1,P+10                                                          
         SR    R2,R2                                                            
*                                                                               
SHWC180  MVC   0(3,R1),0(R5)                                                    
         CLI   2(R5),C' '                                                       
         BE    SHWC185                                                          
         MVI   3(R1),C' '                                                       
         LA    R1,4(R1)                                                         
         B     SHWC190                                                          
SHWC185  LA    R1,3(R1)                                                         
SHWC190  LA    R2,1(R2)                                                         
         LA    R5,3(R5)                                                         
         B     SHWC170                                                          
*                                                                               
SHWC200  GOTO1 REPORT                                                           
*                                                                               
SHWCX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ERRORS FOR   CBLHDEND                                                         
***********************************************************************         
*                                                                               
NMRNTWKS DS    0H                  NO MORE NON-TOP24 103 NETWORK SLOTS          
         MVC   P+25(39),=CL39'HAS TOO MANY NETWORKS. CODE NOT ADDED: '          
         MVC   P+64(L'IN_NWKCD),IN_NWKCD                                        
         B     BADMSGC                                                          
*                                  NO MORE NON-TOP24 103 NETWORK SLOTS          
BADNTWRK DS    0H                                                               
         MVC   P+25(29),=CL29'HAS AN INVALID NETWORK CODE: '                    
         MVC   P+54(L'IN_NWKCD),IN_NWKCD                                        
         B     BADMSGC                                                          
*                                                                               
BADMSGC  NTR1                                                                   
         MVI   P,C'*'              STARS FOR LISA                               
         MVC   P+1(8),P                                                         
         MVC   P+10(9),=C'STATION: '                                            
         MVC   P+19(L'CBLHDEND),CBLHDEND                                        
         MVI   P+90,C'*'                                                        
         MVC   P+91(10),P+90                                                    
BADPRNT  GOTO1 REPORT                                                           
         B     XIT                                                              
***********************************************************************         
* TOO MANY NETWORKS IN THE SYSCODE RECORD                                       
*                                                                               
* ON ENTRY:    R4                  A(2 BYTE NETWORK CODE)                       
***********************************************************************         
TMNYNWKS NTR1                                                                   
         MVI   P,C'*'              STARS FOR LISA                               
         MVC   P+1(8),P                                                         
         MVC   P+10(9),=C'STATION: '                                            
         MVC   P+19(L'STAKCALL),STAKCALL-STAKEY(R6)  SYSCODE IN RECORD          
         MVI   P+90,C'*'                                                        
         MVC   P+91(10),P+90                                                    
*                                                                               
         MVC   P+25(15),=CL15'FOR AGENCY: XX '                                  
         MVC   P+37(L'STAKAGY),STAKAGY-STAKEY(R6)                               
         MVC   P+40(39),=CL39'HAS TOO MANY NETWORKS. CODE NOT ADDED: '          
         L     RF,ACABLTAB    NOW SHOW THE NETWORK WE COULD NOT ADD             
         LH    RE,CBLTABLN    L(SPCBLLIST ENTRY)                                
         XR    R1,R1                                                            
         ICM   R1,3,0(R4)     R4=A(2 BYTE NETWORK CODE)                         
         SHI   R1,1           SPCBLLIST STARTS WITH NETWORK X'0001'             
         MR    R0,RE                                                            
         AR    RF,R1                                                            
         MVC   P+79(L'IN_NWKCD),7(RF)                                           
         B     BADPRNT                                                          
*                                                                               
***********************************************************************         
* ERRORS FOR   SVDHDEND                                                         
***********************************************************************         
*                                                                               
CANTDACT DS    0H                                                               
         MVC   P+30(36),=CL36'DOES NOT EXIST.  CAN NOT DEACTIVATE.'             
         B     BADMSGS                                                          
*                                                                               
SYSNTNEW DS    0H                                                               
         MVC   P+30(20),=CL20'IS NOT A NEW SYSTEM.'                             
         B     BADMSGS                                                          
*                                                                               
SYSNTADD DS    0H                                                               
         MVC   P+30(20),=CL20'HAS NO SYSTEM DATA.'                              
         B     BADMSGS                                                          
*                                                                               
NOSYSTEM DS    0H                                                               
         MVC   P+30(14),=CL14'DOES NOT EXIST'                                   
         B     BADMSGS                                                          
*                                                                               
DACTIVTD DS    0H                                                               
         MVC   P+30(40),=CL40'HAS BEEN DEACTIVATED, NO ACTIONS ALLOWED'         
         B     BADMSGS                                                          
*                                                                               
NOREPURP DS    0H                                                               
         MVC   P+30(40),=CL40'CAN''T REPURPOSE AS SYSCODE IS ACTIVE!'           
         B     BADMSGS                                                          
*                                                                               
BADMKTCD DS    0H                                                               
         MVC   P+30(34),=CL34'HAS AN INVALID ALPHA MARKET CODE: '               
         MVC   P+64(L'IN_AMRKT),IN_AMRKT                                        
         B     BADMSGS                                                          
*                                                                               
BADMKTCH DS    0H                                                               
         MVC   P+30(26),=CL26'TRYING TO CHANGE MARKETS: '                       
         MVC   P+26(L'IN_AMRKT),IN_AMRKT                                        
         B     BADMSGS                                                          
*                                                                               
BADMSGS  NTR1                                                                   
         L     R2,ADCURAGY                                                      
         USING AGYENTRY,R2                                                      
         MVC   P(8),=CL8'AGENCY: '                                              
         MVC   P+8(L'AGYPOWCD),AGYPOWCD                                         
         MVC   P+15(9),=C'STATION: '                                            
         MVC   P+24(L'SVDHDEND),SVDHDEND                                        
         MVI   P+90,C'*'                                                        
         MVC   P+91(10),P+90                                                    
         GOTO1 REPORT                                                           
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE CHECKS TO SEE IF THE STATION RECORD FOR THIS CABLE               
* HEADEND EXISTS.                                                               
*                                                                               
* ON ENTRY:    SVDHDEND            CABLE HEADEND                                
*              ADCURAGY            A(CURRENT AGENCY ENTRY)                      
*                                                                               
* ON EXIT:     CONDITION CODE      EQ OR NEQ                                    
***********************************************************************         
*                                                                               
CKSTATN  NTR1                                                                   
         L     R2,ADCURAGY                                                      
         USING AGYENTRY,R2                                                      
*                                                                               
         L     R6,AREC                                                          
         XC    KEY,KEY             SET THE STATION RECORD KEY                   
         LA    R4,KEY                                                           
         USING STAKEY,R4                                                        
         MVI   STAKTYPE,C'S'                                                    
         MVI   STAKMED,C'T'                                                     
         MVC   STAKCALL,SVDHDEND                                                
         MVC   STAKAGY,AGYPOWCD                                                 
         MVC   STAKCLT,XCPTCLT                                                  
         MVC   STAKFILL,=CL3'000'                                               
         DROP  R4                                                               
*                                                                               
         BAS   RE,STAHIGHD                                                      
         CLI   DMCB+8,X'02'        RECORD IS DELETED?                           
         BE    *+14                YES, PASS IT BACK SO WE CAN RECREATE         
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   KEY(STAKEYLN),0(R6)                                              
         BNE   CSTANO              COULDN'T FIND IT                             
*                                                                               
CSTAYES  B     YES                                                              
*                                                                               
CSTANO   B     NO                                                               
         DROP  R2                                                               
         EJECT                                                                  
*&&DO                                                                           
***********************************************************************         
* THIS ROUTINE CHECKS TO SEE IF THE STATION RECORD PASSIVE KEY FOR THE          
* GIVEN CABLE HEADEND EXISTS.                                                   
*                                                                               
* ON ENTRY:    SVDHDEND            CABLE HEADEND                                
*              NUMMRKT             EBCDIC MARKET CODE FOR THIS STATION          
*                                                                               
* ON EXIT:     CONDITION CODE      EQ OR NEQ                                    
***********************************************************************         
*                                                                               
CKSTAPSV NTR1                                                                   
         L     R2,ADCURAGY                                                      
         USING AGYENTRY,R2                                                      
*                                                                               
         MVC   AREC,AREC2          USE SECOND AREA TO READ IN PASSIVE           
         L     R6,AREC2                                                         
         XC    KEY,KEY             SET THE STATION RECORD KEY                   
         LA    R4,KEY                                                           
         USING STAKEY,R4                                                        
         MVI   STNKTYPE,C'N'                                                    
         MVC   STNKAGY,AGYPOWCD                                                 
         MVI   STNKMED,C'T'                                                     
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(L'SVDHDEND),SVDHDEND                                        
         GOTO1 MSPACK,DMCB,NUMMRKT,WORK,BMKTSTA                                 
         MVC   STNKMS,BMKTSTA                                                   
*                                                                               
         MVI   STNKCLT,C'0'                                                     
         MVC   STNKCLT+1(STNKLEN-(STNKCLT+1)),STNKCLT                           
         DROP  R4                                                               
*                                                                               
         BAS   RE,STAHIGHD                                                      
         CLI   DMCB+8,X'02'        RECORD IS DELETED?                           
         BE    *+14                YES, PASS IT BACK SO WE CAN RECREATE         
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   KEY(STAKEYLN),0(R6)                                              
         BNE   CKPSVNO                                                          
*                                                                               
CKPSVYES B     YES                                                              
*                                                                               
CKPSVNO  B     NO                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE CREATES A NEW STATION RECORD FOR THE GIVEN CABLE HEADEND         
*                                                                               
* ON ENTRY:    SVDHDEND            CABLE HEADEND                                
*              NUMMRKT             EBCDIC MARKET CODE FOR THIS STATION          
*              BITFLAG2  (X'80')   ON=ADD RECORD, OFF=WRITE RECORD              
***********************************************************************         
*                                                                               
MKSTAPSV NTR1                                                                   
         L     R2,ADCURAGY                                                      
         USING AGYENTRY,R2                                                      
*                                                                               
         L     R6,AREC             SET THE STATION RECORD                       
         USING STAREC,R6                                                        
*                                                                               
         TM    BITFLAG2,B2ADDING                                                
         BZ    *+10                                                             
         XC    0(20,R6),0(R6)      CLEAR THE RECORD FIRST IF ADDING             
*                                                                               
         MVI   STNKTYPE,C'N'                                                    
         MVC   STNKAGY,AGYPOWCD                                                 
         MVI   STNKMED,C'T'                                                     
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(L'SVDHDEND),SVDHDEND                                        
         GOTO1 MSPACK,DMCB,NUMMRKT,WORK,BMKTSTA                                 
         MVC   STNKMS,BMKTSTA                                                   
*                                                                               
         MVI   STNKCLT,C'0'                                                     
         MVC   STNKCLT+1(STNKLEN-(STNKCLT+1)),STNKCLT                           
         MVC   STNKLEN,=H'20'                                                   
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(STAKEYLN),STAKEY                                             
*                                                                               
         LA    R1,=CL31'ADDING PASSIVE STATION REC'                             
         ST    R1,DMCB+8                                                        
         LA    R1,31                                                            
         ST    R1,DMCB+12                                                       
         TM    BITFLAG2,B2ADDING   ADDING THE RECORD?                           
         BNZ   MKPSV10                                                          
         LA    R1,=CL33'CHANGING PASSIVE STATION REC'                           
         ST    R1,DMCB+8                                                        
         LA    R1,33                                                            
         ST    R1,DMCB+12                                                       
*                                                                               
MKPSV10  GOTO1 MYTRACE,DMCB,AREC,20,,,0                                         
*                                                                               
         TM    BITFLAG2,B2ADDING   ADDING THE RECORD?                           
         BZ    *+12                                                             
         BAS   RE,STAADD           YES                                          
         B     *+8                                                              
         BAS   RE,STAWRT           NO, WRITE THE RECORD                         
*                                                                               
         CLI   DMCB+8,0                                                         
         BE    MKPSVX                                                           
         DC    H'0'                                                             
*                                                                               
MKPSVX   B     XIT                                                              
         DROP  R2,R6                                                            
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE CHECKS TO SEE IF THE STATION RECORD C'K' PASSIVE KEY FOR         
* THE GIVEN CABLE HEADEND EXISTS.                                               
*                                                                               
* ON ENTRY:    SVDHDEND            CABLE HEADEND                                
*              NUMMRKT             EBCDIC MARKET CODE FOR THIS STATION          
*                                                                               
* ON EXIT:     CONDITION CODE      EQ OR NEQ                                    
***********************************************************************         
*                                                                               
CKSTAKPS NTR1                                                                   
         L     R2,ADCURAGY                                                      
         USING AGYENTRY,R2                                                      
*                                                                               
         MVC   AREC,AREC2          USE SECOND AREA TO READ IN PASSIVE           
         L     R6,AREC2                                                         
         XC    KEY,KEY             SET THE STATION RECORD KEY                   
         LA    R4,KEY                                                           
         USING STAKEY,R4                                                        
         MVI   STKKTYPE,C'K'       RECORD TYPE                                  
         MVC   STKKAGY,AGYPOWCD    AGENCY POWER CODE                            
         MVI   STKKMED,C'T'        CABLE UPLOAD IS ALWAYS TV                    
         PACK  DUB,NUMMRKT                                                      
         CVB   R1,DUB                                                           
         STCM  R1,3,BMKT                                                        
         MVC   STKKMKT,BMKT                                                     
         MVC   STKKSTA,SVDHDEND                                                 
         DROP  R4                                                               
*                                                                               
         BAS   RE,STAHIGHD                                                      
         CLI   DMCB+8,X'02'        RECORD IS DELETED?                           
         BE    *+14                YES, PASS IT BACK SO WE CAN RECREATE         
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   KEY(STAKEYLN),0(R6)                                              
         BNE   CKKPSNO                                                          
*                                                                               
CKKPSYES B     YES                                                              
*                                                                               
CKKPSNO  B     NO                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE DELETES C'K' PASSIVE KEY FOR THE GIVEN CABLE HEADEND AND         
* THE PREVIOUS MARKET NUMBER                                                    
*                                                                               
* ON ENTRY:    SVDHDEND            CABLE HEADEND                                
*              NUMMRKT             EBCDIC MARKET CODE FOR THIS STATION          
***********************************************************************         
*                                                                               
DLSTAKPS NTR1                                                                   
         L     R2,ADCURAGY                                                      
         USING AGYENTRY,R2                                                      
*                                                                               
         MVC   AREC,AREC2          USE SECOND AREA TO READ IN PASSIVE           
         L     R6,AREC2                                                         
         XC    KEY,KEY             SET THE STATION RECORD KEY                   
         LA    R4,KEY                                                           
         USING STAKEY,R4                                                        
         MVI   STKKTYPE,C'K'       RECORD TYPE                                  
         MVC   STKKAGY,AGYPOWCD    AGENCY POWER CODE                            
         MVI   STKKMED,C'T'        CABLE UPLOAD IS ALWAYS TV                    
         MVC   STKKMKT,REPRPMKT                                                 
         MVC   STKKSTA,SVDHDEND                                                 
         DROP  R4                                                               
*                                                                               
         OI    DMINBTS,X'80'       READ FOR UPDATE                              
         BAS   RE,STAHIGHD                                                      
         CLI   DMCB+8,X'02'        RECORD IS DELETED?                           
         BE    DLKPSYES            YES, THEN WE'RE OKAY                         
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   KEY(STAKEYLN),0(R6)                                              
         BE    DLKPS10                                                          
         MVC   P(28),=C'** NO ** OLD K PASSIVE FOR: '                           
         MVC   P+28(4),SVDHDEND                                                 
         MVC   P+36(8),=C'OLD MKT='                                             
         EDIT  (B2,REPRPMKT),(4,P+44),FILL=0                                    
         GOTO1 REPORT                                                           
         B     DLKPSNO                                                          
*                                                                               
DLKPS10  OI    SCNTL-STAKEY(R6),X'80'  MARK AS DELETED                          
         BAS   RE,STAWRT                                                        
*                                                                               
         MVC   P(27),=C'DELETED OLD K PASSIVE FOR: '                            
         MVC   P+27(4),SVDHDEND                                                 
         MVC   P+35(8),=C'OLD MKT='                                             
         EDIT  (B2,REPRPMKT),(4,P+43),FILL=0                                    
         GOTO1 REPORT                                                           
*                                                                               
DLKPSYES B     YES                                                              
*                                                                               
DLKPSNO  B     NO                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE CREATES A NEW STATION RECORD FOR THE GIVEN CABLE HEADEND         
*                                                                               
* ON ENTRY:    SVDHDEND            CABLE HEADEND                                
*              NUMMRKT             EBCDIC MARKET CODE FOR THIS STATION          
*              BITFLAG2  (X'40')   ON=ADD RECORD, OFF=WRITE RECORD              
***********************************************************************         
*                                                                               
MKSTAKPS NTR1                                                                   
         L     R2,ADCURAGY                                                      
         USING AGYENTRY,R2                                                      
*                                                                               
         L     R6,AREC             SET THE STATION RECORD                       
         USING STAREC,R6                                                        
*                                                                               
         TM    ACTNFLG2,CDREPURP   REPURPOSING THIS SYSCODE?                    
         BNZ   *+12                                                             
         TM    BITFLAG2,B2ADDK                                                  
         BZ    *+10                                                             
         XC    0(20,R6),0(R6)      CLEAR THE RECORD FIRST IF ADDING             
*                                                                               
         MVI   STKKTYPE,C'K'                                                    
         MVC   STKKAGY,AGYPOWCD                                                 
         MVI   STKKMED,C'T'                                                     
         MVC   STKKMKT,BMKT                                                     
         MVC   STKKSTA,SVDHDEND                                                 
*                                                                               
         MVC   STKKLEN,=Y(STKKLNQ)   RECORD LENGTH                              
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(STAKEYLN),STAKEY                                             
*                                                                               
         LA    R1,=CL31'ADDING "K" PASSIVE STATION'                             
         ST    R1,DMCB+8                                                        
         LA    R1,31                                                            
         ST    R1,DMCB+12                                                       
         TM    BITFLAG2,B2ADDK     ADDING THE RECORD?                           
         BNZ   MKKPS10                                                          
         TM    ACTNFLG2,CDREPURP   OR   REPURPOSING THIS SYSCODE?               
         BNZ   MKKPS10                                                          
         LA    R1,=CL33'CHANGING "K" PASSIVE STATION'                           
         ST    R1,DMCB+8                                                        
         LA    R1,33                                                            
         ST    R1,DMCB+12                                                       
*                                                                               
MKKPS10  GOTO1 MYTRACE,DMCB,AREC,20,,,0                                         
*                                                                               
         TM    BITFLAG2,B2ADDK     ADDING THE RECORD?                           
         BNZ   *+12                                                             
         TM    ACTNFLG2,CDREPURP   OR   REPURPOSING THIS SYSCODE?               
         BZ    MKKPS20                                                          
         BAS   RE,STAADD           YES                                          
         B     *+8                                                              
*                                                                               
MKKPS20  BAS   RE,STAWRT           NO, WRITE THE RECORD                         
*                                                                               
         CLI   DMCB+8,0                                                         
         BE    MKKPSX                                                           
         DC    H'0'                                                             
*                                                                               
MKKPSX   B     XIT                                                              
         DROP  R2,R6                                                            
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE CHECKS TO SEE IF THE STATION ADDRESS RECORD FOR THIS             
* CABLE EXISTS ALREADY.                                                         
*                                                                               
* ON ENTRY:    SVDHDEND            CABLE HEADEND                                
*                                                                               
* ON EXIT:     CONDITION CODE      EQ OR NEQ                                    
***********************************************************************         
*                                                                               
CKSTAADD NTR1                                                                   
         L     R2,ADCURAGY                                                      
         USING AGYENTRY,R2                                                      
*                                                                               
         L     R6,AREC                                                          
         XC    KEY,KEY             SET THE STATION RECORD KEY                   
         LA    R4,KEY                                                           
         USING ADDRKEY,R4                                                       
         MVI   ADDKTYPE,C'A'                                                    
         MVI   ADDKMED,C'T'                                                     
         MVC   ADDKCALL,SVDHDEND                                                
         MVC   ADDKAGY,AGYPOWCD                                                 
         MVI   ADDKFILL,C'0'                                                    
         MVC   ADDKFILL+1(L'ADDKFILL-1),ADDKFILL                                
         DROP  R4                                                               
*                                                                               
         BAS   RE,STAHIGHD                                                      
         CLI   DMCB+8,X'02'        RECORD IS DELETED?                           
         BE    *+14                YES, PASS IT BACK SO WE CAN RECREATE         
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   KEY(L'ADDRKEY),0(R6)       NEW STATION FORMAT                    
         BNE   CKADRNO             COULDN'T FIND IT                             
*                                                                               
CKADRYES B     YES                                                              
*                                                                               
CKADRNO  B     NO                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE CREATES A NEW STATION ADDRESS RECORD.                            
*                                                                               
* ON ENTRY:    SVDHDEND            CABLE HEADEND                                
*              BITFLAG2  (X'80')   ON=ADD RECORD, OFF=WRITE RECORD              
***********************************************************************         
*                                                                               
MKSTAADD NTR1                                                                   
         L     R2,ADCURAGY                                                      
         USING AGYENTRY,R2                                                      
*                                                                               
         L     R6,AREC             SET THE STATION RECORD                       
         USING ADDRREC,R6                                                       
*                                                                               
         TM    BITFLAG2,B2ADDING                                                
         BZ    *+10                                                             
         XC    0(ADRRECLQ,R6),0(R6)   CLEAR THE RECORD FIRST IF ADDING          
*                                                                               
         MVI   ADDKTYPE,C'A'                                                    
         MVI   ADDKMED,C'T'                                                     
         MVC   ADDKCALL,SVDHDEND                                                
         MVC   ADDKAGY,AGYPOWCD                                                 
         MVI   ADDKFILL,C'0'                                                    
         MVC   ADDKFILL+1(L'ADDKFILL-1),ADDKFILL                                
         MVC   ARECL,=Y(ADRRECLQ)                                               
*                                                                               
         MVC   ANAME,IN_COMNM                                                   
         MVC   A1LINE,IN_COMAD                                                  
         MVC   A2LINE(L'IN_COMCT),IN_COMCT                                      
         OC    A2LINE,SPACES                                                    
         MVC   A3LINE(L'IN_COMST),IN_COMST                                      
         OC    A3LINE,SPACES                                                    
         MVC   AZIP,IN_COMZP                                                    
         MVC   ABIGZIP,IN_COMZP                                                 
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(3,ADDRCHDT)   UPDATE ADD/CHANGE DATE          
         XC    ADDRCHBY,ADDRCHBY   CLEAR OUT THE PID, NULLS = NCC/DDS           
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'ADDRKEY),ADDRKEY                                           
*                                                                               
         LA    R1,=CL31'ADDING STATION ADDRESS REC'                             
         ST    R1,DMCB+8                                                        
         LA    R1,31                                                            
         ST    R1,DMCB+12                                                       
         TM    BITFLAG2,B2ADDING   ADDING THE RECORD?                           
         BNZ   MSTAD10                                                          
         LA    R1,=CL33'CHANGING STATION ADDRESS REC'                           
         ST    R1,DMCB+8                                                        
         LA    R1,33                                                            
         ST    R1,DMCB+12                                                       
*                                                                               
MSTAD10  GOTO1 MYTRACE,DMCB,AREC,ADRRECLQ,,,0                                   
*                                                                               
         TM    BITFLAG2,B2ADDING   ADDING THE RECORD?                           
         BZ    *+12                                                             
         BAS   RE,STAADD           YES                                          
         B     *+8                                                              
         BAS   RE,STAWRT           NO, WRITE THE RECORD                         
*                                                                               
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
MSTADX   B     XIT                                                              
         DROP  R2,R6                                                            
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE CHECKS TO SEE IF THE CABLE DATA RECORD EXISTS                    
*                                                                               
* ON ENTRY:    SVDHDEND            CABLE HEADEND                                
*                                                                               
* ON EXIT:     CONDITION CODE      EQ OR NEQ                                    
***********************************************************************         
*                                                                               
CKCBLREC NTR1                                                                   
         L     R2,ADCURAGY                                                      
         USING AGYENTRY,R2                                                      
*                                                                               
         L     R6,AREC                                                          
         XC    KEY,KEY             SET THE STATION RECORD KEY                   
         LA    R4,KEY                                                           
         USING CBLKEY,R4                                                        
         MVI   CBLKTYPE,C'Y'                                                    
         MVI   CBLKMED,C'T'                                                     
         MVC   CBLKCALL,SVDHDEND                                                
         MVC   CBLKAGY,AGYPOWCD                                                 
         MVC   CBLKCLT,XCPTCLT                                                  
         MVC   CBLKFIL2,=CL3'000'                                               
         DROP  R4                                                               
*                                                                               
         BAS   RE,STAHIGHD                                                      
         CLI   DMCB+8,X'02'        RECORD IS DELETED?                           
         BE    *+14                YES, PASS IT BACK SO WE CAN RECREATE         
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   KEY(L'CBLKEY),0(R6)                                              
         BNE   CCBLNO              COULDN'T FIND IT                             
*                                                                               
CCBLYES  B     YES                                                              
*                                                                               
CCBLNO   B     NO                                                               
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE CHECKS TO SEE IF THE TRAFFIC ADDRESS RECORD EXISTS               
*                                                                               
* ON ENTRY:    SVDHDEND            CABLE HEADEND                                
*                                                                               
* ON EXIT:     CONDITION CODE      EQ OR NEQ                                    
***********************************************************************         
*                                                                               
CKTRAADD NTR1                                                                   
         L     R2,ADCURAGY                                                      
         USING AGYENTRY,R2                                                      
*                                                                               
         XC    KEY,KEY             SET THE STATION RECORD KEY                   
         LA    R4,KEY                                                           
         USING STADDKEY,R4                                                      
         MVC   STAKID,=X'0A28'                                                  
         MVC   STAKAM,AGYBAGYM                                                  
         MVC   STAKSTA,SVDHDEND                                                 
         DROP  R4                                                               
*                                                                               
         BAS   RE,TRHIGHD                                                       
         CLI   DMCB+8,X'02'                                                     
         BE    CTRA10              KEEP DELETED RECORDS                         
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CTRA10   CLC   KEY(L'STADDKEY),KEYSAVE                                          
         BNE   CTRANO              COULDN'T FIND IT                             
*                                                                               
CTRAYES  B     YES                                                              
*                                                                               
CTRANO   B     NO                                                               
         DROP  R2                                                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
* CTFILE CALLS                                                                  
***********************************************************************         
CTLHIGH  MVC   COMMAND,DMRDHI                                                   
         B     CTLFILE                                                          
*                                                                               
CTLSEQ   MVC   COMMAND,DMRSEQ                                                   
*                                                                               
CTLFILE  MVC   DATADISP,=Y(CT5DATA-CT5REC)                                      
         ST    RE,SVDREGRE                                                      
         GOTO1 DATAMGR,DMCB,(DMINBTS,COMMAND),=C'CTFILE',KEY,AREC               
         L     RE,SVDREGRE                                                      
         BR    RE                                                               
***********************************************************************         
* STATION CALLS                                                                 
***********************************************************************         
STAHIGHD OI    DMINBTS,X'08'                                                    
STAHIGH  MVC   COMMAND,DMRDHI                                                   
         B     STAFILE                                                          
*                                                                               
STASEQ   MVC   COMMAND,DMRSEQ                                                   
         B     STAFILE                                                          
*                                                                               
STAADD   CLI   RCWRITE,C'Y'                                                     
         BNE   DONTWRIT                                                         
         MVC   COMMAND,DMADD                                                    
         B     STAFILE                                                          
*                                                                               
STAWRT   DS    0H                                                               
         CLI   RCWRITE,C'Y'                                                     
         BNE   DONTWRIT                                                         
         MVC   COMMAND,DMWRT                                                    
*                                                                               
STAFILE  DS    0H                                                               
         ST    RE,SVDREGRE                                                      
         GOTO1 DATAMGR,DMCB,(DMINBTS,COMMAND),STATION,KEY,AREC                  
         MVI   DMINBTS,0           ALWAYS CLEAR OUT DMINBTS                     
         L     RE,SVDREGRE                                                      
         BR    RE                                                               
***********************************************************************         
* SPTDIR CALLS                                                                  
***********************************************************************         
SPHIGH   MVC   COMMAND,DMRDHI                                                   
         MVC   KEYSAVE,KEY                                                      
         B     SPDIR                                                            
*                                                                               
SPDIR    ST    RE,SVDREGRE                                                      
         GOTO1 DATAMGR,DMCB,(DMINBTS,COMMAND),SPTDIR,KEY,KEY                    
         MVI   DMINBTS,0           ALWAYS CLEAR OUT DMINBTS                     
         L     RE,SVDREGRE                                                      
         BR    RE                                                               
***********************************************************************         
* SPTFILE CALLS                                                                 
***********************************************************************         
SPFGETR  MVC   COMMAND,GETREC                                                   
         B     SPFILE                                                           
*                                                                               
SPFILE   MVC   DATADISP,=Y(STADTAEL-STADDKEY)                                   
         ST    RE,SVDREGRE                                                      
         GOTO1 DATAMGR,DMCB,COMMAND,SPTFILE,KEY,AREC,DMWORK                     
         L     RE,SVDREGRE                                                      
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* TRFDIR CALLS                                                                  
***********************************************************************         
TRHIGHD  OI    DMINBTS,X'08'                                                    
TRHIGH   MVC   COMMAND,DMRDHI                                                   
         MVC   KEYSAVE,KEY                                                      
         B     TRDIR                                                            
*                                                                               
TRADD    CLI   RCWRITE,C'Y'                                                     
         BNE   DONTWRIT                                                         
         MVC   COMMAND,DMADD                                                    
         B     TRDIR                                                            
*                                                                               
TRWRT    CLI   RCWRITE,C'Y'                                                     
         BNE   DONTWRIT                                                         
         MVC   COMMAND,DMWRT                                                    
*                                                                               
TRDIR    ST    RE,SVDREGRE                                                      
*                                                                               
         L     RE,UTL                                                           
         CLI   FCUPTRF,C'Y'        TEST TRAFFIC SYSTEM OPERATIONAL              
         BNE   *+10                                                             
         MVC   4(1,RE),RCUTLTRF    SET TRAFFIC SYSTEM NUMBER                    
*                                                                               
         GOTO1 DATAMGR,DMCB,(DMINBTS,COMMAND),=C'TRFDIR',KEY,KEY                
         MVI   DMINBTS,0           ALWAYS CLEAR OUT DMINBTS                     
*                                                                               
         L     RE,UTL                                                           
         MVC   4(1,RE),SPOTSE      RESTORE SPOT SYSTEM NUMBER                   
*                                                                               
         L     RE,SVDREGRE                                                      
         BR    RE                                                               
***********************************************************************         
* TRFFILE CALLS                                                                 
***********************************************************************         
TRFGETR  MVC   COMMAND,GETREC                                                   
         B     TRFILE                                                           
*                                                                               
TRFADDR  CLI   RCWRITE,C'Y'                                                     
         BNE   DONTWRIT                                                         
         MVC   COMMAND,ADDREC                                                   
         B     TRFILE                                                           
*                                                                               
TRFPUTR  CLI   RCWRITE,C'Y'                                                     
         BNE   DONTWRIT                                                         
         MVC   COMMAND,PUTREC                                                   
*                                                                               
TRFILE   MVC   DATADISP,=Y(STADTAEL-STADDKEY)                                   
         ST    RE,SVDREGRE                                                      
*                                                                               
         L     RE,UTL                                                           
         CLI   FCUPTRF,C'Y'        TEST TRAFFIC SYSTEM OPERATIONAL              
         BNE   *+10                                                             
         MVC   4(1,RE),RCUTLTRF    SET TRAFFIC SYSTEM NUMBER                    
*                                                                               
         GOTO1 DATAMGR,DMCB,COMMAND,=C'TRFFILE',KEY,AREC,DMWORK                 
*                                                                               
         L     RE,UTL                                                           
         MVC   4(1,RE),SPOTSE      RESTORE SPOT SYSTEM NUMBER                   
*                                                                               
         L     RE,SVDREGRE                                                      
         BR    RE                                                               
***********************************************************************         
* IN CASE RCWRITE=C'N', THEN ASSUME NO ERROR FROM DATAMGR                       
***********************************************************************         
DONTWRIT DS    0H                                                               
         MVI   DMCB+8,0            MAKE IT SEEM LIKE WE HAVE NO ERROR           
         BR    RE                                                               
*                                                                               
         EJECT                                                                  
*=====================================================================*         
* TRACE DATA BLOCK                                                              
*                                                                               
*        PARAMETER 1 - A(DATA)                                                  
*        PARAMETER 2 - L(DATA)  OR  ZERO FOR ELEMENTAL RECORD                   
*        PARAMETER 3 - A(LABEL) OR  ZERO FOR NO LABEL                           
*        PARAMETER 4 - L(LABEL) IF PARM 3 IS NOT ZERO                           
*                                                                               
*=====================================================================*         
MYTRACE  NTR1                                                                   
         CLI   QOPT1,C'Y'          OPTION SET TO DISPLAY TRACE?                 
         BNE   TRX                 NO                                           
*                                                                               
         LM    R2,R5,0(R1)         R2 = A(DATA)                                 
*                                  R3 = L(DATA)                                 
*                                  R4 = A(LABEL)                                
*                                  R5 = L(LABEL)                                
*                                                                               
         LTR   R4,R4               IF CALLER SUPPLIED A LABEL                   
         BZ    TR10                                                             
*                                                                               
         MVI   P,C'-'              THEN FILL PRINT LINE WITH '-'S               
         MVC   P+1(131),P                                                       
*                                                                               
         LR    RE,R5               RF = A(PLACE TO CENTER LABEL)                
         SRL   RE,1                                                             
         LA    RF,66                                                            
         SR    RF,RE                                                            
         LA    RF,P(RF)                                                         
*                                                                               
         BCTR  R5,0                MOVE LABEL TO CENTER OF PRINT LINE           
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),0(R4)                                                    
*                                                                               
         GOTO1 REPORT              PRINT LABEL LINE                             
*                                                                               
TR10     LTR   R3,R3               IF DATA IS A RECORD                          
         BNZ   TR50                                                             
         OC    DATADISP,DATADISP   IF THERE IS A KEY                            
         BZ    TR15                                                             
*                                  PRINT OUT ITS KEY                            
         LH    R3,DATADISP                                                      
         GOTO1 PRNTBL,DMCB,0,(R2),C'DUMP',(R3),=X'01C4'                         
*                                                                               
TR15     LR    R6,R2               A(RECORD)                                    
         AH    R6,DATADISP         + DISPLACEMENT TO FIRST ELEMENT              
         MVI   ELCODE,0                                                         
         BAS   RE,FIRSTEL                                                       
         BNE   TR100                                                            
*                                                                               
TR20     ZIC   R4,1(R6)            PRINT ELEMENT                                
         GOTO1 PRNTBL,DMCB,0,(R6),C'DUMP',(R4),=X'01C4'                         
*                                                                               
         BAS   RE,NEXTEL           REPEAT UNTIL NO MORE ELEMENTS                
         BE    TR20                                                             
         B     TR100                                                            
*                                  ELSE PRINT ENTIRE DATA BLOCK                 
TR50     GOTO1 PRNTBL,DMCB,0,(R2),C'DUMP',(R3),=X'01C4'                         
*                                                                               
TR100    DS    0H                                                               
*                                                                               
TRX      B     XIT                                                              
*                                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
***********************************************************************         
* VARIABLES AND STUFF                                                           
***********************************************************************         
CODTABLE DS    0XL2                                                             
         DC    C'D',AL1(CDNEWSYS)      NEW SYSTEM                               
         DC    C'E',AL1(CDPAYADR)      PAYABLE ADDRESS CHANGE                   
         DC    C'F',AL1(CDNETCHG)      NETWORK CHANGE                           
         DC    C'G',AL1(CDEXCLUS)      EXCLUSIVITY CHANGE                       
         DC    C'H',AL1(CDSYSNAM)      SYSTEM NAME CHANGE                       
         DC    C'I',AL1(CDMSOINT)      MSO AND INTERCONNECT CHANGE              
         DC    C'J',AL1(CDTPEADR)      TAPE ADDRESS CHANGE                      
         DC    C'X',AL1(CDNTACTV)      SYSTEM DEACTIVATED                       
* FOR THE SECOND ACTION BYTE                                                    
         DC    C'B',AL1(CDGRPCOD)      GROUP CODE CHANGE                        
         DC    C'K',AL1(CDEIXSTA)      ELECTRONIC INVOICING STATION             
******   DC    C'!',AL1(CDREPURP)      REPURPOSE (OVERWRITE) WAS C'R'           
         DC    X'00'                                                            
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
RELO     DS    A                                                                
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
         LTORG                                                                  
*                                                                               
FILEIN   DCB   DDNAME=CBLTEST,MACRF=GM,DSORG=PS,RECFM=VB,BLKSIZE=4096, +        
               LRECL=4004,EODAD=NOMORE                                          
*                                                                               
* TZIH 03/30/05: SINCE NEW SPCBLLIST INCLUDES NCC AND DDS NETWORK CODES         
* THERE IS NO NEED TO USE SPCBLEQUIV ANYMORE                                    
* INCLUDE STATEMENTS FOR SPCBLLST AND SPCBLEQUIV HAVE BEEN REMOVED              
*                                                                               
***********************************************************************         
* THIS ROUTINE CREATES A NEW CABLE DATA RECORD                                  
*                                                                               
* ON ENTRY:    SVDHDEND            CABLE HEADEND                                
*              BITFLAG2  (X'80')   ON=ADD RECORD, OFF=WRITE RECORD              
***********************************************************************         
*                                                                               
MKCBLREC NTR1  BASE=*,LABEL=*                                                   
         L     R2,ADCURAGY                                                      
         USING AGYENTRY,R2                                                      
*                                                                               
         L     R6,AREC             SET THE STATION RECORD                       
         USING CBLREC,R6                                                        
*                                                                               
         TM    ACTNFLG2,CDREPURP   CLEAR THE RECORD FIRST IF ADDING             
         BNZ   *+12                   OR REPURPOSING                            
         TM    BITFLAG2,B2ADDING                                                
         BZ    *+10                                                             
         XC    0(CBLLNQ,R6),0(R6)                                               
*                                                                               
         MVI   CBLKTYPE,C'Y'                                                    
         MVI   CBLKMED,C'T'                                                     
         MVC   CBLKCALL,SVDHDEND                                                
         MVC   CBLKAGY,AGYPOWCD                                                 
         MVC   CBLKCLT,XCPTCLT                                                  
         MVC   CBLKFIL2,=CL3'000'                                               
         MVC   CBLKLEN,=Y(CBLLNQ)  RECORD LENGTH                                
*                                                                               
         MVC   CSYSICN,IN_INCNM                                                 
         MVC   CSYSMSO,IN_MSONM                                                 
         MVC   CSYSICC,IN_INCCD                                                 
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'CBLKEY),CBLKEY                                             
*                                                                               
         LA    R1,=CL26'ADDING CABLE DATA REC'                                  
         ST    R1,DMCB+8                                                        
         LA    R1,26                                                            
         ST    R1,DMCB+12                                                       
         TM    BITFLAG2,B2ADDING   ADDING THE RECORD?                           
         BNZ   MKCBL20                                                          
         TM    ACTNFLG2,CDREPURP                                                
         BZ    *+12                                                             
         LA    R1,=CL28'REPURPOSING CABLE REC'                                  
         B     *+8                                                              
         LA    R1,=CL28'CHANGING CABLE DATA REC'                                
         ST    R1,DMCB+8                                                        
         LA    R1,28                                                            
         ST    R1,DMCB+12                                                       
*                                                                               
MKCBL20  GOTO1 MYTRACE,DMCB,AREC,CBLLNQ,,,0                                     
*                                                                               
         TM    BITFLAG2,B2ADDING   ADDING THE RECORD?                           
         BZ    *+12                                                             
         BAS   RE,STAADD           YES                                          
         B     *+8                                                              
         BAS   RE,STAWRT           NO, WRITE THE RECORD                         
*                                                                               
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
MKCBLX   B     XIT                                                              
         DROP  R2,R6                                                            
         LTORG                                                                  
         DROP  RB                                                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE CREATES A NEW TRAFFIC ADDRESS RECORD                             
*                                                                               
* ON ENTRY:    SVDHDEND            CABLE HEADEND                                
***********************************************************************         
*                                                                               
MKTRAADD NTR1  BASE=*,LABEL=*                                                   
         L     R2,ADCURAGY                                                      
         USING AGYENTRY,R2                                                      
*                                                                               
         L     R6,AREC             SET THE STATION RECORD                       
         USING STARECD,R6                                                       
*****    XC    0(256,R6),0(R6)     CLEAR THE RECORD FIRST                       
         L     RE,AREC             THE ABOVE XC WAS NOT ENOUGH                  
         LHI   RF,L'SPOTREC                                                     
         XCEFL                                                                  
*                                                                               
         MVC   STAKID,=X'0A28'                                                  
         MVC   STAKAM,AGYBAGYM                                                  
         MVC   STAKSTA,SVDHDEND                                                 
*                                                                               
         MVC   STARECD+13(2),=Y(STARECX-STARECD)                                
         MVC   STAAGYA,AGYPOWCD                                                 
*                                                                               
         MVI   STADTAEL,X'10'      ADDRESS DATA ELEMENT                         
         MVI   STADTALN,STADTAX-STADTAEL                                        
         MVC   STALINE1,IN_TRANM                                                
         MVC   STALINE2,IN_TRAAD                                                
         MVC   STALINE3(L'IN_TRACT),IN_TRACT                                    
         OC    STALINE3,SPACES                                                  
         MVC   STALINE4(L'IN_TRAST),IN_TRAST                                    
         MVI   STALINE4+2,C' '                                                  
         MVC   STALINE4+3(5),IN_TRAZP                                           
         OC    STALINE4,SPACES                                                  
         MVC   STACMLT,IN_TPESZ                                                 
*                                                                               
         MVI   STAAMSEL,X'20'      AMS DATA ELEMENT                             
         MVI   STAMSALN,STAAMSX-STAAMSEL                                        
         MVC   STAAMSTD,IN_TPEDL                                                
         MVC   STAAMSGC,=CL15'AMS'                                              
*                                                                               
         MVI   STAACTEL,X'F1'      ACTIVITY ELEMENT                             
         MVI   STAACTLN,20                                                      
         GOTO1 DATCON,DMCB,(5,0),(3,STAADDT)                                    
         MVC   STAADDID,AGYPOWCD                                                
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'STADDKEY),STADDKEY                                         
         DROP  R6                                                               
*                                                                               
         MVC   DATADISP,=Y(STADTAEL-STADDKEY)                                   
         GOTO1 MYTRACE,DMCB,AREC,0,=CL23'ADD TRAFFIC ADDRESS REC',23            
*                                                                               
         BAS   RE,TRFADDR                                                       
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
MKTRA10  LA    R4,KEY                                                           
         USING STARECD,R4                                                       
         XC    KEY+4(L'KEY-4),KEY+4                                             
         MVC   KEY+14(4),KEY       SAVE THE DISK ADDRESS                        
         MVC   STADDKEY,0(R6)                                                   
         DROP  R4                                                               
*                                                                               
         GOTO1 MYTRACE,DMCB,KEY,STAAGYA-STARECD,                       X        
               =CL26'ADDING TRAFFIC ADDRESS KEY',26                             
*                                                                               
         BAS   RE,TRADD                                                         
         CLI   DMCB+8,X'20'                                                     
         BE    MKTRAX                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
MKTRAX   B     XIT                                                              
         DROP  R2                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE CREATES A NEW STATION RECORD FOR THE GIVEN CABLE HEADEND         
*                                                                               
* ON ENTRY:    SVDHDEND            CABLE HEADEND                                
*              BITFLAG2  (X'80')   ON=ADD RECORD, OFF=WRITE RECORD              
***********************************************************************         
*                                                                               
MKSTATN  NTR1  BASE=*,LABEL=*                                                   
         L     R2,ADCURAGY                                                      
         USING AGYENTRY,R2                                                      
*                                                                               
         L     R6,AREC             SET THE STATION RECORD                       
         USING STAREC,R6                                                        
*                                                                               
         XC    OLDCSMKT,OLDCSMKT   CLEAR PREV CLT-SPECIFIC'S MARKET             
         TM    BITFLAG2,B2ADDING                                                
         BNZ   MKSTA05                                                          
         TM    ACTNFLG2,CDREPURP   REPURPOSING SHOULD CLEAN OUT RECORD          
         JZ    MKSTA02               BUT NOT THE MARKET IN CLT-SPECIFIC         
         CLC   =C'000',STAKCLT     DO WE HAVE A CLT-SPECIFIC STATION?           
         JE    MKSTA02                                                          
         MVC   OLDCSMKT,SMKT       PREV CLIENT SPECIFIC MARKET                  
         J     MKSTA05                                                          
*                                                                               
MKSTA02  CLC   STAKLEN,=Y(STARLNQ)                                              
         BNE   MKSTA03                                                          
         LA    RE,STARLNQ(R6)                                                   
         LA    RF,SCBLSQNQ-STARLNQ                                              
         XCEFL                                                                  
         B     MKSTA10                                                          
MKSTA03  DS    0H                                                               
         CLC   STAKLEN,=Y(STACRLNQ)                                             
         BNE   MKSTA10                                                          
         LA    RE,STACRLNQ(R6)                                                  
         LA    RF,SCBLSQNQ-STACRLNQ                                             
         XCEFL                                                                  
         B     MKSTA10                                                          
*                                                                               
MKSTA05  L     R0,AREC                                                          
         LR    R6,R0                                                            
         LA    R1,SCBLSQNQ                                                      
******** LA    R1,STANCLNQ                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR THE REC FIRST IF ADDING                
***      XC    0(STANCLNQ,R6),0(R6)  CLEAR THE RECORD FIRST IF ADDING           
         MVI   SREP,C'0'                                                        
         MVC   SREP+1(L'SREP-1),SREP                                            
         MVI   STYPE,C' '                                                       
         MVI   SUBMEDIA,C' '                                                    
*                                                                               
MKSTA10  MVI   STAKTYPE,C'S'                                                    
         MVI   STAKMED,C'T'                                                     
         MVC   STAKCALL,SVDHDEND                                                
         MVC   STAKAGY,AGYPOWCD                                                 
         MVC   STAKCLT,XCPTCLT                                                  
         MVC   STAKFILL,=CL3'000'                                               
         MVC   STAKLEN,=Y(SCBLSQNQ)                                             
******** MVC   STAKLEN,=Y(STANCLNQ)                                             
*                                                                               
         CLC   =CL3'000',XCPTCLT   AN EXCEPTION RECORD?                         
         BNE   MKSTA13             YES, EXCEPTION RECS KEEP THEIR MKT           
         MVC   SMKT,AGYMKTNO       NO, EXCEPTION RECORDS KEEP THEIR MKT         
         J     MKSTA16                                                          
*                                                                               
MKSTA13  OC    OLDCSMKT,OLDCSMKT   DO WE HAVE A CLT-SPECIFIC MARKET?            
         JZ    MKSTA16             NO, LEAVE IT ALONE                           
         MVC   SMKT,OLDCSMKT       YES, RESTORE IT                              
*                                                                               
MKSTA16  TM    BITFLAG2,B2ADDING                                                
         BNZ   *+12                                                             
         TM    ACTNFLG1,CDEXCLUS                                                
         BZ    MKSTA20                                                          
         CLI   IN_PYREP,C'N'       WE HAVE A PAYING REP OF NCA?                 
         BNE   MKSTA20                                                          
         CLC   SPAYREP,=3C'0'      ANY PAYING REP?                              
         BNE   *+14                                                             
         MVC   SPAYREP,=C'NCA'     NONE, CHANGE IT TO NCA                       
         B     MKSTA30                                                          
*                                                                               
         MVC   P+10(28),=CL28'PAYING REP SHOULD NOW BE NCA'                     
         GOTO1 REPORT                                                           
*                                                                               
MKSTA20  TM    BITFLAG2,B2ADDING                                                
         BNZ   *+12                                                             
         TM    ACTNFLG1,CDEXCLUS                                                
         BZ    MKSTA30                                                          
         CLI   IN_PYREP,C'C'       WE HAVE A PAYING REP OF CNI?                 
         BNE   MKSTA30                                                          
         CLC   SPAYREP,=3C'0'      ANY PAYING REP?                              
         BNE   *+14                                                             
         MVC   SPAYREP,=C'CNI'     NONE, CHANGE IT TO CNI                       
         B     MKSTA30                                                          
*                                                                               
         MVC   P+10(28),=CL28'PAYING REP SHOULD NOW BE CNI'                     
         GOTO1 REPORT                                                           
*                                                                               
MKSTA30  TM    BITFLAG2,B2ADDING                                                
         BNZ   *+12                                                             
         TM    ACTNFLG1,CDSYSNAM                                                
         BZ    *+10                                                             
         MVC   SSYSNAME,IN_SYSNM                                                
*                                                                               
MKSTA50  TM    BITFLAG2,B2ADDING                                                
         BNZ   *+12                                                             
         TM    ACTNFLG2,CDEIXSTA                                                
         BZ    *+10                                                             
         MVC   SEIXSTA,IN_EIXST                                                 
*                                                                               
         TM    BITFLAG2,B2ADDING      GROUP CODE                                
         BNZ   MKSTA70                                                          
         CLI   QOPT2,C'Y'                                                       
         BE    MKSTA70                                                          
         TM    ACTNFLG2,CDGRPCOD                                                
         BZ    *+10                                                             
MKSTA70  MVC   SGRPCD,=CL15'AMS'                                                
*                                                                               
         TM    BITFLAG2,B2ADDING      ORDER DEADLINE                            
         BNZ   MKSTA80                                                          
         CLI   QOPT2,C'Y'                                                       
         BE    MKSTA80                                                          
         TM    ACTNFLG1,CDTPEADR                                                
         BZ    *+10                                                             
MKSTA80  MVC   SORDDLN,IN_ORDDL                                                 
*                                                                               
MKSTA90  TM    ACTNFLG1,CDNEWSYS   ADDING THIS SYSTEM?                          
         BZ    MKSTA94                                                          
         TM    ACTNFLG1,CDNTACTV                                                
         BZ    MKSTA92             YES, STRAIGHT ADD                            
         TM    BITFLAG2,B2ADDING                                                
         BZ    MKSTA94             NO                                           
*                                                                               
MKSTA92  GOTO1 DATCON,DMCB,(5,0),(3,SEFFDATE)  NO MORE 3 BUSINESS DAYS          
*                                                                               
MKSTA94  DS    0H                                                               
*                                                                               
         BAS   RE,CABLENW          CABLE NETWORKS: SCBL24 & SCBLSEQ             
*                                                                               
******** ZICM  R3,STAKLEN,2                                                     
******** GOTO1 PRNTBL,DMCB,=C'MY TEST ',(R6),C'DUMP',(R3),=C'1D'                
******** DC    H'0'                DEATH                                        
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(STAKEYLN),STAKEY                                             
*                                                                               
         LA    R1,=CL18'ADDING STATION REC'                                     
         ST    R1,DMCB+8                                                        
         LA    R1,18                                                            
         ST    R1,DMCB+12                                                       
         TM    BITFLAG2,B2ADDING   ADDING THE RECORD?                           
         BNZ   MKSTA100                                                         
*                                                                               
         TM    ACTNFLG2,CDREPURP   REPURPOSING THE RECORD??                     
         BZ    MKSTA96                                                          
         LA    R1,=CL20'REPURPOSING STATION '                                   
         B     MKSTA98                                                          
*                                                                               
MKSTA96  LA    R1,=CL20'CHANGING STATION REC'                                   
MKSTA98  ST    R1,DMCB+8                                                        
         LA    R1,20                                                            
         ST    R1,DMCB+12                                                       
*                                                                               
MKSTA100 GOTO1 MYTRACE,DMCB,AREC,SCBLSQNQ,,,0                                   
******** GOTO1 MYTRACE,DMCB,AREC,STANCLNQ,,,0                                   
*                                                                               
         TM    BITFLAG2,B2ADDING   ADDING THE RECORD?                           
         BZ    *+12                                                             
         BAS   RE,STAADD           YES                                          
         B     *+8                                                              
         BAS   RE,STAWRT           NO, WRITE THE RECORD                         
*                                                                               
         CLI   DMCB+8,0                                                         
         JNE   *+2                                                              
*                                                                               
         BRAS  RE,SHWCBLST         SHOW SYSCODE AND NETWORK LIST                
*                                                                               
**** DEBUG CODE ****                                                            
*                                                                               
*        BRAS  RE,DUMPREC                                                       
*                                                                               
**** END DEBUG CODE ****                                                        
*                                                                               
MKSTAX   B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE UPDATES 2 NEW FIELDS IN SPGENSTA: SCBL24 AND SCBLSEQ             
***********************************************************************         
*                                                                               
CABLENW  NTR1                                                                   
*                                                                               
         XC    SCBL24,SCBL24       CLEAR OUT WHAT IS IN THE RECORD              
         XC    SCBLSEQ,SCBLSEQ                                                  
*                                                                               
         MVC   SCBL24,TOP24                                                     
         OC    SCBL24,WKTOP24                                                   
*                                                                               
         LA    R4,WKCBLSEQ         R4=A(NETWORKS TO INCORPORATE)                
         LA    R0,WKCBLSEQ+L'WKCBLSEQ                                           
*                                                                               
CABLE05  LA    R5,CBLSEQ           R5=A(NETWORKS IN RECORD)                     
         LA    R1,CBLSEQ+L'CBLSEQ                                               
*                                                                               
* FIX FOR 8191 PROBLEM - LAST 2 POSITIONS CAN'T BE FILLED IN                    
         CLC   =C'ST8191T',0(R6)                                                
         BNE   *+8                                                              
         LA    R1,CBLSEQ+L'CBLSEQ-4                                             
*                                                                               
CABLE10  XC    LASTFFFF,LASTFFFF                                                
         CR    R4,R0               ALL NETWORKS INCORPORATED?                   
         BNL   CABLE100              YES, BOUNDARY SURPASSED                    
         CLC   0(2,R4),=XL2'00'    -OR- NO MORE NTWRKS TO BE INCORP             
         BE    CABLE100             THEN WE'RE DONE WITH THIS SYSCODE           
*                                                                               
CABLE30  DS    0H                                                               
         CR    R5,R1               ROOM FOR MORE NETWORKS IN RECORD?            
         BNL   CABLE90             NO MORE ROOM, MAYBE A PLACEHOLDER?           
*                                                                               
         CLC   0(2,R5),=XL2'00'    NULLS IS AN EMPTY SLOT                       
         BE    CABLE40             NOT FOUND OR END OF LIST REACHED?            
         CLC   0(2,R5),0(R4)                                                    
         BE    CABLE50             CABLE SEQ ALREADY EXIST                      
         CLC   =X'FFFF',0(R5)      IS IT A PLACEHOLDER?                         
         JNE   CABLE35                                                          
         ST    R5,LASTFFFF         YES, USE PLACEHOLDERS FROM THE BACK          
CABLE35  LA    R5,2(R5)            HAVE TO CHECK PAST ALL PLACEHOLDERS          
         B     CABLE30             AS NETWORK COULD BE THERE                    
*                                                                               
CABLE40  OC    LASTFFFF,LASTFFFF   DO WE HAVE ANY PLACEHOLDER TO USE?           
         JZ    *+8                                                              
CABLE45  L     R5,LASTFFFF         YES, USE IT THEN                             
         MVC   0(2,R5),0(R4)       INSERT THE NEW CABLE SEQ                     
*                                                                               
CABLE50  LA    R4,2(R4)            R4 = A(NEXT NETWORK TO INCORPORATE)          
         B     CABLE05                                                          
*                                                                               
CABLE90  OC    LASTFFFF,LASTFFFF   DO WE HAVE ANY PLACEHOLDER TO USE?           
         JNZ   CABLE45             YES                                          
         BRAS  RE,TMNYNWKS         TOO MANY NETWORKS                            
         J     CABLE50             BUT WE WANT TO SHOW ALL THAT CAN'T           
*                                                                               
CABLE100 MVC   SCBLSEQ,CBLSEQ                                                   
         MVC   STAKLEN(2),=Y(SCBLSQNQ)                                          
*                                                                               
         XIT1                                                                   
*                                                                               
         DROP  R2,R6                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* GETAGCYS                                                                      
*                                                                               
* THIS ROUTINE GETS ALL THE AGENCIES THAT BELONG TO THE SAME SPOT               
* FILE AS THE 'DDSZ?' ID                                                        
***********************************************************************         
*                                                                               
GETAGCYS NTR1  BASE=*                                                           
*****                                                                           
* READ THE ACCESS RECORD FOR THE 'DDSZ?' RECORD                                 
*****                                                                           
         XC    KEY,KEY             SET UP ACCESS KEY                            
         LA    R4,KEY                                                           
         USING CT5KEY,R4                                                        
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,AGENCY                                                  
         OC    CT5KALPH,SPACES                                                  
         DROP  R4                                                               
*                                                                               
         BAS   RE,CTLHIGH          MAKE SURE DDS? ACCESS RECORD EXISTS          
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AREC                                                          
         USING CT5REC,R6                                                        
         CLC   CT5KEY,KEY                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   ELCODE,CTSYSELQ     FETCH THE SPOT SENUM                         
         BAS   RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
GAGY10   BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING CTSYSD,R6                                                        
         CLI   CTSYSNUM,X'02'      SPOT SYSTEM?                                 
         BNE   GAGY10                                                           
         MVC   SPTSENUM,CTSYSSE    YES, COPY SENUM                              
*                                                                               
         TM    CTSYSIND,CTSYSNCA   IS THIS A SPECIFIC AGENCY?                   
         BZ    GAGY50              NO                                           
*                                                                               
         LA    R2,AGYTABLE         YES, SET UP JUST ONE ENTRY IN TABLE          
         USING AGYENTRY,R2                                                      
         MVC   AGYPOWCD,AGENCY                                                  
         LA    R2,L'AGYENTRY(R2)                                                
         MVI   0(R2),X'FF'                                                      
         B     GAGY100                 AND GET THE BINARY AGY/MED CODE          
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
*****                                                                           
* SAVE ALL THE ACCESS POWER CODES THAT USE NCA AND HAVE THE SAME SENUM          
*****                                                                           
GAGY50   XC    KEY,KEY             SET UP ACCESS KEY                            
         LA    R4,KEY                                                           
         USING CT5KEY,R4                                                        
         MVI   CT5KTYP,CT5KTYPQ                                                 
         DROP  R4                                                               
*                                                                               
         LA    R2,AGYTABLE         R2 = A(1ST AGENCY ENTRY IN TABLE)            
         USING AGYENTRY,R2                                                      
         XC    AGYTABLE,AGYTABLE   CLEAR THE AGENCY TABLE FIRST                 
         MVI   0(R2),X'FF'         PUT END-OF-TABLE MARKER                      
*                                                                               
         BAS   RE,CTLHIGH          GO THROUGH ALL THE ACCESS RECORDS            
*                                                                               
GAGY60   CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AREC                                                          
         USING CT5REC,R6                                                        
         CLI   CT5KTYP,CT5KTYPQ                                                 
         BNE   GAGY100                                                          
*                                                                               
GAGY70   MVI   ELCODE,CTSYSELQ     EXAMINE SYSTEM ELEMENT                       
         BAS   RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
GAGY80   BAS   RE,NEXTEL           ID DOES NOT USE SPOT                         
         BNE   GAGY90                                                           
*                                                                               
         USING CTSYSD,R6                                                        
         CLI   CTSYSNUM,X'02'      SPOT SYSTEM?                                 
         BNE   GAGY80                                                           
*                                                                               
         TM    CTSYSIND,CTSYSNCA   USES NCA?                                    
         BZ    GAGY90              NO, NEXT ACCESS RECORD                       
*                                                                               
         CLC   CTSYSSE,SPTSENUM    YES, SEE IF IT IS THE SAME SYSTEM            
         BNE   GAGY90                                                           
         DROP  R6                                                               
*                                                                               
         L     R6,AREC                                                          
         USING CT5REC,R6                                                        
         MVC   AGYPOWCD,CT5KALPH                                                
         LA    R2,L'AGYENTRY(R2)                                                
         MVI   0(R2),X'FF'         PUT END-OF-TABLE MARKER                      
         DROP  R6                                                               
*                                                                               
GAGY90   BAS   RE,CTLSEQ           CHECK NEXT ACCESS RECORD                     
         B     GAGY60                                                           
         DROP  R2                                                               
         EJECT                                                                  
*****                                                                           
* FIND THE BAGYMD CODE FOR EACH AGENCY THAT IS STORED IN OUR TABLE              
*****                                                                           
GAGY100  LA    R2,AGYTABLE                                                      
         USING AGYENTRY,R2                                                      
*                                                                               
GAGY110  CLI   0(R2),X'FF'         END OF AGENCY TABLE?                         
         BE    GAGYX                                                            
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY              LOOK AT THE AGENCY RECORD                    
         USING AGYKEY,R4                                                        
         MVI   AGYKTYPE,X'06'                                                   
         MVC   AGYKAGY,AGYPOWCD                                                 
         DROP  R4                                                               
*                                                                               
         BAS   RE,SPHIGH                                                        
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   KEY(L'AGYKEY),KEYSAVE                                            
         BE    GAGY115                                                          
         LA    RE,L'AGYENTRY(R2)   REMOVE THIS AGENCY FROM TABLE                
         LA    RF,AGYTBLND          BY MOVING NEXT AGENCY UP AN ENTRY           
         SR    RF,RE                                                            
         LR    R0,R2                                                            
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
         B     GAGY110                                                          
*                                                                               
GAGY115  MVC   KEY(4),KEY+14                                                    
         BAS   RE,SPFGETR                                                       
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AREC                                                          
         CLC   0(L'AGYKEY,R6),KEYSAVE   DIE IN CASE IF WE GOT A DIFF            
         BE    *+6                          RECORD THAN WHAT WE EXPECT          
         DC    H'0'                                                             
*                                                                               
         MVI   ELCODE,X'02'        FIND THE MEDIA CODE ELEMENT                  
         BAS   RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
GAGY120  BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING AGYMEDEL,R6         LOOK FOR MEDIA 'T' ELEMENT                   
         CLI   AGYMEDCD,C'T'                                                    
         BNE   GAGY120                                                          
         MVC   AGYBAGYM,AGYMEDBT                                                
         LA    R2,L'AGYENTRY(R2)                                                
         B     GAGY110                                                          
         DROP  R2,R6                                                            
*                                                                               
GAGYX    B     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
***********************************************************************         
* ADCNIREP                                                                      
*                                                                               
* THIS ROUTINE ADD OR CHANGES THE 'CNI' STATION REP RECORD                      
***********************************************************************         
*                                                                               
ADCNIREP NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING AGYENTRY,R2                                                      
         LA    R2,AGYTABLE                                                      
CNIREP05 CLI   AGYENTRY,X'FF'                                                   
         BE    CNIREPX                                                          
*                                                                               
         L     R6,AREC                                                          
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING REPKEY,R4                                                        
         MVI   REPKTYPE,C'R'                                                    
         MVI   REPKMED,C'T'                                                     
         MVC   REPKREP,=C'CNI'                                                  
         MVC   REPKAGY,AGENCY                                                   
         MVI   REPKFILL,C'0'                                                    
         MVC   REPKFILL+1(L'REPKFILL-1),REPKFILL                                
         DROP  R4                                                               
*                                                                               
         BAS   RE,STAHIGHD                                                      
         CLI   DMCB+8,X'02'                                                     
         BE    *+14                                                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         NI    BITFLAG2,X'FF'-B2ADDING   SIGNIFY CHANGING FIRST                 
         CLC   KEY(REPKEYLQ),0(R6)                                              
         BE    CNIREP10                                                         
         OI    BITFLAG2,B2ADDING         WE'RE GOING TO ADD                     
         XC    0(L'REPREC,R6),0(R6)                                             
         MVC   0(REPKEYLQ,R6),KEY                                               
*                                                                               
         USING REPREC,R6                                                        
CNIREP10 MVC   REPRLEN,=Y(L'REPREC)                                             
         NI    RCNTL,X'FF'-X'80'                                                
         MVC   RNAME(19),=CL19'CABLE NETWORKS, INC'                             
         MVC   R1LINE(12),=CL12'PO BOX 19252'                                   
         MVC   R2LINE(6),=CL6'NEWARK'                                           
         MVC   R3LINE,=C'NJ '                                                   
         MVI   RUNWNET,C'N'                                                     
         MVC   RBIGZIP(5),=CL5'07195'                                           
*                                                                               
         TM    BITFLAG2,B2ADDING   ADD RECORD?                                  
         BZ    CNIREP20                                                         
         GOTO1 MYTRACE,DMCB,AREC,L'REPREC,                             X        
               =CL21'ADDING REP RECORD CNI',21                                  
*                                                                               
         BAS   RE,STAADD                                                        
         CLI   DMCB+8,0                                                         
         BE    CNIREP30                                                         
         DC    H'0'                                                             
*                                                                               
CNIREP20 GOTO1 MYTRACE,DMCB,AREC,L'REPREC,                             X        
               =CL23'CHANGING REP RECORD CNI',23                                
*                                                                               
         BAS   RE,STAWRT                                                        
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CNIREP30 LA    R2,L'AGYENTRY(R2)                                                
         B     CNIREP05                                                         
*                                                                               
CNIREPX  B     XIT                                                              
         DROP  R2,R6                                                            
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*****************************************************************               
DUMPREC  NTR1  BASE=*,LABEL=*                                                   
         L     R2,AREC                                                          
         SR    R5,R5                                                            
         ICM   R5,3,15(R2)                                                      
         GOTO1 PRNTBL,DMCB,=CL20'*DUMP*',(R2),C'DUMP',(R5),=C'2D'               
         B     XIT                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
SPUCWRKD DS    0A                                                               
AREC1    DS    A                   A(FIRST RECORD AREA)                         
AREC2    DS    A                   A(SECOND RECORD AREA)                        
ACABLTAB DS    A                   A(CABLE NETWORK TABLE)                       
LASTFFFF DS    A                   A(LAST FFFF PLACEHOLDER)                     
CBLTABLN DS    H                                                                
         DS    H                                                                
*                                                                               
SVDREGRE DS    A                   SAVED REGISTER RE                            
*                                                                               
ADCURAGY DS    A                   A(CURRENT AGENCY ENTRY)                      
VXSORT   DS    V                                                                
*                                                                               
COUNTER1 DS    PL8                 COUNTER OF STATION RECORDS PROCESSED         
COUNTER3 DS    PL8                 COUNTER OF ADDRESS RECORDS PROCESSED         
COUNTER4 DS    PL8                 COUNTER OF CABLE RECORDS PROCESSED           
COUNTER5 DS    PL8                 COUNTER OF TRAFFIC RECORDS PROCESSED         
COUNTER6 DS    PL8                 COUNTER OF K PASSIVE STATION KEY             
*                                                                               
BITFLAG1 DS    XL1                 FIRST SET OF BIT FLAGS                       
B1DBLQTE EQU   X'80'                   WE FOUND A DOUBLE QUOTE ALREADY          
B1COMMA  EQU   X'40'                   WE FOUND A COMMA ALREADY                 
B1NETWRK EQU   X'20'                   GOT NETWORK DATA                         
B1ACTION EQU   X'10'                   GOT ACTION CODE DATA                     
*                                                                               
BITFLAG2 DS    XL1                 SECOND SET OF BIT FLAGS                      
B2ADDING EQU   X'80'                   ADDING RECORD (0=WRITE RECORD)           
B2ADDK   EQU   X'40'                   ADD K KEY (0=WRITE RECORD)               
*                                                                               
ACTNFLG1 DS    XL1                 SAME BIT DEFINITION AS CODEFLG1              
CODEFLG1 DS    XL1                 ACTION CODE BIT FLAG 1                       
CDNEWSYS EQU   X'80'                   'D' - NEW SYSTEM                         
CDPAYADR EQU   X'40'                   'E' - PAYABLE ADDRESS CHANGE             
CDNETCHG EQU   X'20'                   'F' - NETWORK CHANGE                     
CDEXCLUS EQU   X'10'                   'G' - EXCLUSIVITY CHANGE                 
CDSYSNAM EQU   X'08'                   'H' - SYSTEM NAME CHANGE                 
CDMSOINT EQU   X'04'                   'I' - MSO/INTERCONNECT CHANGE            
CDTPEADR EQU   X'02'                   'J' - TAPE ADDRESS CHANGE                
CDNTACTV EQU   X'01'                   'X' - SYSTEM DEACTIVATED                 
*                                                                               
ACTNFLG2 DS    XL1                 SAME BIT DEFINITION AS CODEFLG2              
CODEFLG2 DS    XL1                 ACTION CODE BIT FLAG 2                       
CDGRPCOD EQU   X'80'                   'B' - GROUP CODE                         
CDEIXSTA EQU   X'40'                   'K' - ELECTRONIC INVOICING               
CDREPURP EQU   X'20'                   'A' - REPURPOSE DEACTIV SYSCODE          
*                                                                               
ELCODE   DS    XL1                                                              
*                                                                               
IN_SYSCD DS    XL2                 SYSTEM CODE (CABLE HEADEND)                  
IN_SYSNM DS    CL35                       NAME                                  
IN_COMNM DS    CL25                COMPANY NAME                                 
IN_COMAD DS    CL35                        ADDRESS                              
IN_COMCT DS    CL20                        CITY                                 
IN_COMST DS    CL2                         STATE                                
IN_COMZP DS    CL10                        ZIP                                  
IN_INCCD DS    CL5                 INTERCONNECT CODE                            
IN_INCNM DS    CL20                             NAME                            
IN_AMRKT DS    CL3                 ALPHA MARKET                                 
IN_TRANM DS    CL25                TRAFFIC NAME                                 
IN_TRAAD DS    CL35                        ADDRESS                              
IN_TRACT DS    CL20                        CITY                                 
IN_TRAST DS    CL2                         STATE                                
IN_TRAZP DS    CL10                        ZIP                                  
IN_MSONM DS    CL15                MSO NAME                                     
IN_NWKCD DS    CL4                 NETWORK CODE                                 
IN_PYREP DS    CL1                 NCA YES OR NO                                
IN_ACTCD DS    CL1                 ACTION CODE                                  
IN_TPEDL DS    CL50                TAPE DEADLINE INFO                           
IN_ORDDL DS    CL50                ORDER DEADLINE INFO                          
IN_GRPCD DS    CL15                GROUP CODE  (SHOULD BE "AMS" NOW)            
IN_TPESZ DS    CL4                 TAPE SIZE (COMML TYPE)                       
IN_EIXST DS    CL1                 ON ELECTRONIC INVOICING                      
IN_DATAX EQU   *                                                                
*                                                                               
ACTHDEND DS    CL5                 ACTION CABLE HEADEND (LAST USED)             
SYSHDEND DS    CL5                 SYSTEM CABLE HEADEND (LAST USED)             
NETHDEND DS    CL5                 SYSTEM CABLE HEADEND (LAST USED)             
CBLHDEND DS    CL5                 CABLE HEADEND (CURRENT)                      
SVDHDEND DS    CL5                 STATION CALL LETTERS                         
NUMMRKT  DS    CL4                 MKT NUM EBCDIC (BASED ON ALPHA MKT)          
PRVMRKT  DS    CL4                 PREVIOUS MARKET OF THE STATION               
TODAYDTE DS    CL6                 TODAY'S DATE EBCDIC YY/MM/DD                 
*                                                                               
SPTSENUM DS    XL1                 SPOT SENUM                                   
XCPTCLT  DS    CL3                 EXCEPTION CLIENT RECORD, 000 = REG.          
*                                                                               
OLDCSMKT DS    CL4                 OLD CLIENT-SPECIFIC'S MARKET                 
REPRPMKT DS    XL2                 MKT OF REPURPOSED SYSCODE                    
TOP24    DC    XL3'00'             TOP 24 NETWORKS                              
CBLSEQ   DC    XL206'00'            CABLE SEQ                                   
WKTOP24  DC    XL3'00'             WORKING FIELD FOR TOP 24 NETWORKS            
SVTOP24  DC    XL3'00'             SAVED TOP 24 WORKING FIELD                   
WKCBLSEQ DC    XL206'00'            WORKING FIELD FOR CABLE SEQ                 
*                                                                               
MXAGENCY EQU   20                  MAX NUMBER OF AGENCIES IN TABLE              
AGYTABLE DS    CL((MXAGENCY)*(L'AGYENTRY))                                      
AGYTBLND DS    X                   TABLE END                                    
*                                                                               
ELEM     DS    CL128                                                            
MYCABTAB DS    CL384                                                            
MYCABEND DC    X'000000'                                                        
*                                                                               
INPTLINE DS    0CL4004             INPUT LINE FROM THE FILE                     
INPTLGTH DS    XL2                     LENGTH OF DATA FOR THIS LINE             
         DS    XL2                     FOR QSAM MACRO                           
INPTDATA DS    CL4000                  DATA FOR THIS LINE                       
*                                                                               
SPOTREC  DS    CL4000                                                           
SPOTREC2 DS    CL4000                                                           
*                                                                               
AGYTABLD DSECT                                                                  
AGYENTRY DS    0CL8                AGENCY ENTRY IN AGENCY TABLE                 
AGYPOWCD DS    CL2                 AGENCY POWER CODE                            
AGYBAGYM DS    CL1                 AGENCY BAGYMD                                
         DS    CL1                 SPARE                                        
AGYMKTNO DS    CL4                 MARKET NUMBER FOR THE ALPHA CODE             
         EJECT                                                                  
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
         EJECT                                                                  
GENSTAD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
GENADDD  DSECT                                                                  
       ++INCLUDE SPGENADD                                                       
         EJECT                                                                  
GENCBLD  DSECT                                                                  
       ++INCLUDE SPGENCBL                                                       
         EJECT                                                                  
REPRECD  DSECT                                                                  
       ++INCLUDE SPGENREP                                                       
         EJECT                                                                  
       ++INCLUDE SPTRSTA                                                        
STARECX  EQU   *                   END OF TRAFFIC ADDRESS RECORD                
         EJECT                                                                  
*SPGENAGY                                                                       
         PRINT OFF                                                              
       ++INCLUDE SPGENAGY                                                       
         PRINT ON                                                               
*SPGENANMK                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPGENANMK                                                      
         PRINT ON                                                               
*CTGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'025SPREPUC02 02/02/21'                                      
         END                                                                    
