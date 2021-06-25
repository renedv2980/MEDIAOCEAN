*          DATA SET SPREPUP02N AT LEVEL 043 AS OF 02/13/04                      
*PHASE SPUP02NA                                                                 
*         ==>  <==  CAN'T USE TEST=SPUP02NA, SO RENAME DDS.LOADLIB TO           
*                       USE SPUP02B IF AVAILABLE                                
*INCLUDE PRNTBL                                                                 
SPUP02N  TITLE 'SPREPUP02N - LOAD CABLE FILE FOR NEW AGENCY'                    
SPUP02   CSECT                                                                  
         DS    8192C                                                            
         ORG   *-8192                                                           
         PRINT NOGEN                                                            
*                                                                               
         NMOD1 0,SPUP02,R8,R7                                                   
*        LA    RC,2048(RB)                                                      
*        LA    RC,2048(RC)                                                      
*        USING SPUP02+4096,RC                                                   
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    MAIN                                                             
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* MAIN                                                                          
***************                                                                 
*     MAIN SECTION OF THE PROGRAM                                               
***********************************************************************         
*                                                                               
MAIN     DS    0H                                                               
         LR    RE,RB                                                            
         AHI   RE,INPTLINE-SPUP02                                               
         ST    RE,AINPTLIN                                                      
*                                                                               
         LR    R6,RB               SET ADDRESS OF RECORD FOR SPONSOR            
         A     R6,=A(SPOTREC-SPUP02)                                            
         ST    R6,AREC1                                                         
         LR    R6,RB               SET ADDRESS OF RECORD #2 FOR SPONSOR         
         A     R6,=A(SPOTREC2-SPUP02)                                           
         ST    R6,AREC2                                                         
         ST    R6,ADSTAT                                                        
         ST    R6,ADSTATAD                                                      
*                                                                               
         MVC   AREC,AREC1          DEFAULT RECORD AREA                          
*                                                                               
         OPEN  (FILEIN,INPUT)      OPEN UP INPUT FILE FOR USE                   
*        OPEN  (FILEOUT,OUTPUT)    OPEN UP OUTPUT FILE FOR USE                  
*                                                                               
         ZAP   COUNTER1,=P'0'                                                   
         ZAP   COUNTER2,=P'0'                                                   
         ZAP   COUNTER3,=P'0'                                                   
         ZAP   COUNTER4,=P'0'                                                   
         ZAP   COUNTER5,=P'0'                                                   
**** DEBUG CODE ****                                                            
*                                                                               
*        ZAP   COUNTREC,=P'0'                                                   
*                                                                               
**** END DEBUG CODE ****                                                        
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
*                                                                               
         BAS   RE,ADNCAREP         ADD THE 'NCA' REP                            
         BAS   RE,ADCNIREP         ADD THE 'CNI' REP                            
*                                                                               
READALIN L     RE,AINPTLIN         CLEAR THE INPUT LINE                         
         LA    RF,4004                                                          
         XCEFL                                                                  
         MVI   BITFLAG1,0          CLEAR OUR BIT FLAGS                          
*                                                                               
         L     R3,AINPTLIN                                                      
         GET   FILEIN,(R3)         READ A LINE FROM THE FILE                    
*                                                                               
         BAS   RE,FETCHINF         FETCH IMPORTANT INFO FROM LINE               
         BNE   READALIN                                                         
*                                                                               
         BAS   RE,PRCSSINF         PROCESS THE INFORMATION                      
         B     READALIN            READ THE NEXT LINE                           
*                                                                               
***********************************************************************         
* NOMORE                                                                        
***************                                                                 
*     WHAT HAPPENS WHEN THERE IS NO MORE LINES LEFT IN THE FILE                 
***********************************************************************         
*                                                                               
NOMORE   DS    0H                                                               
         CLOSE FILEIN                                                           
*                                                                               
         OC    ACTHDEND,ACTHDEND   ANYTHING TO DO BEFORE WE LEAVE?              
         BZ    NOMORE10            NO, PRINT NUMBERS                            
*                                                                               
         MVC   SVDHDEND,ACTHDEND   YES, PROCESS LAST HEADEND                    
         MVC   ACTNFLAG,CODEFLAG                                                
         XC    CBLHDEND,CBLHDEND   SIGNIFY LAST RECORD                          
         OI    BITFLAG1,B1ACTION                                                
         BAS   RE,PRCSSINF         PROCESS THE LAST RECORD                      
*                                                                               
NOMORE10 MVC   P(30),=CL30'NUMBER OF STATIONS ADDED: '                          
         EDIT  (P8,COUNTER1),(17,P+30),ALIGN=LEFT                               
         MVC   P2(30),=CL30'NUMBER OF PASSIVES ADDED: '                         
         EDIT  (P8,COUNTER2),(17,P2+30),ALIGN=LEFT                              
         MVC   P3(30),=CL30'NUMBER OF ADDRESSES ADDED: '                        
         EDIT  (P8,COUNTER3),(17,P3+30),ALIGN=LEFT                              
         MVC   P4(30),=CL30'NUMBER OF CABLE RECS ADDED: '                       
         EDIT  (P8,COUNTER4),(17,P4+30),ALIGN=LEFT                              
         MVC   P5(30),=CL30'NUMBER OF TRAFFICS ADDED: '                         
         EDIT  (P8,COUNTER5),(17,P5+30),ALIGN=LEFT                              
         GOTO1 REPORT                                                           
*                                                                               
*        CLOSE FILEOUT                                                          
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
         L     R6,AREC                                                          
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING REPKEY,R4                                                        
         MVI   REPKTYPE,C'R'                                                    
         MVI   REPKMED,C'T'                                                     
         MVC   REPKREP,=C'NCA'                                                  
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
         BE    NCAREP10                                                         
         OI    BITFLAG2,B2ADDING         WE'RE GOING TO ADD                     
         XC    0(L'REPREC,R6),0(R6)                                             
         MVC   0(REPKEYLQ,R6),KEY                                               
*                                                                               
         USING REPREC,R6                                                        
NCAREP10 MVC   REPRLEN,=Y(L'REPREC)                                             
         NI    RCNTL,X'FF'-X'80'                                                
         MVC   RNAME(19),=CL19'NATIONAL CABLE COMM'                             
         MVC   R1LINE(11),=CL11'PO BOX 3350'                                    
         MVC   R2LINE(6),=CL6'BOSTON'                                           
         MVC   R3LINE,=C'MA '                                                   
         MVI   RUNWNET,C'N'                                                     
         MVC   RBIGZIP(5),=CL5'02241'                                           
*                                                                               
         TM    BITFLAG2,B2ADDING   ADD RECORD?                                  
         BZ    NCAREP20                                                         
         GOTO1 MYTRACE,DMCB,AREC,L'REPREC,                             X        
               =CL21'ADDING REP RECORD NCA',21                                  
*                                                                               
         BAS   RE,STAADD                                                        
         CLI   DMCB+8,0                                                         
         BE    NCAREPX                                                          
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
NCAREPX  B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* ADCNIREP                                                                      
***************                                                                 
*     THIS ROUTINE ADD OR CHANGES THE 'CNI' STATION REP RECORD                  
***********************************************************************         
*                                                                               
ADCNIREP NTR1                                                                   
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
         BE    CNIREPX                                                          
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
CNIREPX  B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* FETCHINF                                                                      
***************                                                                 
*     THIS ROUTINE FETCHES ALL THE NECESSARY INFOMATION FROM THE LINE           
* WE GOT FROM THE FILE AND PUTS IT INTO OUR STORAGE AREA                        
***********************************************************************         
*                                                                               
FETCHINF NTR1                                                                   
         L     R2,AINPTLIN                                                      
         AHI   R2,4                                                             
*                                                                               
         CLI   5(R2),C'0'          SEE IF WE HAVE ACTION CODE DATA              
         BE    FINF00                                                           
         CLI   5(R2),C'1'          SEE IF WE HAVE SYSTEM CODE DATA              
         BE    FINF10                                                           
         CLI   5(R2),C'2'          SEE IF WE HAVE NETWORK CODE DATA             
         BE    FINF20                                                           
*                                                                               
         MVC   P(23),=C'LINE THAT BEGINS WITH: '     INVALID CODE               
         L     R1,AINPTLIN                                                      
         AHI   R1,4                                                             
         MVC   P+25(10),0(R1)                                                   
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
         MVC   ACTNFLAG,CODEFLAG       SAVE PREVIOUS ACTION CODE FLAG           
         MVI   CODEFLAG,0              CLEAR CURRENT ACTION CODE FLAG           
*                                                                               
FINF02   GOTO1 NEXTFLD,DMCB,(2,(R2)),(L'IN_ACTCD,IN_ACTCD)                      
         BNE   FINFER00                                                         
*                                                                               
         LA    R1,CODTABLE         VALIDATE THE ACTION CODE                     
FINF04   CLI   0(R1),0             ERROR IF END OF TABLE                        
         BE    FINFER00                                                         
*                                                                               
         CLC   IN_ACTCD,0(R1)      CODE IN THIS ENTRY?                          
         BE    *+12                                                             
         LA    R1,L'CODTABLE(R1)   NO, CHECK THE NEXT ENTRY                     
         B     FINF04                                                           
*                                                                               
         TM    CODEFLAG,CDNEWSYS   ALREADY ASKING TO ADD THE SYSTEM?            
         BNZ   FINF00X             YES, DON'T CARE ABOUT OTHER CODES            
*                                                                               
         CLI   1(R1),CDNEWSYS      ASKING TO ADD THE SYSTEM?                    
         BNE   *+12                                                             
         MVI   CODEFLAG,CDNEWSYS   YES, DON'T CARE ABOUT OTHER CODES            
         B     FINF00X                                                          
*                                                                               
         MVC   BYTE,CODEFLAG       SET THE APPROPRIATE BIT ON FOR THIS          
         OC    BYTE,1(R1)              ACTION CODE                              
         MVC   CODEFLAG,BYTE                                                    
*                                                                               
FINF00X  MVC   ACTHDEND,CBLHDEND   SAVE CABLE HEADEND FOR THIS ACTION           
         B     FINFYES                                                          
*                                                                               
FINFER00 MVC   P(13),=C'ACTION LINE: '                                          
         L     R1,AINPTLIN                                                      
         AHI   R1,4                                                             
         MVC   P+15(10),0(R1)                                                   
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
*                                                                               
         CLC   ACTHDEND,CBLHDEND   SAME HEADEND AS THE ACTION CODE?             
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
         L     R1,AINPTLIN                                                      
         AHI   R1,4                                                             
         MVC   P+15(4),0(R1)                                                    
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
*                                                                               
         CLC   ACTHDEND,CBLHDEND   SAME CABLE HEADEND AS ACTION'S?              
         BNE   FINFNO                                                           
         TM    CODEFLAG,CDNEWSYS   ADDING NEW SYSTEM?                           
         BZ    FINF25              NO                                           
         CLC   SYSHDEND,CBLHDEND   SAME CABLE HEADEND AS SYSTEM'S?              
         BNE   FINFNO              YES, IT HAS TO BE                            
*                                                                               
FINF25   GOTO1 NEXTFLD,DMCB,(2,(R2)),(L'IN_NWKCD,IN_NWKCD)                      
         BNE   FINFER20                                                         
         OC    IN_NWKCD,SPACES                                                  
         B     FINFYES                                                          
*                                                                               
FINFER20 MVC   P(14),=C'NETWORK LINE: '                                         
         L     R1,AINPTLIN                                                      
         AHI   R1,4                                                             
         MVC   P+15(13),0(R1)                                                   
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
         L     RE,AINPTLIN                                                      
         LR    R1,R2                                                            
         SR    R1,RE                                                            
         CLM   R1,3,0(RE)          DID WE GO BEYOND THE DATA?                   
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
         BZ    PINFA20                                                          
*                                                                               
         BAS   RE,UPDSTATN         YES, ADD OR UPDATE STATION RECORD            
*                                                                               
         BAS   RE,CKSTAPSV              ADD OR UPDATE PASSIVE RECORD            
         BE    PINFA10                                                          
         OI    BITFLAG2,B2ADDING                                                
         AP    COUNTER2,=P'1'                                                   
         B     *+8                                                              
PINFA10  NI    BITFLAG2,X'FF'-B2ADDING                                          
         BAS   RE,MKSTAPSV                                                      
*                                                                               
         BAS   RE,UPDADDRS              ADD OR UPDATE ADDRESS RECORD            
         BAS   RE,UPDCABLE              ADD OR UPDATE CABLE DATA RECORD         
         BAS   RE,UPDTRAFF              ADD OR UPDATE TRAFFIC ADDRESS           
*                                                                               
PINFA20  MVI   BITFLAG2,0                                                       
         XC    SVDHDEND,SVDHDEND   SO WE WON'T WRITE OUT RECORD AGAIN           
         MVC   ACTHDEND,CBLHDEND                                                
         B     PINFXIT                                                          
*****                                                                           
* SYSTEM DATA                                                                   
*****                                                                           
PINFS00  BAS   RE,GETMKTNO         GET MKT NUM BASED ON ALPHA MKT               
         BE    PINFS10                                                          
         BAS   RE,BADMKTCD         COULDN'T FIND ALPHA MKT                      
         B     PINFSNO             SKIP HEADEND, SYSTEM NOT PROCESSED           
*                                                                               
PINFS10  MVC   SYSHDEND,CBLHDEND                                                
         BAS   RE,SHWCBLST         SHOW THE CABLE STATION                       
         B     PINFXIT                                                          
*                                                                               
PINFSNO  XC    ACTHDEND,ACTHDEND   DON'T PROCESS THIS HEADEND                   
         MVI   CODEFLAG,0          CLEAR ACTION CODES FOR THIS HEADEND          
         B     PINFXIT                                                          
*****                                                                           
* NETWORK DATA                                                                  
*****                                                                           
PINFN00  BRAS  RE,SETNTWRK         SET APPROPRIATE NETWORK BIT                  
         BE    PINFN10                                                          
         BAS   RE,BADNTWRK                                                      
         B     PINFXIT                                                          
*                                                                               
PINFN10  BAS   RE,SHWCBLST         SHOW THE NETWORK                             
*                                                                               
PINFXIT  B     XIT                                                              
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
         B     *+8                                                              
UADR10   NI    BITFLAG2,X'FF'-B2ADDING                                          
         BAS   RE,MKSTAADD                                                      
*                                                                               
UADRX    B     XIT                                                              
*                                                                               
***********************************************************************         
* THIS ROUTINE UPDATES THE STATION RECORD                                       
*                                                                               
* ON ENTRY:    SVDHDEND            CABLE HEADEND                                
***********************************************************************         
*                                                                               
UPDSTATN NTR1                                                                   
         BAS   RE,CKSTATN          STATION EXISTS?                              
         BNE   USTA10                                                           
         NI    BITFLAG2,X'FF'-B2ADDING                                          
         L     R6,AREC                                                          
         USING GENSTAD,R6                                                       
         DROP  R6                                                               
         B     USTA20                                                           
USTA10   OI    BITFLAG2,B2ADDING   ADD RECORD                                   
         AP    COUNTER1,=P'1'                                                   
USTA20   BAS   RE,MKSTATN                                                       
*                                                                               
USTAX    B     XIT                                                              
*                                                                               
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
         BAS   RE,MKCBLREC                                                      
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
         BAS   RE,CKTRAADD         TRAFFIC ADDRESS RECORD EXISTS?               
         BE    UTRA10                                                           
         AP    COUNTER5,=P'1'                                                   
         BAS   RE,MKTRAADD         NO, ADD IT                                   
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
         MVC   STAAGYA,AGENCY                                                   
*                                                                               
         LA    R6,STADTAEL                                                      
UTRA30   CLI   0(R6),X'10'         ADDRESS DATA ELEMENT?                        
         BE    UTRA32              YES                                          
         ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         CLI   0(R6),0             END OF RECORD?                               
         BNE   UTRA30                                                           
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
         L     R6,AREC                                                          
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
UTRA40   L     R6,AREC                                                          
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
         MVC   STAADDID,AGENCY                                                  
         GOTO1 DATCON,DMCB,(5,0),(3,STACHGDT)                                   
         DROP  R3                                                               
         GOTO1 RECUP,DMCB,(C'S',AREC),ELEM,(R6)                                 
*                                                                               
UTRA68   L     R6,AREC                                                          
         USING STARECD,R6                                                       
*****    MVC   STARECD+13(2),=Y(STARECX-STARECD) <==== THIS IS NOT GOOD         
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
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE GETS THE MARKET NUMBER BASED ON THE ALPHA MARKET FOUND           
* IN THE LINE FROM THE FILE.                                                    
*                                                                               
* ON ENTRY:    IN_AMRKT            ALPHA MARKET CODE                            
*                                                                               
* ON EXIT:     NUMMRKT             EBCDIC MARKET NUMBER FOR THIS ALPHA          
***********************************************************************         
*                                                                               
GETMKTNO NTR1                                                                   
         L     R6,AREC                                                          
         XC    KEY,KEY             FIND PASSIVE KEY FOR ALPHA MARKET            
         LA    R4,KEY                                                           
         USING ANMKEYD,R4                                                       
         MVI   ANMKTYPE,ANMKTYPQ                                                
         MVC   ANMKAGCY,AGENCY                                                  
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
         BNE   GMKTNO              COULDN'T FIND IT                             
*                                                                               
         USING ANMKEYD,R6                                                       
         MVC   NUMMRKT,ANMKNMRK    GOT IT                                       
         DROP  R6                                                               
*                                                                               
GMKTYES  B     YES                                                              
*                                                                               
GMKTNO   B     NO                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE SETS THE APPROPRIATE NETWORK BIT ON                              
*                                                                               
* ON ENTRY:    IN_NWKCD            3 BYTE NETWORK CODE                          
*                                                                               
* ON EXIT:     2-BYTE NETWORK CODE ADDED TO SCBLSEQ (OR BIT TURNED ON           
*              IN SCBL24)                                                       
***********************************************************************         
***********************************************************************         
* THIS ROUTINE SHOW THE CABLE STATION AND NETWORK                               
***********************************************************************         
*                                                                               
SHWCBLST NTR1                                                                   
         TM    BITFLAG1,B1NETWRK   NETWORK DATA?                                
         BNZ   SHWC10              YES                                          
*                                                                               
         XC    P,P                                                              
         GOTO1 REPORT                                                           
         MVC   P(15),=CL15'CABLE STATION: '                                     
         MVC   P+15(5),CBLHDEND                                                 
         MVC   P+25(35),IN_SYSNM                                                
         MVC   P+65(8),=CL8'MARKET: '                                           
         MVC   P+73(4),NUMMRKT                                                  
         GOTO1 REPORT                                                           
         B     SHWCX                                                            
*                                                                               
SHWC10   MVC   P+30(9),=CL9'NETWORK: '                                          
         MVC   P+40(L'IN_NWKCD),IN_NWKCD                                        
         GOTO1 REPORT                                                           
*                                                                               
SHWCX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ERRORS FOR   CBLHDEND                                                         
***********************************************************************         
*                                                                               
BADNTWRK DS    0H                                                               
         MVC   P+25(29),=CL29'HAS AN INVALID NETWORK CODE: '                    
         MVC   P+54(L'IN_NWKCD),IN_NWKCD                                        
         B     BADMSGC                                                          
*                                                                               
BADMKTCD DS    0H                                                               
         MVC   P+25(34),=CL34'HAS AN INVALID ALPHA MARKET CODE: '               
         MVC   P+59(L'IN_AMRKT),IN_AMRKT                                        
         B     BADMSGC                                                          
*                                                                               
BADMSGC  NTR1                                                                   
         MVC   P+10(9),=C'STATION: '                                            
         MVC   P+19(L'CBLHDEND),CBLHDEND                                        
         GOTO1 REPORT                                                           
         B     XIT                                                              
***********************************************************************         
* ERRORS FOR   SVDHDEND                                                         
***********************************************************************         
*                                                                               
CANTDACT DS    0H                                                               
         MVC   P+25(36),=CL36'DOES NOT EXIST.  CAN NOT DEACTIVATE.'             
         B     BADMSGS                                                          
*                                                                               
SYSNTNEW DS    0H                                                               
         MVC   P+25(20),=CL20'IS NOT A NEW SYSTEM.'                             
         B     BADMSGS                                                          
*                                                                               
NOSYSTEM DS    0H                                                               
         MVC   P+25(14),=CL14'DOES NOT EXIST'                                   
         B     BADMSGS                                                          
*                                                                               
BADMSGS  NTR1                                                                   
         MVC   P+10(9),=C'STATION: '                                            
         MVC   P+19(L'SVDHDEND),SVDHDEND                                        
         GOTO1 REPORT                                                           
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE CHECKS TO SEE IF THE STATION RECORD FOR THIS CABLE               
* HEADEND EXISTS.                                                               
*                                                                               
* ON ENTRY:    SVDHDEND            CABLE HEADEND                                
*                                                                               
* ON EXIT:     CONDITION CODE      EQ OR NEQ                                    
***********************************************************************         
*                                                                               
CKSTATN  NTR1                                                                   
         L     R2,DMCB             R2 = A(CABLE HEADEND)                        
*                                                                               
         L     R6,AREC                                                          
         XC    KEY,KEY             SET THE STATION RECORD KEY                   
         LA    R4,KEY                                                           
         USING STAKEY,R4                                                        
         MVI   STAKTYPE,C'S'                                                    
         MVI   STAKMED,C'T'                                                     
         MVC   STAKCALL,SVDHDEND                                                
         MVC   STAKAGY,AGENCY                                                   
         MVI   STAKCLT,C'0'                                                     
         MVC   STAKCLT+1(STAKLEN-(STAKCLT+1)),STAKCLT                           
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
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE CREATES A NEW STATION RECORD FOR THE GIVEN CABLE HEADEND         
*                                                                               
* ON ENTRY:    SVDHDEND            CABLE HEADEND                                
*              BITFLAG2  (X'80')   ON=ADD RECORD, OFF=WRITE RECORD              
***********************************************************************         
*                                                                               
MKSTATN  NTR1                                                                   
         L     R0,AREC             CLEAR THE RECORD FIRST                       
         LA    R1,SCBLSQNQ                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R6,AREC             SET THE STATION RECORD                       
         USING STAREC,R6                                                        
         MVI   STAKTYPE,C'S'                                                    
         MVI   STAKMED,C'T'                                                     
         MVC   STAKCALL,SVDHDEND                                                
         MVC   STAKAGY,AGENCY                                                   
         MVI   STAKCLT,C'0'                                                     
         MVC   STAKCLT+1(STAKLEN-(STAKCLT+1)),STAKCLT                           
         MVC   STAKLEN,=Y(SCBLSQNQ)                                             
*                                                                               
         MVC   SMKT,NUMMRKT                                                     
         MVI   SREP,C'0'                                                        
         MVC   SREP+1(L'SREP-1),SREP                                            
*                                                                               
         CLI   IN_PYREP,C'N'       WE HAVE A PAYING REP OF NCA?                 
         BNE   *+10                                                             
         MVC   SPAYREP,=C'NCA'     YES, IT'S CODE IS   NCA                      
         CLI   IN_PYREP,C'C'       WE HAVE A PAYING REP OF CNI?                 
         BNE   *+10                                                             
         MVC   SPAYREP,=C'CNI'     YES, IT'S CODE IS   CNI                      
*                                                                               
         MVI   STYPE,C' '                                                       
         MVI   SUBMEDIA,C' '                                                    
         MVC   SSYSNAME,IN_SYSNM                                                
         MVC   SGRPCD,=CL15'AMS'                                                
         MVC   SORDDLN,IN_ORDDL                                                 
         MVC   SEIXSTA,IN_EIXST                                                 
*                                                                               
         CLI   ACTNFLAG,CDNEWSYS   ADDING THIS SYSTEM?                          
         BNE   MKSTA05             NO, EFFECTIVE DATE SHOULD BE CLEARED         
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(0,TODAYDTE)  OTHERWISE IT SHOULD BE           
         SR    R0,R0                                                            
MKSTA00  GOTO1 ADDAY,DMCB,TODAYDTE,TODAYDTE,1     3 DAYS FROM TODAY             
         GOTO1 GETDAY,DMCB,TODAYDTE,DUB                                         
         CLI   DMCB,6                                                           
         BNL   MKSTA00                                                          
         AH    R0,=H'1'                                                         
         CH    R0,=H'3'                                                         
         BL    MKSTA00                                                          
*                                                                               
         GOTO1 DATCON,DMCB,(0,TODAYDTE),(3,SEFFDATE)                            
*                                                                               
MKSTA05  DS    0H                                                               
*                                                                               
         BAS   RE,CABLENW          NEW FIELDS: SCBL24 & SCBLSEQ                 
*                                                                               
******** ZICM  R3,STAKLEN,2                                                     
******** GOTO1 PRNTBL,DMCB,=C'MY TEST ',(R6),C'DUMP',(R3),=C'1D'                
         XC    WKTOP24,WKTOP24                                                  
         XC    WKCBLSEQ,WKCBLSEQ                                                
         XC    TOP24,TOP24                                                      
         XC    CBLSEQ,CBLSEQ                                                    
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
         BNZ   MKSTA10                                                          
         LA    R1,=CL20'CHANGING STATION REC'                                   
         ST    R1,DMCB+8                                                        
         LA    R1,20                                                            
         ST    R1,DMCB+12                                                       
*                                                                               
MKSTA10  GOTO1 MYTRACE,DMCB,AREC,SCBLSQNQ,,,0                                   
*******  GOTO1 MYTRACE,DMCB,AREC,STANCLNQ,,,0                                   
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
**** DEBUG CODE ****                                                            
*                                                                               
*        CP    COUNTREC,=PL8'50'                                                
*        BNL   SKIPPRT                                                          
*        AP    COUNTREC,=P'1'                                                   
*        BAS   RE,DUMPREC                                                       
SKIPPRT  DS    0H                                                               
*                                                                               
**** END DEBUG CODE ****                                                        
*                                                                               
MKSTAX   B     XIT                                                              
*                                                                               
***********************************************************************         
* THIS ROUTINE FILLS IN 2 NEW FIELDS THAT ARE ADDED TO SPGENSTA.                
* THE 2 FIELDS ARE SCBL24 AND SCBLSEQ                                           
***********************************************************************         
*                                                                               
CABLENW  NTR1                                                                   
*                                                                               
         XC    SCBL24,SCBL24                                                    
         XC    SCBLSEQ,SCBLSEQ                                                  
*                                                                               
*        MVC   SCBL24,TOP24                                                     
         OC    SCBL24,WKTOP24                                                   
*                                                                               
         LA    R4,WKCBLSEQ                                                      
         LA    R5,CBLSEQ                                                        
         LA    R1,CBLSEQ+L'CBLSEQ                                               
*                                                                               
         LA    R0,WKCBLSEQ+L'WKCBLSEQ                                           
CABLE10  DS    0H                                                               
         CR    R4,R0                                                            
         BNL   CABLE100            NO MORE CABLE SEQ TO BE ADDED                
         CLC   =XL2'0',0(R4)                                                    
         BE    CABLE100                                                         
*                                                                               
CABLE30  DS    0H                                                               
         CR    R5,R1                                                            
         BNL   CABLE40             NOT FOUND OR END OF LIST REACHED?            
         CLC   =XL2'0',0(R5)                                                    
         BE    CABLE40                                                          
         CLC   0(2,R5),0(R4)                                                    
         BE    CABLE50             CABLE SEQ ALREADY EXIST                      
         LA    R5,2(R5)                                                         
         B     CABLE30                                                          
*                                                                               
CABLE40  MVC   0(2,R5),0(R4)       INSERT THE NEW CABLE SEQ                     
*                                                                               
CABLE50  LA    R5,CBLSEQ                                                        
         LA    R1,CBLSEQ+L'CBLSEQ                                               
         LA    R4,2(R4)                                                         
         B     CABLE10                                                          
*                                                                               
CABLE100 MVC   SCBLSEQ,CBLSEQ                                                   
         MVC   STAKLEN(2),=Y(SCBLSQNQ)                                          
*                                                                               
GENX     XIT1                                                                   
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
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
         MVC   AREC,AREC2          USE SECOND AREA TO READ IN PASSIVE           
         L     R6,AREC2                                                         
         XC    KEY,KEY             SET THE STATION RECORD KEY                   
         LA    R4,KEY                                                           
         USING STAKEY,R4                                                        
         MVI   STNKTYPE,C'N'                                                    
         MVC   STNKAGY,AGENCY                                                   
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
         L     R6,AREC             SET THE STATION RECORD                       
         USING STAREC,R6                                                        
         XC    0(20,R6),0(R6)      CLEAR THE RECORD FIRST                       
         MVI   STNKTYPE,C'N'                                                    
         MVC   STNKAGY,AGENCY                                                   
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
         DROP  R6                                                               
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
         L     R6,AREC                                                          
         XC    KEY,KEY             SET THE STATION RECORD KEY                   
         LA    R4,KEY                                                           
         USING ADDRKEY,R4                                                       
         MVI   ADDKTYPE,C'A'                                                    
         MVI   ADDKMED,C'T'                                                     
         MVC   ADDKCALL,SVDHDEND                                                
         MVC   ADDKAGY,AGENCY                                                   
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
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE CREATES A NEW STATION ADDRESS RECORD.                            
*                                                                               
* ON ENTRY:    SVDHDEND            CABLE HEADEND                                
*              BITFLAG2  (X'80')   ON=ADD RECORD, OFF=WRITE RECORD              
***********************************************************************         
*                                                                               
MKSTAADD NTR1                                                                   
         L     R6,AREC             SET THE STATION RECORD                       
         USING ADDRREC,R6                                                       
         XC    0(ADRRECLQ,R6),0(R6)   CLEAR THE RECORD FIRST                    
         MVI   ADDKTYPE,C'A'                                                    
         MVI   ADDKMED,C'T'                                                     
         MVC   ADDKCALL,SVDHDEND                                                
         MVC   ADDKAGY,AGENCY                                                   
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
         DROP  R6                                                               
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
         L     R6,AREC                                                          
         XC    KEY,KEY             SET THE STATION RECORD KEY                   
         LA    R4,KEY                                                           
         USING CBLKEY,R4                                                        
         MVI   CBLKTYPE,C'Y'                                                    
         MVI   CBLKMED,C'T'                                                     
         MVC   CBLKCALL,SVDHDEND                                                
         MVC   CBLKAGY,AGENCY                                                   
         MVI   CBLKCLT,C'0'                                                     
         MVC   CBLKCLT+1(L'CBLKCLT-1+L'CBLKFIL2),CBLKCLT                        
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
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE CREATES A NEW CABLE DATA RECORD                                  
*                                                                               
* ON ENTRY:    SVDHDEND            CABLE HEADEND                                
*              BITFLAG2  (X'80')   ON=ADD RECORD, OFF=WRITE RECORD              
***********************************************************************         
*                                                                               
MKCBLREC NTR1                                                                   
         L     R6,AREC             SET THE STATION RECORD                       
         USING CBLREC,R6                                                        
         XC    0(CBLLNQ,R6),0(R6)  CLEAR THE RECORD FIRST                       
         MVI   CBLKTYPE,C'Y'                                                    
         MVI   CBLKMED,C'T'                                                     
         MVC   CBLKCALL,SVDHDEND                                                
         MVC   CBLKAGY,AGENCY                                                   
         MVI   CBLKCLT,C'0'                                                     
         MVC   CBLKCLT+1(L'CBLKCLT-1+L'CBLKFIL2),CBLKCLT                        
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
         BNZ   MKCBL10                                                          
         LA    R1,=CL28'CHANGING CABLE DATA REC'                                
         ST    R1,DMCB+8                                                        
         LA    R1,28                                                            
         ST    R1,DMCB+12                                                       
*                                                                               
MKCBL10  GOTO1 MYTRACE,DMCB,AREC,CBLLNQ,,,0                                     
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
         DROP  R6                                                               
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
         XC    KEY,KEY             SET THE STATION RECORD KEY                   
         LA    R4,KEY                                                           
         USING STADDKEY,R4                                                      
         MVC   STAKID,=X'0A28'                                                  
         MVC   STAKAM,BAGYMD                                                    
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
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE CREATES A NEW TRAFFIC ADDRESS RECORD                             
*                                                                               
* ON ENTRY:    SVDHDEND            CABLE HEADEND                                
***********************************************************************         
*                                                                               
MKTRAADD NTR1                                                                   
         L     R6,AREC             SET THE STATION RECORD                       
         USING STARECD,R6                                                       
*****    XC    0(256,R6),0(R6)     CLEAR THE RECORD FIRST                       
         L     RE,AREC             THE ABOVE XC WAS NOT ENOUGH                  
         LHI   RF,L'SPOTREC                                                     
         XCEFL                                                                  
*                                                                               
         MVC   STAKID,=X'0A28'                                                  
         MVC   STAKAM,BAGYMD                                                    
         MVC   STAKSTA,SVDHDEND                                                 
*                                                                               
         MVC   STARECD+13(2),=Y(STARECX-STARECD)                                
         MVC   STAAGYA,AGENCY                                                   
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
         MVC   STAADDID,AGENCY                                                  
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
         EJECT                                                                  
***********************************************************************         
* STATION CALLS                                                                 
***********************************************************************         
STAHIGHD OI    DMINBTS,X'08'                                                    
STAHIGH  MVC   COMMAND,DMRDHI                                                   
         B     STAFILE                                                          
*                                                                               
STAADD   CLI   RCWRITE,C'Y'                                                     
         BNE   DONTWRIT                                                         
         MVC   COMMAND,DMADD                                                    
         B     STAFILE                                                          
*                                                                               
STAWRT   CLI   RCWRITE,C'Y'                                                     
         BNE   DONTWRIT                                                         
         MVC   COMMAND,DMWRT                                                    
*                                                                               
STAFILE  ST    RE,SVDREGRE                                                      
         GOTO1 DATAMGR,DMCB,(DMINBTS,COMMAND),STATION,KEY,AREC                  
         MVI   DMINBTS,0           ALWAYS CLEAR OUT DMINBTS                     
         L     RE,SVDREGRE                                                      
         BR    RE                                                               
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
* TRFFIL CALLS                                                                  
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
TRFILE   ST    RE,SVDREGRE                                                      
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
DONTWRIT MVI   DMCB+8,0            MAKE IT SEEM LIKE WE HAVE NO ERROR           
         BR    RE                                                               
         EJECT                                                                  
*=================================================================*             
* TRACE DATA BLOCK                                                              
*                                                                               
*        PARAMETER 1 - A(DATA)                                                  
*        PARAMETER 2 - L(DATA)  OR  ZERO FOR ELEMENTAL RECORD                   
*        PARAMETER 3 - A(LABEL) OR  ZERO FOR NO LABEL                           
*        PARAMETER 4 - L(LABEL) IF PARM 3 IS NOT ZERO                           
*                                                                               
*=================================================================*             
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
         EJECT                                                                  
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
DUMPREC  NTR1                                                                   
         L     R2,AREC                                                          
         SR    R5,R5                                                            
         ICM   R5,3,15(R2)                                                      
         GOTO1 PRNTBL,DMCB,=CL20'*DUMP*',(R2),C'DUMP',(R5),=C'2D'               
         B     XIT                                                              
***********************************************************************         
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* VARIABLES AND STUFF                                                           
***********************************************************************         
CODTABLE DS    0XL2                                                             
         DC    C'N',AL1(CDNEWSYS)      NEW SYSTEM                               
         DC    X'00'                                                            
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SET THE NETWORK BITS AND BYTES                                                
***********************************************************************         
SETNTWRK NTR1  BASE=*,LABEL=*                                                   
*****                                                                           
* NETWORKS TO BE IGNORED AS PER CHRIS O'CONNOR                                  
*****                                                                           
         CLC   IN_NWKCD,=CL4'CBN'   OLD FAMILY CHANNEL                          
         BE    STNETNO                                                          
         CLC   IN_NWKCD,=CL4'CMT'                                               
         BE    STNETNO                                                          
*        CLC   IN_NWKCD,=CL4'LO'    LOCAL ORIGIN BACK AGAIN AS PER              
*        BE    STNETNO                  MO EFFRON OF ADVANCED MEDIA SYS         
*****                                                                           
* HARDCODE CHANGES   (ALL OTHERS DROP LAST LETTER)                              
*****                                                                           
         LA    R1,NTWKCONV         NETWORK CODE CONVERSION                      
STNET05  CLI   0(R1),0                                                          
         BE    STNET10                                                          
         CLC   IN_NWKCD,0(R1)                                                   
         BNE   *+14                                                             
         MVC   IN_NWKCD,4(R1)                                                   
         B     STNET10                                                          
         LA    R1,8(R1)                                                         
         B     STNET05                                                          
*                                                                               
STNET10  L     RF,ACABLTAB         RF = A(CABLE NETWORK LISTING)                
         SR    R4,R4                                                            
*                                                                               
STNET20  CLI   0(RF),X'FF'         END OF CABLE NETWORK LISTING?                
         BE    STNETNO             YES, NETWORK CODE NOT IN OUR LISTING         
*                                                                               
         IC    R4,5(RF)            NETWORK CODE SAME AS THIS ENTRY?             
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RF),IN_NWKCD                                                 
         BE    STNET30             YES                                          
*                                                                               
         AHI   RF,7                NO, CHECK NEXT ENTRY                         
         B     STNET20                                                          
*                                                                               
STNET30  DS    0H                                                               
*                                  BUILDING TOP 24 NETWORKS                     
         MVC   HALF,3(RF)                                                       
         L     R3,ACABLTAB                                                      
         L     R4,=X'01000000'     SET INITIAL VALUE                            
*                                                                               
*****STNET40  LA    R0,256              SET LOOP COUNT                          
*                                                                               
STNET45  TM    6(R3),X'40'         TEST TOP 24                                  
         BZ    STNET50                                                          
         SRL   R4,1                POSITION TO NEXT BIT                         
         CLC   HALF,3(R3)         SAME AS THAT OF INPUT DATA?                   
         BNE   STNET55                                                          
*                                                                               
         ST    R4,FULL             SAVE CURRENT BIT                             
         OC    WKTOP24,FULL+1      'OR' INTO PREVIOUS                           
         B     STNETYES                                                         
*                                  BUILDING CABLE NETWORKS SEQ                  
STNET50  CLC   HALF,3(R3)          SAME AS THAT OF INPUT DATA?                  
         BNE   STNET55                                                          
         LA    RE,WKCBLSEQ                                                      
STNET52  CLC   0(2,RE),=XL2'0'                                                  
         BE    *+12                                                             
         LA    RE,2(RE)                                                         
         B     STNET52                                                          
         MVC   0(2,RE),HALF                                                     
         B     STNETYES                                                         
*                                                                               
STNET55  AHI   R3,7                NEXT ITEM IN CABLE NETWORK TABLE             
*****    BCT   R0,STNET45                                                       
         B     STNET45                                                          
*                                                                               
         DC    H'0'                DEATH, ONLY IF END OF TABLE REACHED          
*                                                                               
STNETYES J     YES                                                              
*                                                                               
STNETNO  J     NO                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
FILEIN   DCB   DDNAME=CBLTEST,MACRF=GM,DSORG=PS,RECFM=VB,BLKSIZE=4096, +        
               LRECL=4004,EODAD=NOMORE                                          
*&&DO                                                                           
FILEOUT  DCB   DDNAME=TOSTATN,MACRF=PM,DSORG=PS,RECFM=VB,BLKSIZE=9500, +        
               LRECL=4004,BUFNO=2                                               
*&&                                                                             
*                                                                               
AINPTLIN DS    A                   A(INPUT LINE)                                
AREC1    DS    A                   A(FIRST RECORD AREA)                         
AREC2    DS    A                   A(SECOND RECORD AREA)                        
ACABLTAB DS    A                   A(CABLE NETWORKS TABLE)                      
*                                                                               
SVDREGRE DS    A                   SAVED REGISTER RE                            
*                                                                               
COUNTER1 DS    PL8                 COUNTER OF STATION RECORDS PROCESSED         
COUNTER2 DS    PL8                 COUNTER OF PASSIVE STATION RECORDS           
COUNTER3 DS    PL8                 COUNTER OF ADDRESS RECORDS PROCESSED         
COUNTER4 DS    PL8                 COUNTER OF CABLE RECORDS PROCESSED           
COUNTER5 DS    PL8                 COUNTER OF TRAFFIC RECORDS PROCESSED         
COUNTREC DS    PL8                 COUNTER OF TRAFFIC RECORDS PROCESSED         
*                                                                               
BITFLAG1 DS    XL1                 FIRST SET OF BIT FLAGS                       
B1DBLQTE EQU   X'80'                   WE FOUND A DOUBLE QUOTE ALREADY          
B1COMMA  EQU   X'40'                   WE FOUND A COMMA ALREADY                 
B1NETWRK EQU   X'20'                   GOT NETWORK DATA                         
B1ACTION EQU   X'10'                   GOT ACTION CODE DATA                     
*                                                                               
BITFLAG2 DS    XL1                 SECOND SET OF BIT FLAGS                      
B2ADDING EQU   X'80'                   ADD RECORD (0=WRITE RECORD)              
*                                                                               
ACTNFLAG DS    XL1                 SAME BIT DEFINITION AS CODEFLAG              
CODEFLAG DS    XL1                 ACTION CODE BIT FLAG                         
CDNEWSYS EQU   X'80'                   'N' - NEW SYSTEM                         
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
IN_PYREP DS    CL1                 PAYING REP OF N=NCA , C=CNI                  
IN_ACTCD DS    CL1                 ACTION CODE                                  
IN_TPEDL DS    CL50                TAPE DEADLINE INFO                           
IN_ORDDL DS    CL50                ORDER DEADLINE INFO                          
IN_GRPCD DS    CL15                GROUP CODE                                   
IN_TPESZ DS    CL4                 TAPE SIZE (COMML TYPE)                       
IN_EIXST DS    CL1                 ON ELECTRONIC INVOICING                      
IN_DATAX EQU   *                                                                
*                                                                               
ACTHDEND DS    CL5                 ACTION CABLE HEADEND (LAST USED)             
SYSHDEND DS    CL5                 SYSTEM CABLE HEADEND (LAST USED)             
CBLHDEND DS    CL5                 CABLE HEADEND (CURRENT)                      
SVDHDEND DS    CL5                 STATION CALL LETTERS                         
NUMMRKT  DS    CL4                 MKT NUM EBCDIC (BASED ON ALPHA MKT)          
TODAYDTE DS    CL6                 TODAY'S DATE EBCDIC YY/MM/DD                 
*                                                                               
TOP24    DC    XL3'00'             TOP 24 NETWORKS                              
CBLSEQ   DC    XL206'00'           CABLE SEQ                                    
WKTOP24  DC    XL3'00'             WORKING FIELD FOR TOP 24 NETWORKS            
SVTOP24  DC    XL3'00'             SAVED TOP 24 WORKING FIELD                   
WKCBLSEQ DC    XL206'00'           WORKING FIELD FOR CABLE SEQ                  
*                                                                               
ELEM     DS    CL128                                                            
       ++INCLUDE SPCBLEQUIV                                                     
       ++INCLUDE SPCBLLST                                                       
INPTLINE DS    0CL4004             INPUT LINE FROM THE FILE                     
INPTLGTH DS    XL2                     LENGTH OF DATA FOR THIS LINE             
         DS    XL2                     FOR QSAM MACRO                           
INPTDATA DS    CL4000                  DATA FOR THIS LINE                       
*                                                                               
SPOTREC  DS    CL4000                                                           
SPOTREC2 DS    CL4000                                                           
*                                                                               
*                                                                               
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
*SPGENANMK                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPGENANMK                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'043SPREPUP02N02/13/04'                                      
         END                                                                    
