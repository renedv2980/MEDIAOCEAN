*          DATA SET SPREP4D02  AT LEVEL 010 AS OF 08/09/04                      
*PHASE SP4D02A                                                                  
*INCLUDE SORTER                                                                 
*INCLUDE DLFLD                                                                  
                                                                                
*================================================================               
* QOPT2 = Y FOR ALL MEDIA REQUEST                                               
* QOPT3 = I FOR 1000 INPUT RECORDS PER MEDIA                                    
*       = O FOR 1000 OUTPUT RECORDS                                             
*================================================================               
         TITLE 'SP4D02 - DOWNLOAD STATION MASTER DATA'                          
SP4D02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SP4D02                                                         
         L     RC,=A(SP4DWORK)                                                  
         USING SP4DWORK,RC                                                      
*                                                                               
         L     RA,0(R1)                                                         
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
         USING SPWORKD,RA,R9                                                    
*                                                                               
D        USING DLCBD,DLCB                                                       
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
*                                                                               
         CLI   MODE,RUNLAST                                                     
         BNE   EXIT                                                             
                                                                                
*=================================================================              
* AT RUNLAST, PUT OUT EOR                                                       
*=================================================================              
                                                                                
         MVI   D.DLCBACT,C'R'      SET E-O-R                                    
         GOTO1 =V(DLFLD),DLCB                                                   
         J     EXIT                                                             
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
*============================================================                   
* OPEN A REPORT ON THE PRTQUE AND INITALIZE FOR DOWNLOADING                     
*============================================================                   
                                                                                
REQF     DS    0H                                                               
*                                                                               
         CLI   QOPT2,C'Y'          FOR ALL MEDIA REQUEST                        
         BNE   *+8                                                              
         MVI   QMED,C'R'           START WITH MEDIA R                           
*                                                                               
         OI    DRECCLT,X'80'       DEFAULT CLIENT TO NOT OUTPUT                 
         CLC   =C'ALL',QCLT                                                     
         BNE   *+8                                                              
         NI    DRECCLT,X'7F'       OUTPUT IF QCLT=ALL                           
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         XC    DLCB,DLCB                                                        
*                                                                               
         MVI   D.DLCBACT,C'I'      START AND INITIALIZE REPORT                  
         MVC   D.DLCBAPR,=A(BLPRINT) PRINT ROUTINE ADDRESS                      
         LA    R0,P                                                             
         ST    R0,D.DLCBAPL        PRINT LINE ADDRESS                           
         OI    D.DLCBFLG1,DLCBFXTN                                              
         MVC   D.DLCXTND(7),MAXLINE                                             
*                                                                               
         GOTO1 =V(DLFLD),DLCB                                                   
*                                                                               
         MVC   DUB(8),=CL8'T00A'   LOAD EDITOR                                  
*                                                                               
         MVI   BYTE,QEDITOR                                                     
         GOTO1 HEXOUT,DMCB,BYTE,DUB+4,1,0                                       
*                                                                               
         GOTO1 LOADER,DMCB,DUB,0                                                
         OC    4(4,R1),4(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   D.DLCBAED,4(R1)     DLFLD REQUIRES A(EDITOR)                     
         B     REQF4                                                            
         EJECT                                                                  
*===============================================================                
* ALL MEDIA REQUESTS HAVE QOPT2=Y                                               
* BECAUSE FILCON REJECTS MEDIA *                                                
*===============================================================                
                                                                                
REQF2    CLI   QMED,C'R'           ADVANCE TO NEXT MEDIA                        
         BNE   *+12                                                             
         MVI   QMED,C'T'                                                        
         B     REQF4                                                            
*                                                                               
         CLI   QMED,C'T'                                                        
         BNE   *+12                                                             
         MVI   QMED,C'N'                                                        
         B     REQF4                                                            
*                                                                               
         CLI   QMED,C'N'                                                        
         BNE   *+12                                                             
         MVI   QMED,C'X'                                                        
         B     REQF4                                                            
*                                                                               
         CLI   QMED,C'X'                                                        
         JE    EXIT                                                             
         DC    H'0'                                                             
*                                                                               
REQF4    XC    STAWORK,STAWORK     INITIALIZE STAWORK                           
         LA    R1,STAWORK                                                       
         USING STAPACKD,R1                                                      
         MVI   STAPCTRY,C'U'                                                    
         MVC   STAPAGY,QAGY                                                     
         MVC   STAPMED,QMED                                                     
         MVC   STAPACOM,ACOMFACS                                                
         DROP  R1                                                               
*                                                                               
REQF10   BRAS  RE,BLDMKTS          BUILD MARKET TABLE                           
*                                                                               
         BRAS  RE,BLDMSOS          BUILD MSO NAMES                              
*                                                                               
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD   INITIALIZE SORT               
         XC    SORTCNT,SORTCNT                                                  
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,C'S'                                                         
         MVC   KEY+1(1),QMED                                                    
*                                                                               
         L     R8,ADSTAT                                                        
         USING STARECD,R8                                                       
         GOTO1 HIGHSTA                                                          
         B     REQF14                                                           
*                                                                               
REQF12   GOTO1 SEQSTA                                                           
*                                                                               
REQF14   CLI   QOPT3,C'I'          TEST INPUT UP TO 1000 RECORDS                
         BNE   REQF16                                                           
         CLC   SORTCNT,=F'1000'                                                 
         BH    REQF100                                                          
*                                                                               
REQF16   CLC   STAKTYPE(2),KEY     SAME TYPE/MEDIA                              
         BNE   REQF100                                                          
         CLC   STAKAGY,QAGY                                                     
         BNE   REQF12                                                           
*                                                                               
         XC    SORTREC,SORTREC                                                  
         MVC   SRMED,STAKMED                                                    
         MVC   SRMKT,SMKT                                                       
*                                                                               
         MVC   SRCLT,SPACES        SET 'ALL' CLT REC TO SORT LOW                
         CLC   STAKCLT,=C'000'     TEST ALL CLIENT REC                          
         BE    REQF18              YES - ALWAYS INCLUDE                         
*                                                                               
         CLC   =C'ALL',QCLT        TEST TO INCLUDE CLT SPEC STATIONS            
         BNE   REQF12              NO - JUST SKIP IT                            
         MVC   SRCLT,STAKCLT                                                    
*                                                                               
REQF18   CLI   STAKCALL,C'0'          TEST CABLE                                
         BL    REQF20                                                           
         GOTO1 BINSRCH,MSOPAR1,STAKMED                                          
*                                                                               
         CLI   0(R1),0                TEST  FOUND                               
         BNE   REQF20                                                           
*                                                                               
         L     RE,0(R1)                                                         
         USING MSOTABD,RE                                                       
         MVC   SRMSO,MSOTABNM                                                   
         DROP  RE                                                               
*                                                                               
REQF20   MVC   SRSTA(4),STAKCALL                                                
         LA    RE,SRSTA+3          POINT TO LAST CHAR                           
         CLI   0(RE),C' '                                                       
         BNE   *+6                                                              
         BCTR  RE,0                                                             
         MVI   1(RE),C'-'                                                       
         MVC   2(1,RE),STAKCALL+4  MOVE A/F/T                                   
         MVI   3(RE),C'V'          ASSUME TV                                    
         CLI   STAKMED,C'T'                                                     
         BE    REQF22                                                           
         MVI   3(RE),C'M'                                                       
         CLI   STAKMED,C'R'                                                     
         BE    REQF22                                                           
         MVI   3(RE),C' '                                                       
*                                                                               
REQF22   MVC   SRSTANET,STAKCALL                                                
         CLI   SRSTANET+4,C'T'                                                  
         BNE   *+8                                                              
         MVI   SRSTANET+4,C' '                                                  
*                                                                               
         MVC   SRAFFIL,SNETWRK                                                  
         MVC   SRFMT,SFORMAT                                                    
         MVC   SRCHAN,SCHNL                                                     
*                                                                               
         CLI   SRSTA,C'0'          TEST CABLE                                   
         BNL   REQF30              YES                                          
*                                                                               
REQF24   GOTO1 =V(SORTER),DMCB,=C'PUT',SORTREC                                  
         L     R0,SORTCNT                                                       
         AHI   R0,1                                                             
         ST    R0,SORTCNT                                                       
         B     REQF12                                                           
         DROP  R8                                                               
         EJECT                                                                  
*==================================================================             
* FOR CABLE, NEED TO GENERATE A RECORD FOR EACH NETWORK                         
*==================================================================             
                                                                                
REQF30   BRAS  RE,GETCBNET         GET CABLE NETWORK CODE                       
*                                                                               
         LA    R4,MYCBLTAB                                                      
         ICM   R5,15,MYCBLCNT                                                   
         BZ    REQF24                                                           
*                                                                               
REQF32   MVI   SRSTA+4,C'/'                                                     
         MVC   SRSTA+5(3),0(R4)                                                 
         MVC   SRCBLNET,0(R4)                                                   
         GOTO1 =V(SORTER),DMCB,=C'PUT',SORTREC                                  
         L     R0,SORTCNT                                                       
         AHI   R0,1                                                             
         ST    R0,SORTCNT                                                       
*                                                                               
         LA    R4,3(R4)                                                         
         BCT   R5,REQF32                                                        
         B     REQF12                                                           
         EJECT                                                                  
*================================================================               
* END OF INPUT - GET OUTPUT RECORDS FROM SORT                                   
*================================================================               
                                                                                
REQF100  OC    SORTCNT,SORTCNT                                                  
         BZ    REQFX                                                            
         XC    SORTCNT,SORTCNT                                                  
*                                                                               
REQF102  GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   RE,15,4(R1)         GET RECORD ADDRESS                           
         BZ    REQFX                                                            
         MVC   SORTREC,0(RE)       ELSE MOVE TO MY STORAGE                      
*                                                                               
         MVC   THISMED,SRMED                                                    
         MVC   THISMKT,SRMKT                                                    
         MVC   THISMSO,SRMSO                                                    
         MVC   THISSTA,SRSTA                                                    
         MVC   THISCLT,SRCLT                                                    
         MVC   THISSNET(4),SRSTANET  RADIO MAYBE SHOULD BE 5                    
         MVC   THISCNET,SRCBLNET                                                
         MVC   THISAFF,SRAFFIL                                                  
         MVC   THISFMT,SRFMT                                                    
         MVC   THISCHAN,SRCHAN                                                  
*                                                                               
         MVC   THISMKNM(13),=CL13'** UNKNOWN **'                                
         GOTO1 BINSRCH,MKTPAR1,SRMED   LOOK UP MKTNAME                          
         L     RE,0(R1)                GET ENTRY ADDRESS                        
         USING MKTTABD,RE                                                       
         CLI   0(R1),0                TEST  FOUND                               
         BNE   *+10                                                             
         MVC   THISMKNM,MKTTABNM                                                
         DROP  RE                                                               
*                                                                               
         CLI   THISSTA,C'0'        TEST CABLE                                   
         BL    REQF104             NO                                           
*                                                                               
         XC    DUB,DUB                                                          
         MVC   DUB(1),SRMED                                                     
         MVC   DUB+1(4),SRSTA                                                   
*                                                                               
         MVC   THISMSO(13),=CL13'** UNKNOWN **'                                 
         GOTO1 BINSRCH,MSOPAR1,DUB     LOOK UP MSONAME                          
         L     RE,0(R1)                GET ENTRY ADDRESS                        
         USING MSOTABD,RE                                                       
         CLI   0(R1),0                TEST  FOUND                               
         BNE   *+10                                                             
         MVC   THISMSO,MSOTABNM                                                 
         DROP  RE                                                               
*                                                                               
REQF104  LA    R4,DRECTAB          POINT TO RECORD TABLE                        
         BRAS  RE,OUTPUT                                                        
*                                                                               
         L     R0,SORTCNT                                                       
         AHI   R0,1                                                             
         ST    R0,SORTCNT                                                       
*                                                                               
         CLI   QOPT3,C'O'          TEST LIMIT OUTPUT TO 1000 RECORDS            
         BNE   REQF102                                                          
         CLC   SORTCNT,=F'1000'                                                 
         BNH   REQF102                                                          
*                                                                               
REQFX    GOTO1 =V(SORTER),DMCB,=C'END'                                          
*                                                                               
         CLI   QOPT2,C'Y'          TEST ALL MEDIA REQUEST                       
         BE    REQF2                                                            
         GOTO1 AENDREQ                                                          
         EJECT                                                                  
         USING STARECD,R8                                                       
GETCBNET NTR1                                                                   
         XC    MYCBLCNT,MYCBLCNT                                                
         L     R0,=A(MYCBLTAB)                                                  
         LHI   R1,MYCBLTBX-MYCBLTAB                                             
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
* PACK STATION USING STAPACK SO NO DEATH ON ERROR                               
         LA    R1,STAWORK                                                       
         USING STAPACKD,R1                                                      
         MVI   STAPACT,C'P'                                                     
         MVC   STAPQMKT,=C'0000'                                                
         MVC   STAPQSTA,STAKCALL                                                
         MVC   STAPQNET,SPACES                                                  
         GOTO1 VSTAPACK,STAWORK                                                 
         CLI   STAPERR,0                                                        
         BNE   GETCBX              EXIT ON ERROR                                
         MVC   BMKTSTA,STAPMKST                                                 
         DROP  R1                                                               
*                                                                               
         SR    R3,R3                                                            
         ICM   R3,7,SCBL24                                                      
         SLL   R3,8                                                             
         LHI   R4,1                SET NETWORK POSITION FOR TOP24               
         L     R5,=A(MYCBLTAB)                                                  
*                                                                               
GETCB2   LTR   R3,R3               BIT ON AT THIS POSITION?                     
         BNM   GETCB4              NO                                           
                                                                                
* USE STAPACK SO NO DEATH ON ERROR                                              
                                                                                
         STC   R4,BYTE             SET NETWORK BITS IN PACKED STA               
         NI    BMKTSTA+4,X'80'     DROP ALL BUT X'80'                           
         OC    BMKTSTA+4(1),BYTE  'OR' IN THIS VALUE                            
*                                                                               
         LA    R1,STAWORK                                                       
         USING STAPACKD,R1                                                      
         MVI   STAPACT,C'U'                                                     
         MVC   STAPMKST,BMKTSTA                                                 
         GOTO1 VSTAPACK,STAWORK                                                 
         CLI   STAPERR,0                                                        
         BNE   GETCB4              IGNORE ERROR                                 
*                                                                               
         MVC   0(3,R5),STAPQNET    MOVE NETWORK TO LIST                         
         AHI   R5,3                                                             
         LA    R0,MYCBLTBX                                                      
         CR    R5,R0                                                            
         BL    *+6                                                              
         DC    H'0'                                                             
         DROP  R1                                                               
*                                                                               
GETCB4   SLL   R3,1                                                             
         AHI   R4,1                                                             
         LTR   R3,R3               TEST ANY MORE BITS                           
         BNZ   GETCB2              YES - CONTINUE                               
                                                                                
GETCB10  LA    R3,SCBLSEQ          NOW DO SEQUENCE NUMBERS                      
         LHI   R4,25               NETWORK POSITION COUNTER                     
*                                                                               
GETCB12  OC    0(2,R3),0(R3)       TEST SEQNUM PRESENT                          
         BZ    GETCB14             NO                                           
*                                                                               
         STC   R4,BYTE             SET NETWORK BITS IN PACKED STA               
         NI    BMKTSTA+4,X'80'     DROP ALL BUT X'80'                           
         OC    BMKTSTA+4(1),BYTE  'OR' IN THIS VALUE                            
*                                                                               
         LA    R1,STAWORK                                                       
         USING STAPACKD,R1                                                      
         MVI   STAPACT,C'U'                                                     
         MVC   STAPMKST,BMKTSTA                                                 
         GOTO1 VSTAPACK,STAWORK                                                 
         CLI   STAPERR,0                                                        
         BNE   GETCB14             IGNORE ERROR                                 
         MVC   0(3,R5),STAPQNET    MOVE NETWORK TO LIST                         
         AHI   R5,3                                                             
         LA    R0,MYCBLTBX                                                      
         CR    R5,R0                                                            
         BL    *+6                                                              
         DC    H'0'                                                             
         DROP  R1                                                               
*                                                                               
GETCB14  LA    R3,2(R3)            ADVANCE TO NEXT NETWORK                      
         AHI   R4,1                                                             
         LA    R0,SCBLSEQ+L'SCBLSEQ                                             
         CR    R3,R0                                                            
         BL    GETCB12                                                          
* NOW SORT NETWORKS TO ALPHA SEQ                                                
         LA    R0,MYCBLTAB                                                      
         SR    R5,R0                                                            
         JZ    EXIT                                                             
         SR    R4,R4                                                            
         D     R4,=F'3'            GIVES COUNT IN R5                            
         ST    R5,MYCBLCNT         AND SAVE                                     
*                                                                               
         GOTO1 XSORT,DMCB,(0,MYCBLTAB),(R5),3,3,0                               
*                                                                               
GETCBX   J     EXIT                                                             
         DROP  R8                                                               
         LTORG                                                                  
         EJECT                                                                  
*==============================================================                 
* BUILD TABLE OF MARKET NAMES FOR THIS AGY                                      
*==============================================================                 
                                                                                
BLDMKTS  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R0,=A(MYMKTTAB)                                                  
         L     R1,=A(MYMKTTBX-MYMKTTAB)                                         
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),QMED                                                    
         L     R8,ADMARKET                                                      
         USING MKTRECD,R8                                                       
*                                                                               
         L     R4,MKTPAR2          A(MYMKTTAB)                                  
         USING MKTTABD,R4                                                       
         SR    R5,R5               CLEAR COUNTER                                
*                                                                               
         GOTO1 HIGHMKT                                                          
         B     BLDMKT4                                                          
*                                                                               
BLDMKT2  GOTO1 SEQMKT                                                           
*                                                                               
BLDMKT4  CLC   MKTKTYPE(2),KEY     SAME TYPE/MED                                
         BNE   BLDMKTX                                                          
         CLC   MKTKAGY(2),QAGY     RIGHT AGENCY                                 
         BNE   BLDMKT2                                                          
         MVC   MKTTABMD,MKTKMED                                                 
         MVC   MKTTABMK,MKTKMKT                                                 
         MVC   MKTTABNM,MKTNAME                                                 
         LA    R1,MKTTABNM                                                      
         LHI   R0,L'MKTTABNM                                                    
         BAS   RE,STRIPIT          REMOVE INADVERTENT DELIMITERS                
         AHI   R4,L'MYMKTTAB                                                    
         BCT   R5,BLDMKT2                                                       
         DC    H'0'                                                             
*                                                                               
BLDMKTX  LPR   R5,R5                                                            
         ST    R5,MKTPAR3                                                       
         J     EXIT                                                             
         DROP  R4,R8                                                            
         LTORG                                                                  
         EJECT                                                                  
*==============================================================                 
* BUILD TABLE OF MSO NAMES FROM CABLE RECORDS FOR MEDIA T                       
*==============================================================                 
                                                                                
BLDMSOS  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         SR    R5,R5               CLEAR COUNTER                                
         CLI   QMED,C'T'                                                        
         BNE   BLDMSOX                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,C'Y'                                                         
         MVC   KEY+1(1),QMED                                                    
         L     R8,ADSTAT                                                        
         USING CBLRECD,R8                                                       
*                                                                               
         L     R4,MSOPAR2          A(MYMSOTAB)                                  
         USING MSOTABD,R4                                                       
         SR    R5,R5               CLEAR COUNTER                                
*                                                                               
         GOTO1 HIGHSTA                                                          
         B     BLDMSO4                                                          
*                                                                               
BLDMSO2  GOTO1 SEQSTA                                                           
*                                                                               
BLDMSO4  CLC   CBLKTYPE(2),KEY     SAME TYPE/MED                                
         BNE   BLDMSOX                                                          
         CLC   CBLKAGY(2),QAGY     RIGHT AGENCY                                 
         BNE   BLDMSO2                                                          
         CLC   CBLKCLT,=C'000'     TEST ALL CLIENT RECORD                       
         BNE   BLDMSO2             NO - IGNORE                                  
*                                                                               
         MVC   MSOTABMD,CBLKMED                                                 
         MVC   MSOTABCD,CBLKCALL                                                
         MVC   MSOTABNM,CSYSMSO                                                 
         LA    R1,MSOTABNM                                                      
         LHI   R0,L'MSOTABNM                                                    
         BAS   RE,STRIPIT          REMOVE INADVERTENT DELIMITERS                
         AHI   R4,L'MYMSOTAB                                                    
         BCT   R5,BLDMSO2                                                       
         DC    H'0'                                                             
*                                                                               
BLDMSOX  LPR   R5,R5                                                            
         ST    R5,MSOPAR3                                                       
         J     EXIT                                                             
         DROP  R4,R8                                                            
         LTORG                                                                  
         EJECT                                                                  
*===================================================================            
* REMOVE DELIMITERS FROM NAME AT 0(R1) FOR LEN IN (R0)                          
*===================================================================            
                                                                                
STRIPIT  LTR   R0,R0                                                            
         JP    *+6                                                              
         DC    H'0'                                                             
         CHI   R0,24                                                            
         JNH   *+6                                                              
         DC    H'0'                                                             
*                                                                               
STRIPIT2 CLC   0(1,R1),D.DLCXDELC                                               
         JE    STRIPIT4                                                         
         CLC   0(1,R1),D.DLCXEOTC                                               
         JE    STRIPIT4                                                         
         CLC   0(1,R1),D.DLCXEOTA                                               
         JE    STRIPIT4                                                         
         CLC   0(1,R1),D.DLCXEOLC                                               
         JE    STRIPIT4                                                         
         CLC   0(1,R1),D.DLCXEORC                                               
         JNE   *+8                                                              
*                                                                               
STRIPIT4 MVI   0(R1),C' '                                                       
         AHI   R1,1                                                             
         JCT   R0,STRIPIT2                                                      
         BR    RE                                                               
         EJECT                                                                  
*=============================================================*                 
* OUTPUT  DATA TO PRTQUE REPORT                                                 
*=============================================================*                 
                                                                                
OUTPUT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
OUTPUT2  ICM   RE,15,0(R4)         GET DATA ADDR                                
         BNP   OUTPUT31            IF NEGATIVE, SKIP FIELD                      
         SR    RF,RF                                                            
         IC    RF,4(R4)            GET DATA LEN                                 
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   D.DLCBFLD(0),0(RE)                                               
*                                                                               
         CLI   5(R4),C'T'          TEST TEXT                                    
         BNE   OUTPUT6                                                          
         EX    RF,*+8                                                           
         B     *+10                                                             
         OC    D.DLCBFLD(0),SPACES                                              
**NOP**  CLI   ?????,C'Y'          TEST FIXED LENGTH OUTPUT                     
**NOP**  BNE   *+10                                                             
**NOP**  MVC   D.DLCBLEN,4(R4)     FIX OUTPUT LENGTH                            
         B     OUTPUT18                                                         
*                                                                               
OUTPUT6  CLI   5(R4),C'B'          TEST BINARY                                  
         BNE   OUTPUT14                                                         
**NOP**  CLI   ?????,C'Y'          TEST FIXED LENGTH OUTPUT                     
**NOP**  BE    OUTPUT8                                                          
         MVC   D.DLCBNDP,7(R4)     SET NUMBER OF DECIMAL PLACES                 
         MVC   D.DLCBLEN,4(R4)     SET DATE LENGTH                              
         B     OUTPUT20                                                         
*                                                                               
* FIXED LEN NUMERIC OUTPUT                                                      
*                                                                               
OUTPUT8  ICM   R0,15,0(RE)         GET VALUE IN R0                              
         MVI   D.DLCBTYP,C'N'      TYPE=NUMERIC                                 
*                                                                               
         CLI   7(R4),1             TEST 1 DECIMAL                               
         BNE   OUTPUT10                                                         
         MVC   WORK(11),=X'4021202020202020204B20'                              
         CVD   R0,DUB                                                           
         ED    WORK(11),DUB+3                                                   
         MVC   D.DLCBFLD(8),WORK+3                                              
         MVI   D.DLCBLEN,8         FIX OUTPUT LEN                               
         B     OUTPUT30                                                         
*                                                                               
OUTPUT10 CLI   7(R4),2             TEST 2 DECIMAL                               
         BNE   OUTPUT13                                                         
*                                                                               
         MVC   WORK(17),=X'40212020202020202020202020204B2020'                  
         LA    R1,DUB                                                           
         LTR   R0,R0                                                            
         BNM   OUTPUT11                                                         
         MVC   WORK(17),=X'404021202020202020202020204B202060'                  
         LA    R1,DUB+1                                                         
*                                                                               
OUTPUT11 CVD   R0,DUB                                                           
         ED    WORK(17),DUB                                                     
         MVC   D.DLCBFLD(13),WORK+4                                             
         MVI   D.DLCBLEN,13                                                     
         B     OUTPUT30                                                         
*                                                                               
OUTPUT13 DC    H'0'                                                             
*                                                                               
OUTPUT14 TM    7(R4),X'01'         TEST CVD REQUIRED                            
         BZ    OUTPUT16                                                         
         ICM   R0,15,0(RE)         GET DATA VALUE                               
         CVD   R0,DUB                                                           
         LTR   R0,R0                                                            
         BM    *+8                                                              
         OI    DUB+7,X'0F'                                                      
         SLL   RF,4                SET LEN TO UNPK TO                           
         EX    RF,*+8                                                           
         B     *+10                                                             
         UNPK  D.DLCBFLD(0),DUB                                                 
         B     OUTPUT20                                                         
*                                                                               
OUTPUT16 CLI   5(R4),C'X'          TEST HEX                                     
         BNE   OUTPUT18                                                         
**NOP**  CLI   ?????,C'Y'          TEST FIXED LENGTH OUTPUT                     
**NOP**  BNE   *+10                                                             
**NOP**  MVC   D.DLCBLEN,4(R4)     FIX OUTPUT LENGTH                            
*                                                                               
OUTPUT18 CLI   6(R4),0             TEST FIELD CAN END RECORD                    
         BE    OUTPUT20            NO                                           
         CLC   D.DLCBFLD(1),6(R4)  ELSE COMPARE                                 
         BNH   OUTPUT32            AND POSSIBLY END                             
*                                                                               
OUTPUT20 MVC   D.DLCBTYP(1),5(R4)                                               
         CLI   5(R4),C'X'          TEST HEX OUTPUT                              
         BNE   *+8                                                              
         MVI   D.DLCBTYP,C'T'      TELL DLFLD IT'S TEXT                         
*                                                                               
OUTPUT30 MVI   D.DLCBACT,DLCBPUT                                                
*                                                                               
         GOTO1 =V(DLFLD),DLCB                                                   
         MVI   D.DLCXDELC,C' '     ALWAYS RESTORE TERMINATOR                    
*                                                                               
OUTPUT31 LA    R4,L'RECTAB(R4)                                                  
         CLI   0(R4),X'FF'                                                      
         BNE   OUTPUT2                                                          
*                                                                               
OUTPUT32 MVI   D.DLCBACT,DLCBEOL                                                
         GOTO1 =V(DLFLD),DLCB                                                   
*                                                                               
         MVI   D.DLCBACT,DLCBEOL                                                
         GOTO1 =V(DLFLD),DLCB                                                   
*                                                                               
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*==============================================================*                
* USER PRINT ROUTINE EXIT CALLED BY DLFLD                      *                
* ALL DATA PRINTED HERE GOES ON PAGE 2                         *                
*==============================================================*                
                                                                                
BLPRINT  NTR1  BASE=*,LABEL=*                                                   
         CLI   QOPT5,C'X'          TEST SUPPRESS OUTPUT                         
         BNE   BLPRINT2                                                         
         LHI   R0,14                                                            
         LA    R1,P                                                             
         MVC   0(132,R1),SPACES                                                 
         AHI   R1,132                                                           
         BCT   R0,*-10                                                          
         J     EXIT                                                             
*                                                                               
BLPRINT2 MVI   LINE,0              FORCE NO PAGE BREAK                          
         GOTO1 REPORT                                                           
         MVI   FORCEHED,C'N'                                                    
         MVI   FORCEMID,C'N'       OR MIDLINES !                                
         J     EXIT                                                             
         LTORG                                                                  
         DS    0D                                                               
         DC    CL8'SP4DWORK'                                                    
SP4DWORK DS    0D                                                               
*                                                                               
SORTCNT  DS    F                                                                
SORTREC  DS    CL128                                                            
         ORG   SORTREC                                                          
*                                                                               
SRMED    DS    CL1                                                              
SRMKT    DS    CL4                                                              
SRMSO    DS    CL15                FROM CABLE RECORD                            
SRSTA    DS    CL8                 3001/CNN                                     
SRCLT    DS    CL3                 CLIENT                                       
*                                                                               
SRSTANET DS    CL4                 CALL LETTERS ONLY (WABC OR 3001)             
SRCBLNET DS    CL3                 CNN                                          
SRAFFIL  DS    CL3                                                              
SRFMT    DS    CL4                                                              
SRCHAN   DS    CL4                                                              
*                                                                               
SORTRECX EQU   *                                                                
         ORG                                                                    
*                                                                               
THISMED  DS    CL1                                                              
THISMKT  DS    CL4                                                              
THISMKNM DS    CL24                                                             
THISMSO  DS    CL15                                                             
THISSTA  DS    CL8                                                              
THISCLT  DS    CL3                                                              
THISSNET DS    CL5                                                              
THISCNET DS    CL3                                                              
THISAFF  DS    CL3                                                              
THISFMT  DS    CL4                                                              
THISCHAN DS    CL4                                                              
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,28,BI,A),WORK=1 '                            
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=128 '                                  
*                                                                               
         DS    0D                                                               
DLCB     DS    XL256                                                            
         DS    0D                                                               
STAWORK  DS    XL32                                                             
*                                                                               
MAXLINE  DC    H'132'              MAX LINE WIDTH                               
DELIM    DC    C' '                FIELD DELIMITER CHR                          
EOTCHR   DC    C'"'                END OF TEXT FIELD DELIMITER                  
EOTALT   DC    C''''               END OF TEXT CHR ALTERNATE                    
EOLCHR   DC    X'5E'               END OF LINE CHAR - SEMICOLON                 
EORCHR   DC    C':'                END OF REPORT CONTROL CHR                    
SVQMED   DC    X'00'                                                            
*                                                                               
MKTPARS  DS    0D                                                               
MKTPAR1  DC    A(0)                                                             
MKTPAR2  DC    A(MYMKTTAB)         A(TABLE)                                     
MKTPAR3  DC    F'0'                RECORD COUNT                                 
MKTPAR4  DC    A(32)               RECORD LENGTH                                
MKTPAR5  DC    A(5)                KEYDSPL/KEYLEN                               
MKTPAR6  DC    A((MYMKTTBX-MYMKTTAB)/L'MYMKTTAB)                                
*                                                                               
MSOPARS  DS    0D                                                               
MSOPAR1  DC    A(0)                                                             
MSOPAR2  DC    A(MYMSOTAB)         A(TABLE)                                     
MSOPAR3  DC    F'0'                RECORD COUNT                                 
MSOPAR4  DC    A(L'MYMSOTAB)       RECORD LENGTH                                
MSOPAR5  DC    A(5)                KEYDSPL/KEYLEN                               
MSOPAR6  DC    A((MYMSOTBX-MYMSOTAB)/L'MYMSOTAB)                                
*                                                                               
MYCBLCNT DS    F                                                                
         EJECT                                                                  
* ENTRIES ARE                                                                   
* AL4(DATA)        X'80' IS NOP FIELD                                           
* AL1(L'DATA)                                                                   
* CL1'TYPE'                                                                     
* C'  '            IF NOT X'00' EOR IF FIELD NOT > THIS VALUE                   
* X'01'            CONVERT THE FIELD TO DECIMAL BEFORE WRITE                    
* OR IF TYPE=B,    LAST BYTE IS NUMBER OF DECIMAL PLACES                        
         SPACE 1                                                                
*===============================================================                
* IN FIXED LENGTH MODE, BINARY FIELDS ARE OUTPUT AT FIXED LENGTHS               
*===============================================================                
         SPACE 1                                                                
         DS    0D                                                               
RECTAB   DS    0XL8                                                             
         DC    CL8'DRECTAB*'                                                    
DRECTAB  DC    AL4(THISMED),AL1(L'THISMED),C'T',2X'00'                          
         DC    AL4(THISMKT),AL1(L'THISMKT),C'N',2X'00'                          
         DC    AL4(THISMKNM),AL1(L'THISMKNM),C'T',2X'00'                        
         DC    AL4(THISMSO),AL1(L'THISMSO),C'T',2X'00'                          
         DC    AL4(THISSTA),AL1(L'THISSTA),C'T',2X'00'                          
DRECCLT  DC    X'80',AL3(THISCLT),AL1(L'THISCLT),C'T',2X'00'                    
         DC    AL4(THISSNET),AL1(L'THISSNET),C'T',2X'00'                        
         DC    AL4(THISCNET),AL1(L'THISCNET),C'T',2X'00'                        
         DC    AL4(THISAFF),AL1(L'THISAFF),C'T',2X'00'                          
         DC    AL4(THISFMT),AL1(L'THISFMT),C'T',2X'00'                          
         DC    AL4(THISCHAN),AL1(L'THISCHAN),C'T',2X'00'                        
         DC    X'FF'                                                            
                                                                                
         DS    0D                                                               
         DC    CL8'MYCBLTAB'                                                    
MYCBLTAB DS    256CL3                                                           
MYCBLTBX EQU   *                                                                
*                                                                               
         DS    CL8'*MKTTAB*'                                                    
MYMKTTAB DS    5000CL32            MEDIA(1)/MKTCODE(4)/MKTNAME(24)              
MYMKTTBX EQU   *                                                                
*                                                                               
MKTTABD  DSECT                                                                  
MKTTABMD DS    CL1                                                              
MKTTABMK DS    CL4                                                              
MKTTABNM DS    CL24                                                             
         DS    CL3                                                              
*                                                                               
SP4D02   CSECT                                                                  
*                                                                               
         DS    CL8'*MSOTAB*'                                                    
MYMSOTAB DS    8000CL20                                                         
MYMSOTBX EQU   *                                                                
*                                                                               
MSOTABD  DSECT                                                                  
MSOTABMD DS    CL1                                                              
MSOTABCD DS    CL4                                                              
MSOTABNM DS    CL15                                                             
*                                                                               
SP4D02   CSECT                                                                  
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
CBLRECD  DSECT                                                                  
       ++INCLUDE SPGENCBL                                                       
       ++INCLUDE DDDLCB                                                         
       ++INCLUDE DDCOREQUS                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010SPREP4D02 08/09/04'                                      
         END                                                                    
