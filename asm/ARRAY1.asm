*          DATA SET ARRAY1     AT LEVEL 045 AS OF 05/01/02                      
*PHASE ARRAY1,*                                                                 
*INCLUDR MRCARDS                                                                
*INCLUDR MRPRINT                                                                
*INCLUDR MRFILES                                                                
*INCLUDR RCYMRETC                                                               
ARY1     TITLE 'DTAB ARRAY BUILDER      DOS VERSION'                            
ARRAY    CSECT                                                                  
         PRINT NOGEN                                                            
         ENTRY IOCRDARA,IOPRTARA                                                
R0       EQU   0                                                                
R1       EQU   1                                                                
R2       EQU   2                                                                
R3       EQU   3                                                                
R4       EQU   4                                                                
R5       EQU   5                                                                
R6       EQU   6                                                                
R7       EQU   7                                                                
R8       EQU   8                                                                
R9       EQU   9                                                                
RA       EQU   10                                                               
RB       EQU   11                                                               
RC       EQU   12                                                               
RD       EQU   13                                                               
RE       EQU   14                                                               
RF       EQU   15                                                               
ARRAYX   BASR  RB,R0                                                            
         BCTR  RB,R0                                                            
         BCTR  RB,R0                                                            
         USING *-6,RB,R8                                                        
         B     *+12                                                             
         DC    CL8'ARRAY'                                                       
         LA    R8,2048(RB)                                                      
         LA    R8,2048(R8)                                                      
         LA    R3,0                OPEN CARD DISK PRINT                         
         EXTRN READER,PRINTER                                                   
*     *     $$$$$$ START SYSTEM DEPENDENT OPERATION $$$$$$                      
         OPEN  PRINTER,READER                                                   
         MVCOM 23,1,FAKEUPSI                                                    
*     *     $$$$$$   END SYSTEM DEPENDENT OPERATION $$$$$$                      
         LA    RC,OPEN2                                                         
         MVI   IOPRTARA,X'8B'                                                   
         CALL  IOPRINT                                                          
OPEN2    LA    R3,0                                                             
         MVI   PRTLN,C' '                                                       
         MVC   PRTLN+1(L'PRTLN-1),PRTLN                                         
         LA    R4,DWPARM                                                        
         LA    RC,READCD1                                                       
         CALL  IOFILEC             WORK DISK OPEN FOR OUTPUT                    
* PRINT CONTROL CHARS USED IN THIS PGM-                                         
* HEX CONFIG-                                                                   
* 09=1SPACE AFTER PRINT.                                                        
* 11=2 SPACES AFTER PRINT.                                                      
* 89=SKIP TO CHANNEL 1 AFTER PRINT.                                             
* 8B=IMMEDIATE SKIP TO CHANNEL 1.                                               
         EJECT                                                                  
*                           GO TO GET CARD ROUTINE                              
READCD1  BAS   RA,INSTRDIN                                                      
         CLC   CCODE(5),=C'$DTAB'     OVERAL CC.                                
         BE    OVALLC                                                           
         CLC   CCODE(5),=C'$DMSG'       MESSAGE CARD                            
         BE    MSGCD                                                            
         CLI   CCODE,C'F'   OVERALL FILTER.                                     
         BE    OVALLF                                                           
         CLI   CCODE,C'A'   ARRAY CARD.                                         
         BE    ARRCD                                                            
         CLC   CARDIN(3),=C'END'                                                
         BE    ENDFILE                                                          
         MVC   0(5,R6),=C'AGONE'                                                
         B     MVCD                                                             
OVALLC   MVC   OCCARD,CARDIN                                                    
         BAS   RA,PTCD                                                          
         CLI   OCCARD+79,C'R'                                                   
         BNE   NORJE                                                            
         OI    FAKEUPSI,X'08'                                                   
NORJE    CLI   OCTORC,C'T'  TAPE?                                               
         BNE   NOTPIN          NO.                                              
         LA    R3,0                                                             
         LA    R4,TRPARM                                                        
         LA    RC,NOTPIN2                                                       
         CALL  IOFILEA                                                          
NOTPIN   OI    FAKEUPSI,X'80'   TURN ON DISK SWITCH NOW                         
         B     CHKCB                                                            
NOTPIN2  MVI   SWITCH,C'T'  YES.                                                
CHKCB    MVI   OSW,C'1'     INDICATED PRESENCE.                                 
*     *     $$$$$$ START SYSTEM DEPENDENT OPERATION $$$$$$                      
         MVCOM 23,1,FAKEUPSI                                                    
*     *     $$$$$$   END SYSTEM DEPENDENT OPERATION $$$$$$                      
         CLC   CARDIN+26(2),=C'CB'                                              
         BNE   READCD1                                                          
         OI    CBSW,B'00000001'                                                 
         MVC   TRLEN,=X'00A0'                                                   
         MVC   DWLEN,=X'00A0'                                                   
         MVC   DRLEN,=X'00A0'                                                   
         MVI   SWITCH,C'T'         MAKE CB CARDS LOOK LIKE TAPE ALSO            
         B     READCD1                                                          
OVALLF   MVC   OFILTERC,CARDIN                                                  
         BAS   RA,PTCD                                                          
         MVI   FSW,C'1'  INDICATEES PRESENCE OF OFILTER CARD.                   
         B     READCD1                                                          
ARRCD    CLI   ASW,C'1'     FIRST ARRAY CARD?                                   
         BE    *+8          NO.                                                 
         LA    R6,ARTBL     YES, START AT FIRST TBL ENTRY.                      
         BAS   RA,PTCD                                                          
         C     R6,=A(ARTBL+2120) TABLE FULL?                                    
         BNL    TOOMANY     YES.                                                
         MVC   0(53,R6),CARDIN  NO, STORE ELEMENT,                              
         LA    R6,53(R6)   UP R6 TO NEXT ELEMENT.                               
         MVI   ASW,C'1'    INDICATE PRESENCE.                                   
         B     READCD1                                                          
TOOMANY  MVC   PRTLN(L'ERR01),ERR01                                             
         B     EOJOBX                                                           
ENDFILE  B     GDARRAY                                                          
GDARRAY  MVC   0(5,R6),=C'AGONE'                                                
         CLC   OCID,=C'$DTAB'                                                   
         BE    NOMORE                                                           
         MVC   PRTLN(13),=C'NO $DTAB CARD'                                      
         B     EOJOBX                                                           
MSGCD    MVC   DMSG,CCODE+5                                                     
         BAS   RA,PTCD                                                          
         B     READCD1                                                          
         SPACE 2                                                                
PTCD     MVC   IOPRTARA+1(80),CARDIN                                            
         MVI   IOPRTARA,X'09'                                                   
         LR    RC,RA                                                            
         CALL  IOPRINT                                                          
         EJECT                                                                  
EOJOBX   MVC   IOPRTARA,PRINT                                                   
         LA    RC,BEOJ                                                          
         CALL  IOPRINT                                                          
*     *     $$$$$$ START SYSTEM DEPENDENT OPERATION $$$$$$                      
BEOJ     EOJ                                                                    
*     *     $$$$$$   END SYSTEM DEPENDENT OPERATION $$$$$$                      
*                                                                               
NOMORE   CLI   OCCARD+13,C'D'                                                   
         BNE   MVCD2                                                            
         DC    H'0'                                                             
MVCD     MVC   DISKIO,CARDIN         MOVE CD TO DSK & WRITE                     
         BAS   RA,RITDSK                                                        
MVCD2    BAS   RA,READIN                                                        
         CLC   CARDIN(3),=X'5B4040'                                             
         BE    DUNDISK                                                          
         B     MVCD                                                             
DUNDISK  MVC   DISKIO(3),=C'END'                                                
         BAS   RA,RITDSK                                                        
         LA    R3,8                                                             
         LA    R4,DWPARM                                                        
         LA    RC,CHGUPSI                                                       
         CALL  IOFILEC             CLOSE WORK DISK AS OUTPUT                    
CHGUPSI  TM    23(1),B'10000000'   FOR DISK IOFILEA                             
         BO    NOCLSA                                                           
         LA    R3,8                                                             
         LA    R4,TRPARM                                                        
         LA    RC,NOCLSA                                                        
         CALL  IOFILEA                                                          
NOCLSA   OI    FAKEUPSI,B'10000000'       FOR DISK                              
*     *     $$$$$$ START SYSTEM DEPENDENT OPERATION $$$$$$                      
         MVCOM 23,1,FAKEUPSI                                                    
*     *     $$$$$$   END SYSTEM DEPENDENT OPERATION $$$$$$                      
         EJECT                                                                  
PROCESS  EQU   *                                                                
*                   THE FIRST THING TO DO IS SET UP THE REGISTERS FOR           
*                   THE EX'S AND CHECK NNUMERIC FIELDS IN CONTROL CARDS         
*                   (OVERALL). THEN IT WILL LOOP THRU THE FIRST TO THE          
*                   LAST ARRAY CARDS CHECKING NUMERICS, ETC. UNTIL THEY         
*                   ARE ALL GONE. EACH NEW ARRAY WILL CAUSE THE CARDS           
*                   ON DISK TO BE CLOSED THEN OPENED.                           
*                                                                               
         CLI   FSW,C'1'                                                         
         BNE   NOOF1                                                            
         MVC   WORK(4),=4X'F0'    CHECK OVERALL FILTER FOR NUMERICS.            
         MVZ   WORK(4),OFCOL      COL & LENGTH.                                 
         CLC   WORK(4),=4X'F0'                                                  
         BNE   NOTNUM                                                           
         PACK  PACKDW,OFCOL       PACK START COL.                               
         CVB   R6,PACKDW          CONV. TO BIN.                                 
         XI    CBSW,B'00000010'                                                 
         BAS   RA,SHIFTST                                                       
         BCTR  R6,0               SUBT. 1.                                      
         ST    R6,OFSTCOLR                                                      
         LA    R5,CARDIN                                                        
         AR    R6,R5                                                            
         ST    R6,OFSTCOL         STORE FOR USE.                                
         PACK  PACKDW,OFLNGTH     SAME THING                                    
         CVB   R6,PACKDW          WITH LENGTH                                   
         BAS   RA,SHIFTST                                                       
         BCTR  R6,0               EXCEPT ALSO 00 ENTRIES ARE NG SO,             
         LTR   R6,R6          SET COND CODE - FOR NEG. # TEST.                  
         BC    4,LENERR       LENERR ROUTINE PRINTS LENGTH ERR, BOMBS,          
         ST    R6,OFLENGTH                    PRINTS CARD.                      
         ST    R6,OFLDBL                                                        
         SRA   R6,1                                                             
         ST    R6,OFLSNGL                                                       
NOOF1    LA    RA,ARTBL                                                         
         ST    RA,SVRA                                                          
         CLI   OCTORC,C'T'                                                      
         BNE   BEGIN                                                            
         MVC   WORK(2),=2X'F0'                                                  
         MVZ   WORK(2),OCNBLKS                                                  
         CLC   WORK(2),=2X'F0'                                                  
         BNE   NOTNUM3                                                          
         PACK  PACKDW,OCNBLKS                                                   
         CVB   R6,PACKDW                                                        
         ST    R6,TPNOBLKS                                                      
         CLI   FSW,C'1'                                                         
         BNE   BEGIN                                                            
         MVC   WORK(2),=2X'F0'                                                  
         MVZ   WORK(2),OFBLOCK                                                  
         CLC   WORK(2),=2X'F0'                                                  
         BNE   NOTNUM4                                                          
         PACK  PACKDW,OFBLOCK                                                   
         CVB   R6,PACKDW                                                        
         ST    R6,OFBLK                                                         
         EJECT                                                                  
BEGIN    L     RA,SVRA                                                          
         MVC   ARRAYCD,0(RA)                                                    
         CLC   ARRAYCD(5),=C'AGONE'                                             
         BE    EOJOBN                                                           
         PACK  PACKDW,AREST   RESP NO START COL.                                
         MVC   WORK(3),=3X'F0'                                                  
         MVZ   WORK(3),AREST                                                    
         CLC   WORK(3),=3X'F0'                                                  
         BNE   NOTNUMA                                                          
         CVB   R6,PACKDW                                                        
         XI    CBSW,B'00000010'                                                 
         BAS   RA,SHIFTST                                                       
         BCTR  R6,0                                                             
         ST    R6,RSTCOLR                                                       
         LTR   R6,R6                                                            
         BC    4,NOTNUMA                                                        
         LA    R5,CARDIN                                                        
         AR    R6,R5                                                            
         ST    R6,RSTCOL                                    PRINTS CARD         
         PACK  PACKDW,ARESLN  RESP. NO LENGTH.                                  
         CVB   R6,PACKDW                                                        
         ST    R6,RLENGTHP     REAL LENGTH FOR INDEXING                         
         BAS   RA,SHIFTST                                                       
         BCTR  R6,0                                                             
         LTR   R6,R6                                                            
         BC    4,NOTNUMA                                                        
         ST    R6,RLENGTH                                                       
         ST    R6,RLDBL                                                         
         SRA   R6,1                                                             
         ST    R6,RLSNGL                                                        
         CLI   AFILT,C'F'                                                       
         BNE   NOAF1                                                            
         PACK  PACKDW,AFST    AFILTER START COL.                                
         MVC   WORK(4),=4X'F0'                                                  
         MVZ   WORK(4),AFST                                                     
         CLC   WORK(4),=4X'F0'                                                  
         BNE   NOTNUMA                                                          
         CVB   R6,PACKDW                                                        
         XI    CBSW,B'00000010'                                                 
         BAS   RA,SHIFTST                                                       
         BCTR  R6,0                                                             
         ST    R6,AFSTCOLR                                                      
         LTR   R6,R6                                                            
         BC    4,NOTNUMA                                                        
         LA    R5,CARDIN                                                        
         AR    R6,R5                                                            
         ST    R6,AFSTCOL                                                       
         PACK  PACKDW,AFLN    AFILTER LENGTH.                                   
         CVB   R6,PACKDW                                                        
         BAS   RA,SHIFTST                                                       
         BCTR  R6,0                                                             
         LTR   R6,R6                                                            
         BC    4,NOTNUMA                                                        
         ST    R6,AFLENGTH                                                      
         ST    R6,AFLDBL                                                        
         SRA   R6,1                                                             
         ST    R6,AFLSNGL                                                       
NOAF1    CLI   OCTORC,C'T'                                                      
         BNE   NOTPPROC                                                         
         MVC   WORK(2),=2X'F0'                                                  
         MVZ   WORK(2),AREBL                                                    
         CLC   WORK(2),=2X'F0'                                                  
         BNE   NOTNUMA                                                          
         PACK  PACKDW,AREBL                                                     
         CVB   R6,PACKDW                                                        
         ST    R6,RBLK                                                          
         CLI   AFILT,C'F'                                                       
         BNE   NOTPPROC                                                         
         MVC   WORK(2),=2X'F0'                                                  
         MVZ   WORK(2),AFBL                                                     
         CLC   WORK(2),=2X'F0'                                                  
         BNE   NOTNUMA                                                          
         PACK  PACKDW,AFBL                                                      
         CVB   R6,PACKDW                                                        
         ST    R6,AFBLK                                                         
NOTPPROC LA    R3,0                                                             
         LA    R4,DRPARM                                                        
         LA    RC,PREFLOW                                                       
         CALL  IOFILEA             OPEN WOK DISK AS INPUT                       
PREFLOW  CLI   OCCARD+26,C'C'                                                   
         BE    FLOWST                                                           
         MVC   AFLSNGL,AFLENGTH                                                 
         MVC   OFLSNGL,OFLENGTH                                                 
         MVC   RLSNGL,RLENGTH                                                   
         B     FLOWST                                                           
NOTNUM   MVC   PRTLN(L'ERR04),ERR04                                             
         B     EOJOBX                                                           
NOTNUM3  MVC   PRTLN(25),=C'NO OF BLKS IN OCC NOT NUM'                          
         B     EOJOBX                                                           
NOTNUM4  MVC   PRTLN(27),=C'OFILT BLK NO IN OFC NOT NUM'                        
         B     EOJOBX                                                           
LENERR   MVC   PRTLN(L'ERR05),ERR05                                             
         B     EOJOBX                                                           
         EJECT                                                                  
FLOWST   BAS   RA,REEDSK                                                        
         CLC   DISKIO(3),=C'END'                                                
         BE    EODATA                                                           
CONT     CLC   OFID,=C'FILTER='    TEST FOR OFILTER PRESENCE.                   
         BE    OFTEST                                                           
         B     NOFTEST                                                          
OFTEST   BAS   RA,TESTOF                                                        
         CLI   PASSFAIL,C'F'                                                    
         BE    FLOWST                                                           
NOFTEST  CLC   AFILT,=C'FILTER='   TEST FOR AFILTER PRESENCE.                   
         BNE   INITR                                                            
         BAS   RA,TESTAF                                                        
         CLI   PASSFAIL,C'F'                                                    
         BE    FLOWST                                                           
         B     INITR                                                            
* IF WE GOT THIS FAR EITHER THERE WAS NO FILTERING OR RECORD PASSED             
*       FILTER TEST(S).                                                         
INITR    EQU   *                                                                
SEARCH   LA    R6,BIGTABLE                                                      
         L     R7,RSTCOL                                                        
         L     R9,RLENGTH                                                       
         EX    R9,*+8                                                           
         B     *+10                                                             
         CLC   0(1,R7),=10C' '                                                  
         BE    FLOWST                                                           
         CLI   OCCARD+33,C'T'                                                   
         BNE   LOGIC1                                                           
         L     R1,RLENGTH                                                       
         LA    R1,1(R1)                                                         
         L     R2,RSTCOL                                                        
LOGIC0   CLI   0(R2),X'40'                                                      
         BNE   LOGIC1              GET OUT AT FIRST NON ZERO                    
         MVI   0(R2),X'F0'                                                      
         LA    R2,1(R2)                                                         
         BCT   R1,LOGIC0                                                        
LOGIC1   EX    R9,INSTA       COMP RESP TO TBL ENTRY.                           
         BE    FOUNDIT                                                          
         EX    R9,*+8                                                           
         B     *+10                                                             
         CLC   0(1,R6),=10C' '     EMPTY ENTRY?                                 
         BE    MVNEW                                                            
         A     R6,RLENGTHP                                                      
         AH    R6,=H'4'                                                         
         EX    R0,INSTD            TEST FOR BIGTABLE LIMIT                      
         BNL   BOMB                                                             
         B     LOGIC1                                                           
FOUNDIT  A     R6,RLENGTHP                                                      
         AP    0(4,R6),=P'1'  COUNT HIT                                         
         AP    TFREQ,=P'1'                                                      
         B     FLOWST                                                           
MVNEW    EX    R0,INSTD            CREATE NEW TBL ENTRY-TEST FOR LIMIT,         
         BNL   BOMB           BOMB IS ABEND FOR EXCEEDED TABLE.                 
         EX    R9,INSTB       INSERT NEW ENTRY.                                 
         A     R6,RLENGTHP                                                      
         EX    R0,INSTC       FORMAT NEW COUNTER.                               
         ST    R6,HIGHEST                                                       
         AP    0(4,R6),=P'1'  ADD 1 TO COUNT AS HIT.                            
         AP    TFREQ,=P'1'                                                      
         B     FLOWST                                                           
         SPACE 3                                                                
BOMB     MVC   PRTLN(L'ERR07),ERR07                                             
         MVC   IOPRTARA,PRINT                                                   
         LA    RC,BOMBX                                                         
         CALL  IOPRINT                                                          
BOMBX    DC    H'0'   BOMB, BIGTABLE EXCEEDED.                                  
         EJECT                                                                  
*  REPORT ROUTINE 'EODATA',   SORT AND PRINT BIGTABLE, DO HEADINGS,             
*                       SORT CLEARS OUT BIGTABLE,GO GET ANOTHER                 
*                             ARRAY ENTRY AFTER CLOSING & OPENING DATA          
*                             DISK, INCREMENTS RB TO NEXT ARRAY ENTRY           
*                             AND BRANCHES BACK TO BEGIN.                       
EODATA   LA    R6,BIGTABLE                                                      
         L     R9,RLENGTH                                                       
         EX    R9,*+8                                                           
         B     *+10                                                             
         CLC   0(1,R6),=14C' '                                                  
         BE    EMPTY                                                            
         AH    R9,=H'4'       MOVE ENTRY TO WORK1.                              
LOGIC4   EX    R9,INSTE       CLEAR OUT ENTRY                                   
         EX    R9,INSTM       AND ACCUM.                                        
LOGIC2   MVI   IND,C' '      KNOCK OUT LIVE ENTRY INDICATOR.                    
         AR    R6,R9                                                            
         LA    R6,1(R6)                                                         
RETRY    EX    R9,*+8                                                           
         B     *+10                                                             
         CLC   0(1,R6),=14C' '    CHECK IF EMPTY ENTRY                          
         BE    NEXTTRY                                                          
         B     COMPAR                                                           
NEXTTRY  AR    R6,R9                                                            
         LA    R6,1(R6)                                                         
         C     R6,HIGHEST                                                       
         BNL   LASTENT                                                          
         B     RETRY                                                            
COMPAR   MVI   IND,C'1'     TURN ON LIVE SW.                                    
         EX    R9,INSTN                                                         
         BL    SWITCHX                                                          
         B     NEXTTRY                                                          
SWITCHX  EX    R9,INSTF       MOVE WORK1 TO 2.                                  
         EX    R9,INSTE       MOVE BIGTABLE ENTRY TO WORK1.                     
         EX    R9,INSTO       MOVE WORK2 BACK TO BIG TABLE.                     
         B     RETRY                                                            
LASTENT  EQU   *    A SWEEP THRU THE TABLE AND STILL LOW.                       
         BAS   RA,PRINTIT     PRINT ENTRY.                                      
         CLI   IND,C'1'       ANY MORE LIVE ENTRIES.                            
         BNE   TOTALS         NO                                                
         MVI   IND,C' '                                                         
         LA    R6,BIGTABLE    BACK TO SQUARE ONE.                               
LOGIC3   EX    R9,*+8                                                           
         B     *+10                                                             
         CLC   0(1,R6),=14C' '     FIND FIRST LIVE ENTRY                        
         BE    UPIT                                                             
         B     LOGIC4                                                           
UPIT     AR    R6,R9                                                            
         LA    R6,1(R6)                                                         
         B     LOGIC3                                                           
PRINTIT  LA    R5,WORK1                                                         
         CP    LINES,=P'50'                                                     
         BNL   OVFLO                                                            
BACKFLO  A     R5,RLENGTHP                                                      
         EX    R0,INSTJ            MV ACCUM TO WORK ACCUM.                      
         AP    INTFREQ,ACCUM                                                    
         ZAP   DVDND,ACCUM                                                      
         MP    DVDND,=P'10000'                                                  
         DP    DVDND,TFREQ                                                      
         AP    DVDND(4),=P'5'                                                   
         MVO   DVDND(4),DVDND(3)                                                
         MVC   PRTLN+10(9),=X'402020202020204B20'                               
         ED    PRTLN+10(9),DVDND                                                
         ZAP   DVDND,INTFREQ                                                    
         MP    DVDND,=P'10000'                                                  
         DP    DVDND,TFREQ                                                      
         AP    DVDND(4),=P'5'                                                   
         MVO   DVDND(4),DVDND(3)                                                
         MVC   PRTLN+33(9),=X'402020202020204B20'                               
         ED    PRTLN+33(9),DVDND                                                
         L     R5,RLENGTH                                                       
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   PRTLN+46(1),WORK1   MOVE IN RESP NO VIA 'EX'                     
         MVC   PRTLN(8),=X'4020202020202020'                                    
         ED    PRTLN(8),ACCUM                                                   
         MVC   PRTLN+22(8),=X'4020202020202020'                                 
         ED    PRTLN+22(8),INTFREQ                                              
         MVC   IOPRTARA,PRINT                                                   
         MVI   IOPRTARA,X'09'                                                   
         LA    RC,BACKFLO2                                                      
         CALL  IOPRINT                                                          
BACKFLO2 AP    LINES,=P'1'                                                      
         MVI   PRINT,C' '                                                       
         MVC   PRINT+1(L'PRINT-1),PRINT                                         
         BR    RA                                                               
OVFLO    MVI   PASA,X'89'                                                       
         MVI   PRTLN,C' '                                                       
         MVC   PRTLN+1(L'PRTLN-1),PRTLN                                         
         MVC   IOPRTARA,PRINT                                                   
         LA    RC,OVFLO0X                                                       
         CALL  IOPRINT                                                          
OVFLO0X  MVC   IOPRTARA+1(75),DMSG                                              
         MVI   IOPRTARA,X'11'                                                   
         LA    RC,OVFLO0                                                        
         CALL  IOPRINT                                                          
OVFLO0   MVC   PRTLN+1(10),=C'DATA COLS-'                                       
         MVC   PRTLN+12(2),AREST                                                
         PACK  AREND,AREST                                                      
         PACK  ADDX,ARESLN                                                      
         AP    AREND,ADDX                                                       
         SP    AREND,=P'1'                                                      
         UNPK  AREND,AREND                                                      
         MVZ   AREND+1(1),=X'F0'                                                
         CLC   AREST,AREND                                                      
         BE    *+14                                                             
         MVC   PRTLN+15(2),AREND                                                
         MVI   PRTLN+14,C'-'                                                    
         MVC   PRTLN+19(8),=C'OFILTER-'                                         
         MVC   PRTLN+62(8),=C'AFILTER-'                                         
         MVC   PRTLN+28(2),OFCOL                                                
         MVC   PRTLN+34(26),OFARG                                               
         MVC   PRTLN+71(2),AFST                                                 
         MVC   PRTLN+77(26),AFARG                                               
         MVC   PRTLN+31(2),OFLNGTH                                              
         MVC   PRTLN+74(2),AFLN                                                 
         CLI   FSW,C'1'                                                         
         BE    *+10                                                             
         MVC   PRTLN+28(4),=C'NONE'                                             
         CLI   AFILT,C'F'                                                       
         BE    *+10                                                             
         MVC   PRTLN+71(4),=C'NONE'                                             
         MVI   PASA,X'09'                                                       
         MVC   IOPRTARA,PRINT                                                   
         LA    RC,OVFLO1A                                                       
         CALL  IOPRINT                                                          
OVFLO1A  MVI   PRTLN,C' '                                                       
         MVC   PRTLN+1(L'PRTLN-1),PRTLN                                         
         CLI   OCTORC,C'T'                                                      
         BNE   NOTPOV                                                           
         MVC   PRTLN+1(10),=C'TP BLOCKS-'                                       
         MVC   PRTLN+12(2),AREBL                                                
         MVC   PRTLN+28(2),OFBLOCK                                              
         MVC   PRTLN+52(2),AFBL                                                 
         MVI   PASA,X'11'                                                       
LARCOV   LA    RC,OVFLO2                                                        
         MVC   IOPRTARA,PRINT                                                   
         CALL  IOPRINT                                                          
NOTPOV   MVI   PASA,X'09'                                                       
         B     LARCOV                                                           
OVFLO2   MVI   PRINT,C' '                                                       
         MVC   PRINT+1(L'PRINT-1),PRINT                                         
         MVC   IOPRTARA+1(132),HFIL2                                            
         MVI   IOPRTARA,X'11'                                                   
         LA    RC,OVFLO3                                                        
         CALL  IOPRINT                                                          
OVFLO3   ZAP   LINES,=P'0'                                                      
         B     BACKFLO                                                          
TOTALS   MVC   PRTLN+6(8),=X'4020202020202020'                                  
         ED    PRTLN+6(8),TFREQ                                                 
         ZAP   TFREQ,=P'0'                                                      
         MVC   PRTLN+1(6),=C'BASE ='                                            
         MVI   PASA,X'11'                                                       
         MVC   IOPRTARA,PRINT                                                   
         LA    RC,MVIX                                                          
         CALL  IOPRINT                                                          
MVIX     MVI   PRINT,C' '                                                       
         MVC   PRINT+1(L'PRINT-1),PRINT                                         
         MVI   PASA,X'09'                                                       
         L     RA,SVRA                                                          
         LA    RA,53(RA)                                                        
         ST    RA,SVRA                                                          
         ZAP   LINES,=P'50'                                                     
         SP    TFREQ,TFREQ                                                      
         SP    INTFREQ,INTFREQ                                                  
         LA    R3,8                                                             
         LA    R4,DRPARM                                                        
         LA    RC,BEGIN                                                         
         CALL  IOFILEA             CLOSE OF WORK DISK AS INPUT                  
EMPTY    MVC   PRTLN(37),ARRAYCD                                                
         MVC   PRTLN+38(18),=C'* ARRAY IS EMPTY *'                              
         MVI   IOPRTARA,X'8B'                                                   
         LA    RC,BKEM                                                          
         CALL  IOPRINT                                                          
BKEM     MVC   IOPRTARA,PRINT                                                   
         MVI   IOPRTARA,X'11'                                                   
         CLI   OCCARD+14,C'D'                                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         LA    RC,MVIX                                                          
         CALL  IOPRINT                                                          
         EJECT                                                                  
READIN   ST    RA,STRA                                                          
         CLI   SWITCH,C'T'                                                      
         BNE   READIN2                                                          
         BAS   RA,RDTP                                                          
         L     RA,STRA                                                          
         BR    RA                                                               
READIN2  LA    R4,PARAS            READS EBCDIC CARDS.                          
         LR    RC,RA                                                            
         CALL  IOCRDRD                                                          
         DS    0F                                                               
PARAS    DC    A(CARDIN)           CARD READ PARAMS.                            
         DC    X'0050'                                                          
         DC    C' '                                                             
*                                                                               
INSTRDIN LA    R4,PARAS                                                         
         LA    RC,LTRXX                                                         
         CALL  IOCRDRD                                                          
LTRXX    CLC   CARDIN(3),=C'$  '                                                
         BNE   0(RA)                                                            
         MVC   CARDIN(3),=C'END'                                                
         BR    RA                                                               
         EJECT                                                                  
*     *     $$$$$$ START SYSTEM DEPENDENT OPERATION $$$$$$                      
EOJOBN   CLOSE PRINTER,READER                                                   
*     *     $$$$$$   END SYSTEM DEPENDENT OPERATION $$$$$$                      
EOJX     CLI   OCCARD+12,C'D'                                                   
         BNE   *+8                                                              
         DC    4C'0'                                                            
*     *     $$$$$$ START SYSTEM DEPENDENT OPERATION $$$$$$                      
         EOJ                                                                    
*     *     $$$$$$   END SYSTEM DEPENDENT OPERATION $$$$$$                      
         SPACE 3                                                                
****           TEST OF OVERALL FILTER ROUTINE                                   
TESTOF   MVI   PASSFAIL,C'F' SET FOR FAILURE                                    
         ST    R7,STR7                                                          
         LA    R7,OFARG                                                         
         L     R6,OFLENGTH                                                      
         L     R5,OFSTCOL                                                       
CHKO     EX    R6,INSTK                                                         
         BE    PASSO                                                            
         AR    R7,R6                                                            
         LA    R7,1(R7)                                                         
         CLI   0(R7),C' '                                                       
         BE    FAILO                                                            
         B     CHKO                                                             
PASSO    MVI   PASSFAIL,C'P'       SET FPR PASSAGE                              
FAILO    L     R7,STR7                                                          
         BR    RA                  EXIT FOR PASS                                
****                                                                            
*                                                                               
*                                                                               
****           TEST OF ARRAY FILTER ROUTINE                                     
TESTAF   MVI   PASSFAIL,C'F'       SET FOR FAILURE                              
         ST    R7,STR7                                                          
         LA    R7,AFARG                                                         
         L     R6,AFLENGTH                                                      
         L     R5,AFSTCOL                                                       
CHKA     EX    R6,INSTL                                                         
         BE    PASSA                                                            
         AR    R7,R6                                                            
         LA    R7,1(R7)                                                         
         CLI   0(R7),C' '                                                       
         BE    FAILA                                                            
         B     CHKA                                                             
PASSA    MVI   PASSFAIL,C'P'       SET FOR PASSAGE                              
FAILA    L     R7,STR7                                                          
         BR    RA                  EXIT AS PASS                                 
****                                                                            
*                                                                               
*                                                                               
         EJECT                                                                  
RDTP     CLI   OCTORC,C'T'                                                      
         BNE   RDTP2                                                            
         LA    R3,4                INPUT READ FR TP AS IOFILEA                  
         LA    R4,TRPARM                                                        
         LA    RC,BKTPA                                                         
         CALL  IOFILEA                                                          
BKTPA    BZ    0(RA)     GOT A RECORD OR EOF                                    
         B     EOFTP                                                            
* READ ROUTINE FOR COLBIN TAPE AND CARDS AND FOR EBCD TAPE.                     
*                                                                               
         SPACE                                                                  
RDTP2    LA    R4,TRPARM                                                        
         LA    RC,BACKTP                                                        
         CALL  IOBINRD                                                          
BACKTP   CLC   IOCRDARA(4),=X'44204420'                                         
         BE    EOFTP                                                            
         CLC   IOCRDARA(4),=X'11021102'                                         
         BE    EOFTP                                                            
         BR    RA                                                               
EOFTP    MVC   IOCRDARA(3),=X'5B4040'                                           
         BR    RA                                                               
         CLI   OCCARD+14,C'D'                                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         EJECT                                                                  
RITDSK   LA    R3,4                GEN WRITE WORK DISK ROUTINE                  
*              LRECL HAS BEEN LOADELRECL HAS BEEN LOADED INTO DWLEN             
         LA    R4,DWPARM                                                        
         LR    RC,RA                                                            
         CALL IOFILEC                                                           
*                                                                               
         EJECT                                                                  
* THE REEDSK ROUTINE FIRST GETS AN 80 C REC FOR EBCD CARD OR TP OR A            
* 160 C REC FROM COLBIN CARD OR TAPE.  THE TAPE RECORD PASSED                   
* IS A COMPOSITE OF DATA FOR THE RESP NOS AND FILTERS AND WILL LOOK THE         
* SAME AS A ONE RECORD BLOCK, IN EBCDIC.                                        
* ONLY THE REQUIRED FIELDS ARE TRANSLATED.                                      
*                                                                               
         SPACE                                                                  
REEDSK   CLI   SWITCH,C'T'         WILL ALSO BE FOR COLBIN CARDS.               
         BE    TAPEIP                                                           
REEDSK2  LA    R3,4                GEN READ WORK DISK ROUTINE                   
*                                  LRECL HAS BEEN LOADED INTO DRLEN             
         LA    R4,DRPARM                                                        
         LA    RC,LTRX                                                          
         CALL  IOFILEA                                                          
LTRX     LTR   R3,R3               TEST CC                                      
         BZ    GOT1                0=RECORD ELSE                                
*                                  END OF FILE                                  
         MVC   DISKIO(3),=C'END'                                                
GOT1     MVC   CARDIN,DISKIO                                                    
         BR    RA                                                               
*                                                                               
*                                                                               
*  TAPEIP ROUTINE REALLY READS FROM DISK BUT THE INPUT WAS ORIGINALLY           
*  FROM TAPE OR COLBIN CARDS.                                                   
*                                                                               
TAPEIP   ST    RA,STRA                                                          
         L     R9,TPNOBLKS                                                      
         LA    R6,0                                                             
         LA    RD,CARDIN                                                        
         ST    RD,LAST                                                          
         CLI   OCCARD+26,C'C'                                                   
         BNE   TAPEIP2                                                          
         MVC   RLENGTH,RLDBL                                                    
         MVC   OFLENGTH,OFLDBL                                                  
         MVC   AFLENGTH,AFLDBL                                                  
TAPEIP2  LA    R3,4                                                             
         LA    R4,DRPARM                                                        
         LA    RC,BACK1                                                         
         CALL  IOFILEA                                                          
BACK1    LTR   R3,R3                                                            
         BZ    GOTA                                                             
         MVC   DISKIO(3),=C'END'                                                
         B     TAPEX                                                            
GOTA     L     R7,TPNOBLKS                                                      
         LA    R6,1(R6)                                                         
         C     R6,OFBLK                                                         
         BNE   NOTOF                                                            
         BAS   R5,DOOF                                                          
NOTOF    C     R6,AFBLK                                                         
         BNE   NOTAF                                                            
         BAS   R5,DOAF                                                          
NOTAF    C     R6,RBLK                                                          
         BNE   NOTRES                                                           
         BAS   R5,DORES                                                         
NOTRES   BCT   R9,TAPEIP2                                                       
* AT THIS POINT A RECORD IS READY TO BE PASSED.                                 
* ALL THAT REMAINS TO DO IS TRANSLATE THE COLBIN.                               
GOTALL   CLI   OCCARD+26,C'C'                                                   
         BNE   TAPEINXT                                                         
         MVC   HWWK1,CARDIN                                                     
         CLC   OCCARD+26(6),=C'CB1130'                                          
         BE    C1130                                                            
         L     R4,RLENGTH                                                       
         A     R4,OFLENGTH                                                      
         A     R4,AFLENGTH                                                      
         LA    R4,3(R4)            PLUS 3 CAUSE EACH ARE LENGTH CODES.          
         LA    R2,HWWK1                                                         
         SRA   R4,1                FR BYTES TO HWS                              
         BAS   RA,CNCVTB                                                        
         MVC   CARDIN,HWWK1                                                     
C1130    L     R4,RLENGTH                                                       
         A     R4,OFLENGTH                                                      
         A     R4,AFLENGTH                                                      
         LA    R4,3(R4)                                                         
         LA    R2,HWWK1                                                         
         LA    R3,HWWK2                                                         
         SRA   R4,1                                                             
         BAS   RA,CNREVE                                                        
         MVC   CARDIN,HWWK2                                                     
TAPEINXT L     RA,STRA                                                          
         CLI   OCCARD+26,C'C'                                                   
         BNE   0(RA)                                                            
TAPEX    MVC   RLENGTH,RLSNGL                                                   
         MVC   OFLENGTH,OFLSNGL                                                 
         MVC   AFLENGTH,AFLSNGL                                                 
         BR    RA                  GET OUT                                      
*                                                                               
DOOF     LA    R7,DISKIO                                                        
         CLI   FSW,C'1'                                                         
         BE    GO1                                                              
         LA    R7,0                                                             
         SH    R7,=H'1'                                                         
         ST    R7,OFLENGTH                                                      
         BR    R5                                                               
GO1      A     R7,OFSTCOLR                                                      
         L     RE,LAST                                                          
STR1     ST    RE,OFSTCOL                                                       
         A     RE,OFLSNGL                                                       
         LA    RE,1(RE)                                                         
         ST    RE,LAST                                                          
         L     RE,OFLENGTH                                                      
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(1,RD),0(R7)                                                    
         AR    RD,RE                                                            
         LA    RD,1(RD)                                                         
         BR    R5                                                               
*                                                                               
DOAF     LA    R7,DISKIO                                                        
         CLI   AFILT,C'F'                                                       
         BE    GO2                                                              
         LA    R7,0                                                             
         SH    R7,=H'1'                                                         
         ST    R7,AFLENGTH                                                      
         BR    R5                                                               
GO2      A     R7,AFSTCOLR                                                      
         L     RE,LAST                                                          
STR2     ST    RE,AFSTCOL                                                       
         A     RE,AFLSNGL                                                       
         LA    RE,1(RE)                                                         
         ST    RE,LAST                                                          
         L     RE,AFLENGTH                                                      
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(1,RD),0(R7)                                                    
         AR    RD,RE                                                            
         LA    RD,1(RD)                                                         
         BR    R5                                                               
*                                                                               
DORES    LA    R7,DISKIO                                                        
         A     R7,RSTCOLR                                                       
         L     RE,LAST                                                          
STR3     ST    RE,RSTCOL                                                        
         A     RE,RLSNGL                                                        
         LA    RE,1(RE)                                                         
         ST    RE,LAST                                                          
         L     RE,RLENGTH                                                       
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(1,RD),0(R7)                                                    
         AR    RD,RE                                                            
         LA    RD,1(RD)                                                         
         BR    R5                                                               
*                                                                               
*   END OF TAPEIP ROUTINE                                                       
         EJECT                                                                  
NOTNUMA  MVI   IOPRTARA,X'8B'                                                   
         LA    RC,NOTNUMAX                                                      
         CALL  IOPRINT                                                          
NOTNUMAX MVI   PASA,X'09'                                                       
         MVC   PRTLN(37),ARRAYCD                                                
         MVC   IOPRTARA,PRINT                                                   
         LA    RC,LAX                                                           
         CALL  IOPRINT                                                          
LAX      L     RA,SVRA                                                          
         LA    RA,53(RA)                                                        
         ST    RA,SVRA                                                          
         MVC   PRTLN(L'MSGN),MSGN                                               
         MVC   IOPRTARA,PRINT                                                   
         LA    RC,CLRIT                                                         
         MVI   IOPRTARA,X'09'                                                   
         CALL  IOPRINT                                                          
CLRIT    MVI   PRTLN,C' '                                                       
         MVC   PRTLN+1(L'PRTLN-1),PRTLN                                         
         B     BEGIN                                                            
         EJECT                                                                  
         SPACE 3                                                                
**                                                                              
***            CONVERT 360 COLUMN BINARY DATA TO 1130 FORMAT (CNCVTB)           
**                                                                              
*              INPUT -  R2= FIRST POSN OF INPUT/OUTPUT AREA (HWDS)              
*              -----    R3= WORK                                                
*                       R4= NO OF HALFWORDS TO CONVERT                          
*                       R10= LINKAGE REGISTER                                   
*                       R12= WORK                                               
*                                                                               
CNCVTB   LR    12,2                                                             
*                                                                               
         IC    2,0(0,12)                                                        
         IC    3,1(0,12)                                                        
         SLL   3,26                                                             
         SLDL  2,10                                                             
         STH   2,0(0,12)                                                        
         LA    12,2(12)                                                         
         BCT   4,CNCVTB+2                                                       
         BR    10                                                               
*                                          BYTE 1    BYTE 2                     
*             1130 COLUMN BINARY FORMAT   &-012345  6789XXXX                    
         EJECT                                                                  
**                                                                              
***            CONVERT 1130 COLUMN BINARY DATA TO EBCDIC DATA (CNREVE)          
**                                                                              
*              INPUT -  R2= FIRST POSN OF INPUT DATA (HWDS)                     
*              -----    R3= FIRST POSN OF OUTPUT DATA                           
*                       R4= NO OF CHARACTERS(HWDS) TO CONVERT                   
*                       R10= LINKAGE REGISTER                                   
*                       R12= WORK                                               
*                                                                               
*              NB- EBCDIC OUTPUT MAY OCCUPY FIRST HALF OF INPUT AREA.           
*                - ONLY THE 'DISKTAB' CHARS ARE CONVERTED TO VALID              
*                        EBCDIC OUTPUT - OTHERS OUTPUT AS HEX'40'               
*                                                                               
CNREVE   ST    10,CNREVSAV                                                      
*                                                                               
         LH    12,0(0,2)                                                        
         LA    10,20                                                            
         CH    12,CCLOOKCB(10)                                                  
         BE    CNREVR1                                                          
         BCTR  10,0                                                             
         BCT   10,*-10                                                          
         LA    10,112                                                           
         CH    12,CCLOOKCB(10)                                                  
         BE    CNREVR1                                                          
         BCTR  10,0                                                             
         BCT   10,*-10                                                          
CNREVR1  SRL   10,1                                                             
         IC    12,SBTRANS(10)                                                   
         STC   12,0(0,3)                                                        
         LA    2,2(2)                                                           
         LA    3,1(3)                                                           
         BCT   4,CNREVE+4                                                       
*                                                                               
         L     10,CNREVSAV                                                      
         BR    10                                                               
*                                                                               
*                                                                               
CNREVSAV DS    F              (R10) SAVE AREA                                   
         SPACE 3                                                                
*                                                                               
*              TRANSLATE TABLE (DISKTAB CODE INTO EBCDIC)                       
*                                                                               
SBTRANS  DC    CL30' 1234567890-&&SZK G(E)L$H%VRIA'''                           
         DC    CL25'N.+,XDMP*=/WOFUBC    QTYJ'                                  
         DC    X'C0D0'        (+ ZERO / - ZERO)                                 
         SPACE 3                                                                
         EJECT                                                                  
**                                                                              
***            1130 COLUMN BINARY CHARACTER LOOKUP LIST                         
**                                                                              
         DS    0H                                                               
CCLOOKCB DC    X'0000'        0         BLANK     *=BRANCH ADDRESS USED         
         DC    X'1000'        1    *    1                                       
         DC    X'0800'        2    *    2                                       
         DC    X'0400'        3    *    3                                       
         DC    X'0200'        4    *    4                                       
         DC    X'0100'        5    *    5                                       
         DC    X'0080'        6    *    6                                       
         DC    X'0040'        7    *    7                                       
         DC    X'0020'        8    *    8                                       
         DC    X'0010'        9    *    9                                       
         DC    X'2000'       10    *    0                                       
         DC    X'4000'       11    *    -                                       
         DC    X'8000'       12    *    &                                       
         DC    X'2800'       13    *    S                                       
         DC    X'2010'       14    *    Z                                       
         DC    X'4800'       15    *    K                                       
         DC    X'FFFF'       16    *    SPECIAL '/' FOR 'V' SPECS               
         DC    X'8040'       17    *    G                                       
         DC    X'8120'       18         (                                       
         DC    X'8100'       19    *    E                                       
         DC    X'4120'       20         )                                       
         DC    X'4400'       21    *    L                                       
         DC    X'4420'       22         $                                       
         DC    X'8020'       23    *    H                                       
         DC    X'2220'       24         %                                       
         DC    X'2100'       25    *    V                                       
         DC    X'4010'       26    *    R                                       
         DC    X'8010'       27    *    I                                       
         DC    X'9000'       28    *    A                                       
         DC    X'0120'       29    *    '                                       
         DC    X'4100'       30    *    N                                       
         DC    X'8420'       31    *    .                                       
         DC    X'80A0'       32    *    +                                       
         DC    X'2420'       33    *    ,                                       
         DC    X'2040'       34    *    X                                       
         DC    X'8200'       35    *    D                                       
         DC    X'4200'       36    *    M                                       
         DC    X'4040'       37    *    P                                       
         DC    X'4220'       38    *    *                                       
         DC    X'00A0'       39         =                                       
         DC    X'3000'       40    *    / (REGULAR)                             
         DC    X'2080'       41    *    W                                       
         DC    X'4080'       42         O                                       
         DC    X'8080'       43    *    F                                       
         DC    X'2200'       44         U                                       
         DC    X'8800'       45    *    B                                       
         DC    X'8400'       46         C                                       
         DC    X'0001'       47    *    END OF 'V' SPECS                        
         DC    X'0002'       48    *    END OF 'M' SPECS                        
         DC    X'0003'       49    *    END OF 'R' SPECS                        
         DC    X'0004'       50    *    END OF 'C' SPECS                        
         DC    X'4020'       51    *    Q                                       
         DC    X'2400'       52    *    T                                       
         DC    X'2020'       53    *    Y                                       
         DC    X'5000'       54    *    J                                       
*                                                                               
         DC    X'A000'       55         + ZERO                                  
         DC    X'6000'       56         - ZERO                                  
         EJECT                                                                  
* ALL PURPOSE MULT X2 OF LENGTHS AND STARTS FOR COLBIN.                         
*   CSSW= 02 START COL, EBCDIC.                                                 
*   CSSW= 00 LENGTH CODE, EBCDIC.                                               
*   CSSW= 01 LENGTH CODE, COLBIN, MUST BE DOUBLED.                              
*   CSSW= 03 START COL, COLBIN, MUST BE REDUCED BY 1 AFTER DOUBLING.            
*                                                                               
SHIFTST  CLI   CBSW,X'02'                                                       
         BE    SHIFTX                                                           
         CLI   CBSW,X'00'                                                       
         BE    SHIFTX                                                           
         SLA   R6,1                                                             
         CLI   CBSW,X'01'                                                       
         BE    SHIFTX                                                           
         BCTR  R6,0                                                             
SHIFTX   NI    CBSW,B'00000001'                                                 
         BR    RA                                                               
CBSW     DC    F'0'                                                             
         EJECT                                                                  
RBLK     DC    F'1' RESP. NO BLOCK NO. VALUE.                                   
RSTCOL   DS    F    RESP. NO COL. NO. DISP. VALUE.                              
RLENGTH  DS    F    RESP. NO LENGTH VALUE.                                      
OFBLK    DC    F'1' OFILTER BLOCK NO.                                           
OFSTCOL  DC    F'0' OFILTER START COL DISP VALUE.                               
OFLENGTH DS    F    OFILTER LENGTH VALUE.                                       
AFBLK    DC    F'1' AFILTER BLOCK NO.                                           
AFSTCOL  DC    F'0' AFILTER START COL DISP VALUE.                               
AFLENGTH DS    F    AFILTER LENGTH VALUE.                                       
TPNOBLKS DC    F'1'                NO OF BLKS IN LOGCL TP RECS.                 
RLENGTHP DS    F                   REAL RESP LENGTH FOR INDEXING                
* LENGTHS FOR WORKING EBCDIC.                                                   
OFLSNGL  DC    F'0'                                                             
AFLSNGL  DC    F'0'                                                             
RLSNGL   DC    F'0'                                                             
* LENGTHS FOR WORKING COLBIN.                                                   
OFLDBL   DC    F'0'                                                             
AFLDBL   DC    F'0'                                                             
RLDBL    DC    F'0'                                                             
* RELATIVE START COLUMNS.                                                       
RSTCOLR  DC    F'0'                                                             
AFSTCOLR DC    F'0'                                                             
OFSTCOLR DC    F'0'                                                             
LAST     DC    F'0'                                                             
*                                                                               
* ALL OF THE BLOCK NOS HAVE BEEN INITIALIZED TO 1 SO CARDS CAN BE               
* TREATED THE SAME AS SINGLE BLK TAPE RECS IN READ, CONV, ETC.                  
*                                                                               
* RLENGTH IS USED FOR ALL MOVEMENT AND COMPARES OF RESP NO.                     
IOCRDARA DS    0CL160               NAME CARDIN FOR IOCRDRD MODULE.             
TAPEIO   DS    0CL160                                                           
CARDIN   DS    0CL160    GENERAL CARD INPUT AREA.                               
CCODE    DS    CL1      CODE.                                                   
         DS    CL159     REST.                                                  
* END OF GENERAL CARD INPUT AREA.                                               
RDR      DC    C' '                                                             
HIGHEST  DS    F         HIGHEST TBL ENTRY ADDR.                                
SWITCH   DC    C'C'      TAPE OR CARD SWITCH-INITIALIZED FOR CARDS.             
OSW      DC    C' '      OVERALL CC SW.                                         
FSW      DC    C' '      OVERALL FILTER CD SW.                                  
ASW      DC    C' '      ARRAY CARD SW.                                         
PRINT    DS    0CL133                                                           
PASA     DC    X'8B'                                                            
PRTLN    DC    CL132' '                                                         
IND      DC    C' '                                                             
IOPRTARA DS    0CL133                                                           
         DC    XL1'09'                                                          
         DS    CL132                                                            
LINES    DC    PL2'50'                                                          
WORK     DS    F                                                                
PACKDW   DS    D                                                                
ERR04    DC    C'NON NUM OFILT ST COL'                                          
ERR05    DC    C'ZERO OFILT LENGTH'                                             
TFREQ    DC    PL4'0'              TOTAL FREQ HITS FO PCT CALC + TOTS.          
INTFREQ  DC    PL4'0'              CUMM FREQ COUNTER                            
ACCUM    DC    PL4'0'              PCT WORK ACCUMULATOR                         
DVDND    DC    PL8'0'              DIVIDEND (ACCUM X 10000)                     
AREND    DC    PL2'0'                                                           
PASSFAIL DC    C' '                FILTER PASS OF FAIL INDICATOR                
ADDX     DC    PL2'0'                                                           
MSGN     DS    0CL95                                                            
         DC    C'ABOVE CARD CONTAINS EITHER ZERO LENGTHS OR NON-NUMERI'         
         DC    C'C PUNCHES WHERE NUM REQD - ARRAY NOT BUILT'                    
*                                                                               
         DS    0F                  PARAMETER LISTS                              
*DISK WRITE PARM                                                                
DWPARM   DS    0CL6                                                             
         DC    A(DISKIO)           IOAREA                                       
DWLEN    DC    X'0050'             LRECL                                        
*CARD READ PARM                                                                 
         DS    0F                                                               
CRPARM   DS    0CL6                                                             
         DC    A(CARDIN)           IOAREA                                       
         DC    X'0050'             LRECL                                        
*DISK READ PARM                                                                 
         DS    0F                                                               
DRPARM   DS    0CL6                                                             
         DC    A(DISKIO)           IOAREA                                       
DRLEN    DC    X'0050'             LRECL                                        
*TAPE READ                                                                      
         DS    0F                                                               
TRPARM   DS    0CL6                                                             
         DC    A(TAPEIO)           IOAREA                                       
TRLEN    DC    X'0050'             LRECL                                        
*                                                                               
         DS    0H                                                               
HWWK1    DS    CL160                                                            
         DS    0H                                                               
HWWK2    DS    CL160                                                            
*                                                                               
ERR01    DC   C'TOO MANY ARRAY CARDS - LIMIT IS 40'                             
ERR07    DC    C'NO. OF DIFF RESP. CAUSED ARRAY TBL OVFLO-SEE PGMMR'            
SPACES   DC    CL80' '                                                          
OCCARD   DS    0CL80     OVERALL CONTROL CARD.                                  
OCID     DS    CL5       ID = $DTAB                                             
         DS    CL4                                                              
OCTORC   DS    CL1       TAPE=T CARD=C                                          
OCHILR   DS    CL1       HIGH LOGICAL RECORD NO.                                
OCRST    DS    CL2       START COL. OF RESPONDENT NO.                           
OCRNOCOL DS    CL1       NO. OF COLS. IN RESP. NO.                              
         DS    CL3                                                              
OCFCOL   DS    CL2       FIRST COL. FOR PHYSICAL CARD TYPE.                     
         DS    CL41                                                             
OCNBLKS  DS    CL2       NO. OF BLOCKS IN LOGICAL TAPE RECORDS.                 
OCRESBL  DS    CL2       BLOCK THAT RESPONDENT NO. IS IN.                       
OCRESST  DS    CL2       START COL. IN RESP. BLOCK.                             
OCRESLN  DS    CL1       LENGTH OF RESP. NO.                                    
OCNOUSE  DS    CL2       NO. OF UNUSED COLS. IN LAST 80C BLOCK.                 
         DS    CL10                                                             
* END OF OVERALL CONTROL CARD.                                                  
OFILTERC DS    0CL80     OVERALL FILTER CARD.                                   
OFID     DS    CL7       ID ='FILTER='.                                         
OFBLOCK  DS    CL2       BLOCK CONTAINING FILTER DATA.                          
OFCOL    DS    CL2       COL WITHIN BLOCK FILTER DATA STARTS.                   
OFLNGTH  DS    CL2       LENGTH OF FILTER ARGUMENT.                             
OFARG    DS    CL26                FILTER ARGUMENTS                             
         DC    CL41' '                                                          
* END OF OVERALL FILTER CARD.                                                   
ARRAYCD  DS    0CL80     ARRAY DESCRIPTION CARD.                                
AID      DS    CL6       ID = 'ARRAY='.                                         
ASEQ     DS    CL2       ARRAY CARD NO.                                         
AREBL    DS    CL2       RESPONDENT BLOCK NO. 'CC' FOR CARD I/P.                
AREST    DS    CL2       START COLUMN FOR RESP. NO.                             
ARESLN   DS    CL1       LENGTH OF RESPONDENT NO.                               
ACOMMA   DS    CL1       COMMA FOR FOLLOWING FIELD.                             
AFILT    DS    CL7       'FILTER=' FOR ARRAY FILTER.                            
AFBL     DS    CL2       FILTER FIELD BLK. 'CC' FOR CARD I/P.                   
AFST     DS    CL2       FILTER FIELD START COL.                                
AFLN     DS    CL2       FILTER FIELD LENGTH.                                   
AFARG  DS    CL26      FILTER ARGUMENT.                                         
         DC    CL27' '                                                          
* END OF ARRAY DESCRIPTION CARD.                                                
WORK1    DS    CL160                                                            
WORK2    DS    CL160                                                            
         DS    0F                                                               
FAKEUPSI DC    B'01100000'                                                      
STRA     DS    F                                                                
STR7     DS    F                                                                
*                                                                               
HFIL2    DS    0CL133   HDR LINE TWO.                                           
         DC    C' '                                                             
         DC    C'FREQUENCY   PERCENT   CUMM FREQ   CUMM PCT   DATA'             
         DC    CL82' '                                                          
SVRA     DS    F                                                                
DMSG     DC    CL75' '                                                          
         EJECT                                                                  
*                                                                               
* MOVE & ETC. INSTRUCTIONS WHICH ARE EXECUTED IN MAIN STREAM.                   
         DS    0D                                                               
* COMPARES INPT DATA TO TABLE ENTRY.                                            
INSTA    CLC   0(1,R6),0(R7)                                                    
         SPACE 2                                                                
* MOVES DATA FROM INPT TO TABLE ENTRY.                                          
INSTB    MVC   0(1,R6),0(R7)                                                    
         SPACE 2                                                                
* SETS ACCUM AT END OF NEW ENTRY (R6 IS LOADED BY INCR R6 START BY              
* LENGTH IN THE REG USED FOR EX (R9)).                                          
INSTC    MVC   0(4,R6),=X'0000000C'                                             
         SPACE 2                                                                
* TESTS FOR BIGTABLE EXCEEDED. EX WITH R0.                                      
INSTD    C     R6,=A(BIGTABLE+30000)                                            
         SPACE 2                                                                
* MOVES FROM BIGTABLE TO WORK AREA-1.                                           
INSTE    MVC   WORK1(1),0(R6)                                                   
         SPACE 2                                                                
* MOVES FROM WORK AREA-1 TO WORK AREA-2.                                        
INSTF    MVC   WORK2(1),WORK1                                                   
         SPACE 2                                                                
* MOVES OCCURRENCE ACCUM TO PCT. CALC. WORK ACCUM.(R5 = BIGTBL+L'DATA).         
INSTJ    MVC   ACCUM,0(R5)                                                      
         SPACE 2                                                                
* COMPARES FOR OVERALL FILTER. R5 IS ADDR OF FILTER AREA.                       
INSTK    CLC   0(1,R5),0(R7)                                                    
         SPACE 2                                                                
* COMPARES FOR ARRAY FILTER. R5 IS ADDR OF FILTER AREA.                         
INSTL    CLC   0(1,R5),0(R7)                                                    
         SPACE 2                                                                
* MOVES SPACES TO TABLE ENTRY.                                                  
INSTM    MVC   0(1,R6),SPACES                                                   
         SPACE 2                                                                
* COMPARES WORK1 WITH BIGTABLE ENTRY.                                           
INSTN    CLC   0(1,R6),WORK1                                                    
         SPACE 2                                                                
* MOVES FROM WORK2 TO BIGTABLE.                                                 
INSTO    MVC   0(1,R6),WORK2                                                    
         LTORG                                                                  
         EJECT                                                                  
ARTBL    DS    40CL53              ARRAY CARD TABLE                             
         DS    CL5                                                              
* ENTRIES FROM HERE WILL BE MOVED INTO ARRAYCD AS THEY ARE USED.                
DISKIO   DS    CL160                                                            
BIGTABLE DS    0CL30208       FREEFORM DATA TABLE.                              
         DC    118CL256' '                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'045ARRAY1    05/01/02'                                      
         END                                                                    
