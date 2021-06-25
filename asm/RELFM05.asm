*          DATA SET RELFM05    AT LEVEL 065 AS OF 05/01/02                      
*PHASE T80405A,+0                                                               
*INCLUDE REBKLST                                                                
         TITLE 'T80405 - REPPAK FILE MAINT - INV /ADD/CHA/DIS/DEL/ '            
********************************************************************            
* HISTORY OF CHANGES                                               *            
********************************************************************            
* FEB24/92 (BU ) --- PERMIT A NON-HIT ON INVENTORY DATE TO FIND    *            
*                    THE RECORD WITH THE APPROPRIATE EFFECTIVE     *            
*                    DATE RANGE.                                   *            
*                                                                  *            
* APR29/93 (SKU) --- CHANGE REC2+500 TO REC2+400 (WAS CLOBBERING   *            
*                    STORAGE AFTER END OF REC2)                    *            
*                                                                  *            
* MAR07/95 (BU ) --- FIX INV ADD TRAILER INFO DISAPPEARANCE        *            
*                                                                  *            
* JUN26/95 (BU ) --- FIX 'UT' DEMUP PROBLEM... HA!                 *            
*                                                                  *            
*                                                                  *            
*                    ***  END TOMBSTONE  ***                       *            
********************************************************************            
T80405   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T80405,RR=R5                                                   
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T804FFD,RA                                                       
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         USING T80405+4096,R9                                                   
*                                                                               
         L     R7,ACOMFACS                                                      
         USING COMFACSD,R7                                                      
         EJECT                                                                  
*              GET THE MARKET RECORD                                            
         SPACE 1                                                                
         LA    R2,LFMKEYH                                                       
         FOUT  INAMRKTH,SPACES,25                                               
         SPACE 1                                                                
         GOTO1 VPAVSTA,DMCB,8(R2),WORK   GET STATION AND MEDIA                  
         MVC   INVSTA,WORK                                                      
         MVC   INVMED,WORK+5                                                    
         SPACE 1                                                                
         GOTO1 VMKTNME,DMCB,WORK,INAMRKTH                                       
         MVC   DEMEDIA(8),INVMED   MEDIA/STATION/MARKET FOR DEMOS               
         SPACE 1                                                                
         EJECT                                                                  
         USING RINVD,R4                                                         
         L     R4,AIOAREA                                                       
         CLI   BACT,C'A'                                                        
         BE    INV100                                                           
         TM    LFMKEYH+4,X'20'     KEY CHANGE FORCE DISPLAY                     
         BO    *+8                                                              
         MVI   BFMTSW,0                                                         
         SPACE 1                                                                
         CLI   BFMTSW,0                                                         
         BNE   INV090                                                           
         BAS   RE,DISPLAY          DISPLAY THE RECORD                           
         B     EXXMOD                                                           
         SPACE 1                                                                
INV090   CLI   BACT,C'X'           DELETE                                       
         BE    INVDEL                                                           
         CLI   BACT,C'C'           CHANGE                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
INV100   BAS   RE,BLDREC           BUILD A NEW RECORD                           
         CLI   ERRAREA,0                                                        
         BNE   EXXMOD                                                           
         MVC   INVDAY(5),RINVPDAY  SAVE DAY AND TIME                            
         SPACE 1                                                                
         BAS   RE,CHKDATE          CHECK FOR OVERLAPPING DATE                   
         CLI   ERRAREA,0                                                        
         BNE   EXXMOD                                                           
         SPACE 1                                                                
         MVC   KEY,RINVREC                                                      
         BAS   RE,HIGH             LOOK FOR DUPLICATE KEY                       
         CLC   KEYSAVE(27),KEY                                                  
         BNE   INV110                                                           
         SPACE 1                                                                
         LA    R3,ERRDUP                                                        
         LA    R2,LFMKEYH                                                       
         CLI   BACT,C'A'                                                        
         BE    ERROR                                                            
         CLC   KEY(27),BKEY        CHANGED KEY                                  
         BNE   ERROR               BUT ONE ALREADY EXISTS                       
         EJECT                                                                  
INV110   DS    0H                                                               
         BAS   RE,CHAOLD           CHANGE OLD RECORDS IF KEY CHANGE             
         BAS   RE,BLDREC           BUILD NEW RECORD                             
         MVC   WORK2(2),RINVPEFF+2 SAVE END DATE                                
         CLI   RINVPEFF+2,0        END DATE +1 MIGHT HAVE                       
         BNE   *+8                 NUMBER OF DAYS TO SUBTRACT                   
         MVI   RINVPEFF+3,0                                                     
         SPACE 1                                                                
         BAS   RE,FLADD            ADD NEW/CHANGED RECORD                       
         BAS   RE,MSGOUT           PUT OUT THE MESSAGE                          
         BAS   RE,ENDOLD           END OLD INVENTORY IF OVERLAP                 
         SPACE 1                                                                
         CLI   BACT,C'A'                                                        
         BNE   INV120                                                           
INV115   DC    0H'0'                                                            
         GOTO1 INVPTR,DMCB,REC,WORK2    CREATE PASSIVE POINTERS                 
         GOTO1 NWPT,DMCB,WORK2                                                  
         B     INV125                                                           
         SPACE 1                                                                
INV120   CLI   BACT,C'C'                                                        
         BE    *+6                                                              
         DC    H'0'                I DON'T KNOW WHAT I AM DOING                 
         GOTO1 INVPTR,DMCB,WORK3,WORK2   POINTERS FOR OLD                       
         GOTO1 DELPT,DMCB,WORK2         DELETE THEM                             
         B     INV115                   ADD CHANGED RECORD                      
         SPACE 1                                                                
INV125   EQU   *                                                                
         CLI   BACT,C'A'                                                        
         BNE   INV126                                                           
         MVC   KEY(27),RINVKEY                                                  
         BAS   RE,HIGH                                                          
         CLI   KEY+27,0                                                         
         BE    INV126                                                           
         DC    H'0'                                                             
INV126   EQU   *                                                                
         MVC   BKEY,RINVKEY                                                     
         OI    LFMKEYH+4,X'20'                                                  
         LA    R2,INATIMEH                                                      
         MVC   INATIME,SPACES                                                   
         GOTO1 VUNTIME,DMCB,RINVPTIM,8(R2)                                      
         FOUT  (R2)                                                             
         OC    RINVPTIM+2(2),RINVPTIM+2                                         
         BNZ   INV127                                                           
         LA    R3,INATIME                                                       
         CLI   0(R3),C' '                                                       
         BNH   *+12                                                             
         LA    R3,1(R3)                                                         
         B     *-12                                                             
         MVC   0(2,R3),=C',B'                                                   
         B     INV130                                                           
         SPACE 1                                                                
INV127   LA    R2,INAATIMH                                                      
         MVC   INAATIM,SPACES                                                   
         FOUT  (R2)                                                             
         OC    RINVPATM,RINVPATM   INPUT IS OPTIONAL                            
         BZ    INV130                                                           
         GOTO1 VUNTIME,DMCB,RINVPATM,8(R2)                                      
         OC    RINVPATM+2(2),RINVPATM+2                                         
         BNZ   INV130                                                           
         LA    R3,INAATIM                                                       
         CLI   0(R3),C' '                                                       
         BNH   *+12                                                             
         LA    R3,1(R3)                                                         
         B     *-12                                                             
         MVC   0(2,R3),=C',B'                                                   
         B     INV130                                                           
         SPACE 1                                                                
INV130   DS    0H                                                               
         BAS   RE,TRNSFER                                                       
         B     EXXMOD                                                           
         EJECT                                                                  
*              DISPLAY HEADLINE                                                 
         SPACE 1                                                                
DISPLAY  NTR1                                                                   
         XC    RINVKEY,RINVKEY                                                  
         L     R7,ACOMFACS                                                      
         GOTO1 VFOUTBLK,DMCB,INACODEH,INALAST     CLEAR SCREEN                  
         MVI   RINVKTYP,X'12'                                                   
         MVC   RINVKREP,REPALPHA                                                
         MVC   RINVKSTA,INVSTA                                                  
         SPACE 1                                                                
         LA    R2,LFMKEYH                                                       
         LA    R3,131              X'83' - ASK FOR DISPLACEMENTS                
*                                    FOR UP TO 3 SUBFIELDS                      
         PRINT GEN                                                              
         GOTO1 CSCANNER,DMCB,(R2),((R3),WORK2)                                  
         PRINT NOGEN                                                            
         MVC   HALF(1),WORK2+68    SAVE DISPLACEMENT OF 3RD FIELD               
         LA    R3,INVERR                                                        
         XR    R5,R5                                                            
         IC    R5,DMCB+4                                                        
         LTR   R5,R5                                                            
         BZ    ERROR                                                            
         SPACE 1                                                                
         CH    R5,=H'2'                                                         
         BL    ERROR               NO INV. IN KEY                               
         LA    R6,WORK2+32                                                      
         BAS   R8,INVEDT           EDIT INV. NUMBER                             
         SPACE 1                                                                
         CH    R5,=H'3'                                                         
         BL    DISP10              NO DATE                                      
         LA    R6,32(R6)                                                        
         BAS   R8,DATEDT                                                        
         SPACE 1                                                                
DISP10   BAS   RE,HEADER           GET THE HEADER                               
         CLI   ERRAREA,0                                                        
         BNE   EXXMOD                                                           
         SPACE 1                                                                
         MVC   INACODE,RINVKLEN    SPECIAL CODE                                 
         OI    INACODE,X'40'                                                    
         CLI   INACODE,C'Z'                                                     
         BNH   *+8                                                              
         MVI   INACODE,C' '                                                     
         SPACE 1                                                                
         CLI   RINVPAUT,C'N'                                                    
         BNE   *+10                                                             
         MVC   INATRNF,=C'NT'      NO OVERNIGHT TRANSFER                        
         SPACE 1                                                                
         GOTO1 VUNDAY,DMCB,RINVPDAY,INADAYS    DAYS                             
         MVC   INAADAY,SPACES                                                   
         OC    RINVPADY,RINVPADY                                                
         BZ    DISP12                                                           
         GOTO1 VUNDAY,DMCB,RINVPADY,INAADAY    AVAIL DAYS                       
         SPACE 1                                                                
DISP12   GOTO1 VUNTIME,DMCB,RINVPTIM,INATIME   TIME                             
         OC    RINVPTIM+2(2),RINVPTIM+2                                         
         BNZ   DISP13                                                           
         LA    R3,INATIME                                                       
         CLI   0(R3),C' '                                                       
         BNH   *+12                                                             
         LA    R3,1(R3)                                                         
         B     *-12                                                             
         MVC   0(2,R3),=C',B'                                                   
         SPACE 1                                                                
DISP13   MVC   INAATIM,SPACES                                                   
         OC    RINVPATM,RINVPATM                                                
         BZ    DISP15                                                           
         GOTO1 VUNTIME,DMCB,RINVPATM,INAATIM    AVAIL TIME                      
         OC    RINVPATM+2(2),RINVPATM+2                                         
         BNZ   DISP15                                                           
         LA    R3,INAATIM                                                       
         CLI   0(R3),C' '                                                       
         BNH   *+12                                                             
         LA    R3,1(R3)                                                         
         B     *-12                                                             
         MVC   0(2,R3),=C',B'                                                   
         EJECT                                                                  
DISP15   XR    R3,R3               PROGRAM                                      
         IC    R3,RINVPLEN                                                      
         SH    R3,=H'40'                                                        
         SR    R2,R2                                                            
         D     R2,=F'27'                                                        
         LTR   R2,R2                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
         LTR   R3,R3               R3 NUMBER OF PROGRAM LINES                   
         BNZ   *+6                                                              
         DC    H'0'                                                             
         LA    R2,INAPROGH                                                      
         LA    R5,RINVPROG                                                      
         SPACE 1                                                                
         MVC   8(27,R2),0(R5)                                                   
         LA    R5,27(R5)                                                        
         XR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         BCT   R3,*-18                                                          
         SPACE 1                                                                
         MVC   INADYPT,RINVDP      DAYPARTS                                     
         MVC   INAFILT,RINVPFLT                                                 
         LA    R2,INAEDTE                                                       
         GOTO1 VDATCON,DMCB,(2,RINVPEFF),(5,0(R2))                              
         SPACE 1                                                                
         LA    R3,RINVPEFF+2                                                    
         OC    0(2,R3),0(R3)                                                    
         BZ    DISP20              NO END                                       
         LA    R2,8(R2)                                                         
         MVI   0(R2),C'-'                                                       
         GOTO1 VDATCON,DMCB,(2,0(R3)),(5,1(R2))                                 
         SPACE 1                                                                
DISP20   MVC   BKEY,0(R4)          SAVE KEY OF DISPLAYED RECORD                 
         OI    LFMKEYH+4,X'20'                                                  
         B     EXXMOD                                                           
         EJECT                                                                  
*              GET A HEADER RECORD                                              
         SPACE 1                                                                
*                                                                               
*   ROUTINE ATTEMPTS TO FIND AN INVENTORY HEADER FOR THE REQUEST.               
*      IF A DATE WAS ENTERED, A FIRST PASS TRIES TO FIND THAT EXACT             
*      DATE.  IF NOT FOUND, THE DATE IS USED TO TEST THE EFFECTIVE-             
*      DATE RANGES OF THE AVAILABLE INVENTORY.  IF THE REQUEST DATE             
*      FALLS WITHIN A SET OF EFFECTIVE DATES, THE DATE ON THE KEY               
*      LINE IS CHANGED TO REFLECT THE EFFECTIVE START DATE.  THIS               
*      ENABLES THE DATE TO BE USED BY OTHER RECORD TYPES.                       
* ***  NOTE:  AT THIS POINT, 'WORK2' STILL CONTAINS THE 'SCANNER'               
*      OUTPUT DESCRIBING THE BREAKDOWN OF THE KEY.                              
*                                                                               
HEADER   NTR1                                                                   
         MVC   KEY,RINVREC                                                      
         XC    KEY+24(3),KEY+24    RINVKSRC/RINVKTXT                            
         BAS   RE,HIGH                                                          
         CLC   KEY(27),KEYSAVE                                                  
         BNE   HREC0004            NOT FOUND                                    
         BAS   RE,GETREC           FOUND A MATCH                                
         B     HREC0030                                                         
HREC0004 EQU   *                                                                
         LA    R3,ERRNF                                                         
         OC    RINVKSTD,RINVKSTD                                                
         BNZ   HREC0014            INPUT A DATE - LOOK FOR EFF RANGE            
*                                                                               
         CLC   KEYSAVE(21),KEY     DID I FIND INVENTORY NUMBER                  
         BNE   ERROR                                                            
*                                                                               
         OC    KEY+24(3),KEY+24    MAKE SURE IT'S HEADER                        
         BZ    HREC0008                                                         
         DC    H'0'                PROBLEM WITH FILE                            
HREC0008 BAS   RE,GETREC                                                        
         OC    RINVPEFF+2(2),RINVPEFF+2                                         
         BZ    HREC0030            TAKE ONE WITH NO END                         
HREC0012 EQU   *                                                                
         BAS   RE,SEQ              IF ALL HAVE END TAKE HIGHEST                 
         CLC   KEYSAVE(21),KEY                                                  
         BNE   HREC0030                                                         
         OC    KEY+24(3),KEY+24                                                 
         BZ    HREC0008                                                         
         B     HREC0012            NOT A HEADER                                 
         SPACE 1                                                                
*                                                                               
*   INVENTORY W/DATE ENTERED.  EXACT HIT NOT MADE.  SCAN FOR INV                
*      WITH EFFECTIVE DATE ENCOMPASSING THE ENTERED DATE WILL BE                
*      MADE.  IF DATE OUTSIDE ALL INVENTORY EFFECTIVE RANGES, A                 
*      'NO FOUND' ERROR WILL BE TAKEN.                                          
*                                                                               
HREC0014 EQU   *                                                                
         GOTO1 VDATCON,DMCB,(3,RINVKSTD),(2,FULL)                               
         MVC   KEY,RINVREC         RESET KEY AFTER HIGH READ                    
*                                  MAY HAVE PASSED INVENTORY COMPLETELY         
         XC    KEY+21(6),KEY+21    BLANK OUT END OF KEY                         
HREC0016 EQU   *                                                                
         BAS   RE,HIGH             RETRIEVE 1ST/NEXT RECORD                     
         CLC   KEYSAVE(21),KEY     SAME INVENTORY #?                            
         BNE   ERROR               OUTSIDE ALL RANGES                           
         BAS   RE,GETREC           RETRIEVE RECORD                              
         MVC   DUB,RINVPEFF        UNLOAD EFFECTIVE DATES                       
         OC    DUB+2(2),DUB+2      ANY END DATE?                                
         BNZ   HREC0020            YES                                          
         MVC   DUB+2(2),=X'FFFF'   NO  - SET TO HIGH VALUES                     
HREC0020 EQU   *                                                                
         CLC   FULL(2),DUB         DATE INPUT VS EFFECTIVE START                
         BL    ERROR               EARLIER THAN START                           
*                                  WILL NEVER BE FOUND                          
         CLC   DUB+2(2),FULL       EFFECTIVE END VS DATE INPUT                  
         BL    HREC0024            END < DATE INPUT: KEEP GOING                 
         B     HREC0028            FOUND: NOT < START, NOT > END                
HREC0024 EQU   *                                                                
         MVC   KEY+24(3),=X'FFFFFF' SET KEY TO SKIP TRACKS, RATL                
         B     HREC0016            GO BACK FOR NEXT                             
HREC0028 EQU   *                                                                
         ZIC   RE,WORK2+64         L(DATE FIELD INPUT)                          
         LA    R2,LFMKEY           A(KEY FIELD)                                 
         ZIC   RF,HALF             DISP(DATE FIELD INTO KEY FIELD)              
         AR    R2,RF               A(KEY FIELD) + DISP = A(DATE)                
         EX    RE,HREC0032         WIPE OUT DATE BY LENGTH                      
         GOTO1 VDATCON,DMCB,(2,DUB),(5,(R2))                                    
*                                  REPLACE DATE INPUT WITH EFFECTIVE-           
*                                    DATE START DATE                            
         LA    R2,LFMKEYH          A(HEADER OF KEY FIELD)                       
         FOUT  (R2)                TURN ON TRANSMIT BIT                         
         OI    4(R2),X'20'         SET PREVIOUSLY VALID BIT                     
HREC0030 EQU   *                                                                
         B     EXXMOD                                                           
*                                                                               
HREC0032 MVC   0(0,R2),SPACES                                                   
         EJECT                                                                  
*              BUILD NEW RECORD IN IOAREA                                       
         SPACE 1                                                                
BLDREC   NTR1                                                                   
         XC    RINVREC(34),RINVREC                                              
         XC    RINVKEY,RINVKEY                                                  
         MVI   RINVKTYP,X'12'                                                   
         MVC   RINVKREP,REPALPHA                                                
         MVC   RINVKSTA,INVSTA                                                  
         SPACE 1                                                                
BLD10    XC    RINVPEL(256),RINVPEL                                             
         MVI   RINVPCOD,1                                                       
         LA    R2,INACODEH                                                      
         LA    R3,INVERR                                                        
         CLI   5(R2),0                                                          
         BE    BLD11                                                            
         MVC   RINVKLEN,INACODE                                                 
         CLI   RINVKLEN,C'A'                                                    
         BL    ERROR                                                            
         CLI   RINVKLEN,C'Z'                                                    
         BH    ERROR                                                            
         SPACE 1                                                                
BLD11    LA    R2,INATRNFH                                                      
         CLI   5(R2),0                                                          
         BE    BLD12                                                            
         CLC   8(2,R2),=C'NT'                                                   
         BNE   ERROR                                                            
         MVI   RINVPAUT,C'N'       NO AUTOMATIC TRANSFER                        
         SPACE                                                                  
BLD12    LA    R2,INADAYSH         EDIT THE DAYS                                
         BAS   RE,ANY                                                           
         LA    R3,INVERR                                                        
         XR    R5,R5                                                            
         IC    R5,5(R2)            LENGTH OF EXPRESSION                         
         GOTO1 VDAYVAL,DMCB,((R5),8(R2)),RINVPDAY,WORK                          
         CLI   RINVPDAY,0                                                       
         BE    ERROR                                                            
         SPACE 1                                                                
         GOTO1 VINVDAY,DMCB,((R5),8(R2)),RINVKDAY,WORK,VDAYVAL                  
         CLI   RINVKDAY,0                                                       
         BE    ERROR                                                            
         SPACE 1                                                                
         LA    R2,INAADAYH         EDIT THE AVAIL DAYS                          
         XC    RINVPADY,RINVPADY                                                
         CLI   5(R2),0                                                          
         BE    BLD13                                                            
         LA    R3,INVERR                                                        
         XR    R5,R5                                                            
         IC    R5,5(R2)            LENGTH OF EXPRESSION                         
         GOTO1 VDAYVAL,DMCB,((R5),8(R2)),RINVPADY,WORK                          
         CLI   RINVPADY,0                                                       
         BE    ERROR                                                            
         EJECT                                                                  
BLD13    LA    R2,INATIMEH         EDIT THE TIME                                
         BAS   RE,ANY                                                           
         LA    R3,INVERR                                                        
         CLI   8(R2),C'N'          NONE                                         
         BE    ERROR                                                            
         CLI   8(R2),C'V'            VARIOUS ARE NOT VALID                      
         BE    ERROR                                                            
         SPACE 1                                                                
         XR    R5,R5                                                            
         IC    R5,5(R2)            LENGTH OF EXPRESSION                         
         LA    R7,6(R5,R2)                                                      
         CLC   0(2,R7),=C',B'                                                   
         BNE   *+8                                                              
         SH    R5,=H'2'                                                         
         GOTO1 VTIMVAL,DMCB,((R5),8(R2)),RINVPTIM                               
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
         SPACE 1                                                                
         GOTO1 VHRTOQH,DMCB,RINVPTIM,RINVKQTR                                   
         CLC   0(2,R7),=C',B'                                                   
         BNE   BLD15                                                            
         OC    RINVPTIM+2(2),RINVPTIM+2                                         
         BNZ   ERROR                                                            
         CLI   RINVKLEN,0                                                       
         BNE   BLD30                                                            
         MVI   RINVKLEN,C'0'                                                    
         B     BLD30                                                            
         SPACE 1                                                                
BLD15    CLC   RINVPTIM+2(2),=C'CC'                                             
         BNE   BLD16                                                            
         CLI   RINVKLEN,0                                                       
         BNE   BLD30                                                            
         MVI   RINVKLEN,C'9'                                                    
         B     BLD30                                                            
         SPACE 1                                                                
BLD16    MVC   HALF,RINVPTIM       START TIME TO MINUTES                        
         BAS   R8,TOMIN                                                         
         LH    R5,HALF             START MINUTE TO R5                           
         SPACE 1                                                                
         MVC   HALF,RINVPTIM+2     END TIME TO MINUTES                          
         BAS   R8,TOMIN                                                         
         LH    R6,HALF             END MINUTES                                  
         SPACE 1                                                                
         LTR   R6,R6                                                            
         BNZ   *+8                                                              
         LA    R6,30(R5)           IF NO END / ADD 30 TO START                  
         LR    RF,R6               END TIME MINUTES                             
         SPACE 1                                                                
         CH    RF,=H'1440'                                                      
         BNH   *+8                                                              
         SH    RF,=H'1440'         PAST MIDNIGHT                                
         XR    RE,RE                                                            
         SPACE 1                                                                
         D     RE,=F'60'           GET MILITARY END FROM MINUTES                
         MH    RF,=H'100'                                                       
         AR    RF,RE                                                            
         STH   RF,HALF                                                          
         MVC   RINVPTIM+2(2),HALF                                               
         SPACE 1                                                                
         CLI   RINVKLEN,0                                                       
         BNE   BLD30                                                            
         CR    R5,R6               START/END MINUTES                            
         BNH   *+8                                                              
         AH    R6,=H'1440'         ADD 24 X 60 TO END                           
         SPACE 1                                                                
         SR    R6,R5               END - START                                  
         LR    R7,R6                                                            
         XR    R6,R6                                                            
         D     R6,=F'30'           GET NUMBER 1/2 HOURS                         
         LTR   R6,R6                                                            
         BZ    *+8                                                              
         AH    R7,=H'1'            ADD 1 TO HALF HOURS                          
         STC   R7,RINVKLEN                                                      
         SPACE 1                                                                
         LA    R3,10               GET CODE FROM LENGTH TABLE                   
         LA    R5,LENGTH                                                        
         CLC   RINVKLEN,0(R5)                                                   
         BNH   *+14                                                             
         LA    R5,2(R5)                                                         
         BCT   R3,*-14                                                          
         DC    H'0'                                                             
         MVC   RINVKLEN,1(R5)                                                   
         B     BLD30                                                            
         SPACE 1                                                                
TOMIN    XR    RE,RE                                                            
         LH    RF,HALF                                                          
         D     RE,=F'100'                                                       
         MH    RF,=H'60'                                                        
         AR    RE,RF                                                            
         STH   RE,HALF                                                          
         BR    R8                                                               
         SPACE 1                                                                
*              BYTE 1  = NUMBER HALF HOURS, BYTE 2 CODE                         
*                                                                               
LENGTH   DC    AL1(01),C'0'        UP TO 1/2 HOUR                               
         DC    AL1(02),C'1'        FROM MORE THAN 1/2  TO     1 HOUR            
         DC    AL1(03),C'2'                         1     1 1/2                 
         DC    AL1(04),C'3'                     1 1/2         2                 
         DC    AL1(05),C'4'                         2     2 1/2                 
         DC    AL1(06),C'5'                     2 1/2         3                 
         DC    AL1(08),C'6'                         3         4                 
         DC    AL1(12),C'7'                         4         6                 
         DC    AL1(16),C'8'                         6         8                 
         DC    AL1(99),C'9'        OVER 8 HOURS                                 
         EJECT                                                                  
BLD30    LA    R2,INAATIMH         EDIT THE AVAIL TIME                          
         XC    RINVPATM,RINVPATM                                                
         CLI   5(R2),0                                                          
         BE    BLD35                                                            
         LA    R3,INVERR                                                        
         CLI   8(R2),C'N'          NONE AND                                     
         BE    ERROR                                                            
         CLI   8(R2),C'V'          VARIOUS ARE NOT VALID                        
         BE    ERROR                                                            
         SPACE 1                                                                
         XR    R5,R5                                                            
         IC    R5,5(R2)            LENGTH OF EXPRESSION                         
         LA    R7,6(R5,R2)                                                      
         CLC   0(2,R7),=C',B'                                                   
         BNE   *+8                                                              
         SH    R5,=H'2'                                                         
         GOTO1 VTIMVAL,DMCB,((R5),8(R2)),RINVPATM                               
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
         SPACE 1                                                                
         CLC   0(2,R7),=C',B'                                                   
         BNE   BLD33                                                            
         OC    RINVPATM+2(2),RINVPATM+2                                         
         BNZ   ERROR                                                            
         B     BLD35                                                            
         SPACE 1                                                                
BLD33    OC    RINVPATM+2(2),RINVPATM+2     IF NO END                           
         BNZ   BLD35                                                            
         MVC   HALF,RINVPATM       ADD 30 MINUTES TO START                      
         BAS   R8,TOMIN                                                         
         LH    R5,HALF                                                          
         LA    R6,30(R5)                                                        
         LR    RF,R6                                                            
         SPACE 1                                                                
         CH    RF,=H'1440'         MAY PUSH IT PAST MIDNIGHT                    
         BNH   *+8                                                              
         SH    RF,=H'1440'                                                      
         XR    RE,RE                                                            
         SPACE 1                                                                
         D     RE,=F'60'           GET MILITARY END FROM MINUTES                
         MH    RF,=H'100'                                                       
         AR    RF,RE                                                            
         STH   RF,HALF                                                          
         MVC   RINVPATM+2(2),HALF                                               
         SPACE 1                                                                
BLD35    LA    R2,INAPROGH         PROGRAM NAME                                 
         BAS   RE,ANY                                                           
         LA    R5,40               ELEMENT LENGTH TO PROG                       
         XR    R7,R7                                                            
         LA    R3,3                                                             
         LA    R6,RINVPROG                                                      
         SPACE 1                                                                
BLD40    MVC   0(27,R6),SPACES                                                  
         IC    R7,5(R2)            INPUT LENGTH                                 
         LTR   R7,R7                                                            
         BZ    BLD50                                                            
         LA    R5,27(R5)           ADD TO ELEMENT LENGTH                        
         BCTR  R7,0                                                             
         EX    R7,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R6),8(R2)       DATA TO RECORD                               
         SPACE 1                                                                
         LA    R6,27(R6)                                                        
         IC    R7,0(R2)                                                         
         AR    R2,R7               NEXT FIELD                                   
         BCT   R3,BLD40                                                         
         SPACE 1                                                                
BLD50    STC   R5,RINVPLEN         ELEMENT LENGTH                               
         SPACE 1                                                                
         LA    R2,INAEDTEH         EDIT DATES                                   
         BAS   RE,ANY                                                           
         L     R7,ACOMFACS                                                      
         GOTO1 CSCANNER,DMCB,(R2),(2,WORK2),C',=,-'                             
         LA    R3,INVERR                                                        
         CLI   DMCB+4,1                                                         
         BNE   ERROR               THEY INPUT A ,                               
         SPACE 1                                                                
         GOTO1 VDATVAL,DMCB,(0,WORK2+12),WORK     START DATE                    
         OC    DMCB(4),DMCB                                                     
         BZ    ERROR                                                            
         GOTO1 VDATCON,DMCB,(0,WORK),(3,RINVKSTD)                               
         GOTO1 VDATCON,DMCB,(0,WORK),(2,RINVPEFF)                               
         MVC   INVEFF,RINVPEFF                                                  
         SPACE 1                                                                
         CLI   WORK2+1,0           NO END                                       
         BE    BLD60                                                            
         MVC   RINVPEFF+2(2),WORK2+10                                           
         CLI   WORK2+1,1                                                        
         BNE   *+16                SHOULD BE A DATE                             
         TM    WORK2+3,X'80'       TEST FOR 1 POSITION NUMERIC                  
         BO    BLD60                                                            
         B     ERROR                                                            
         SPACE 1                                                                
         GOTO1 VDATVAL,DMCB,(0,WORK2+22),WORK                                   
         OC    DMCB(4),DMCB                                                     
         BZ    ERROR                                                            
         GOTO1 VDATCON,DMCB,(0,WORK),(2,RINVPEFF+2)                             
         EJECT                                                                  
BLD60    LA    R2,INADYPTH                                                      
         BAS   RE,ANY                                                           
         BAS   RE,MOVE                                                          
         MVC   RINVDP,WORK         DAYPARTS TO RECORD                           
         SPACE 1                                                                
         LA    R3,INVERR                                                        
         LA    R1,1(R1)                                                         
         LA    R5,RINVDP                                                        
         MVC   WORK(L'DPTBL+1),DPTBL                                            
BLD65    LA    RE,WORK                                                          
         SPACE 1                                                                
BLD70    CLC   0(1,R5),0(RE)                                                    
         BE    BLD80                                                            
         CLI   0(RE),X'FF'                                                      
         BE    ERROR                                                            
         LA    RE,1(RE)                                                         
         B     BLD70                                                            
BLD80    MVI   0(RE),X'FE'         ELIMINATE DUP DPTS-REMOVE FROM TABLE         
         LA    R5,1(R5)                                                         
         BCT   R1,BLD65                                                         
         SPACE 1                                                                
         MVC   RINVPFLT,SPACES                                                  
         LA    R2,INAFILTH              ADD THE FILTERS                         
         BAS   RE,MOVE                                                          
         MVC   RINVPFLT,WORK                                                    
         SPACE 1                                                                
         LA    R3,INVERR                                                        
         LA    R1,1(R1)                                                         
         LA    R5,RINVPFLT                                                      
BLD85    LA    RE,BADFLT           CAN'T USE 1,2,3,4 AS FILTERS                 
*                         (USED TO BE FILTERS FOR FRINGE SUB-DAYPARTS)          
BLD90    CLC   0(1,R5),0(RE)                                                    
         BE    ERROR                                                            
         CLI   0(RE),X'FF'                                                      
         BE    BLD100                                                           
         LA    RE,1(RE)                                                         
         B     BLD90                                                            
BLD100   LA    R5,1(R5)                                                         
         BCT   R1,BLD85                                                         
         SPACE 1                                                                
         XR    R7,R7                                                            
         IC    R7,RINVPLEN                                                      
         LA    R7,34(R7)                                                        
         STC   R7,RINVLEN+1        SET RECORD LENGTH                            
         LA    R7,RINVREC(R7)                                                   
         MVI   0(R7),0             MARK END OF RECORD                           
         B     EXXMOD                                                           
         SPACE 2                                                                
DPTBL    DC    C'MDERATLWKNPVSJOXYU'                                            
         DC    X'FF'                                                            
         SPACE 1                                                                
BADFLT   DC    C'1234'                                                          
         DC    X'FF'                                                            
         EJECT                                                                  
*              CHECK FOR OVERLAPPING DATES                                      
         SPACE 1                                                                
CHKDATE  NTR1                                                                   
         LA    R2,INAEDTEH                                                      
         LA    R3,64                                                            
         CLC   RINVPEFF(2),RINVPEFF+2                                           
         BNH   *+16                                                             
         CLI   RINVPEFF+2,0                                                     
         BE    *+8                                                              
         B     ERROR               START HIGHER THAN END                        
         SPACE 1                                                                
         MVC   WORK(4),RINVPEFF    START AND END OF NEW RECORD                  
         LA    R3,65               DATES OVERLAP                                
         XC    KEY,KEY                                                          
         MVC   KEY(21),RINVREC                                                  
         LA    R4,REC2                                                          
         ST    R4,AIOAREA          SET IOAREA FOR REC2                          
         BAS   RE,HIGH                                                          
         B     INV230                                                           
         SPACE 1                                                                
INV210   BAS   RE,SEQ                                                           
INV230   CLC   KEYSAVE(21),KEY                                                  
         BNE   INV270              NO MATCH OK TO CONTINUE                      
         OC    KEY+24(3),KEY+24    MUST BE A HEADER                             
         BNZ   INV210                                                           
         SPACE 1                                                                
         CLI   BACT,C'C'                                                        
         BNE   INV235                                                           
         CLC   KEY(27),BKEY        THIS IS THE RECORD                           
         BE    INV210              I AM TRYING TO CHANGE                        
         SPACE 1                                                                
INV235   BAS   RE,GETREC                                                        
         OC    WORK+2(2),WORK+2                                                 
         BNZ   *+14                                                             
         OC    RINVPEFF+2(2),RINVPEFF+2                                         
         BZ    ERROR               CAN'T HAVE 2 WITH NO END                     
         SPACE 1                                                                
         OC    WORK+2(2),WORK+2    BUT IF EITHER HAS NO END                     
         BZ    INV210              DO NOT CHECK DATES                           
         OC    RINVPEFF+2(2),RINVPEFF+2                                         
         BZ    INV210                                                           
         SPACE 1                                                                
         CLC   WORK(2),RINVPEFF+2  START OF NEW HIGHER THAN END OF OLD          
         BNH   INV240                                                           
         B     INV210                                                           
         SPACE 1                                                                
INV240   CLC   WORK+2(2),RINVPEFF  END OF NEW LOWER THAN START OF OLD           
         BNL   ERROR                                                            
         B     INV210                                                           
         SPACE 1                                                                
INV270   LA    R4,REC                                                           
         ST    R4,AIOAREA          RESET I/O                                    
         B     EXXMOD                                                           
         EJECT                                                                  
*              DELETE ALL INVENTORY ITEMS                                       
         SPACE 1                                                                
INVDEL   LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         MVC   RINVKEY(27),BKEY    LAST RECORD DISPLAYED                        
         BAS   RE,HIGH                                                          
         CLC   KEYSAVE(27),KEY                                                  
         BNE   INVDELX                                                          
         BAS   RE,GETREC                                                        
         GOTO1 INVPTR,DMCB,AIOAREA,WORK2                                        
         GOTO1 DELPT,DMCB,WORK2         DELETE PASSIVE POINTERS                 
         MVC   RINVKEY(27),BKEY    LAST RECORD DISPLAYED                        
         BAS   RE,HIGH                                                          
         CLC   KEYSAVE(27),KEY                                                  
         BNE   INVDELX                                                          
         SPACE 1                                                                
         OI    KEY+27,X'80'        DELETE ALL BOOKS AND                         
         BAS   RE,WRITE            RATIONAL                                     
         BAS   RE,SEQ                                                           
         CLC   KEY(24),BKEY                                                     
         BE    *-18                                                             
         SPACE 1                                                                
INVDELX  XC    BKEY,BKEY                                                        
         B     EXXMOD                                                           
         EJECT                                                                  
*              CHANGE OLD RECORDS IF KEY CHANGE                                 
         SPACE 1                                                                
CHAOLD   NTR1                                                                   
         CLI   BACT,C'C'                                                        
         BNE   EXXMOD                                                           
         MVC   KEY(27),BKEY        OLD HEADER KEY                               
         BAS   RE,HIGH                                                          
         CLC   KEYSAVE(27),KEY                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
         LA    R2,WORK3                                                         
         ST    R2,AIOAREA                                                       
         BAS   RE,GETREC                                                        
         LA    R2,REC                                                           
         ST    R2,AIOAREA                                                       
         SPACE 1                                                                
         CLC   REC(27),BKEY        HAS KEY CHANGED                              
         BE    EXXMOD                                                           
         SPACE 1                                                                
         MVC   INVKLAST(27),REC    SAVE NEW INVENTORY NUMBER                    
         OI    KEY+27,X'80'                                                     
         BAS   RE,WRITE            DELETE OLD HEADER POINTER                    
         B     CHAOLDP                                                          
         SPACE 1                                                                
CHAOLDN  CLC   KEYSAVE(24),KEY     IS THIS AN OLD BOOK OR TEXT                  
         BNE   EXXMOD              NO, I AM FINISHED                            
         SPACE 1                                                                
         BAS   RE,GETREC                                                        
         OI    KEY+27,X'80'        DELETE OLD POINTER                           
         BAS   RE,WRITE                                                         
         SPACE 1                                                                
         MVC   REC(24),INVKLAST    NEW KEY                                      
         L     R4,AIOAREA                                                       
         GOTO1 VDELELEM,DMCB,(X'CE',(R4))                                       
         XC    WORK,WORK                                                        
         MVC   WORK(2),=X'CE0A'                                                 
         MVC   WORK+2(5),INVDAY                                                 
         MVC   WORK+7(3),INVSRC    FROMM BOOK                                   
         GOTO1 VADDELEM,DMCB,(R4),WORK                                          
         BAS   RE,FLADD            ADD IT                                       
         SPACE 1                                                                
CHAOLDP  MVC   KEY(27),BKEY        ITS DELETED SO I'LL GET NEXT                 
         BAS   RE,HIGH                                                          
         B     CHAOLDN                                                          
         EJECT                                                                  
*              END OLD INVENTORY THAT HAS NO END DATE                           
         SPACE 1                                                                
ENDOLD   NTR1                                                                   
         CLI   WORK2,0                                                          
         BNE   EXXMOD              NEW RECORD HAS END                           
         SPACE 1                                                                
         LA    R4,REC2             LOOK FOR ONE WITH NO END                     
         ST    R4,AIOAREA                                                       
         XC    KEY,KEY                                                          
         MVC   KEY(21),REC                                                      
         BAS   RE,HIGH                                                          
         SPACE 1                                                                
ENDOLDA  CLC   KEYSAVE(21),KEY                                                  
         BNE   ENDOLDX             NO MATCHING KEY                              
         SPACE 1                                                                
         OC    KEY+24(3),KEY+24    MUST BE A HEADER                             
         BNZ   ENDOLDN                                                          
         SPACE 1                                                                
         CLC   KEY(27),REC         THIS IS THE RECORD                           
         BE    ENDOLDN             I JUST ADDED                                 
         SPACE 1                                                                
         BAS   RE,GETREC                                                        
         OC    RINVPEFF+2(2),RINVPEFF+2                                         
         BNZ   ENDOLDN             THIS ONE HAS END DATE                        
         LA    R4,REC                                                           
         GOTO1 VDATCON,DMCB,(2,RINVPEFF),(0,WORK)  START OF NEW ITEM            
         LA    R4,REC2                                                          
         SPACE 1                                                                
         MVC   HALF,WORK2          NUMBER OF DAYS TO DECREASE                   
         LH    R2,HALF                                                          
         LCR   R2,R2                                                            
         GOTO1 VADDAY,DMCB,WORK,WORK+6,(R2)                                     
         GOTO1 VDATCON,DMCB,(0,WORK+6),(2,RINVPEFF+2)                           
         SPACE 1                                                                
         CLC   RINVPEFF(2),RINVPEFF+2   THE END CAN NOT BE LOWER                
         BNH   *+10                     THAN THE START                          
         MVC   RINVPEFF+2(2),RINVPEFF                                           
         BAS   RE,PUTREC                I FIXED IT                              
         B     ENDOLDX                                                          
         SPACE 1                                                                
ENDOLDN  BAS   RE,SEQ                                                           
         B     ENDOLDA                                                          
         SPACE 1                                                                
ENDOLDX  LA    R4,REC                                                           
         ST    R4,AIOAREA                                                       
         B     EXXMOD                                                           
         EJECT                                                                  
*              TRANSFER DATA                                                    
         SPACE 1                                                                
TRNSFER  NTR1                                                                   
         SPACE 1                                                                
         GOTO1 VCALLOV,DMCB,(X'17',0),(RA)                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VT80417,DMCB        A(T80417)                                    
         SPACE 1                                                                
         L     RF,=V(BKLST)                                                     
         RELOC                                                                  
         AR    RF,RE               RELOCATE ADDRESS                             
         ST    RF,VREBKLST                                                      
         SPACE 1                                                                
         MVC   TRSVKEY,REC         SAVE HEADER'S KEY                            
         SPACE                                                                  
TRNSFR1  LA    R2,INABKS1H                                                      
         SPACE                                                                  
TRNSFR2  DS    0H                                                               
         CLI   5(R2),0                                                          
         BE    TRNSFRX             END OF EDIT                                  
         MVI   BYTE4,1                                                          
         CLI   8(R2),C'*'          SKIP EDIT OF LINE IF BOOKS START             
         BE    TRNSFR4             WITH AN ASTERISK                             
         MVC   DEMEDIA(8),INVMED   RESTORE HEADER'S STATION                     
         BAS   RE,DATAREC                                                       
         CLI   ERRAREA,0                                                        
         BNE   TRNSFR6             ERROR ENCOUNTERED                            
*                                                                               
TRNSFR4  DS    0H                                                               
         ZIC   R0,BYTE4                                                         
         LA    R2,LINELEN(R2)      BUMP POINTER FOR EACH                        
         BCT   R0,*-4              CONTINUATION LINE INVOLVED                   
         LA    RF,INALAST                                                       
         CR    R2,RF               END OF SCREEN REACHED                        
         BL    TRNSFR2             NO                                           
         B     TRNSFRX             YES                                          
*                                                                               
* ERROR ROUTINE - PUT A STAR IN FIRST POSITION OF BOOKS TO STOP DOUBLE          
* EDITS ON LINES ABOVE ERROR LINE                                               
*                                                                               
TRNSFR6  DS    0H                                                               
         LA    R1,INABKS1H                                                      
*                                                                               
TRNSFR7  CR    R1,R2               REACHED ERROR LINE                           
         BE    TRNSFR10            YES                                          
         CLI   8(R1),C'*'          STAR THERE FROM PREVIOUS ERROR               
         BE    TRNSFR8             YES                                          
*                                                                               
         MVC   WORK(L'INABKS1),8(R1)                                            
         MVI   8(R1),C'*'          STAR IN BOOKS FIELD                          
         MVC   9(L'INABKS1-1,R1),WORK                                           
         OI    6(R1),X'80'         TRANSMIT                                     
*                                                                               
TRNSFR8  DS    0H                                                               
         LA    R1,LINELEN(R1)                                                   
         B     TRNSFR7                                                          
*                                                                               
TRNSFR10 DS    0H                                                               
         CLI   BACT,C'C'           FOR ACTION ADD, CHANGE THE ACTION            
         BE    TRNSFRX             FIELD TO ALLOW A FOLLOW UP CHANGE            
         MVC   LFMACT,SPACES       WITHOUT RE-POSITIONING CURSOR                
         MVC   LFMACT(3),=C'CHA'                                                
         NI    LFMACTH+4,X'DF'     TURN OFF VALID BIT TO FORCE EDIT             
         OI    LFMACTH+6,X'80'     TRANSMIT                                     
         B     TRNSFRX                                                          
*                                                                               
TRNSFRX  B     EXXMOD                                                           
         EJECT                                                                  
*              EDIT  TRANSFER DATA                                              
         SPACE 1                                                                
* R2 POINTS TO BOOKS HEADER                                                     
* R4 AT ENTRY POINTS TO HEADER RECORD                                           
*                                                                               
DATAREC  NTR1                                                                   
         LA    R5,WORK3                                                         
         LA    R5,1000(R5)         WORKING STORAGE FOR TRANSFER DATA            
         ST    R5,ATRANS                                                        
         XC    TRBKLIST,TRBKLIST                                                
         GOTO1 VREBKLST,DMCB,(R2),TRBKLIST,VBOOKVAL,CSCANNER                    
         LA    R3,INVERR                                                        
         CLI   DMCB,0                                                           
         BE    ERROR                                                            
         MVC   TRBKCNT,DMCB        NUMBER OF BOOK ENTRIES                       
         SPACE                                                                  
DATAR2   EQU   *                                                                
         ST    R2,THISLINE         SAVE LINE START                              
         LA    R2,CODEH(R2)        VALIDATE PROGRAM CODE                        
         BAS   RE,MOVE                                                          
         LA    RE,CODETAB                                                       
         LA    R1,CODES                                                         
         CLC   WORK(2),0(RE)                                                    
         BE    DATAR3                                                           
         LA    RE,L'CODETAB(RE)                                                 
         BCT   R1,*-14                                                          
         LA    RE,MONTAB           NOT A CODE, LOOK FOR A MONTH/YEAR            
         LA    R1,MONTHS                                                        
         CLC   WORK(1),0(RE)                                                    
         BE    *+16                VALID MONTH CODE                             
         LA    RE,1(RE)                                                         
         BCT   R1,*-14                                                          
         B     ERROR                                                            
         SPACE                                                                  
         CLI   WORK+1,C'0'         NOW LOOK FOR A NUMBER                        
         BL    ERROR                                                            
         CLI   WORK+1,C'9'                                                      
         BH    ERROR                                                            
         MVC   INVCODE,WORK                                                     
         MVI   INVCDCTL,PRO+INV                                                 
         B     DATAR5                                                           
         SPACE                                                                  
DATAR3   EQU   *                                                                
         MVC   INVCODE,WORK                                                     
         MVC   INVCDCTL,2(RE)      CONTROL BITS                                 
         SPACE                                                                  
DATAR5   EQU   *                                                                
         MVI   INVTYP,C'P'         DEFAULT-FROM DEMO FILES                      
         L     R2,THISLINE                                                      
         LA    R2,TYPEH(R2)                                                     
         CLI   5(R2),0                                                          
         BE    *+18                                                             
         CLI   5(R2),1             VALIDATE TYPE                                
         BNE   ERROR                                                            
         MVC   INVTYP,8(R2)                                                     
         CLI   INVTYP,C'I'         TEST FOR INVENTORY TRANSFER                  
         BE    DATAR6                                                           
         CLI   INVTYP,C'P'                                                      
         BNE   ERROR                                                            
         TM    INVCDCTL,PRO        CODE CONSISTENCY WITH 'P'                    
         BZ    ERROR                                                            
         SPACE                                                                  
DATAR6   EQU   *                                                                
         MVI   BYTE4,1             NO CONTINUATION                              
         L     R2,THISLINE                                                      
         LA    R2,FROMH(R2)                                                     
         SPACE                                                                  
         CLI   5(R2),0             TEST IF FROM DETAILS INPUT                   
         BNE   DATAR8              YES                                          
         CLI   INVTYP,C'I'                                                      
         BE    *+22                FOR BOOK TRANSFER                            
         CLC   INVCODE,=C'PR'      CODE 'PR' REQUIRES PURE NUMBER               
         BNE   *+12                                                             
         LA    R3,MISINP                                                        
         B     ERROR                                                            
         MVI   INVNO,1             NO-BUILD A DUMMY ENTRY                       
*                                                                               
         LA    R8,REC2+400                                                      
         ST    R8,INVLIST                                                       
         L     R3,ATRANS           TRANSFER DATA STORAGE                        
         XC    0(250,R3),0(R3)     ALSO BUILD A DUMMY HEADER AND                
         MVI   0(R3),250           DATA W DEFAULT DETAILS                       
         LA    R6,8(R3)            POINT R6 AT DATA START                       
         USING INVLD,R8                                                         
         XC    INVLREC,INVLREC     HAS NOT BEEN INPUT BY USER                   
         MVI   INVLWT,1                                                         
         MVC   INVLFLE,INVTYP      TAKE FILE FROM TYPE                          
         CLI   INVLFLE,C'I'        TEST FOR INVENTORY                           
         BE    DATAR7                                                           
         OI    INVLTYP,X'60'       SET DAY/TIME BITS                            
         MVC   INVLDAY,INVDAY      USE HEADER'S DAY/TIME IN                     
         MVC   INVLSTIM(4),INVTIME DUMMY ENTRY FOR BOOK TRANSFER                
         CLC   INVTIME+2(2),=C'CC' TEST FOR TO CONCLUSION                       
         BNE   DATAR6A                                                          
         SR    R1,R1               ADD 2 HOURS TO START                         
         ICM   R1,3,INVLSTIM                                                    
         AH    R1,=H'200'                                                       
         CH    R1,=H'2400'         TEST FOR RUN PAST MIDNIGHT                   
         BNH   *+8                                                              
         SH    R1,=H'2400'                                                      
         STCM  R1,3,INVLSTIM+2     SET END TIME                                 
         B     DATAR6A                                                          
         SPACE 1                                                                
DATAR6A  DS    0H                                                               
         GOTO1 VUNDAY,DMCB,INVDAY,(R6)                                          
         CLI   0(R6),C' '          TEST FOR END OF DAY EXPRESSION               
         BNH   *+12                                                             
         LA    R6,1(R6)                                                         
         B     *-12                                                             
         MVI   0(R6),COMMA         INSERT COMMA AFTER IT                        
         LA    R6,1(R6)                                                         
*                                                                               
         GOTO1 VUNTIME,DMCB,INVTIME,(R6)                                        
         CLI   0(R6),C' '          FIND END OF TIME EXPRESSION                  
         BNH   *+12                                                             
         LA    R6,1(R6)                                                         
         B     *-12                                                             
         OC    INVTIME+2(2),INVTIME+2 TEST FOR BREAK CODE                       
         BNZ   *+14                                                             
         MVC   0(2,R6),=C',B'                                                   
         LA    R6,2(R6)            BUMP OUTPUT POINTER                          
         LA    R1,8(R3)                                                         
         SR    R6,R1               FIND LENGTH OF DATA                          
         STC   R6,5(R3)                                                         
         B     DATAR15                                                          
         SPACE 1                                                                
DATAR7   MVI   INVLTYP,X'80'                                                    
         LA    RE,TRSVKEY                                                       
         MVC   INVLNUMB,RINVKINV-RINVKEY(RE)                                    
         MVC   INVLDATE,RINVKSTD-RINVKEY(RE)                                    
         ZIC   R1,INVLNUMB         QUARTER HOUR                                 
         CVD   R1,DUB                                                           
         UNPK  0(2,R6),DUB+6(2)                                                 
         OI    1(R6),X'F0'                                                      
         MVC   2(1,R6),INVLNUMB+1  DAY CODE                                     
         LA    R6,3(R6)                                                         
         CLI   INVLNUMB+2,C'0'     TEST FOR LENGTH OR SPECIAL CODE              
         BE    *+14                                                             
         MVC   0(1,R6),INVLNUMB+2                                               
         LA    R6,1(R6)                                                         
*                                                                               
         MVI   0(R6),COMMA         COMMA AFTER INVENTORY NUMBER                 
         LA    R6,1(R6)                                                         
         GOTO1 VDATCON,DMCB,(3,INVLDATE),(5,(R6))                               
         LA    R6,8(R6)                                                         
         LA    R1,8(R3)                                                         
         SR    R6,R1               FIND DATA LENGTH                             
         STC   R6,5(R3)                                                         
         B     DATAR15                                                          
         DROP  R8                                                               
         SPACE                                                                  
DATAR8   EQU   *                                                                
         L     R5,ATRANS                                                        
         XC    0(250,R5),0(R5)                                                  
         MVI   0(R5),250           BUILD DUMMY HEADER FIELD                     
         ZIC   RF,5(R2)                                                         
         LR    R1,RF                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R5),8(R2)                                                    
         STC   RF,5(R5)            INPUT LENGTH                                 
         SPACE                                                                  
         L     RE,THISLINE         POINT RE TO NEXT LINE                        
DATAR9   LA    RE,LINELEN(RE)      AND LOOK FOR A CONTINUATION LINE             
         LA    RF,INALAST                                                       
         CR    RE,RF                                                            
         BNL   DATAR12             PAST LAST DATA LINE.                         
         CLI   5(RE),0             BOOKS,TYPE,CODE ON NEXT LINE                 
         BNE   DATAR12             MUST BE EMPTY AND FROM MUST                  
         CLI   TYPEH+5(RE),0       HAVE INPUT TO BE A                           
         BNE   DATAR12             CONTINUATION LINE.                           
         CLI   CODEH+5(RE),0                                                    
         BNE   DATAR12                                                          
         CLI   FROMH+5(RE),0                                                    
         BE    DATAR12                                                          
         SPACE                                                                  
DATAR10  EQU   *                                                                
         AI    BYTE4,1             INCREMENT THE LINE COUNTER                   
         XR    R3,R3                                                            
         LA    R3,FROMH(RE)        POINT R3 AT FROM DETAILS                     
         ZIC   RF,5(R5)                                                         
         LA    R6,7(R5,RF)         POINT R6 AT LAST INPUT BYTE                  
         CLI   0(R6),COMMA                                                      
         BE    *+16                                                             
         LA    R6,1(R6)            POINT R6 AT NEXT BYTE AND                    
         MVI   0(R6),COMMA         INSERT THE COMMA                             
         LA    RF,1(RF)                                                         
         SPACE                                                                  
         LA    R6,1(R6)            BUMP TO FIRST AVAILABLE BYTE                 
         ZIC   R1,5(R3)                                                         
         AR    RF,R1               TOTAL INPUT                                  
         STC   RF,5(R5)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8              MOVE DATA TO STRING                          
         B     DATAR9              EDIT NEXT LINE DATA                          
         MVC   0(0,R6),8(R3)                                                    
         SPACE                                                                  
DATAR12  EQU   *                                                                
         LA    R8,REC2+400         REC2+400 CONTAINS INVLIST                    
         ST    R8,INVLIST                                                       
         LA    R8,WORK3                                                         
         LA    R8,2000(R8)         WORK3+2000 IS FOR SCANNER BLOCK              
         GOTO1 VINVLST,DMCB,(R5),(R8),(RC)                                      
         CLI   INVNO,0                                                          
         BNE   DATAR15                                                          
         ZIC   R3,FERN                                                          
         B     ERROR                                                            
         SPACE 2                                                                
* GENERATE DATA RECORDS IN A LOOP USING BOOK LIST.                              
* R3 WILL USED AS A COUNTER OF BOOK LIST ENTRIES.  ERROR                        
* MESSAGE NUMBERS MUST BE LOADED ONLY UPON ERROR EXIT                           
*                                                                               
DATAR15  DS    0H                                                               
         MVI   TRMODE,C'I'         INITIALIZE FOR BUFFERING AND                 
         GOTO1 =A(BUFFER),DMCB,(RC),RR=YES                                      
         ZIC   R3,TRBKCNT          COUNT OF ENTRIES IN BOOK LIST                
         LA    R5,TRBKLIST         R5 POINTS TO ENTRY                           
         SPACE                                                                  
DATAR16  EQU   *                                                                
         MVC   INVSRC,0(R5)                                                     
         MVC   INVFBK,1(R5)                                                     
         MVC   INVBTYPE,3(R5)      BOOK TYPE                                    
         MVC   INVTOBK(3),4(R5)                                                 
         CLC   INVTOBK+1(2),=C'AM'  TEST FOR SAME                               
         BNE   *+10                                                             
         MVC   INVTOBK(3),INVSRC   SET TO BOOK EQUAL TO FROM BOOK               
         SPACE                                                                  
         CLC   INVCODE,=C'TP'      DO NOT PERMIT CODE 'TP' IF                   
         BNE   DATAR17             FROM BOOK IS PRIOR TO OCT82                  
         CLC   INVFBK,=X'520A'                                                  
         BNL   DATAR17                                                          
         L     R2,THISLINE                                                      
         LA    R2,CODEH(R2)                                                     
         LA    R3,INVERR                                                        
         B     ERROR                                                            
         SPACE 1                                                                
DATAR17  LA    RE,REC              CLEAR AND BUILD ONE DATA                     
         LR    R4,RE               RECORD FOR EACH BOOK ENTRY IN                
         LA    RF,1000             REC.                                         
         SR    R1,R1                                                            
         MVCL  RE,R0               ZERO REC.                                    
         ST    R4,AIOAREA                                                       
         SPACE                                                                  
         MVC   RINVKEY,TRSVKEY     MOVE IN HEADER'S KEY                         
         MVC   RINVKSRC(3),INVTOBK                                              
         SPACE 1                                                                
         LA    R6,SVCLST           CONVERT FROM BOOKVAL TO KSRC                 
DATAR17C CLC   INVTOBK(1),3(R6)                                                 
         BE    DATAR17D                                                         
         LA    R6,L'SVCLST(R6)                                                  
         CLI   0(R6),X'FF'                                                      
         BNE   DATAR17C                                                         
         DC    H'0'                                                             
DATAR17D MVC   RINVKSRC,2(R6)                                                   
         SPACE 1                                                                
DATAR17E MVC   RINVLEN,=H'35'      SET LENGTH FOR VIRGIN RECORD                 
         SPACE                                                                  
         GOTO1 VT80417,DMCB,(RC)                                                
         SPACE                                                                  
         CLI   INVBAD,0                                                         
         BE    DATAR18                                                          
         ZIC   R3,INVBAD           ERROR MESSAGE FROM DEMO MODULE               
         L     R2,THISLINE         POSITION CURSOR AT FROM FIELD                
         LA    R2,FROMH(R2)                                                     
         B     ERROR                                                            
         SPACE                                                                  
DATAR18  DS    0H                                                               
         MVC   HALF,27(R4)         REPAIR RECORD LENGTH AFTER                   
         LH    RE,HALF             DEMO MODULES                                 
         BCTR  RE,0                                                             
         STCM  RE,3,27(R4)                                                      
         SPACE 1                                                                
         XC    WORK,WORK           PUT IN 'CE' EL BEFORE POSSIBLE               
         MVC   WORK(2),=X'CE0A'    DEMUP CALL.                                  
         MVC   WORK+2(5),INVDAY    HEADER DAY TIME                              
         MVC   WORK+7(3),INVSRC    FROM BOOK                                    
         GOTO1 VADDELEM,DMCB,(R4),WORK                                          
         SPACE                                                                  
         XC    REC2(200),REC2      BUILD TRANSFER FROM ELEMENT                  
         LA    RE,REC2             SO DEMUP WILL HAVE FROM SRC/BOOK             
         USING RINVFREL,RE                                                      
         MVI   RINVFRCD,X'03'                                                   
         MVC   RINVFRST,DEMSTA                                                  
         MVC   RINVFRBK,INVSRC                                                  
         MVC   RINVFRTY,INVTYP                                                  
         MVI   RINVFRPR,C'A'                                                    
         MVC   RINVFRBT,INVBTYPE                                                
         CLI   INVTYP,C'I'         TEST FOR INVENTORY TRANSFER                  
         BNE   *+10                                                             
         MVC   RINVFRBT,INVFRBT    USE BK TYPE PASSED BY T80417                 
         L     RF,THISLINE         A(THIS LINE)                                 
         LA    RF,FROMH(RF)        BUMP TO 'FROM DETAIL' FIELD                  
*                                                                               
*    TRANSFER BLOCK HAS BEEN CREAMED BY T80417 USE.  AS THERE IS                
*        NOTHING THERE, THE ORIGINAL LINE IS NOW USED.                          
*                                                                               
****>>>  L     RF,ATRANS           POINT TO TRANSFER BLOCK                      
*                                                                               
         ZIC   R1,5(RF)            VARIABLE DATA LENGTH                         
         LTR   R1,R1               ANYTHING ON LINE?                            
         BZ    DATA19              NO                                           
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   RINVFRDT(0),8(RF)   FROM DETAILS                                 
DATA19   EQU   *                                                                
         LA    R1,RINVFRDT-RINVFREL+1(R1)     FIND EL LEN                       
         STC   R1,RINVFRLN                                                      
         GOTO1 VADDELEM,DMCB,(R4),REC2                                          
         DROP  RE                                                               
         SPACE 1                                                                
         LA    RE,WORK             BUILD CODE ELEMENT                           
         USING RINVCEL,RE                                                       
         XC    WORK,WORK                                                        
         MVI   RINVCCOD,X'CD'                                                   
         MVI   RINVCLEN,10                                                      
         MVC   RINVCODE,INVCODE                                                 
         TM    INVCDCTL,TP         FOR TIME PERIOD TRANSFERS WHERE              
         BZ    *+18                AUTOMATIC FOOTNOTING IS SUPPRESSED,          
         CLI   TRFNOVER,YES        CLEAR THE CODE ON RECORD                     
         BNE   *+10                                                             
         MVC   RINVCODE,SPACES                                                  
         TM    INVTOBK,X'20'       ESTIMATED TO BOOK TEST                       
         BZ    *+8                                                              
         MVI   RINVCSET,C'E'                                                    
         SPACE 1                                                                
         TM    INVTOBK,X'04'       PROJECTED TO BOOK TEST                       
         BZ    *+8                                                              
         MVI   RINVCSET,C'P'                                                    
         SPACE 1                                                                
         TM    INVTOBK,X'02'       SPECIAL SURVEY BOOK TEST                     
         BZ    *+8                                                              
         MVI   RINVCSET,C'S'                                                    
         SPACE 1                                                                
         L     R1,THISLINE         POINT R1 AT FROM FIELD                       
         LA    R1,FROM(R1)                                                      
         CLI   0(R1),C'+'                                                       
         BNE   *+8                                                              
         OI    RINVCTYP,X'80'      ADDED DEMOS                                  
         OC    RINVCTYP,INVIND     CUMULATIVE INDICATORS                        
         GOTO1 VADDELEM,DMCB,(R4),WORK                                          
         DROP  RE                                                               
         SPACE 1                                                                
DATAR20  DS    0H                                                               
         L     R2,THISLINE                                                      
         LA    R2,UPGH(R2)                                                      
         CLI   5(R2),0                                                          
         BE    DATAR24                                                          
*                                                                               
* CODE FOR USER INPUT UPGRADE                                                   
*                                                                               
         XC    WORK,WORK                                                        
         GOTO1 VUPVAL,DMCB,(1,(R2)),WORK,(R7)                                   
         CLI   DMCB,1                                                           
         BE    *+12                                                             
         LA    R3,235              INVALID UPGRADE                              
         B     ERROR                                                            
         SPACE                                                                  
         L     RE,THISLINE         UPGRADES ARE NO GOOD FOR COMBOS              
         LA    RE,FROM(RE)                                                      
         CLI   0(RE),C'+'                                                       
         BNE   *+12                                                             
         LA    R3,INVERR                                                        
         B     ERROR                                                            
         SPACE 1                                                                
         LA    RE,WORK                                                          
         USING RAVLNEL,RE                                                       
         CLI   RAVLNTYP,3          ONLY PERMIT ONE BOOK OPERAND                 
         BNE   DATAR21             FOR PUTS                                     
         CLI   RAVLNCAT,C'P'                                                    
         BNE   DATAR21                                                          
         OC    RAVLNOP2,RAVLNOP2                                                
         BZ    DATAR21                                                          
         LA    R3,INVERR                                                        
         B     ERROR                                                            
         SPACE 1                                                                
DATAR21  CLI   RAVLNTYP,0          MANIPULATION OF UPGRADE DATA                 
         BE    DATAR22                                                          
         OC    RAVLNOP1,RAVLNOP1                                                
         BZ    DATAR22                                                          
         OC    RAVLNOP2,RAVLNOP2                                                
         BNZ   DATAR22                                                          
         MVC   RAVLNOP2,INVFBK                                                  
         DROP  RE                                                               
         SPACE                                                                  
DATAR22  DS    0H                                                               
         GOTO1 VADDELEM,DMCB,(R4),WORK                                          
         SPACE                                                                  
         MVI   BYTE2,0                                                          
         CLI   INVTYP,C'I'         TEST FOR INV TO INV TRANSFER                 
         BNE   *+8                                                              
         MVI   BYTE2,C'I'                                                       
*                                  BECAUSE OF UT/TP PROBLEM                     
         NI    11(R4),X'FF'-X'40'                                               
         GOTO1 VDEMUP,DMCB,(BYTE2,34(R4)),WORK,(R7)                             
         OI    11(R4),X'40'                                                     
         B     DATAR25                                                          
         SPACE 2                                                                
* CODE FOR MISSING UPGRADE INPUT.  FORCE UPGRADE TO GET HPT DATA                
* FOR TO DAY/TIME AND SOURCE/BOOK.                                              
*                                                                               
DATAR24  DS    0H                                                               
         CLC   INVCODE,=C'PJ'      CODE PJ REQUIRES UPGRADE                     
         BNE   *+12                                                             
         LA    R3,MISINP                                                        
         B     ERROR                                                            
*                                                                               
         XC    WORK,WORK           BUILD FORCED UPGRADE ELEMENT                 
         LA    RE,WORK                                                          
         USING RAVLNEL,RE                                                       
         MVI   RAVLNCOD,X'05'                                                   
         MVI   RAVLNLEN,14                                                      
         MVI   RAVLNTYP,4                                                       
         MVC   RAVLNOP1,=H'100'                                                 
         GOTO1 VADDELEM,DMCB,(R4),WORK                                          
         MVI   BYTE2,0                                                          
         CLI   INVTYP,C'I'         TEST FOR INV TO INV TRANSFER                 
         BNE   *+8                                                              
         MVI   BYTE2,C'I'                                                       
*                                  BECAUSE OF UT/TP PROBLEM                     
         NI    11(R4),X'FF'-X'40'                                               
         GOTO1 VDEMUP,(R1),(BYTE2,34(R4)),WORK,(R7)                             
         OI    11(R4),X'40'                                                     
         B     DATAR25                                                          
         DROP  RE                                                               
         SPACE                                                                  
DATAR25  EQU   *                                                                
         MVI   TRMODE,C'P'                                                      
         GOTO1 =A(BUFFER),DMCB,(RC),RR=YES                                      
         LA    R5,7(R5)                                                         
         BCT   R3,DATAR16                                                       
         SPACE                                                                  
         LA    R4,REC                                                           
         ST    R4,AIOAREA                                                       
         MVI   TRMODE,C'F'         FINAL BUFFERING                              
         GOTO1 =A(BUFFER),DMCB,(RC),RR=YES                                      
         MVI   TRMODE,C'W'         HANDLE I/O TO REPFILE                        
         GOTO1 =A(BUFFER),DMCB,(RC),RR=YES                                      
         B     EXXMOD                                                           
         SPACE 2                                                                
CODETAB  DS    0CL3                                                             
         DC    C'TP',AL1(PRO+TP)                                                
         DC    C'TT',AL1(PRO+TP)                                                
         DC    C'ES',AL1(PRO+INV)                                               
         DC    C'PJ',AL1(PRO+INV)                                               
         DC    C'PR',AL1(PRO)                                                   
         DC    C'PA',AL1(PRO+INV)                                               
         DC    C'PT',AL1(PRO+INV+MIX)                                           
         DC    C'TE',AL1(PRO+INV)                                               
         DC    C'PE',AL1(PRO+INV)                                               
         DC    C'NT',AL1(PRO+INV)                                               
         DC    C'FT',AL1(PRO+INV)                                               
         DC    C'MT',AL1(PRO+INV)                                               
         DC    C'YT',AL1(PRO+INV)                                               
         DC    C'JT',AL1(PRO+INV)                                               
         DC    C'OT',AL1(PRO+INV)                                               
         DC    C'RT',AL1(PRO+INV)                                               
         DC    C'NP',AL1(PRO+INV)                                               
         DC    C'FP',AL1(PRO+INV)                                               
         DC    C'MP',AL1(PRO+INV)                                               
         DC    C'YP',AL1(PRO+INV)                                               
         DC    C'OP',AL1(PRO+INV)                                               
         DC    C'RP',AL1(PRO+INV)                                               
         DC    C'JP',AL1(PRO+INV)                                               
         DC    C'  ',AL1(PRO+INV)                                               
CODES    EQU   (*-CODETAB)/L'CODETAB                                            
         SPACE                                                                  
MONTAB   DC    C'NFMAYJO'                                                       
MONTHS   EQU   (*-MONTAB)                                                       
         SPACE 1                                                                
         DS    0H                                                               
         EJECT                                                                  
MSGOUT   NTR1                                                                   
         CLI   BACT,C'A'                                                        
         BE    *+12                                                             
         CLI   BACT,C'C'                                                        
         BNE   EXXMOD                                                           
         XC    LFMMSG,LFMMSG                                                    
         SPACE 1                                                                
         MVC   LFMMSG+18(25),=C' INVENTORY NUMBER IS XXXX'                      
         LA    R5,LFMMSG+39                                                     
         SR    R6,R6                                                            
         IC    R6,RINVKQTR                                                      
         CVD   R6,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DUB(3),DUB+6(2)                                                  
         MVC   0(2,R5),DUB+1                                                    
         LA    R5,2(R5)                                                         
         SPACE 1                                                                
         MVC   0(1,R5),RINVKDAY                                                 
         MVC   1(1,R5),RINVKLEN                                                 
         CLI   1(R5),C'0'                                                       
         BNE   *+8                                                              
         MVI   1(R5),C' '                                                       
         B     EXXMOD                                                           
         EJECT                                                                  
*              ADD THE RECORD TO FILE                                           
         SPACE 1                                                                
FLADD    NTR1                                                                   
         USING RINVAEL,R5                                                       
         MVC   KEY,RINVREC                                                      
         SPACE 1                                                                
         LA    R5,WORK                                                          
         XC    WORK,WORK                                                        
         MVC   RINVACOD(2),=X'EF0C'                                             
         MVC   RINVAFST,TODAY                                                   
         MVC   RINVALST,TODAY                                                   
         MVI   RINVAWHY,C'A'                                                    
         OI    DMINBTS,X'08'       PASS DELETES                                 
         BAS   RE,HIGH                                                          
         CLC   KEYSAVE(27),KEY                                                  
         BE    FLPUT                                                            
         SPACE 1                                                                
         GOTO1 VADDELEM,DMCB,AIOAREA,(R5)                                       
         BAS   RE,ADDREC           ADD THE RECORD                               
         MVC   BSVDA,KEY                                                        
         NI    DMINBTS,X'F7'       TURN OFF PASS DELETES                        
         B     EXXMOD                                                           
         SPACE 1                                                                
FLPUT    TM    KEY+27,X'80'                                                     
         BNO   *+12                                                             
         MVI   KEY+27,0                                                         
         BAS   RE,WRITE            UNDELETE THE POINTER                         
         LA    RE,REC2                                                          
         ST    RE,AIOAREA                                                       
         BAS   RE,GETREC           GET OLD RECORD IN REC2                       
         SPACE 1                                                                
         GOTO1 VGETEL,DMCB,(X'EF',AIOAREA),DMCB+8                               
         L     R5,DMCB+8                                                        
         CLI   DMCB,X'FF'                                                       
         BNE   *+8                                                              
         LA    R5,WORK                                                          
         MVC   RINVALST,TODAY                                                   
         MVI   RINVAWHY,C'C'                                                    
         SPACE 1                                                                
         LA    RE,REC                                                           
         ST    RE,AIOAREA                                                       
         SPACE 1                                                                
         GOTO1 VDELELEM,DMCB,(X'EF',AIOAREA)                                    
         GOTO1 VADDELEM,DMCB,AIOAREA,(R5)                                       
         BAS   RE,PUTREC           WRITE BACK THE NEW                           
         NI    DMINBTS,X'F7'                                                    
         MVC   BSVDA,KEY+28                                                     
         B     EXXMOD                                                           
         EJECT                                                                  
*              EDIT INVENTORY NUMBER                                            
         SPACE 1                                                                
INVEDT   LA    R3,INVERR                                                        
         CLI   0(R6),3             INVENTORY NUMBER MUST BE THREE               
         BL    ERROR                                                            
         CLI   0(R6),4                                                          
         BH    ERROR                                                            
         CLI   12(R6),C'0'                                                      
         BL    ERROR                                                            
         CLI   12(R6),C'9'                                                      
         BH    ERROR                                                            
         CLI   13(R6),C'0'                                                      
         BL    ERROR                                                            
         CLI   13(R6),C'9'                                                      
         BH    ERROR                                                            
         SPACE 1                                                                
         PACK  DUB(8),12(2,R6)     QUARTER HOUR CODE                            
         CVB   R0,DUB                                                           
         STC   R0,RINVKQTR                                                      
         MVC   RINVKDAY,14(R6)     DAY CODE                                     
         MVI   RINVKLEN,C'0'       LENGTH                                       
         CLI   0(R6),4                                                          
         BNE   *+10                                                             
         MVC   RINVKLEN,15(R6)                                                  
         BR    R8                                                               
         SPACE 2                                                                
*              EDIT DATE                                                        
         SPACE 1                                                                
DATEDT   LA    R3,INVERR                                                        
         GOTO1 VDATVAL,DMCB,(0,12(R6)),WORK                                     
         OC    DMCB(4),DMCB                                                     
         BZ    ERROR                                                            
         GOTO1 VDATCON,DMCB,(0,WORK),(3,RINVKSTD)                               
         BR    R8                                                               
         EJECT                                                                  
*              ROUTINE TO ADD PASSIVE POINTERS                                  
         SPACE 1                                                                
*              PARAM 1   BYTES 1-3 A(LIST OF POINTERS)                          
         SPACE 1                                                                
NWPT     NTR1                                                                   
         L     R2,0(R1)                                                         
NWPT1    CLI   0(R2),0                                                          
         BE    EXXMOD              END OF LIST                                  
         MVC   KEY(27),0(R2)                                                    
         OI    DMINBTS,X'08'                                                    
         BAS   RE,HIGH                                                          
         NI    DMINBTS,X'F7'                                                    
         CLC   KEYSAVE(27),KEY                                                  
         BE    NWPT3                                                            
         MVC   KEY(28),0(R2)                                                    
         MVC   KEY+28(4),BSVDA                                                  
         BAS   RE,ADD                                                           
         B     NWPT4                                                            
         SPACE 1                                                                
NWPT3    MVC   KEY(28),0(R2)                                                    
         MVC   KEY+28(4),BSVDA                                                  
         BAS   RE,WRITE                                                         
         SPACE 1                                                                
NWPT4    LA    R2,32(R2)                                                        
         B     NWPT1                                                            
         SPACE 2                                                                
*              ROUTINE TO DELETE POINTERS                                       
         SPACE 1                                                                
DELPT    NTR1                                                                   
         L     R2,0(R1)                                                         
DELPT1   CLI   0(R2),0                                                          
         BE    EXXMOD                                                           
         MVC   KEY(27),0(R2)                                                    
         BAS   RE,HIGH                                                          
         CLC   KEYSAVE(27),KEY                                                  
         BNE   DELPT4                                                           
         OI    KEY+27,X'80'                                                     
         BAS   RE,WRITE                                                         
         SPACE 1                                                                
DELPT4   LA    R2,32(R2)                                                        
         B     DELPT1                                                           
         EJECT                                                                  
       ++INCLUDE REINVPTR                                                       
         EJECT                                                                  
       ++INCLUDE RESVCTAB                                                       
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE RELFMINC                                                       
         EJECT                                                                  
       ++INCLUDE FLDIND                                                         
         EJECT                                                                  
       ++INCLUDE RGENEROL                                                       
         LTORG                                                                  
         EJECT                                                                  
BUFFER   NMOD1 0,*BUFFER*                                                       
         L     RC,0(R1)            RESET A(WORK AREA)                           
         CLI   TRMODE,C'P'                                                      
         BE    BUFPUT                                                           
         CLI   TRMODE,C'I'                                                      
         BE    BUFINIT                                                          
         CLI   TRMODE,C'F'                                                      
         BE    BUFFIN                                                           
         CLI   TRMODE,C'W'                                                      
         BE    BUFWRT                                                           
         DC    H'0'                                                             
         SPACE 2                                                                
BUFINIT  DS    0H                  INITIALIZE                                   
         XC    TRPAGE(2),TRPAGE    CLEAR PAGE AND RECORD COUNT                  
         LA    RE,WORK3                                                         
         LA    RE,2000(RE)                                                      
         ST    RE,TRAPAGE          SET POINTER TO PAGE AREA FOR BUFFER          
         LA    RF,2304                                                          
         SR    R1,R1                                                            
         MVCL  RE,R0               CLEAR THE PAGE AREA                          
         B     BUFFERX                                                          
         SPACE                                                                  
*                                                                               
* PUT TWO RECORDS INTO BUFFER - WHEN TWO ARE THERE, WRITE A PAGE TO TWA         
*                                                                               
BUFPUT   DS    0H                                                               
         L     R5,TRAPAGE                                                       
         CLI   0(R5),0             FIRST I/O AREA FREE                          
         BE    BUFPUT2             YES                                          
         LA    R5,1000(R5)         NO-TRY NEXT ONE                              
         CLI   0(R5),0                                                          
         BE    BUFPUT2             SECOND IS FREE                               
*                                                                               
         L     R5,TRAPAGE          WRITE PAGE WITH RECORDS IN IT FIRST          
         ZIC   R6,TRPAGE                                                        
         LA    R6,1(R6)                                                         
         STC   R6,TRPAGE           UPDATE PAGES ALREADY WRITTEN                 
         XC    DMCB+8(4),DMCB+8                                                 
         STC   R6,DMCB+8                                                        
         MVC   DMCB+10(2),2(RA)    TERMINAL NUMBER                              
         GOTO1 VDATAMGR,DMCB,=C'DMWRT ',=C'TEMPSTR',,(R5)                       
         LR    RE,R5                                                            
         LA    RF,2304                                                          
         SR    R1,R1                                                            
         MVCL  RE,R0               RE-CLEAR PAGE AREA                           
*                                                                               
BUFPUT2  DS    0H                                                               
         GOTO1 VMOVEREC,DMCB,AIOAREA,(R5)                                       
         ZIC   R1,TRRECS                                                        
         LA    R1,1(R1)            INCREMENT COUNT OF RECORDS ALREADY           
         STC   R1,TRRECS           WRITTEN                                      
         B     BUFFERX                                                          
*                                                                               
* WRITE LAST PAGE OUT TO TWA                                                    
*                                                                               
BUFFIN   DS    0H                  END OF LINE ROUTINE                          
         L     R5,TRAPAGE                                                       
         ZIC   R6,TRPAGE                                                        
         LA    R6,1(R6)                                                         
         STC   R6,TRPAGE                                                        
         XC    DMCB+8(4),DMCB+8                                                 
         STC   R6,DMCB+8                                                        
         MVC   DMCB+10(2),2(RA)                                                 
         GOTO1 VDATAMGR,DMCB,=C'DMWRT ',=C'TEMPSTR',,(R5)                       
         B     BUFFERX                                                          
*                                                                               
* READ IN PAGES, MOVE RECORDS TO REC, AND WRITE THEM TO REPFILE                 
*                                                                               
BUFWRT   DS    0H                                                               
         SR    R3,R3               PAGE COUNTER                                 
*                                                                               
BUFWRT2  DS    0H                                                               
         LA    R3,1(R3)                                                         
         L     R5,TRAPAGE                                                       
         XC    DMCB+8(4),DMCB+8                                                 
         STC   R3,DMCB+8                                                        
         MVC   DMCB+10(2),2(RA)                                                 
         GOTO1 VDATAMGR,DMCB,=C'DMRDIR',=C'TEMPSTR',,(R5)                       
         LA    R6,2                                                             
*                                                                               
BUFWRT4  DS    0H                                                               
         GOTO1 VMOVEREC,DMCB,(R5),AIOAREA                                       
         BAS   RE,FLADD                                                         
         LA    R5,1000(R5)                                                      
         BCT   R6,*+8                                                           
         B     BUFWRT6                                                          
         CLI   0(R5),0             SECOND RECORD ON PAGE                        
         BNE   BUFWRT4             YES                                          
*                                                                               
BUFWRT6  DS    0H                                                               
         CLM   R3,1,TRPAGE                                                      
         BL    BUFWRT2                                                          
*                                                                               
BUFFERX  EQU   *                                                                
         XMOD1                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE RGENOLD                                                        
         EJECT                                                                  
       ++INCLUDE RELFMWRK                                                       
         EJECT                                                                  
       ++INCLUDE RELFMTWA                                                       
         EJECT                                                                  
       ++INCLUDE RELFMEFD                                                       
         EJECT                                                                  
* INVENTORY LIST ENTRY DSECT                                                    
*                                                                               
INVLD    DSECT                                                                  
INVLREC  DS    0CL10                                                            
INVLFLE  DS    CL1                 P=PAV, I=INVENTORY                           
INVLTYP  DS    CL1                 X'80'  INVENTORY NUMBER                      
*                                  X'40'  FIRST IN DAY/TIME EXP.                
*                                  X'20'  LAST IN DAY/TIME EXP.                 
*                                  X'08'  ADD EXPRESSION                        
INVLWT   DS    CL1                 WEIGHT (BINARY)                              
INVLDATA DS    0CL6                                                             
INVLSTIM DS    CL2                 START TIME                                   
INVLETIM DS    CL2                 END TIME                                     
INVLDAY  DS    CL1                 DAY                                          
         DS    CL1                 SPARE                                        
         ORG   INVLDATA                                                         
INVLNUMB DS    CL3                 NUMBER                                       
INVLDATE DS    CL3                 START DATE (Y/M/D BINARY)                    
         DS    CL1                 SPARE                                        
         SPACE 2                                                                
* EQUATES                                                                       
*                                                                               
MISINP   EQU   1                                                                
PRO      EQU   X'01'                                                            
INV      EQU   X'02'                                                            
TP       EQU   X'04'               READ TIME PERIOD FILE                        
MIX      EQU   X'08'               READ FROM PAV AND TIME PERIOD                
COMMA    EQU   C','                                                             
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
TYPEH    EQU   INATYPEH-INABKS1H                                                
TYPE     EQU   INATYPE-INABKS1H                                                 
CODEH    EQU   INAPCDEH-INABKS1H                                                
CODE     EQU   INAPCDE-INABKS1H                                                 
FROMH    EQU   INAFROMH-INABKS1H                                                
FROM     EQU   INAFROM-INABKS1H                                                 
UPGH     EQU   INAUPGH-INABKS1H                                                 
UPG      EQU   INAUPG-INABKS1H                                                  
LINELEN  EQU   INABKS2H-INABKS1H                                                
         PRINT OFF                                                              
         EJECT                                                                  
RINVD    DSECT                                                                  
       ++INCLUDE REGENINV                                                       
       ++INCLUDE REGENAVL                                                       
       ++INCLUDE DDCOMFACS                                                      
         SPACE 2                                                                
       ++INCLUDE FATWA                                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'065RELFM05   05/01/02'                                      
         END                                                                    
