*          DATA SET SRPFM01    AT LEVEL 009 AS OF 08/22/00                      
*PHASE T13701A                                                                  
         TITLE 'PFM01 - VALIDATE DATA DEFINITION AND DISPLAY'                   
         PRINT NOGEN                                                            
PFM01    CSECT                                                                  
         NMOD1 032,**PF01**,RA                                                  
         USING PFMTEMPD,R9         R9=A(GLOBAL W/S)                             
         USING PFMSAVED,R3         R3=A(TWA)                                    
         XC    FERRS,FERRS                                                      
         XC    STIFINFO,STIFINFO   CLEAR FILE INFO                              
         XC    STIRINFO,STIRINFO   CLEAR RECORD INFO                            
         XC    STIEINFO,STIEINFO   CLEAR ELEMENT INFO                           
         XC    STITINFO,STITINFO   CLEAR RECOVER INFO                           
         XC    SLNRECL,SLNRECL                                                  
         XC    SLIOAREA,SLIOAREA                                                
         XC    SLELINFO,SLELINFO                                                
         XC    OKEYAH+8(20),OKEYAH+8                                            
         OI    OKEYAH+6,X'80'                                                   
         EJECT                                                                  
*        FILE NAME MUST BE PRESENT & CORRECT                                    
*                                                                               
FILE     CLI   IFILEH+5,0                                                       
         BNE   FILE1                                                            
         MVI   FERN,01                                                          
         B     FILERR              ERROR MISSING FILE NAME                      
FILE1    CLI   IFILEH+5,4                                                       
         BL    FILE3                                                            
         SR    R6,R6                                                            
         IC    R6,IFILEH+5                                                      
         BCTR  R6,R0                                                            
         L     R4,AFILETBL                                                      
FILE2    CLI   0(R4),0             SEARCH FILE TABLE LOOP                       
         BNE   FILE4                                                            
FILE3    MVI   FERN,02                                                          
         B     FILERR              ERROR INVALID FILE NAME                      
FILE4    EX    R6,FILE5                                                         
         BE    FILEOK                                                           
         LA    R4,20(R4)                                                        
         B     FILE2                                                            
FILE5    CLC   0(0,R4),IFILEH+8    EXECUTED COMP FILE NAME                      
*                                                                               
FILERR   LA    R6,IFILEH                                                        
         ST    R6,FERRS                                                         
         OI    FIND,X'01'                                                       
         B     EXIT                                                             
*                                                                               
FILEOK   MVC   STIFINFO,8(R4)      SAVE FILE INFO                               
         CLI   STIFN,2             ADJUST TEMPSTR RECORD LENGTH                 
         BNE   FILEOK1                                                          
         LH    R1,TWAL                                                          
         BCTR  R1,0                                                             
         STH   R1,STIFRL                                                        
         B     RID                                                              
FILEOK1  CLI   STIFN,14            ADJUST TEMPEST RECORD LENGTH                 
         BNE   RID                                                              
         LH    R1,TMSL                                                          
         BCTR  R1,0                                                             
         STH   R1,STIFRL                                                        
         B     RID                                                              
         EJECT                                                                  
*        RECORD ID MUST BE PRESENT & CORRECT                                    
*                                                                               
RID      CLI   IRIDH+5,0                                                        
         BNE   *+12                                                             
RID0     MVI   FERN,03             ERROR MISSING REC ID                         
         B     RIDERR                                                           
         L     R4,AKEYTBL                                                       
RID1     CLI   0(R4),0             SEARCH KEY TABLE                             
         BNE   *+12                                                             
RID2     MVI   FERN,04             ERROR INVALID REC ID                         
         B     RIDERR                                                           
         CLC   0(2,R4),IRIDH+8                                                  
         BE    RID3                                                             
         LA    R4,4(R4)                                                         
         B     RID1                                                             
*                                                                               
RID3     TM    3(R4),X'01'                                                      
         BZ    RID2                ERROR N/D FOR RECORD ID                      
         MVC   STIKN,2(R4)                                                      
*                                                                               
RID4     CLI   STIKN,1             K,..                                         
         BE    RIDK                                                             
         CLI   STIKN,2             A,..                                         
         BE    RIDA                                                             
         CLI   STIKN,3             FI..                                         
         BE    RIDF                                                             
         CLI   STIKN,4             NE..                                         
         BE    RIDN                                                             
         CLI   STIKN,7             LA..                                         
         BE    RIDL                                                             
         DC    H'0'                                                             
         SPACE 2                                                                
RIDK     EQU   *                   KEY FORMAT K,....                            
         SR    R6,R6                                                            
         IC    R6,IRIDH+5                                                       
         SH    R6,=H'3'                                                         
         BM    RID0                ERROR NO KEY DATA                            
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   WSS(0),IRIDH+10     SET WSS FOR V(DECODE)                        
         LA    R6,WSS+1(R6)                                                     
         MVI   0(R6),C' '                                                       
*                                                                               
         IC    R5,STIFKL           GET KEY LEN                                  
         IC    R6,STIFFKBV         GET KEY FILL CHR                             
         GOTO1 ADECODE,HEXWS,((R5),WSS),((R6),STIK),0                           
*                                                                               
         CLI   8(R1),X'FF'                                                      
         BNE   *+18                                                             
         MVC   FERN(4),8(R1)       KEY INVALID                                  
         MVI   FERN,0              SET SPECIAL ERR MSG CODE                     
         B     RIDERR                                                           
         CLI   STIFN,2             TEST TEMPSTR                                 
         BE    RIDK2                                                            
         CLI   STIFN,14            TEST TEMPEST                                 
         BE    RIDK2                                                            
         MVC   STIKL,STIFKL        KEY VALID                                    
         B     RIDOK                                                            
*                                                                               
RIDK2    MVI   STIKL,4             TEMP(STR)/(EST) KEY = X'PP00TTTT'            
         CLI   STIK,X'80'                                                       
         BL    RIDK2A                                                           
         CLI   STIK,X'95'          MAX 4 SESSIONS 2+4*5=22 TWAS/TRM             
         BH    RIDK2B                                                           
         B     RIDK2C                                                           
RIDK2A   CLI   STIK,11                                                          
         BNH   *+12                                                             
RIDK2B   MVI   FERN,44             ERROR IN TEMPSTR PAGE/TERMINAL               
         B     RIDERR                                                           
RIDK2C   CLI   STIK+1,0                                                         
         BNE   RIDK2B                                                           
         MVI   STIK+1,X'FF'                                                     
         CLC   STIK+2(2),=H'5000'                                               
         BH    RIDK2B                                                           
         OC    STIK+2(2),STIK+2    IS TRM MUN ZERO                              
         BNZ   RIDOK               NO                                           
         L     R6,APARM            YES USE UTL ENTRY VALUE                      
         L     R6,8(R6)                                                         
         MVC   STIK+2(2),0(R6)                                                  
         B     RIDOK                                                            
         SPACE 2                                                                
RIDA     EQU   *                   KEY FORMAT A,.......                         
         SR    R6,R6                                                            
         IC    R6,IRIDH+5                                                       
         SH    R6,=H'2'                                                         
         BZ    RID0                                                             
         GOTO1 AHEXIN,HEXWS,IRIDH+10,STIK,(R6)                                  
         OC    12(4,R1),12(R1)                                                  
         BNZ   *+12                                                             
RIDA1    MVI   FERN,07             ERROR INV HEX                                
         B     RIDERR                                                           
         SRL   R6,1                                                             
         STC   R6,STIKL            SET KEY LENGTH                               
         CLI   STIKL,4                                                          
         BE    RIDA4                                                            
         MVI   FERN,09                                                          
         B     RIDERR              ERROR DISK ADDR NOT 8 HEX CHRS               
*                                                                               
RIDA4    OC    STIK(2),STIK        TT MUST BE GE 01                             
         BZ    RIDA5                                                            
         CLI   STIK+2,0            B MUST BE GE 1                               
         BE    RIDA5                                                            
         B     RIDOK                                                            
RIDA5    MVI   FERN,10             ERROR INV DISK ADDR FORMAT                   
         B     RIDERR                                                           
         SPACE 2                                                                
RIDF     EQU   *                   KEY FORMAT FI.....                           
         CLI   STIFFKBN,X'FF'                                                   
         BE    RID2                CANT HAVE FIRST FOR FILE                     
         SR    R6,R6                                                            
         IC    R6,STIFFKBN                                                      
         SR    R5,R5                                                            
         STC   R5,STIK(R6)         SET STIK TO ZEROS                            
         LA    R6,1(R6)                                                         
         STC   R6,STIKL                                                         
         B     RIDOK                                                            
         SPACE 2                                                                
RIDN     EQU   *                   KEY FORMAT NE.....                           
         CLC   STIFN,SLRF                                                       
         BNE   RIDN1                                                            
         CLC   SLRI,0                                                           
         BNE   RIDOK                                                            
RIDN1    MVI   FERN,11             ERROR NO PREV REC FOR FILE                   
         B     RIDERR                                                           
         SPACE 2                                                                
RIDL     EQU   *                   KEY FORMAT LA..                              
         CLC   STIFN,SLRF                                                       
         BNE   RIDL1               NOT SAME FILE                                
         CLC   SLRI,0                                                           
         BE    RIDL1               NO VALID I/O                                 
         CLI   SLIRA,1                                                          
         BNE   RIDL1               PREV ACTION MUST BE DISP                     
         MVC   STIKL,SLIKL                                                      
         MVC   STIK,SLIK                                                        
         B     RIDOK                                                            
RIDL1    MVI   FERN,11             ERROR NO PREV REC FOR FILE                   
         B     RIDERR                                                           
         SPACE 2                                                                
RIDERR   LA    R6,IRIDH                                                         
         ST    R6,FERRS                                                         
         OI    FIND,X'01'                                                       
         B     EXIT                                                             
RIDOK    EQU   *                                                                
         EJECT                                                                  
*        RECORD ACTION,START,END ARE OPTIONALLY PRESENT                         
*                                                                               
RACTN    XC    DISPSB,DISPSB       SET DEFAULT VALUES                           
         XC    0(256,RC),0(RC)                                                  
         MVI   STIRA,1             ACTION=DISPLAY                               
         CLI   STIFT,2                                                          
         BNE   *+8                                                              
         MVI   STIRA,4             ACTION=BROWSE FOR I/S                        
*                                                                               
RACTNO   CLI   IRACTNH+5,0                                                      
         BE    RACTNOK                                                          
         GOTO1 ASCANNER,HEXWS,(0,IRACTNH),(3,(RC))                              
         CLI   4(R1),0                                                          
         BNE   RACTN1                                                           
         MVI   FERN,05             ERROR IN SCANNER                             
         B     RACTNERR                                                         
         SPACE 2                                                                
RACTN1   LR    R5,RC               POINT TO ACTION LINE                         
         CLI   1(R5),0                                                          
         BE    *+12                                                             
RACTN1A  MVI   FERN,25             INVALID ACTION NAME                          
         B     RACTNERR                                                         
         CLI   0(R5),0                                                          
         BE    RACTN2              MISSING ACTION NAME                          
         CLI   0(R5),7                                                          
         BH    RACTN1A             ACTION NAME MUST BE LE 7                     
         SR    R6,R6                                                            
         IC    R6,0(R5)                                                         
         BCTR  R6,0                R6=L'ACTION NAME-1                           
         L     R7,AACTNTBL                                                      
RACTN1B  CLI   0(R7),0             SEARCH ACTION TABLE                          
         BE    RACTN1A                                                          
         EX    R6,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R7),12(R5)                                                   
         BE    *+12                                                             
         LA    R7,9(R7)                                                         
         B     RACTN1B                                                          
         TM    8(R7),X'01'                                                      
         BZ    RACTN1A             ACTION N/D FOR RECORD                        
         MVC   STIRA,7(R7)         SAVE ACTION VALUE                            
         SPACE 2                                                                
RACTN2   CLI   STIRA,6             FIX RECOVER TYPE DISPLAY                     
         BNE   *+20                                                             
         MVI   STIRA,1                                                          
         MVI   STITFILE,X'FF'                                                   
         TM    STIFTL,X'02'                                                     
         BZ    RACTN1A                                                          
         LA    R5,32(R5)           POINT TO START BYTE LINE                     
         CLI   1(R5),0                                                          
         BNE   RACTN2C             FORMAT IS KEYWORD=VALUE                      
         B     *+12                                                             
RACTN2A  MVI   FERN,12             ERROR INVALID START                          
         B     RACTNERR                                                         
         CLI   0(R5),0                                                          
         BE    RACTN3              MISSING START BYTE                           
RACTN2B  CLI   STIFN,2             TEST TEMPSTR                                 
         BNE   RACTN2F                                                          
         CLC   12(3,R5),=C'CHK'    CHK FOR START OF CHKPNT AREA                 
         BNE   *+12                                                             
         LH    R6,TWACHKD                                                       
         B     RACTN2X                                                          
         CLC   12(3,R5),=C'GLO'    CHK FOR START OF GLOBAL AREA                 
         BNE   *+12                                                             
         LH    R6,TWAGBLD                                                       
         B     RACTN2X                                                          
RACTN2C  CLC   12(3,R5),=C'HEX'    HEX=HHHH (HEX)                               
         BNE   RACTN2F                                                          
         TM    3(R5),X'20'                                                      
         BO    *+12                                                             
         MVI   FERN,07             INVALID HEX                                  
         B     RACTNERR                                                         
         CLI   1(R5),2             HEX=XX                                       
         BNE   RACTN2D                                                          
         LA    R6,2                                                             
         LA    R7,11(R5)                                                        
         B     RACTN2E                                                          
RACTN2D  CLI   1(R5),4             HEX=XXXX                                     
         BNE   RACTN2A                                                          
         LA    R6,4                                                             
         LA    R7,10(R5)                                                        
RACTN2E  XC    8(4,R5),8(R5)                                                    
         GOTO1 AHEXIN,HEXWS,22(R5),(R7),(R6)                                    
         L     R6,8(R5)                                                         
         B     RACTN2X                                                          
RACTN2F  TM    2(R5),X'80'         ELSE WE ASSUME A DECIMAL INTEGER             
         BO    *+12                                                             
         MVI   FERN,13             ERROR START NOT NUMERIC                      
         B     RACTNERR                                                         
         L     R6,4(R5)                                                         
RACTN2X  C     R6,=F'16384'                                                     
***NOP** BH    RACTN2A                                                          
         STH   R6,STIB                                                          
         STH   R6,DISPSB                                                        
         SPACE 2                                                                
RACTN3   LA    R5,32(R5)           POINT TO END BYTE LINE                       
         OC    0(2,R5),0(R5)                                                    
         BZ    RACTNOK             NO END BYTE INFO                             
         CLI   1(R5),0                                                          
         BE    RACTN3F             NO SECOND HALF                               
         CLI   0(R5),3                                                          
         BE    *+12                                                             
RACTN3A  MVI   FERN,14             ERROR INVALID END                            
         B     RACTNERR                                                         
         CLC   12(3,R5),=C'END'    END=9999 (DEC)                               
         BE    RACTN3D                                                          
         CLC   12(3,R5),=C'HEX'    HEX=HHHH (HEX)                               
         BE    *+14                                                             
         CLC   12(3,R5),=C'LEN'    LEN=HHHH (HEX)                               
         BNE   RACTN3A                                                          
         TM    3(R5),X'20'                                                      
         BO    *+12                                                             
         MVI   FERN,07             INVALID HEX                                  
         B     RACTNERR                                                         
         CLI   1(R5),2             LEX=XX                                       
         BNE   RACTN3B                                                          
         LA    R6,2                                                             
         LA    R7,11(R5)                                                        
         B     RACTN3C                                                          
RACTN3B  CLI   1(R5),4             LEX=XXXX                                     
         BNE   RACTN3A                                                          
         LA    R6,4                                                             
         LA    R7,10(R5)                                                        
RACTN3C  XC    8(4,R5),8(R5)                                                    
         GOTO1 AHEXIN,HEXWS,22(R5),(R7),(R6)                                    
         L     R6,8(R5)                                                         
         BCTR  R6,0                                                             
         B     RACTN3E                                                          
*                                                                               
RACTN3D  TM    3(R5),X'80'                                                      
         BO    *+12                                                             
         MVI   FERN,15             ERROR END NOT NUMERIC                        
         B     RACTNERR                                                         
         L     R6,8(R5)                                                         
*                                                                               
RACTN3E  LTR   R6,R6                                                            
         BNP   RACTN3A                                                          
         LA    R7,1(R6)                                                         
         STH   R7,SLNRECL          SET NEW RECORD LEN                           
         MVI   STIRNEW,1           SET NEW RECORD LEN FLAG                      
         CLI   STIRA,2             NEW LEN ONLY FOR CHA/ADD                     
         BE    *+12                                                             
         CLI   STIRA,3                                                          
         BNE   RACTN3A                                                          
         B     RACTN3G                                                          
*                                                                               
RACTN3F  TM    2(R5),X'80'         1ST HALF ONLY                                
         BO    *+12                                                             
         MVI   FERN,15             ERROR END NOT NUMERIC                        
         B     RACTNERR                                                         
         L     R6,4(R5)                                                         
*                                                                               
RACTN3G  C     R6,=F'8367'         CHECK END VALUE IN R6                        
         BH    RACTN3A                                                          
         CH    R6,STIB                                                          
         BNL   *+12                                                             
         MVI   FERN,16             ERROR START GT END                           
         B     RACTNERR                                                         
         STH   R6,STIL                                                          
         B     RACTNOK                                                          
         SPACE 2                                                                
RACTNERR LA    R6,IRACTNH                                                       
         ST    R6,FERRS                                                         
         OI    FIND,X'01'                                                       
         B     EXIT                                                             
         SPACE 2                                                                
RACTNOK  CLC   STIB,STIFRL         MORE CHECKS ON START,END                     
         BNH   *+12                                                             
         MVI   FERN,18             ERROR START GT MAX REC LEN                   
         B     RACTNERR                                                         
         CLC   STIL,STIFRL                                                      
         BNH   *+12                                                             
         MVI   FERN,17             ERROR END GT MAX REC LEN                     
         B     RACTNERR                                                         
         OC    STIL,STIL                                                        
         BNZ   RACTNOK1                                                         
         CLI   STIFRT,1                                                         
         BNE   RACTNOK1                                                         
         MVC   STIL,STIFRL         SET STIL FOR F/L FILES                       
*                                                                               
RACTNOK1 CLI   STIRNEW,1                                                        
         BNE   RACTNOK2                                                         
         CLI   STIFRT,1                                                         
         BNE   RACTNOK2                                                         
         CLC   STIL,STIFRL                                                      
         BE    RACTNOK2                                                         
         MVI   FERN,22                                                          
         B     RACTNERR                                                         
*                                                                               
RACTNOK2 EQU   *                                                                
         EJECT                                                                  
*        ELEMENT ID IS OPTIONALLY PRESENT                                       
*                                                                               
EID      CLI   IEIDH+5,0                                                        
         BE    EIDOK               ELEMENT ID NOT INPUT                         
         CLI   STIRA,1             ONLY FOR REC DIS/CHA                         
         BE    EID2                                                             
         CLI   STIRA,2                                                          
         BE    EID2                                                             
EID1     MVI   FERN,06             ERROR INV EL ID                              
         B     EIDERR                                                           
EID2     CLI   STITFILE,0          ONLY FOR RECOVER TYPE DISPLAY                
         BNE   *+12                                                             
         CLI   STIFRT,3            ONLY FOR V/L/ELEMENT FILES                   
         BNE   EID1                                                             
         OC    STIB(4),STIB        ONLY IF NO REC START,END                     
         BNZ   EID1                                                             
*                                                                               
         L     R4,AKEYTBL                                                       
EID3     CLI   0(R4),0             SEARCH ID TABLE                              
         BE    EID1                                                             
         CLC   0(2,R4),IEIDH+8                                                  
         BE    *+12                                                             
         LA    R4,4(R4)                                                         
         B     EID3                                                             
         TM    3(R4),X'02'                                                      
         BZ    EID1                ERROR N/D FOR ELEMENTS                       
         MVC   STIEN,2(R4)                                                      
*                                                                               
EID4     CLI   STIEN,5             S,...                                        
         BE    EIDS                                                             
         CLI   STIEN,6             I,...                                        
         BE    EIDI                                                             
         CLI   STIEN,3             FI...                                        
         BE    EIDOK                                                            
         CLI   STIEN,7             LA...                                        
         BE    EIDOK                                                            
         DC    H'0'                                                             
         SPACE 2                                                                
EIDS     EQU   *                   EL ID S,...                                  
         MVC   WORD1,=4C'0'                                                     
         SR    R6,R6                                                            
         IC    R6,IEIDH+5                                                       
         SH    R6,=H'3'                                                         
         BNM   *+12                                                             
EIDS1    MVI   FERN,12             ERROR INVALID START                          
         B     EIDERR                                                           
         CH    R6,=H'3'                                                         
         BH    EIDS1                                                            
         LA    R7,WORD1+3                                                       
         SR    R7,R6                                                            
         EX    R6,*+8              RT JUST IN WORD1                             
         B     *+10                                                             
         MVC   0(0,R7),IEIDH+10                                                 
         MVC   DUB(4),=4C'0'                                                    
         MVZ   DUB(4),WORD1                                                     
         CLC   DUB(4),=4C'0'                                                    
         BE    *+12                                                             
         MVI   FERN,13             ERROR START NOT NUMERIC                      
         B     EIDERR                                                           
         PACK  DUB,WORD1                                                        
         CVB   R6,DUB                                                           
         C     R6,=F'4095'                                                      
         BH    EIDS1                                                            
         STH   R6,STIE                                                          
         B     EIDOK                                                            
         SPACE 2                                                                
EIDI     EQU   *                   EL ID I,....                                 
         SR    R6,R6                                                            
         IC    R6,IEIDH+5                                                       
         SH    R6,=H'2'                                                         
         BNZ   *+12                                                             
EIDI1    MVI   FERN,07             INV HEX                                      
         B     EIDERR                                                           
         GOTO1 AHEXIN,HEXWS,IEIDH+10,STIE,(R6)                                  
         L     R6,12(R1)                                                        
         LTR   R6,R6                                                            
         BZ    EIDI1                                                            
         STC   R6,STIEL            SAVE EL ID LENGTH                            
         B     EIDOK                                                            
         SPACE 2                                                                
EIDERR   LA    R6,IEIDH                                                         
         ST    R6,FERRS                                                         
         OI    FIND,X'01'                                                       
         B     EXIT                                                             
EIDOK    EQU   *                                                                
         EJECT                                                                  
*        ELEMENT ACTION,START,END ARE OPTIONALLY PRESENT                        
*                                                                               
EACTN    CLI   STIEN,0             MUST HAVE EL ID INPUT                        
         BNE   EACTN0                                                           
         CLI   IEACTNH+5,0                                                      
         BNE   EID1                                                             
         B     EACTNOK                                                          
EACTN0   MVI   STIEA,1             DEFAULT EL ACTION IS DIS                     
         CLI   IEACTNH+5,0                                                      
         BE    EACTN1C                                                          
*                                                                               
         XC    0(256,RC),0(RC)                                                  
         GOTO1 ASCANNER,HEXWS,(0,IEACTNH),(3,(RC))                              
         CLI   4(R1),0                                                          
         BNE   EACTN1                                                           
         MVI   FERN,05             ERROR IN SCANNER                             
         B     EACTNERR                                                         
         SPACE 2                                                                
EACTN1   LR    R5,RC               POINT TO EL ACTION LINE                      
         CLI   1(R5),0                                                          
         BE    *+12                                                             
EACTN1A  MVI   FERN,25             INVALID EL ACTION                            
         B     EACTNERR                                                         
         CLI   0(R5),0                                                          
         BE    EACTN2              MISSING EL ACTION                            
         CLI   0(R5),7                                                          
         BH    EACTN1A             EL ACTION MUST BL LE 7                       
         SR    R6,R6                                                            
         IC    R6,0(R5)                                                         
         BCTR  R6,0                R6=L'EL ACTION -1                            
         L     R7,AACTNTBL                                                      
EACTN1B  CLI   0(R7),0                                                          
         BE    EACTN1A                                                          
         EX    R6,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R7),12(R5)                                                   
         BE    *+12                                                             
         LA    R7,9(R7)                                                         
         B     EACTN1B                                                          
         TM    8(R7),X'02'                                                      
         BZ    EACTN1A             ACTION N/D FOR ELEMENT                       
         MVC   STIEA,7(R7)         SAVE EL ACTION VALUE                         
*                                                                               
EACTN1C  CLI   STIRA,1             DIS/DIS VALID PAIR                           
         BNE   EACTN1D                                                          
         CLI   STIEA,1                                                          
         BNE   EACTN1A                                                          
         CLI   IEACTNH+5,0                                                      
         BE    EACTNOK                                                          
         B     EACTN2                                                           
*                                                                               
EACTN1D  CLI   STIEA,2             CHA/CHA & CHA/ADD VALID                      
         BE    EACTN2                                                           
         CLI   STIEA,3                                                          
         BE    EACTN2                                                           
         B     EACTN1A                                                          
         SPACE 2                                                                
EACTN2   LA    R5,32(R5)           POINT TO EL START BYTE                       
         CLI   1(R5),0                                                          
         BE    *+12                                                             
EACTN2A  MVI   FERN,12             ERROR INVALID START                          
         B     EACTNERR                                                         
         CLI   0(R5),0                                                          
         BE    EACTN3              MISSING START BYTE                           
         TM    2(R5),X'80'                                                      
         BO    *+12                                                             
         MVI   FERN,13             ERROR START NOT NUMERIC                      
         B     EACTNERR                                                         
         L     R6,4(R5)                                                         
         C     R6,=F'255'                                                       
         BH    EACTN2A                                                          
         STH   R6,STIBE                                                         
         LTR   R6,R6                                                            
         BZ    EACTN3                                                           
         CLI   STIEA,1             START NONZERO ONLY FOR DIS                   
         BNE   EACTN2A                                                          
         SPACE 2                                                                
EACTN3   LA    R5,32(R5)           POINT TO EL END BYTE                         
         OC    0(2,R5),0(R5)                                                    
         BNZ   EACTN3B                                                          
         CLI   STIEA,3             MUST HAVE END FOR EL ADD                     
         BNE   EACTNOK                                                          
EACTN3A  MVI   FERN,14             ERROR INVALID END                            
         B     EACTNERR                                                         
EACTN3B  CLI   1(R5),0                                                          
         BE    EACTN3F             NO SECOND HALF                               
         CLI   0(R5),3                                                          
         BNE   EACTN3A                                                          
         CLC   12(3,R5),=C'END'    END=DEC                                      
         BE    EACTN3D                                                          
         CLC   12(3,R5),=C'LEN'    LEN=HEX                                      
         BNE   EACTN3A                                                          
         TM    3(R5),X'20'                                                      
         BO    *+12                                                             
         MVI   FERN,07             INVALID HEX                                  
         B     EACTNERR                                                         
         CLI   1(R5),2             LEN=XX                                       
         BNE   EACTN3A                                                          
         LA    R6,2                                                             
         LA    R7,11(R5)                                                        
         XC    8(4,R5),8(R5)                                                    
         GOTO1 AHEXIN,HEXWS,22(R5),(R7),(R6)                                    
         L     R6,8(R5)                                                         
         BCTR  R6,0                                                             
         B     EACTN3E                                                          
*                                                                               
EACTN3D  TM    3(R5),X'80'                                                      
         BO    *+12                                                             
         MVI   FERN,15             ERROR END NOT NUMERIC                        
         B     EACTNERR                                                         
         L     R6,8(R5)                                                         
*                                                                               
EACTN3E  C     R6,=F'1'                                                         
         BL    EACTN3A                                                          
         MVI   STIENEW,1           SET NEW EL LEN FLAG                          
         CLI   STIEA,2             NEW LEN ONLY FOR CHA/ADD                     
         BE    *+12                                                             
         CLI   STIEA,3                                                          
         BNE   EACTN3A                                                          
         B     EACTN3G                                                          
*                                                                               
EACTN3F  TM    2(R5),X'80'         1ST HALF ONLY                                
         BO    *+12                                                             
         MVI   FERN,15             ERROR END NOT NUMERIC                        
         B     EACTNERR                                                         
         L     R6,4(R5)                                                         
*                                                                               
EACTN3G  C     R6,=F'255'          CHECK END VALUE IN R6                        
         BH    EACTN3A                                                          
         CH    R6,STIBE                                                         
         BNL   *+12                                                             
         MVI   FERN,16             ERROR START GT END                           
         B     EACTNERR                                                         
         STH   R6,STILE                                                         
         B     EACTNOK                                                          
         SPACE 2                                                                
EACTNERR LA    R6,IEACTNH                                                       
         ST    R6,FERRS                                                         
         OI    FIND,X'01'                                                       
         B     EXIT                                                             
*                                                                               
EACTNOK  EQU   *                                                                
         EJECT                                                                  
*        SEARCH PERMUTATION TABLE & IF VALID STORE I/O INFO                     
*                                                                               
PERMS    MVC   STIPFKO+0(1),STIFN                                               
         MVC   STIPFKO+1(1),STIKN                                               
         MVC   STIPFKO+2(1),STIRA                                               
         L     R4,APERMTBL                                                      
PERMS1   CLI   0(R4),0                                                          
         BNE   PERMS2                                                           
         LA    R6,IFILEH                                                        
         ST    R6,FERRS                                                         
         OI    FIND,X'01'                                                       
         MVI   FERN,27                                                          
         B     EXIT                ERROR INVALID REQUEST                        
PERMS2   CLC   STIPFKO(3),0(R4)                                                 
         BE    PERMS3                                                           
         LA    R4,15(R4)                                                        
         B     PERMS1                                                           
PERMS3   MVC   STIPERM,0(R4)                                                    
         EJECT                                                                  
*        A VALID LOOKING REQUEST HAS BEEN ENTERED SO ATTEMPT TO READ            
*        RECORD TO OBTAIN RECORD LENGTH FOR FURTHER CHECKING & IF OK            
*        DISPLAY DATA                                                           
*                                                                               
         MVI   DISKIOOP,0          SET TO FIRST I/O                             
         MVI   HDRN,3                                                           
         CLI   STIP00,0                                                         
         BE    RECMISS             NO I/O REQUIRED                              
RECREAD  GOTO1 ADISKIO                                                          
         CLI   SLRI,0                                                           
         BE    RECMISS                                                          
RECR2    CLI   DISKIOOP,1                                                       
         BE    RECR3                                                            
         MVI   DISKIOOP,1          SET TO SECND I/O                             
         CLI   STIP01,0                                                         
         BE    RECR3               NO SECND I/O REQUIRED                        
         B     RECREAD                                                          
*                                                                               
RECR3    CLI   STIRA,4             RECORD READ WITH LEN=SLRL                    
         BE    RECHIT              NO CHECKING FOR BROWSE                       
         MVC   OKEYAH+8(20),WRK                                                 
         SR    R4,R4                                                            
         ICM   R4,3,SLRL                                                        
         BCTR  R4,0                R4=MAX VALUE FOR STIB/STIL                   
         CLM   R4,3,STIB                                                        
         BNL   *+12                                                             
         MVI   FERN,18             ERROR START GT REC LEN                       
         B     RACTNERR                                                         
*                                                                               
RECR4    OC    SLNRECL,SLNRECL     WAS NEW REC LEN INPUT                        
         BNZ   RECR5               YES BYPASS LENGTH CHECK                      
         CH    R4,STIL                                                          
         BNL   *+12                                                             
         MVI   FERN,17             ERROR END GT REC LEN                         
         B     RACTNERR                                                         
*                                                                               
RECR5    LH    R5,STIL             SET DISPLAY LENGTH                           
         LTR   R5,R5                                                            
         BNZ   *+6                                                              
         LR    R5,R4               NO END INPUT                                 
         SH    R5,STIB                                                          
         LA    R5,1(R5)                                                         
         STH   R5,DISPDL                                                        
         B     RECHIT                                                           
*                                                                               
RECMISS  CLI   HDRN,3                                                           
         BNE   RECMISS1                                                         
         CLI   STIRA,3                                                          
         BE    ADDREC              NOTFOUND OK FOR ADD                          
         CLI   STIRA,5                                                          
         BE    CPYREC              NOTFOUND OK FOR COPY                         
RECMISS1 SR    R6,R6                                                            
         IC    R6,HDRN                                                          
         LA    R6,28(R6)                                                        
         STC   R6,FERN             ERROR DISK/EOF/NOTFOUND                      
RECMISS2 LA    R6,IFILEH                                                        
         ST    R6,FERRS                                                         
         OI    FIND,X'01'                                                       
         B     EXIT                                                             
         EJECT                                                                  
ADDREC   OC    STIB,STIB                                                        
         BZ    ADDREC1                                                          
         MVI   FERN,19                                                          
         B     RACTNERR            ERROR START NE ZERO                          
ADDREC1  OC    STIL,STIL                                                        
         BNZ   ADDREC2                                                          
         MVI   FERN,20                                                          
         B     RACTNERR            ERROR MISSING END                            
ADDREC2  LH    R5,VWDISP                                                        
         MH    R5,VNDISP           R5=SIZE OF DISPLAY SCREEN                    
         BCTR  R5,0                                                             
         CH    R5,STIL                                                          
         BNL   ADDREC3                                                          
         MVI   FERN,24             ERROR CANT FIT ON SCREEN                     
         B     RACTNERR                                                         
ADDREC3  CLI   STIFKL,0                                                         
         BE    ADDREC7                                                          
         SR    R6,R6                                                            
         IC    R6,STIFKL                                                        
         SR    R7,R7                                                            
         IC    R7,STIFCL                                                        
         AR    R6,R7                                                            
         IC    R7,STIFSL                                                        
         AR    R6,R7                                                            
         BCTR  R6,R0               R6=MIN VALUE FOR STIL                        
         CH    R6,STIL                                                          
         BNH   ADDREC7                                                          
         MVI   FERN,23                                                          
         B     RACTNERR            ERROR END TOO SMALL                          
ADDREC7  MVI   CLEAROP,C'N'        CLEAR TWA DISPLAY AREA                       
         MVI   CLEAROP,C'T'        ONLY IF NO CLEAR AFT *****                   
         GOTO1 ACLEAR                                                           
         GOTO1 FMTNEW              FORMAT IOAREA FOR NEW REC                    
         LH    R5,STIL             SET DISPLAY LEN                              
         LA    R5,1(R5)                                                         
         STH   R5,DISPDL                                                        
         XC    DISPSLN,DISPSLN                                                  
         MVI   DISPOP,X'0E'        WHOLE DISP HEX,BYTES,CHRS                    
         GOTO1 ADISP                                                            
         MVI   HDRN,1              HDR=ENTER NEW RECORD                         
         B     EXIT                                                             
         EJECT                                                                  
CPYREC   OC    STIB,STIB                                                        
         BZ    CPYREC1                                                          
         MVI   FERN,12                                                          
         B     RACTNERR            ERROR INV START                              
CPYREC1  OC    STIL,STIL                                                        
         BZ    CPYREC2                                                          
         MVI   FERN,14                                                          
         B     RACTNERR            ERROR INV END                                
CPYREC2  MVC   SLIOAREA(L'STIK),STIK                                            
         B     EXIT                                                             
         EJECT                                                                  
RECHIT   MVI   CLEAROP,C'N'        CLEAR TWA DISPLAY AREA                       
         MVI   CLEAROP,C'T'        ONLY IF NO CLEAR AFT *****                   
         GOTO1 ACLEAR                                                           
         SR    R6,R6                                                            
         STH   R6,DISPSLN          SET START LINE NUM TO 1ST                    
         CLI   STIRA,4                                                          
         BE    BROWSE              GO TO BROWSE ROUTINE                         
         CLI   STIEN,0                                                          
         BNE   ELEMENT             GO TO ELEMENT ROUTINE                        
         OC    SLNRECL,SLNRECL     CHANGE IN FEC LEN                            
         BZ    RECH1               NO                                           
         GOTO1 FMTNEW              INSERT NEW LEN IN REC                        
         CLC   SLNRECL,SLRL        INCREASED REC LEN                            
         BNH   RECH1               NO                                           
         MVI   DISPOP,X'0E'        WHOLE DISP HEX,BYTES,CHRS                    
         GOTO1 ADISP                                                            
         CLI   DISPRES,0           DATA ALL FIT ON SCREEN                       
         BE    RECH2               YES                                          
         MVI   FERN,24                                                          
         B     RACTNERR            ERROR CANT FIT ON SCREEN                     
RECH1    MVI   DISPOP,X'0F'        PART DISP HEX,BYTES,CHRS                     
         GOTO1 ADISP                                                            
RECH2    CLI   STIRA,1                                                          
         BNE   *+12                                                             
         LA    R7,0                HDR=ENTER NEXT REQUEST                       
         B     RECHIT1                                                          
         CLI   STIRA,2                                                          
         BNE   *+12                                                             
         LA    R7,2                HDR=ENTER UPDATE                             
         B     RECHIT1                                                          
         CLI   STIRA,3                                                          
         BNE   *+12                                                             
         MVI   FERN,33                                                          
         B     RECMISS2            ERROR REC ALREADY EXISTS                     
         CLI   STIRA,5                                                          
         BNE   *+12                                                             
         MVI   FERN,33                                                          
         B     RECMISS2            ERROR REC ALREADY EXISTS                     
         DC    H'0'                                                             
RECHIT1  STC   R7,HDRN             SET HDR MSG NUM                              
         B     EXIT                                                             
         EJECT                                                                  
BROWSE   MVC   DISPSB,STIB         SET START BYTE                               
         CLC   STIB,SLRL                                                        
         BL    BROWSE0                                                          
         LH    R5,STIB             SET OUT OF RANGE                             
         LA    R5,IOAREA(R5)                                                    
         MVC   0(20,R5),=C'** REC LEN * XXXX **'                                
         MVC   13(4,R5),WRK+3                                                   
         MVC   DISPDL,=H'20'                                                    
         B     BROWSE1                                                          
*                                                                               
BROWSE0  SR    R5,R5               R5=INPUT END BYTE                            
         ICM   R5,3,STIL                                                        
         SR    R6,R6                                                            
         ICM   R6,3,SLRL                                                        
         BCTR  R6,0                R6=RECORD END BYTE                           
         LTR   R5,R5                                                            
         BNZ   *+6                                                              
         LR    R5,R6               SET INPUT TO RECORD END                      
         CR    R5,R6                                                            
         BNH   *+6                                                              
         LR    R5,R6               RECORD SHORTER THAN INPUT                    
         SH    R5,STIB                                                          
         LA    R5,1(R5)                                                         
         STH   R5,DISPDL                                                        
*                                                                               
BROWSE1  MVI   DISPOP,X'0E'        WHOLE DISP HEX,BYTES,CHRS                    
         GOTO1 ADISP                                                            
         CLI   DISPRES,1                                                        
         BNE   BROWSE2                                                          
         OC    DISPSLN,DISPSLN                                                  
         BZ    *+14                                                             
         MVC   SLIK,0(RC)          RESTORE LAST                                 
         B     BROWSEX                                                          
         LH    R5,VWDISP           FIRST WONT FIT                               
         MH    R5,VNDISP                                                        
         STH   R5,DISPDL           MAKE IT FIT                                  
         MVC   DISPSB,STIB                                                      
         XC    DISPSLN,DISPSLN                                                  
         B     BROWSE1                                                          
*                                                                               
BROWSE2  MVC   OKEYAH+8(20),WRK    BUMP TO NEXT DISPLAY LINE                    
         LH    R6,DISPSLN                                                       
         AH    R6,DISPNLR                                                       
         STH   R6,DISPSLN                                                       
         MVC   0(L'SLIK,RC),SLIK   SAVE LAST                                    
         MVC   STIPERM+6(3),=X'030202'       SET TO READ SEQ                    
         CLI   STIFT,2             I/S FILE                                     
         BE    *+8                 YES                                          
         MVI   STIPERM+8,X'01'                                                  
         GOTO1 ADISKIO             READ NEXT RECORD                             
         CLI   SLRI,0                                                           
         BNE   BROWSE                                                           
         SR    R6,R6               BROWSE TERM BY I/O ERROR                     
         IC    R6,HDRN                                                          
         LA    R6,34(R6)                                                        
         STC   R6,FERN             ERROR SIZE/DISK/EOF/NOTFND                   
         B     RECMISS2                                                         
*                                                                               
BROWSEX  MVI   HDRN,0                                                           
         B     EXIT                                                             
         EJECT                                                                  
ELEMENT  CLI   STITFILE,0          ELEMENT MODE ON RECOVERY FILE                
         BNE   *+14                                                             
         MVC   STITFILE,STIFINFO   NO USE ACTUAL FILE INFO                      
         B     EL0                                                              
         L     R4,ARCVRTBL         SEARCH LIST OF RCVRTBL FILE NUMS             
         CLI   0(R4),0                                                          
         BE    RECH1               LINEAR DISPLAY IF NOT FOUND                  
         CLC   0(1,R4),STITHDR                                                  
         BE    *+12                                                             
         LA    R4,10(R4)                                                        
         B     *-22                                                             
         MVC   STITFILE,0(R4)      USE FILE INFO IN RCVRT8L                     
         CLI   STITRT,3                                                         
         BNE   RECH1               LINEAR DISPLAY IF NO ELEMENTS                
EL0      SR    R5,R5               FIND 1ST EL WITH INPUT ID                    
         SR    R6,R6                                                            
         IC    R5,STITKL                                                        
         IC    R6,STITCL                                                        
         AR    R5,R6                                                            
         IC    R6,STITSL                                                        
         AR    R5,R6               R5=L'KEY+L'CONTROL+L'SYS                     
         STH   R5,SLEFRST          SAVE START OF FIRST EL                       
         LR    R1,R5               R1=EL LEN COUNTER                            
         MVI   WORD1,X'FF'         SET EL NOT FOUND                             
         MVI   WORD2,0             SET REC OK                                   
         LA    R5,IOAREA(R5)       R5=A(FIRST EL)                               
         CLI   STIEN,3                                                          
         BE    EL4                 FI.. ELEMENT FOUND                           
*                                                                               
EL1      CLI   STIEN,7             DID WE WANT LAST                             
         BNE   EL2                                                              
         CLI   0(R5),0                                                          
         BE    EL4                 LA.. ELEMENT FOUND                           
*                                                                               
EL2      CLI   STIEN,5             DID WE WANT S,..                             
         BNE   EL3                                                              
         LR    R7,R5                                                            
         LA    R0,IOAREA                                                        
         SR    R7,R0                                                            
         CH    R7,STIE                                                          
         BE    EL4                 S,.. ELEMENT FOUND                           
*                                                                               
EL3      CLI   STIEN,6             DID WE WANT I,..                             
         BNE   EL5                                                              
         CLI   0(R5),0                                                          
         BNE   *+8                                                              
         MVI   1(R5),1                                                          
         CLC   STIEL,1(R5)         IGNORE IF EL LT INPUT LEN                    
         BH    EL5                                                              
         IC    R6,STIEL                                                         
         BCTR  R6,0                                                             
         EX    R6,*+8              COMPARE FOR INPUT LEN                        
         B     *+10                                                             
         CLC   0(0,R5),STIE                                                     
         BNE   EL5                 I,.. ELEMENT FOUND                           
*                                                                               
EL4      ST    R5,WORD1            SAVE ADDR OF FOUND EL                        
EL5      CLI   0(R5),0                                                          
         BE    EL6                                                              
         CLI   1(R5),2                                                          
         BNL   *+12                                                             
         MVI   WORD2,1             SET INV EL LEN                               
         B     EL7                                                              
         IC    R6,1(R5)                                                         
         AR    R1,R6               BUMP EL LEN SUM                              
         LH    R7,STITRL                                                        
         LA    R7,1(R7)                                                         
         CR    R1,R7                                                            
         BNH   *+12                                                             
         MVI   WORD2,2             SET EL LEN SUM GT MAX                        
         B     EL7                                                              
         AR    R5,R6                                                            
         CLI   WORD1,X'FF'                                                      
         BE    EL1                                                              
         B     EL5                                                              
*                                                                               
EL6      CLM   R1,3,SLRL           END OF ELS FOUND                             
         BE    EL7                                                              
         LA    R1,1(R1)                                                         
         CLM   R1,3,SLRL                                                        
         BE    EL7                                                              
         MVI   WORD2,3             SET EL LEN NEQ REC LEN                       
         BCTR  R1,0                                                             
         STH   R1,WORD2+2                                                       
*                                                                               
EL7      L     R5,WORD1            ELEMENT NOT FOUND                            
         CLI   WORD1,X'FF'                                                      
         BNE   EL8                                                              
         CLI   WORD2,0                                                          
         BNE   *+12                                                             
         MVI   FERN,08             ERROR ELEMENT NOT FOUND                      
         B     EIDERR                                                           
         MVI   SLEACTN,1           SET ACTION TO DISPLAY                        
         LH    R5,SLEFRST                                                       
         LA    R5,IOAREA(R5)       SET TO FIRST ELEMENT                         
         B     ELA                                                              
*                                                                               
EL8      CLI   STIEA,1             ELEMENT FOUND                                
         BE    EL9                                                              
         CLI   WORD2,0                                                          
         BE    EL9                                                              
         MVI   SLEACTN,1           SET ACTN TO DISP IF FUNNY                    
         B     ELA                                                              
EL9      IC    R6,1(R5)            R6=OLD EL LEN                                
         MVC   SLEACTN,STIEA                                                    
ELA      MVI   SLENL,0                                                          
         MVC   SLEID,0(R5)                                                      
         LA    R0,IOAREA                                                        
         SR    R5,R0                                                            
         STH   R5,SLESTRT                                                       
         CLI   SLEACTN,1                                                        
         BE    ELDIS                                                            
*                                                                               
ELB      CLI   SLEACTN,2           CHECK CHANGE PARAMS                          
         BNE   ELD                                                              
         LH    R7,STILE                                                         
         LTR   R7,R7                                                            
         BZ    ELCHA               NO END INPUT                                 
         LA    R7,1(R7)            R7=INPUT EL LEN                              
         CLI   STIENEW,1                                                        
         BE    ELC                                                              
         CR    R7,R6                                                            
         BNH   ELCHA                                                            
         MVI   FERN,21             ERROR END GT EL LEN                          
         B     EACTNERR                                                         
ELC      STC   R7,SLENL            NEW END INPUT                                
         SR    R7,R6               R7=CHANGE IN EL LEN                          
         BNZ   *+12                                                             
         MVI   FERN,14             ERROR NO CHANGE                              
         B     EACTNERR                                                         
         SR    R0,R0                                                            
         ICM   R0,3,SLRL                                                        
         AR    R7,R0                                                            
         BCTR  R7,0                R7=NEW END BYTE FOR REC                      
         CLM   R7,3,STIFRL                                                      
         BNH   ELCHA                                                            
         MVI   FERN,17             ERROR REC LEN GT MAX                         
         B     EACTNERR                                                         
*                                                                               
ELD      CLI   SLEACTN,3           CHECK ADD PARAMS                             
         BNE   ELE                                                              
         LH    R7,STILE                                                         
         LA    R7,1(R7)                                                         
         STC   R7,SLENL                                                         
         SR    R0,R0                                                            
         ICM   R0,3,SLRL                                                        
         AR    R7,R0                                                            
         BCTR  R7,0                R7=NEW END BYTE FOR REC                      
         CLM   R7,3,STIFRL                                                      
         BNH   ELADD                                                            
         MVI   FERN,17             ERROR REC LEN GT MAX                         
         B     EACTNERR                                                         
*                                                                               
ELE      DC    H'0'                                                             
         SPACE 2                                                                
ELADD    LH    R5,SLESTRT          ADD ELEMENT                                  
         LA    R5,IOAREA(R5)                                                    
         SR    R6,R6                                                            
         IC    R6,SLENL                                                         
         SH    R6,=H'2'                                                         
         BZ    ELADD1                                                           
         MVI   0(R5),C' '          SET ELEMENT TO SPACES                        
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R5),0(R5)                                                    
ELADD1   MVI   0(R5),X'FF'         SET EL ID CODE TO FF                         
         MVC   1(1,R5),SLENL       SET EL LEN                                   
*                                                                               
ELADD2   XC    DISPSLN,DISPSLN     WHOLE DISP HEX,BYTES,CHRS                    
         MVC   DISPSB,SLESTRT                                                   
         MVI   DISPDL,0                                                         
         MVC   DISPDL+1(1),1(R5)                                                
         MVI   DISPOP,X'0E'                                                     
         GOTO1 ADISP                                                            
         MVI   HDRN,3              HDR=ENTER ELEMENT DATA                       
         B     EXIT                                                             
         SPACE 2                                                                
ELCHA    LH    R5,SLESTRT          CHANGE ELEMENT                               
         LA    R5,IOAREA(R5)                                                    
         SR    R7,R7                                                            
         IC    R7,SLEID+1          R7=OLD LEN                                   
         SR    R6,R6                                                            
         IC    R6,SLENL            R6=NEW LEN                                   
         LTR   R6,R6                                                            
         BZ    ELCHA1                                                           
         STC   R6,1(R5)            SET NEW LEN IN ELEMENT                       
         SR    R6,R7                                                            
         BNP   ELCHA1              NO INCREASE IN LEN                           
         AR    R7,R5               POINT TO END OF ELEMENT                      
         MVI   0(R7),C' '          EXTEND ELEMENT WITH SPACES                   
         SH    R6,=H'2'                                                         
         BM    ELCHA1                                                           
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R7),0(R7)                                                    
ELCHA1   B     ELADD2                                                           
         SPACE 2                                                                
ELDIS    MVI   DISPOP,X'0E'        DISPLAY ELEMENTS                             
         LA    R5,IOAREA                                                        
         AH    R5,SLESTRT                                                       
         XC    DISPSB,DISPSB       FIRST DISP KEY/CONT/SAVE                     
         MVC   DISPDL,SLEFRST                                                   
         OC    DISPDL,DISPDL                                                    
         BZ    ELDIS3                                                           
         GOTO1 ADISP                                                            
         MVC   DISPSLN,DISPNLR                                                  
         MVI   DISPOP,X'0E'                                                     
         B     ELDIS3                                                           
*                                                                               
ELDIS1   GOTO1 ADISP               DISPLAY ELEMENT                              
         TM    DISPOP,X'01'                                                     
         BO    ELDIS5              END DISP ON PART OF BIG EL                   
         CLI   DISPRES,1                                                        
         BNE   *+20                                                             
         TM    DISPOP,X'80'                                                     
         BO    ELDIS5              END DISP IF DONT FIT                         
         OI    DISPOP,X'01'                                                     
         B     ELDIS1              MAKE FIRST FIT                               
         LH    R6,DISPSLN                                                       
         AH    R6,DISPNLR                                                       
         STH   R6,DISPSLN          BUMP DISPLAY START LINE                      
*                                                                               
ELDIS2   CLI   0(R5),0                                                          
         BE    ELDIS5              END DISPLAY IF LAST EL                       
         SR    R6,R6                                                            
         IC    R6,1(R5)                                                         
         CLI   1(R5),2                                                          
         BL    ELDIS5              END DISPLAY IF FUNNY                         
         AR    R5,R6               BUMP TO NEXT EL                              
*                                                                               
ELDIS3   LR    R7,R5                                                            
         LA    R6,1                                                             
         CLI   0(R5),0                                                          
         BE    ELDIS4              DISPLAY 1 BYTE FOR LAST EL                   
         IC    R6,1(R5)                                                         
         CLI   1(R5),2                                                          
         BNL   *+12                                                             
         LA    R6,2                DISPLAY 2 BYTES IF FUNNY                     
         B     ELDIS4                                                           
         AH    R7,STIBE            AJUST BY START VALUE                         
         SH    R6,STIBE                                                         
         BNP   ELDIS2              IGNORE EL IF GT LENGTH                       
         LH    RE,STILE                                                         
         LTR   RE,RE                                                            
         BZ    ELDIS4              USE EL LEN IF NO END VALUE                   
         SH    RE,STIBE                                                         
         LA    RE,1(RE)                                                         
         CR    R6,RE               USE SMALLER OF EL & END                      
         BNH   ELDIS4                                                           
         LR    R6,RE                                                            
*                                                                               
ELDIS4   LA    RE,IOAREA           SET START & LEN OF DATA                      
         SR    R7,RE                                                            
         STH   R7,DISPSB                                                        
         STH   R6,DISPDL                                                        
         B     ELDIS1                                                           
*                                                                               
ELDIS5   CLI   WORD2,0                                                          
         BNE   *+12                                                             
         MVI   HDRN,0              END OF NORMAL DISPLAY                        
         B     EXIT                                                             
         LR    R6,RC               POINT TO SPECIAL MSG AREA                    
         XC    0(30,R6),0(R6)                                                   
         ST    R6,FERN                                                          
         MVI   FERN,0              SET SPECIAL ERROR NUM                        
         LA    R5,IFILEH                                                        
         MVC   0(14,R6),=C'**WARNING** - '                                      
         CLC   STIEA,SLEACTN                                                    
         BE    *+14                                                             
         LA    R5,IEACTNH                                                       
         MVC   0(14,R6),=C'INVALID REC - '                                      
         ST    R5,FERRS                                                         
         OI    FIND,X'01'                                                       
*                                                                               
ELDIS6   CLI   WORD2,1                                                          
         BNE   ELDIS7                                                           
         MVC   14(15,R6),=C'ELEMENT INVALID'                                    
         B     EXIT                                                             
ELDIS7   CLI   WORD2,2                                                          
         BNE   ELDIS8                                                           
         MVC   14(14,R6),=C'SUM ELS GT MAX'                                     
         B     EXIT                                                             
ELDIS8   CLI   WORD2,3                                                          
         BNE   ELDIS9                                                           
         MVC   14(13,R6),=C'RL NEQ EL=NNNN'                                     
         LH    R1,WORD2+2                                                       
         CVD   R1,DUB                                                           
         UNPK  24(4,R6),DUB                                                     
         OI    27(R6),X'F0'                                                     
         B     EXIT                                                             
ELDIS9   DC    H'0'                                                             
         EJECT                                                                  
*        THIS ROUTINE INITIALISES THE IOAREA TO A SUITABLE FORMAT FOR A         
*        NEW RECORD TO BE ADDED TO AFILE OR FOR AN EXTENSION OF AN              
*        EXISTING RECORD IN A FILE.                                             
*                                                                               
FMTNEW   NTR1                                                                   
         SR    R6,R6                                                            
         CLI   STIRA,2             AMEND                                        
         BNE   *+10                                                             
         SR    R0,R0                                                            
         ICM   R6,3,SLRL           YES                                          
         SR    R5,R5                                                            
         ICM   R5,3,STIL                                                        
         LA    R5,1(R5)                                                         
         SR    R5,R6               R5=L'FORMAT AREA                             
         BNP   FMTN4                                                            
         LA    R6,IOAREA(R6)       R6=A(FORMAT AREA)                            
FMTN1    CH    R5,=H'80'           SET RECORD TO ALL BLANKS                     
         BNH   FMTN2                                                            
         MVC   0(80,R6),BLANKS                                                  
         LA    R6,80(R6)                                                        
         SH    R5,=H'80'                                                        
         B     FMTN1                                                            
FMTN2    SH    R5,=H'1'                                                         
         BM    FMTN4                                                            
         EX    R5,FMTN3                                                         
         B     FMTN4                                                            
FMTN3    MVC   0(0,R6),BLANKS                                                   
*                                                                               
FMTN4    CLI   STIFT,2                                                          
         BE    FMTIS                                                            
         CLI   STIFT,4                                                          
         BE    FMTDAL                                                           
         CLI   STIFTL,1                                                         
         BE    FMTREQ                                                           
         B     FMTLEN                                                           
*                                  INDEX SEQUENTIAL                             
FMTIS    CLI   STIFRT,2                                                         
         BNL   FMTDAL              V/L I/S SAME AS DAL                          
         LH    R5,STIFRL           ZERO CONTROL & POINTER                       
         EX    R5,*+8                                                           
         B     *+10                                                             
         XC    IOAREA(0),IOAREA                                                 
         IC    R5,STIFKL           SET KEY TO INPUT VALUE                       
         BCTR  R5,R0                                                            
         EX    R5,*+8                                                           
         B     FMTNEWX                                                          
         MVC   IOAREA(0),STIK                                                   
*                                  DIRECT ACCESS LINKED                         
FMTDAL   CLI   STIRA,2             EXTENDING AMEND                              
         BE    FMTDAL1             YES                                          
         SR    R5,R5               ZERO CONTROL & SYSTEM                        
         SR    R6,R6                                                            
         IC    R5,STIFKL                                                        
         IC    R6,STIFCL                                                        
         AR    R5,R6                                                            
         IC    R6,STIFSL                                                        
         AR    R5,R6               R5=L'KEY+L'CONTROL+L'SYS                     
         BCTR  R5,R0                                                            
         EX    R5,*+8                                                           
         B     *+10                                                             
         XC    IOAREA(0),IOAREA                                                 
         LA    R5,1(R5)                                                         
         LH    R6,STIL                                                          
         SR    R6,R5               R6=RECLEN - HDRLEN - 1                       
         LA    R7,IOAREA(R5)                                                    
         MVI   0(R7),X'FF'         SET ELEMENT CODE TO X'FF'                    
         STC   R6,1(R7)            SET ELEMENT LEN TO REC LEN                   
         IC    R5,STIFKL           SET KEY TO INPUT VALUE                       
         BCTR  R5,R0                                                            
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   IOAREA(0),STIK                                                   
FMTDAL1  LH    R6,STIL             SET LAST BYTE TO ZERO                        
         LA    R6,IOAREA(R6)                                                    
         MVI   0(R6),0                                                          
*                                                                               
FMTLEN   CLI   STIFRLBN,X'FF'      REC LEN STORED IN REC                        
         BE    FMTNEWX             NO                                           
         SR    R5,R5               YES LOCATE POSN IN REC                       
         IC    R5,STIFRLBN                                                      
         LA    R5,IOAREA(R5)                                                    
         LH    R6,STIL                                                          
         LA    R6,1(R6)                                                         
         STH   R6,DUB                                                           
         MVC   0(2,R5),DUB         SET LENGTH IN REC                            
         B     FMTNEWX                                                          
*                                  REQUEST FILE                                 
FMTREQ   XC    IOAREA(26),IOAREA   SET REQUEST FILE HEADER                      
         CLI   STIK+1,0                                                         
         BNE   FMTREQ1                                                          
         SR    R0,R0               CONVERT BINARY REQ NUM TO ALPHA              
         IC    R0,STIK                                                          
         CVD   R0,DUB                                                           
         UNPK  STIK(2),DUB                                                      
         OI    STIK+1,X'F0'                                                     
FMTREQ1  MVI   IOAREA+15,1                                                      
         MVC   IOAREA+26(2),STIK                                                
         B     FMTNEWX                                                          
*                                                                               
FMTNEWX  MVC   SLIOAREA,IOAREA     SAVE 1ST PART OF RECORD                      
         XIT1                                                                   
         EJECT                                                                  
EXIT     XMOD1 1                                                                
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE SRPFMSAVE                                                      
       ++INCLUDE SRPFMTEMP                                                      
       ++INCLUDE DDFLDIND                                                       
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009SRPFM01   08/22/00'                                      
         END                                                                    
